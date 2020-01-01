use super::*;
use std::{
    error, fmt,
    ops::{Deref, DerefMut},
};

/// The atomic value.
///
/// `Kserd` contains an optional identity (`id`), and the [`Value`]. Taken together the `Kserd` object can
/// describe (almost) all Rust data structures. The identity can be mostly ignored for self-explanatory
/// data such as primitives. It can be used to name structures, and plays a special
/// role in unit structs and enum variants.
///
/// `Kserd` dereferences mutably to [`Value`] so all reading and mutating methods on [`Value`] can
/// use directly. It is encouraged to use the methods rather than accessing the fields directly.
///
/// `Kserd` implements ordering and equality, it is important to note that ordering and equality is
/// done **only on the value**, the identity is ignored.
///
/// # Example
/// Use the methods to quickly see the data if the type is known. Mutating can be done directly.
/// ```rust
/// # use kserd::*;
/// let mut kserd = Kserd::new_str("Hi");
///
/// // access the data without needing to pattern match
/// assert_eq!(kserd.str(), Some("Hi"));
/// // using ctors will name your ds for you
/// assert_eq!(kserd.id(), Some("str"));
/// // if not of the type, nothing returned.
/// assert_eq!(kserd.int(), None);
///
/// kserd.str_mut().map(|s| {
///     s.pop();
///     s.push_str("ello, world!");
/// }); // mutate directly.
///     // this method has the additional effect of cloning the string.
/// assert_eq!(kserd.str(), Some("Hello, world!"));
/// ```
///
/// [`Value`]: crate::Value
pub struct Kserd<'a> {
    /// The _identity_ of the value.
    ///
    /// This can be optional.
    /// Typically the identity is something like the struct or enum name.
    pub id: Option<Kstr<'a>>,
    /// The [`Value`].
    ///
    /// This store the actual value, which comprises of primitive types or more complex
    /// nested types such as sequences or maps.
    ///
    /// [`Value`]: crate::Value
    pub val: Value<'a>,
}

/// Static lifetime constructors. These do not need a [`Serialize`] trait.
///
/// [`Serialize`]: crate::encode::Serialize
impl Kserd<'static> {
    /// A new unit value `()`.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let kserd = Kserd::new_unit();
    /// assert_eq!(kserd.unit(), true);
    /// assert_eq!(kserd.id(), None);
    /// ```
    pub fn new_unit() -> Self {
        Kserd::new(Value::Unit)
    }

    /// A new boolean value.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let kserd = Kserd::new_bool(true);
    /// assert_eq!(kserd.bool(), Some(true));
    /// assert_eq!(kserd.id(), Some("bool"));
    /// ```
    pub fn new_bool(value: bool) -> Self {
        Kserd::with_id_unchk("bool", Value::Bool(value))
    }

    /// A new number value. The trait [`NumberType`] is implemented on all Rust primitive
    /// numbers so number literals can be used.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let kserd = Kserd::new_num(123456);
    /// assert_eq!(kserd.uint(), Some(123456));
    ///
    /// let kserd = Kserd::new_num(-123456);
    /// assert_eq!(kserd.int(), Some(-123456));
    ///
    /// let kserd = Kserd::new_num(3.14);
    /// assert_eq!(kserd.float(), Some(3.14));
    /// ```
    ///
    /// [`NumberType`]: crate::ds::num::NumberType
    pub fn new_num<T: NumberType>(value: T) -> Self {
        Kserd::with_id_unchk(value.identity(), Value::new_num(value))
    }

    /// A new string value. The ownership of the string is transferred and as such
    /// the `Kserd` has a _static_ lifetime.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let kserd = Kserd::new_string(String::from("Hello, world!"));
    /// assert_eq!(kserd.str(), Some("Hello, world!"));
    /// assert_eq!(kserd.id(), Some("String"));
    /// ```
    pub fn new_string(string: String) -> Self {
        Kserd::with_id_unchk("String", Value::new_string(string))
    }

    /// A new byte array value. The ownership of the vector is transferred and as such the `Kserd`
    /// has a _static_ lifetime.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let kserd = Kserd::new_barrv(vec![0,1,2,3]);
    /// assert_eq!(kserd.barr(), Some([0,1,2,3].as_ref()));
    /// assert_eq!(kserd.id(), Some("ByteVec"));
    /// ```
    pub fn new_barrv(byte_array: Vec<u8>) -> Self {
        Kserd::with_id_unchk("ByteVec", Value::new_barrv(byte_array))
    }
}

/// General lifetime contructors. These _do not need_ a [`Serialize`] trait.
///
/// [`Serialize`]: crate::encode::Serialize
impl<'a> Kserd<'a> {
    /// A new `Kserd` with the specified [`Value`].
    /// No identity is ascribed to the `Kserd`.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let kserd = Kserd::new(Value::Unit);
    /// assert_eq!(kserd.val, Value::Unit);
    /// assert_eq!(kserd.id, None);
    /// ```
    ///
    /// [`Value`]: crate::Value
    pub fn new(value: Value<'a>) -> Self {
        Self {
            id: None,
            val: value,
        }
    }

    /// A new `Kserd` with a specified identity and [`Value`].
    ///
    /// The identity is taken as a [`Kstr`], meaning it can be a reference or owned.
    /// `String` and `&str` can be used as the identity. The identity must also be
    /// valid, that is, it cannot contain certain characters or `Err` will be returned.
    ///
    /// > It is recommended to construct `Kserd` in this manner rather than manually
    /// via setting the `id` and `val` fields as an invalid identity will not parse back
    /// from text.
    ///  
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let kserd = Kserd::with_id("an-identity", Value::Bool(true)).unwrap();
    /// assert_eq!(kserd.bool(), Some(true));
    /// assert_eq!(kserd.id(), Some("an-identity"));
    ///
    /// let kserd = Kserd::with_id("<an,> in(valid.)/{identity}\\=", Value::Unit);
    /// assert_eq!(kserd.is_err(), true);
    /// ```
    ///
    /// [`Value`]: crate::Value
    /// [`Kstr`]: crate::Kstr
    pub fn with_id<S: Into<Kstr<'a>>>(identity: S, value: Value<'a>) -> Result<Self, InvalidId> {
        const INVALID: &str = "(){}[]<> ,./\\=";
        let id = identity.into();

        if id.chars().any(|c| INVALID.contains(c)) {
            Err(InvalidId(format!(
                "identity '{}' contains invalid characters. Invalid characters: '{}'",
                id, INVALID
            )))
        } else {
            Ok(Self {
                id: Some(id),
                val: value,
            })
        }
    }

    /// Much like `with_id` but does _not_ check the identity for validity.
    pub(crate) fn with_id_unchk<S: Into<Kstr<'a>>>(identity: S, value: Value<'a>) -> Self {
        Self {
            id: Some(identity.into()),
            val: value,
        }
    }

    /// A new string value. The `Kserd` borrows the string and has the same lifetime.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let kserd = Kserd::new_str("Hello, world!");
    /// assert_eq!(kserd.str(), Some("Hello, world!"));
    /// assert_eq!(kserd.id(), Some("str"));
    /// ```
    pub fn new_str(string: &'a str) -> Self {
        Kserd::with_id_unchk("str", Value::new_str(string))
    }

    /// A new byte array value. The `Kserd` borrows the array and has the same lifetime.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let kserd = Kserd::new_barr([0,1,2,5,10].as_ref());
    /// assert_eq!(kserd.barr(), Some([0,1,2,5,10].as_ref()));
    /// assert_eq!(kserd.id(), Some("barr"));
    /// ```
    pub fn new_barr(byte_array: &'a [u8]) -> Self {
        Kserd::with_id_unchk("barr", Value::new_barr(byte_array))
    }

    /// Construct a new container `Kserd` from a list of field-value pairs.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let pass = Kserd::new_cntr(vec![
    ///     ("a", Kserd::new_num(0))
    /// ]).unwrap();
    ///
    /// let fail = Kserd::new_cntr(vec![
    ///     ("1 wrong/name", Kserd::new_num(0))
    /// ]);
    /// assert_eq!(fail.is_err(), true);
    /// ```
    pub fn new_cntr<I, S>(iter: I) -> Result<Self, InvalidFieldName>
    where
        S: Into<Kstr<'a>>,
        I: IntoIterator<Item = (S, Kserd<'a>)>,
    {
        Ok(Kserd::new(Value::new_cntr(iter)?))
    }

    /// Construct a new map value from a list of key-value pairs.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let kserd = Kserd::new_map(vec![
    ///     (Kserd::new_unit(), Kserd::new_num(0))
    /// ]);
    /// ```
    pub fn new_map<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (Kserd<'a>, Kserd<'a>)>,
    {
        Kserd::new(Value::new_map(iter))
    }
}

impl<'a> Kserd<'a> {
    /// The identity. Same as the `.id` field but mapped as a `&str`.
    ///
    /// # Example
    ///	```rust
    /// # use kserd::*;
    /// let kserd = Kserd::with_id("Hello", Value::Unit).unwrap();
    /// assert_eq!(kserd.id(), Some("Hello"));
    /// ```
    pub fn id(&self) -> Option<&str> {
        self.id.as_ref().map(|x| x.as_str())
    }
}

impl<'a> Deref for Kserd<'a> {
    type Target = Value<'a>;
    fn deref(&self) -> &Value<'a> {
        &self.val
    }
}

impl<'a> DerefMut for Kserd<'a> {
    fn deref_mut(&mut self) -> &mut Value<'a> {
        &mut self.val
    }
}

/// Conversions.
impl<'a> Kserd<'a> {
    /// Clones all data to make a static `Kserd`.
    pub fn to_owned(self) -> Kserd<'static> {
        let Kserd { id, val } = self;

        let id = id.map(|x| x.to_owned());

        let val = val.to_owned();

        Kserd { id, val }
    }

    /// Makes a copy of this `Kserd` that references data in the this `Kserd`.
    ///
    /// This is particularly useful if you want to gaurantee that _all_ data is of the borrowed
    /// variety when decoding back to a data structure (see [`Decoder`] for explanation).
    ///
    /// There is a performance penalty as nested structures have to be rebuilt.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let kserd = Kserd::new_string("Hello, world!".to_owned());
    /// let brwed = kserd.mk_brw();
    /// assert_eq!(kserd, brwed);
    /// ```
    ///
    /// [`Decoder`]: crate::encode::Decoder
    pub fn mk_brw<'b>(&'b self) -> Kserd<'b> {
        let id = self.id().map(Kstr::brwed);
        let val = self.val.mk_brw();
        Kserd { id, val }
    }
}

impl<'a> PartialEq for Kserd<'a> {
    fn eq(&self, other: &Kserd) -> bool {
        self.val == other.val // equality ignores identity, only value based
    }
}

impl<'a> Eq for Kserd<'a> {}

impl<'a> PartialOrd for Kserd<'a> {
    fn partial_cmp(&self, other: &Kserd<'a>) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for Kserd<'a> {
    fn cmp(&self, other: &Kserd<'a>) -> std::cmp::Ordering {
        self.val.cmp(&other.val)
    }
}

impl<'a> Clone for Kserd<'a> {
    fn clone(&self) -> Self {
        let id = self.id.clone();

        let val = self.val.clone();

        Kserd { id, val }
    }
}

impl<'a> fmt::Debug for Kserd<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        struct Id<'b>(Option<&'b str>);

        impl<'b> fmt::Debug for Id<'b> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                if let Some(id) = self.0 {
                    write!(f, "Some({:?})", id)
                } else {
                    write!(f, "None")
                }
            }
        }
        f.debug_struct("Kserd")
            .field("id", &Id(self.id()))
            .field("val", &self.val)
            .finish()
    }
}

/// The id has invalid characters and would not parse back.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InvalidId(String);

impl error::Error for InvalidId {}

impl fmt::Display for InvalidId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "id contains invalid characters: {}", self.0)
    }
}
