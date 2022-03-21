use super::*;
use crate::{Fields, List, Map};
use std::{
    borrow::{Borrow, Cow},
    error, fmt,
    ops::{Deref, DerefMut},
};

/// The value of a [`Kserd`].
///
/// `Value` captures primitive types (units, booleans, numbers, strings, byte arrays), which
/// act as leaves, along with nested structures (tuples, containers, sequences, and maps).
///
/// _Sequences_ and _maps_ are simply aliases for vectors/lists, and hashmaps/dictionaries
/// respectively. Keyed structures are backed by [`BTreeMap`] so keys are lexiographically sorted.
/// _Tuples_ and _containers_ are aliases for tuples/newtype structures and enums, and structures
/// respectively.
///
/// `Value` tries to avoid owning data, especially larger data such as strings and byte arrays.
/// The use of clone-on-write smart pointers are used to facilitate borrowing until mutation is
/// required.
/// `Value` has numerous convenience methods to facilitate constructing, reading, and mutating an
/// object.
///
/// # Examples
/// Use the methods to quickly see the data if the type is known. Mutating can be done directly.
/// ```rust
/// # use kserd::*;
/// let mut val = Value::new_str("Hi");
/// val.str_mut().map(|s| {
///     s.pop();
///     s.push_str("ello, world!");
/// });
/// assert_eq!(val.str(), Some("Hello, world!"));
/// ```
///
/// [`BTreeMap`]: std::collections::BTreeMap
/// [`Kserd`]: crate::Kserd
#[derive(Eq, PartialOrd, Ord)]
pub enum Value<'a> {
    /// A unit value `()`.
    Unit,
    /// A boolean value.
    Bool(bool),
    /// A numerical value. See [`Number`].
    ///
    /// [`Number`]: crate::Number
    Num(Number),
    /// A string value.
    /// Can be borrowed or owned.
    /// See [`Kstr`].
    ///
    /// [`Kstr`]: crate::Kstr
    Str(Kstr<'a>),
    /// A byte array value.
    /// Can be borrowed or owned.
    /// See [`Barr`].
    ///
    /// [`Barr`]: crate::Barr
    Barr(Barr<'a>),
    /// A tuple value.
    ///
    /// A tuple is a _hetrogeneous sequence of unnamed objects_.
    /// It shares the same in-memory representation as a sequence but has different formatting
    /// syntax. Tuples represent not only anonymous tuples (`(u32, u32, u32)`) but also newtype structs and
    /// enums (ie `Point3d(u32, u32, u32)`).
    Tuple(List<'a>),
    /// A container value.
    ///
    /// A container is a _hetrogeneous collection of named objects_.
    /// Containers are akin to structs.
    Cntr(Fields<'a>),
    /// A sequence of values.
    ///
    /// A sequence is a _homogeneous sequence of unnamed objects_.
    Seq(List<'a>),
    /// A map of values.
    ///
    /// A map is a _homogeneous mapping of keys to values_.
    Map(Map<'a>),
}

/// Equality is done per variant, _except for when a tuple and container are both empty_.
///
/// # Example
/// ```rust
/// # use kserd::*;
/// assert_eq!(
///     Value::Tuple(vec![]),
///     Value::new_cntr(<Vec<(&str,_)>>::new()).unwrap()
/// );
/// ```
impl<'a> PartialEq for Value<'a> {
    fn eq(&self, b: &Self) -> bool {
        use Value::*;
        match (self, b) {
            (Tuple(a), Cntr(b)) if a.is_empty() && b.is_empty() => true,
            (Cntr(a), Tuple(b)) if a.is_empty() && b.is_empty() => true,
            (Unit, Unit) => true,
            (Bool(a), Bool(b)) => a == b,
            (Num(a), Num(b)) => a == b,
            (Str(a), Str(b)) => a == b,
            (Barr(a), Barr(b)) => a == b,
            (Tuple(a), Tuple(b)) => a == b,
            (Cntr(a), Cntr(b)) => a == b,
            (Seq(a), Seq(b)) => a == b,
            (Map(a), Map(b)) => a == b,
            _ => false,
        }
    }
}

/// The field name in a container contains invalid characters.
#[derive(PartialEq, Debug, Eq, Clone)]
pub struct InvalidFieldName(pub Cow<'static, str>);

impl InvalidFieldName {
    fn validate(s: &str) -> Result<(), Self> {
        let invalid = " /\\\"\t'";
        if s.is_empty() {
            Err(InvalidFieldName(Cow::Borrowed("name is empty")))
        } else if s.chars().next().unwrap().is_ascii_digit() {
            Err(InvalidFieldName(Cow::Borrowed(
                "name can not begin with digit",
            )))
        } else {
            for c in s.chars() {
                if invalid.contains(c) {
                    return Err(InvalidFieldName(Cow::Owned(format!(
                        "invalid character '{}' exists in name '{}'",
                        c, s
                    ))));
                }
            }
            Ok(())
        }
    }
}

impl error::Error for InvalidFieldName {}

impl fmt::Display for InvalidFieldName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid field name: {}", self.0)
    }
}

/// Static lifetime constructors.
impl Value<'static> {
    /// A new number value. The trait [`NumberType`] is implemented on all Rust primitive
    /// numbers so number literals can be used.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::new_num(123456);
    /// assert_eq!(value.uint(), Some(123456));
    ///
    /// let value = Value::new_num(-123456);
    /// assert_eq!(value.int(), Some(-123456));
    ///
    /// let value = Value::new_num(3.14);
    /// assert_eq!(value.float(), Some(3.14));
    /// ```
    pub fn new_num<T: NumberType>(value: T) -> Self {
        Value::Num(value.into())
    }

    /// A new string value. The ownership of the string is transferred and as such
    /// the `Value` has a _static_ lifetime.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::new_string(String::from("Hello, world!"));
    /// assert_eq!(value.str(), Some("Hello, world!"));
    /// ```
    pub const fn new_string(string: String) -> Self {
        Value::Str(Kstr::owned(string))
    }

    /// A new byte array value. The ownership of the vector is transferred and as such the `Value`
    /// has a _static_ lifetime.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::new_barrv(vec![0,1,2,3]);
    /// assert_eq!(value.barr(), Some([0,1,2,3].as_ref()));
    /// ```
    pub const fn new_barrv(byte_array: Vec<u8>) -> Self {
        Value::Barr(Barr::owned(byte_array))
    }
}

/// General lifetime constructors.
impl<'a> Value<'a> {
    /// A new string value. The `Value` borrows the string and has the same lifetime.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::new_str("Hello, world!");
    /// assert_eq!(value.str(), Some("Hello, world!"));
    /// ```
    pub const fn new_str(string: &'a str) -> Self {
        Value::Str(Kstr::brwed(string))
    }

    /// A new byte array value. The `Value` borrows the array and has the same lifetime.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::new_barr([0,1,2,5,10].as_ref());
    /// assert_eq!(value.barr(), Some([0,1,2,5,10].as_ref()));
    /// ```
    pub const fn new_barr(byte_array: &'a [u8]) -> Self {
        Value::Barr(Barr::brwed(byte_array))
    }

    /// Construct a new container value from a list of field-value pairs.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let pass = Value::new_cntr(vec![
    ///     ("a", Kserd::new_num(0))
    /// ]).unwrap();
    ///
    /// let fail = Value::new_cntr(vec![
    ///     ("1 wrong/name", Kserd::new_num(0))
    /// ]);
    /// assert_eq!(fail.is_err(), true);
    /// ```
    pub fn new_cntr<I, S>(iter: I) -> Result<Self, InvalidFieldName>
    where
        S: Into<Kstr<'a>>,
        I: IntoIterator<Item = (S, Kserd<'a>)>,
    {
        let map: BTreeMap<_, _> = iter.into_iter().map(|(k, v)| (k.into(), v)).collect();
        for (k, _) in map.iter() {
            InvalidFieldName::validate(k)?;
        }

        Ok(Value::Cntr(map))
    }

    /// Construct a new map value from a list of key-value pairs.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::new_map(vec![
    ///     (Kserd::new_unit(), Kserd::new_num(0))
    /// ]);
    /// ```
    pub fn new_map<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (Kserd<'a>, Kserd<'a>)>,
    {
        let map = BTreeMap::from_iter(iter);
        Value::Map(map)
    }
}

/// Convenience methods for accessing values straight from the [`Value`] enum.
impl<'a> Value<'a> {
    /// `Value` is a unit value (`Value::Unit`).
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::Unit;
    /// assert_eq!(value.unit(), true);
    /// ```
    pub fn unit(&self) -> bool {
        matches!(&self, Value::Unit)
    }

    /// `Value` is a boolean value.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::Bool(true);
    /// assert_eq!(value.bool(), Some(true));
    /// ```
    pub fn bool(&self) -> Option<bool> {
        match &self {
            Value::Bool(val) => Some(*val),
            _ => None,
        }
    }

    /// `Value` is a boolean value. Can be altered.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let mut value = Value::Bool(false);
    /// value.bool_mut().map(|x| *x = true);
    /// assert_eq!(value.bool(), Some(true));
    /// ```
    pub fn bool_mut(&mut self) -> Option<&mut bool> {
        match self {
            Value::Bool(val) => Some(val),
            _ => None,
        }
    }

    /// `Value` is a string with a _single_ character.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::new_str("A");
    /// assert_eq!(value.ch(), Some('A'));
    ///
    /// let value = Value::new_str("Hello, world!");
    /// assert_eq!(value.ch(), None);
    /// ```
    pub fn ch(&self) -> Option<char> {
        match &self {
            Value::Str(val) if val.chars().count() == 1 => val.chars().next(),
            _ => None,
        }
    }

    /// `Value` is an unsigned integer.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::new_num(123456);
    /// assert_eq!(value.uint(), Some(123456));
    /// ```
    pub fn uint(&self) -> Option<u128> {
        match &self {
            Value::Num(val) => val.as_u128().ok(),
            _ => None,
        }
    }

    /// `Value` is a signed integer. A positive integer can be both signed and unsigned
    /// up to `i128::max_value()`.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::new_num(-123456);
    /// assert_eq!(value.int(), Some(-123456));
    /// ```
    pub fn int(&self) -> Option<i128> {
        match &self {
            Value::Num(val) => val.as_i128().ok(),
            _ => None,
        }
    }

    /// `Value` is a floating point number. Both signed and unsigned integers can be represented
    /// as floats.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::new_num(-3.14);
    /// assert_eq!(value.float(), Some(-3.14));
    /// ```
    pub fn float(&self) -> Option<f64> {
        match &self {
            Value::Num(val) => Some(val.as_f64()),
            _ => None,
        }
    }

    /// `Value` is a numerical value, and can be altered.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let mut value = Value::new_num(123456);
    /// value.num_mut().map(|x| *x = Number::from(100));
    /// assert_eq!(value.uint(), Some(100));
    /// ```
    pub fn num_mut(&mut self) -> Option<&mut Number> {
        match self {
            Value::Num(val) => Some(val),
            _ => None,
        }
    }

    /// `Value` is a string.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::new_str("Hello, world!");
    /// assert_eq!(value.str(), Some("Hello, world!"));
    /// ```
    pub fn str(&self) -> Option<&str> {
        match &self {
            Value::Str(val) => Some(val.as_str()),
            _ => None,
        }
    }

    /// `Value` is a string. Can be altered.
    ///
    /// **Clones string value if not owned.**
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let mut value = Value::new_str("Hello");
    /// value.str_mut().map(|x| { x.push_str(", world!"); });
    /// assert_eq!(value.str(), Some("Hello, world!"));
    /// ```
    pub fn str_mut(&mut self) -> Option<&mut String> {
        match self {
            Value::Str(val) => Some(val.to_mut()),
            _ => None,
        }
    }

    /// `Value` is a byte array.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::new_barr([0,1,2,5,10].as_ref());
    /// assert_eq!(value.barr(), Some([0,1,2,5,10].as_ref()));
    /// ```
    pub fn barr(&self) -> Option<&[u8]> {
        match &self {
            Value::Barr(barr) => Some(barr.as_bytes()),
            _ => None,
        }
    }

    /// `Value` is a byte array. Can be altered.
    ///
    /// **Clones data if not already owned.**
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let mut value = Value::new_barr([0,1,2].as_ref());
    /// value.barr_mut().map(|x| { x.push(3); });
    /// assert_eq!(value.barr(), Some([0,1,2,3].as_ref()));
    /// ```
    pub fn barr_mut(&mut self) -> Option<&mut Vec<u8>> {
        match self {
            Value::Barr(val) => Some(val.to_mut()),
            _ => None,
        }
    }

    /// `Value` is a tuple.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::Tuple(vec![Kserd::new_num(101)]);
    /// assert_eq!(value.tuple(), Some(&vec![Kserd::new_num(101)]));
    /// ```
    pub fn tuple(&self) -> Option<&List<'a>> {
        match self {
            Value::Tuple(val) => Some(val),
            _ => None,
        }
    }

    /// `Value` is a tuple. Can be altered.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let mut value = Value::Tuple(vec![Kserd::new_num(101)]);
    /// value.tuple_mut().map(|x| x.push(Kserd::new_str("Hello")));
    /// assert_eq!(
    ///     value.tuple(),
    ///     Some(&vec![Kserd::new_num(101), Kserd::new_str("Hello")])
    /// );
    /// ```
    pub fn tuple_mut(&mut self) -> Option<&mut List<'a>> {
        match self {
            Value::Tuple(val) => Some(val),
            _ => None,
        }
    }

    /// `Value` is a container.
    ///
    /// Returns an [`Accessor`] into the fields, which has utility methods for accessing the
    /// fields.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::new_cntr(vec![("a", Kserd::new_num(101))]).unwrap();
    /// let accessor = value.cntr().unwrap();
    /// assert_eq!(accessor.get_num("a"), Some(101.into()));
    /// ```
    pub fn cntr(&self) -> Option<Accessor<&Fields<'a>>> {
        match self {
            Value::Cntr(fields) => Some(Accessor(fields)),
            _ => None,
        }
    }

    /// `Value` is a container. Can be altered.
    ///
    /// Returns an [`Accessor`] into the fields, which has utility methods for accessing the
    /// fields.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let mut value = Value::new_cntr(vec![("a", Kserd::new_num(101))]).unwrap();
    /// let mut accessor = value.cntr_mut().unwrap();
    /// accessor.get_num_mut("a").map(|x| *x = 1.into());
    /// assert_eq!(value, Value::new_cntr(vec![("a", Kserd::new_num(1))]).unwrap());
    /// ```
    pub fn cntr_mut(&mut self) -> Option<Accessor<&mut Fields<'a>>> {
        match self {
            Value::Cntr(fields) => Some(Accessor(fields)),
            _ => None,
        }
    }

    /// `Value` is a sequence.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::Seq(vec![Kserd::new_num(101)]);
    /// assert_eq!(value.seq(), Some(&vec![Kserd::new_num(101)]));
    /// ```
    pub fn seq(&self) -> Option<&List<'a>> {
        match self {
            Value::Seq(val) => Some(val),
            _ => None,
        }
    }

    /// `Value` is a sequence. Can be altered.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let mut value = Value::Seq(vec![Kserd::new_num(101)]);
    /// value.seq_mut().map(|x| x.push(Kserd::new_str("Hello")));
    /// assert_eq!(
    ///     value.seq(),
    ///     Some(&vec![Kserd::new_num(101), Kserd::new_str("Hello")])
    /// );
    /// ```
    pub fn seq_mut(&mut self) -> Option<&mut List<'a>> {
        match self {
            Value::Seq(val) => Some(val),
            _ => None,
        }
    }

    /// `Value` is a tuple or a sequence.
    ///
    /// As tuple and sequence share the same backing store, testing for either can be useful.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::Seq(vec![Kserd::new_num(101)]);
    /// assert_eq!(value.tuple_or_seq(), Some(&vec![Kserd::new_num(101)]));
    /// ```
    pub fn tuple_or_seq(&self) -> Option<&List<'a>> {
        match self {
            Value::Tuple(x) | Value::Seq(x) => Some(x),
            _ => None,
        }
    }

    /// `Value` is a tuple or a sequence. Can be altered.
    ///
    /// As tuple and sequence share the same backing store, testing for either can be useful.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let mut value = Value::Tuple(vec![Kserd::new_num(101)]);
    /// value.tuple_or_seq_mut().map(|x| x.push(Kserd::new_str("Hello")));
    /// assert_eq!(
    ///     value.tuple_or_seq(),
    ///     Some(&vec![Kserd::new_num(101), Kserd::new_str("Hello")])
    /// );
    /// ```
    pub fn tuple_or_seq_mut(&mut self) -> Option<&mut List<'a>> {
        match self {
            Value::Tuple(x) | Value::Seq(x) => Some(x),
            _ => None,
        }
    }

    /// `Value` is a map.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::new_map(vec![(
    ///     Kserd::new_str("a"),
    ///     Kserd::new_num(3.14)
    /// )]);
    /// let num = value.map().and_then(|map| {
    ///     map.get(&Kserd::new_str("a"))
    /// });
    /// assert_eq!(num, Some(&Kserd::new_num(3.14)));
    /// ```
    pub fn map(&self) -> Option<&Map<'a>> {
        match self {
            Value::Map(val) => Some(val),
            _ => None,
        }
    }

    /// `Value` is a map. Can be altered.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let mut value = Value::new_map(vec![(
    ///     Kserd::new_str("a"),
    ///     Kserd::new_num(3.14)
    /// )]);
    ///
    /// value.map_mut().and_then(|map| {
    ///     map.get_mut(&Kserd::new_str("a"))
    /// }).map(|x| *x = Kserd::new_str("Hello"));
    ///
    /// let expect = Value::new_map(vec![(
    ///     Kserd::new_str("a"),
    ///     Kserd::new_str("Hello")
    /// )]);
    ///
    /// assert_eq!(value, expect);
    /// ```
    pub fn map_mut(&mut self) -> Option<&mut Map<'a>> {
        match self {
            Value::Map(val) => Some(val),
            _ => None,
        }
    }
}

/// Conversions.
impl<'a> Value<'a> {
    /// Clones all data to make a static `Value`.
    pub fn into_owned(self) -> Value<'static> {
        match self {
            Value::Unit => Value::Unit,
            Value::Bool(v) => Value::Bool(v),
            Value::Num(v) => Value::Num(v),
            Value::Str(s) => Value::Str(s.into_owned()),
            Value::Barr(b) => Value::Barr(b.into_owned()),
            Value::Tuple(seq) => Value::Tuple({
                let mut v = Vec::with_capacity(seq.len());
                for i in seq {
                    v.push(i.into_owned())
                }
                v
            }),
            Value::Cntr(map) => Value::Cntr({
                let mut m = BTreeMap::new();
                for (k, v) in map {
                    m.insert(k.into_owned(), v.into_owned());
                }
                m
            }),
            Value::Seq(seq) => Value::Seq({
                let mut v = Vec::with_capacity(seq.len());
                for i in seq {
                    v.push(i.into_owned())
                }
                v
            }),
            Value::Map(map) => Value::Map({
                let mut m = BTreeMap::new();
                for (k, v) in map {
                    m.insert(k.into_owned(), v.into_owned());
                }
                m
            }),
        }
    }

    /// Makes a copy of this `Value` that references data in the this `Value`.
    ///
    /// This is particularly useful if you want to gaurantee that _all_ data is of the borrowed
    /// variety when decoding back to a data structure (see [`Decoder`] for explanation).
    ///
    /// There is a performance penalty as nested structures have to be rebuilt.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let value = Value::new_string("Hello, world!".to_owned());
    /// let brwed = value.mk_brw();
    /// assert_eq!(value, brwed);
    /// ```
    ///
    /// [`Decoder`]: crate::encode::Decoder
    pub fn mk_brw(&self) -> Value {
        match &self {
            Value::Unit => Value::Unit,
            Value::Bool(v) => Value::Bool(*v),
            Value::Num(v) => Value::Num(*v),
            Value::Str(v) => Value::Str(Kstr::brwed(v.as_str())),
            Value::Barr(v) => Value::Barr(Barr::brwed(v.as_bytes())),
            Value::Tuple(seq) => Value::Tuple({
                let mut v = Vec::with_capacity(seq.len());
                for i in seq {
                    v.push(i.mk_brw());
                }
                v
            }),
            Value::Cntr(map) => Value::Cntr({
                let mut m = BTreeMap::new();
                for (k, v) in map {
                    m.insert(Kstr::brwed(k.as_str()), v.mk_brw());
                }
                m
            }),
            Value::Seq(seq) => Value::Seq({
                let mut v = Vec::with_capacity(seq.len());
                for i in seq {
                    v.push(i.mk_brw());
                }
                v
            }),
            Value::Map(map) => Value::Map({
                let mut m = BTreeMap::new();
                for (k, v) in map {
                    m.insert(k.mk_brw(), v.mk_brw());
                }
                m
            }),
        }
    }
}

impl<'a> Clone for Value<'a> {
    fn clone(&self) -> Self {
        match &self {
            Value::Unit => Value::Unit,
            Value::Bool(v) => Value::Bool(*v),
            Value::Num(v) => Value::Num(*v),
            Value::Str(s) => Value::Str(s.clone()),
            Value::Barr(b) => Value::Barr(b.clone()),
            Value::Tuple(seq) => Value::Tuple(seq.clone()),
            Value::Cntr(map) => Value::Cntr(map.clone()),
            Value::Seq(seq) => Value::Seq(seq.clone()),
            Value::Map(map) => Value::Map(map.clone()),
        }
    }
}

impl<'a> fmt::Debug for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "Unit"),
            Value::Bool(v) => write!(f, "Bool({})", v),
            Value::Num(v) => write!(f, "Num({:?})", v),
            Value::Str(v) => write!(f, "Str({:?})", v),
            Value::Barr(v) => write!(f, "Barr({:?})", v),
            Value::Tuple(v) => {
                let mut d = f.debug_tuple("Tuple");
                for i in v {
                    d.field(i);
                }
                d.finish()
            }
            Value::Cntr(v) => {
                let mut d = f.debug_struct("Cntr");
                for (k, v) in v {
                    d.field(k.as_str(), v);
                }
                d.finish()
            }
            Value::Seq(v) => f.debug_list().entries(v.iter()).finish(),
            Value::Map(v) => f.debug_map().entries(v.iter()).finish(),
        }
    }
}

// ########### ACCESSORS ######################################################
/// `Accessor` provides a wrapper around a value, adding additional accessing methods.
///
/// # Containers
/// A container accessor can be constructed through [`Value::cntr`] or [`Value::cntr_mut`]. These
/// give you immutable or mutable access to the underlying map store, along with additional methods
/// for accessing an entry if it matches the name + the type of [`Value`].
///
/// # Example
/// ```rust
/// # use kserd::*;
/// let mut cntr = Value::new_cntr(vec![
///     ("a", Kserd::new_str("Hello")),
///     ("b", Kserd::new_num(101))
/// ]).unwrap();
///
/// let mut accessor = cntr.cntr_mut().unwrap();
///
/// assert_eq!(accessor.get_str("a"), Some("Hello"));
/// assert_eq!(accessor.get_str("b"), None); // not a string
/// assert_eq!(accessor.get_num("b"), Some(101.into()));
///
/// // mutating is also possible
/// accessor.get_str_mut("a").map(|s| s.push_str(", world!"));
///
/// assert_eq!(accessor.get_str("a"), Some("Hello, world!"));
/// ```
#[derive(Debug, PartialEq)]
pub struct Accessor<T>(T);

impl<T> Deref for Accessor<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> DerefMut for Accessor<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

/// **Immutable access to a container's fields.**
impl<'a, T> Accessor<T>
where
    T: Deref<Target = Fields<'a>>,
{
    /// Returns if the container has a unit value along the key `name`.
    pub fn get_unit<K>(&self, name: &K) -> Option<()>
    where
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get(name)
            .and_then(|v| if v.unit() { Some(()) } else { None })
    }

    /// Returns if the container has a boolean value along the key `name`.
    pub fn get_bool<K>(&self, name: &K) -> Option<bool>
    where
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get(name).and_then(|v| v.bool())
    }

    /// Returns if the container has a numeric value along the key `name`.
    pub fn get_num<K>(&self, name: &K) -> Option<Number>
    where
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get(name).and_then(|k| match &k.val {
            Value::Num(n) => Some(*n),
            _ => None,
        })
    }

    /// Returns if the container has a string value along the key `name`.
    pub fn get_str<'b, K>(&'b self, name: &K) -> Option<&'b str>
    where
        'a: 'b,
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get(name).and_then(|k| k.str())
    }

    /// Returns if the container has a byte array value along the key `name`.
    pub fn get_barr<'b, K>(&'b self, name: &K) -> Option<&'b [u8]>
    where
        'a: 'b,
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get(name).and_then(|k| k.barr())
    }

    /// Returns if the container has a tuple value along the key `name`.
    pub fn get_tuple<K>(&self, name: &K) -> Option<&List<'a>>
    where
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get(name).and_then(|k| k.tuple())
    }

    /// Returns if the container has a sequence value along the key `name`.
    pub fn get_seq<K>(&self, name: &K) -> Option<&List<'a>>
    where
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get(name).and_then(|k| k.seq())
    }

    /// Returns if the container has a tuple or sequence value along the key `name`.
    pub fn get_tuple_or_seq<K>(&self, name: &K) -> Option<&List<'a>>
    where
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get(name).and_then(|k| k.tuple_or_seq())
    }

    /// Returns if the container has a container value along the key `name`.
    pub fn get_cntr<K>(&self, name: &K) -> Option<Accessor<&Fields<'a>>>
    where
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get(name).and_then(|k| k.cntr())
    }

    /// Returns if the container has a map value along the key `name`.
    pub fn get_map<K>(&self, name: &K) -> Option<&Map<'a>>
    where
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get(name).and_then(|k| k.map())
    }
}

/// **Mutable access to a container's fields.**
impl<'a, T> Accessor<T>
where
    T: DerefMut<Target = Fields<'a>>,
{
    /// Returns if the container has a boolean value along the key `name`.
    pub fn get_bool_mut<'b, K>(&'b mut self, name: &K) -> Option<&'b mut bool>
    where
        'a: 'b,
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get_mut(name).and_then(|v| v.bool_mut())
    }

    /// Returns if the container has a numeric value along the key `name`.
    pub fn get_num_mut<'b, K>(&'b mut self, name: &K) -> Option<&'b mut Number>
    where
        'a: 'b,
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get_mut(name).and_then(|k| k.num_mut())
    }

    /// Returns if the container has a string value along the key `name`.
    pub fn get_str_mut<'b, K>(&'b mut self, name: &K) -> Option<&'b mut String>
    where
        'a: 'b,
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get_mut(name).and_then(|k| k.str_mut())
    }

    /// Returns if the container has a byte array value along the key `name`.
    pub fn get_barr_mut<'b, K>(&'b mut self, name: &K) -> Option<&'b mut Vec<u8>>
    where
        'a: 'b,
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get_mut(name).and_then(|k| k.barr_mut())
    }

    /// Returns if the container has a tuple value along the key `name`.
    pub fn get_tuple_mut<K>(&mut self, name: &K) -> Option<&mut List<'a>>
    where
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get_mut(name).and_then(|k| k.tuple_mut())
    }

    /// Returns if the container has a sequence value along the key `name`.
    pub fn get_seq_mut<K>(&mut self, name: &K) -> Option<&mut List<'a>>
    where
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get_mut(name).and_then(|k| k.seq_mut())
    }

    /// Returns if the container has a tuple or sequence value along the key `name`.
    pub fn get_tuple_or_seq_mut<K>(&mut self, name: &K) -> Option<&mut List<'a>>
    where
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get_mut(name).and_then(|k| k.tuple_or_seq_mut())
    }

    /// Returns if the container has a container value along the key `name`.
    pub fn get_cntr_mut<K>(&mut self, name: &K) -> Option<Accessor<&mut Fields<'a>>>
    where
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get_mut(name).and_then(|k| k.cntr_mut())
    }

    /// Returns if the container has a map value along the key `name`.
    pub fn get_map_mut<K>(&mut self, name: &K) -> Option<&mut Map<'a>>
    where
        K: Ord + ?Sized,
        Kstr<'a>: Borrow<K>,
    {
        self.get_mut(name).and_then(|k| k.map_mut())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ToKserd;

    fn k<'a, T: ToKserd<'a>>(t: T) -> Kserd<'a> {
        t.into_kserd().unwrap()
    }

    #[test]
    fn debug_fmt() {
        let x = Value::new_barr(&[1, 2, 3]);
        assert_eq!(&format!("{:?}", x), "Barr([1, 2, 3])");

        let x = Value::Seq(vec![k(101)]);
        assert_eq!(
            &format!("{:?}", x),
            r#"[Kserd { id: Some("i32"), val: Num(Int(101)) }]"#
        );

        let x = Value::new_map(vec![(k(1), k(2))]);
        assert_eq!(
            &format!("{:?}", x),
            r#"{Kserd { id: Some("i32"), val: Num(Int(1)) }: Kserd { id: Some("i32"), val: Num(Int(2)) }}"#
        );
    }

    #[test]
    fn invalid_field_test() {
        fn f(s: &str) -> String {
            InvalidFieldName::validate(s).unwrap_err().to_string()
        };

        assert_eq!(&f(""), "invalid field name: name is empty");
        assert_eq!(
            &f("9name"),
            "invalid field name: name can not begin with digit"
        );
        assert_eq!(
            &f("na\tme"),
            "invalid field name: invalid character '\t' exists in name 'na\tme'"
        );
    }

    #[test]
    fn mutable_testing() {
        let mut x = Value::Bool(true);
        x.bool_mut().map(|x| *x = false);
        assert_eq!(x.bool(), Some(false));
        assert_eq!(x.num_mut(), None);

        let mut x = Value::new_num(123);
        x.num_mut().map(|x| *x = 3.14.into());
        assert_eq!(x.float(), Some(3.14));
        assert_eq!(x.bool_mut(), None);
        assert_eq!(x.str_mut(), None);
        assert_eq!(x.barr_mut(), None);

        let mut x = Value::new_str("Hello");
        x.str_mut().map(|x| x.push_str(", world!"));
        assert_eq!(x.str(), Some("Hello, world!"));

        let mut x = Value::new_barr([0, 1, 2].as_ref());
        x.barr_mut().map(|x| x.push(3));
        assert_eq!(x.barr(), Some([0, 1, 2, 3].as_ref()));
        assert_eq!(x.tuple_or_seq(), None);
        assert_eq!(x.tuple_or_seq_mut(), None);

        let mut x = Value::Tuple(vec![k("a"), k("b")]);
        x.tuple_mut().map(|x| x.push(k("c")));
        assert_eq!(x.tuple(), Some(&vec![k("a"), k("b"), k("c")]));
        assert_eq!(x.seq(), None);
        assert_eq!(x.seq_mut(), None);
        assert_eq!(x.tuple_or_seq(), Some(&vec![k("a"), k("b"), k("c")]));

        let mut x = Value::Seq(vec![k("a"), k("b")]);
        x.seq_mut().map(|x| x.push(k("c")));
        assert_eq!(x.seq(), Some(&vec![k("a"), k("b"), k("c")]));
        assert_eq!(x.tuple(), None);
        assert_eq!(x.tuple_mut(), None);
        assert_eq!(x.tuple_or_seq(), Some(&vec![k("a"), k("b"), k("c")]));
        assert_eq!(x.map(), None);
        assert_eq!(x.map_mut(), None);
    }

    #[test]
    fn conversion_testing() {
        let unit = Value::Unit.to_owned();
        assert_eq!(unit, Value::Unit);

        let b = Value::Bool(true).to_owned();
        assert_eq!(b, Value::Bool(true));

        let n = Value::new_num(3.14).to_owned();
        assert_eq!(n, Value::new_num(3.14));

        let s = Value::new_str("What!?").to_owned();
        assert_eq!(s, Value::new_str("What!?"));

        let barr = Value::new_barr([0, 1].as_ref()).to_owned();
        assert_eq!(barr, Value::new_barr([0, 1].as_ref()));

        let b = Value::Bool(true);
        assert_eq!(b, b.clone());

        let barr = Value::new_barr([0, 1].as_ref()).to_owned();
        assert_eq!(barr, barr.clone());

        fn test(v: Kserd) {
            let vclone = v.clone();
            assert_eq!(v.to_owned(), vclone);
        };

        test(Kserd::new(Value::Tuple(vec![
            Kserd::new_str("Hello"),
            Kserd::new_num(3.14),
        ])));
        test(Kserd::new_cntr(vec![("a", Kserd::new_unit())]).unwrap());
        test(Kserd::new(Value::Seq(vec![
            Kserd::new_str("Hello"),
            Kserd::new_num(3.14),
        ])));
        test(Kserd::new_map(vec![(Kserd::new_unit(), Kserd::new_num(0))]));

        let unit = Value::Unit;
        assert_eq!(unit, unit.mk_brw());

        let b = Value::Bool(true);
        assert_eq!(b, b.mk_brw());

        let n = Value::new_num(3.14);
        assert_eq!(n, n.mk_brw());

        let s = Value::new_str("What!?");
        assert_eq!(s, s.mk_brw());

        let barr = Value::new_barr([0, 1].as_ref());
        assert_eq!(barr, barr.mk_brw());

        fn test2(v: Kserd) {
            assert_eq!(v.mk_brw(), v);
        };

        test2(Kserd::new(Value::Tuple(vec![
            Kserd::new_str("Hello"),
            Kserd::new_num(3.14),
        ])));
        test2(Kserd::new_cntr(vec![("a", Kserd::new_unit())]).unwrap());
        test2(Kserd::new(Value::Seq(vec![
            Kserd::new_str("Hello"),
            Kserd::new_num(3.14),
        ])));
        test2(Kserd::new_map(vec![(Kserd::new_unit(), Kserd::new_num(0))]));
    }

    #[test]
    fn into_owned_test() {
        use crate::to_kserd::ToKserd;

        let prims = Kserd::new_cntr(vec![
            ("unit", Kserd::new_unit()),
            ("bool", Kserd::new_bool(true)),
            ("num", Kserd::new_num(1.01234)),
            ("str", Kserd::new_str("Hello, world!")),
            ("barr", Kserd::new_barr([0, 1, 2, 4, 8].as_ref())),
        ])
        .unwrap();

        let nested = Kserd::new_cntr(vec![
            ("prims", prims.clone()),
            (
                "tuple",
                Kserd::new(Value::Tuple(vec![prims.clone(), prims.clone()])),
            ),
            (
                "seq",
                Kserd::new(Value::Seq(vec![
                    prims.clone(),
                    prims.clone(),
                    prims.clone(),
                    prims.clone(),
                ])),
            ),
            (
                "map",
                Kserd::new_map(vec![
                    ("first".into_kserd().unwrap(), prims.clone()),
                    ("second".into_kserd().unwrap(), prims.clone()),
                ]),
            ),
        ])
        .unwrap();

        let nested_owned = nested.clone().into_owned();
        assert_eq!(nested, nested_owned);
    }

    #[test]
    fn cntr_accessor() {
        let mut x = Value::new_num(101);
        assert_eq!(x.cntr(), None);
        assert_eq!(x.cntr_mut(), None);

        let cntr = Value::new_cntr(vec![
            ("a", Kserd::new_unit()),
            ("b", Kserd::new_bool(false)),
            ("c", Kserd::new_num(101)),
            ("d", Kserd::new_str("hello")),
            ("e", Kserd::new_barr(&[0])),
            ("f", Kserd::new(Value::Tuple(vec![k(1)]))),
            ("g", Kserd::new(Value::Seq(vec![k(2)]))),
            ("h", Kserd::new_cntr(vec![("a", k(()))]).unwrap()),
            ("i", Kserd::new_map(vec![(k(()), k(2))])),
        ])
        .unwrap();

        let x = cntr.cntr().unwrap();

        assert_eq!(x.get_unit("a"), Some(()));
        assert_eq!(x.get_num("a"), None);
        assert_eq!(x.get_bool("b"), Some(false));
        assert_eq!(x.get_num("c"), Some(101.into()));
        assert_eq!(x.get_str("d"), Some("hello"));
        assert_eq!(x.get_barr("e"), Some(&[0u8] as &[u8]));
        assert_eq!(x.get_tuple("f"), Some(&vec![k(1)]));
        assert_eq!(x.get_seq("g"), Some(&vec![k(2)]));
        assert_eq!(x.get_tuple_or_seq("f"), Some(&vec![k(1)]));
        assert_eq!(x.get_tuple_or_seq("g"), Some(&vec![k(2)]));
        assert_eq!(x.get_cntr("h").is_some(), true);
        assert_eq!(x.get_map("i").is_some(), true);
    }

    #[test]
    fn cntr_mut_accessor() {
        let mut x = Value::new_num(101);
        assert_eq!(x.cntr(), None);
        assert_eq!(x.cntr_mut(), None);

        let mut cntr = Value::new_cntr(vec![
            ("a", Kserd::new_unit()),
            ("b", Kserd::new_bool(false)),
            ("c", Kserd::new_num(101)),
            ("d", Kserd::new_str("hello")),
            ("e", Kserd::new_barr(&[0])),
            ("f", Kserd::new(Value::Tuple(vec![k(1)]))),
            ("g", Kserd::new(Value::Seq(vec![k(2)]))),
            ("h", Kserd::new_cntr(vec![("a", k(()))]).unwrap()),
            ("i", Kserd::new_map(vec![(k(()), k(2))])),
        ])
        .unwrap();

        let mut x = cntr.cntr_mut().unwrap();

        assert_eq!(x.get_num_mut("a"), None);
        assert_eq!(x.get_bool_mut("b"), Some(&mut false));
        assert_eq!(x.get_num_mut("c"), Some(&mut 101.into()));
        assert_eq!(x.get_str_mut("d"), Some(&mut String::from("hello")));
        assert_eq!(x.get_barr_mut("e"), Some(&mut vec![0u8]));
        assert_eq!(x.get_tuple_mut("f"), Some(&mut vec![k(1)]));
        assert_eq!(x.get_seq_mut("g"), Some(&mut vec![k(2)]));
        assert_eq!(x.get_tuple_or_seq_mut("f"), Some(&mut vec![k(1)]));
        assert_eq!(x.get_tuple_or_seq_mut("g"), Some(&mut vec![k(2)]));
        assert_eq!(x.get_cntr_mut("h").is_some(), true);
        assert_eq!(x.get_map_mut("i").is_some(), true);
    }

    #[test]
    fn accessor_both() {
        let mut x = Value::new_cntr(vec![("a", Kserd::new_str("Hello"))]).unwrap();

        let mut accessor = x.cntr_mut().unwrap();

        assert_eq!(accessor.get_str("a"), Some("Hello"));

        accessor.get_str_mut("a").map(|s| s.push_str(", world!"));

        assert_eq!(accessor.get_str("a"), Some("Hello, world!"));
    }
}
