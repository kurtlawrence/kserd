use super::*;
use crate::Fields;
use std::borrow::Cow;
use std::{error, fmt};

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
#[derive(PartialEq, Eq, PartialOrd, Ord)]
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
    Tuple(Vec<Kserd<'a>>),
    /// A container value.
    ///
    /// A container is a _hetrogeneous collection of named objects_.
    /// Containers are akin to structs.
    Cntr(Fields<'a>),
    /// A sequence of values.
    ///
    /// A sequence is a _homogeneous sequence of unnamed objects_.
    Seq(Vec<Kserd<'a>>),
    /// A map of values.
    ///
    /// A map is a _homogeneous mapping of keys to values_.
    Map(BTreeMap<Kserd<'a>, Kserd<'a>>),
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
    ///
    /// [`NumberType`]: crate::ds::num::NumberType
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
        use std::iter::FromIterator;
        let map = BTreeMap::from_iter(iter.into_iter().map(|(k, v)| (k.into(), v)));
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
        use std::iter::FromIterator;
        let map = BTreeMap::from_iter(iter);
        Value::Map(map)
    }
}

/// Convenience methods for accessing values straight from the [`Value`] enum.
///
/// [`Value`]: crate::Value
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
        match &self {
            Value::Unit => true,
            _ => false,
        }
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
    ///	```rust
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
    ///	let value = Value::new_num(-3.14);
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
    ///	let mut value = Value::new_str("Hello");
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
}

/// Conversions.
impl<'a> Value<'a> {
    /// Clones all data to make a static `Value`.
    pub fn to_owned(self) -> Value<'static> {
        match self {
            Value::Unit => Value::Unit,
            Value::Bool(v) => Value::Bool(v),
            Value::Num(v) => Value::Num(v),
            Value::Str(s) => Value::Str(s.to_owned()),
            Value::Barr(b) => Value::Barr(b.to_owned()),
            Value::Tuple(seq) => Value::Tuple({
                let mut v = Vec::with_capacity(seq.len());
                for i in seq {
                    v.push(i.to_owned())
                }
                v
            }),
            Value::Cntr(map) => Value::Cntr({
                let mut m = BTreeMap::new();
                for (k, v) in map {
                    m.insert(k.to_owned(), v.to_owned());
                }
                m
            }),
            Value::Seq(seq) => Value::Seq({
                let mut v = Vec::with_capacity(seq.len());
                for i in seq {
                    v.push(i.to_owned())
                }
                v
            }),
            Value::Map(map) => Value::Map({
                let mut m = BTreeMap::new();
                for (k, v) in map {
                    m.insert(k.to_owned(), v.to_owned());
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
    pub fn mk_brw<'b>(&'b self) -> Value<'b> {
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
            Value::Num(v) => Value::Num(v.clone()),
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
                let mut d = f.debug_tuple("");
                for i in v {
                    d.field(i);
                }
                d.finish()
            }
            Value::Cntr(v) => {
                let mut d = f.debug_struct("");
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

#[cfg(test)]
mod tests {
    use super::*;

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

        let mut x = Value::new_num(123);
        x.num_mut().map(|x| *x = 3.14.into());
        assert_eq!(x.float(), Some(3.14));

        let mut x = Value::new_str("Hello");
        x.str_mut().map(|x| x.push_str(", world!"));
        assert_eq!(x.str(), Some("Hello, world!"));

        let mut x = Value::new_barr([0, 1, 2].as_ref());
        x.barr_mut().map(|x| x.push(3));
        assert_eq!(x.barr(), Some([0, 1, 2, 3].as_ref()));
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
}
