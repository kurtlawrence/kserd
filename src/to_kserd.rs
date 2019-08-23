use crate::{Kserd, Value};
use std::{error, fmt};

/// _Convert_ something into a `Kserd`.
///
/// This differs to [_encoding_](encode::Encoder) in that the object is _consumed_. This trait
/// allows for less copying as data can be _moved_, in the case of strings or byte arrays.
///
/// # Implementing
/// The crate `kserd_derive` can be used to annotate objects with `#[derive(ToKserd)]`. The crate
/// works much in the same way as `serde_derive`.
///
/// If you want to manually implement `ToKserd` an example is given below.
///
/// ```rust
/// use kserd::*;
///
/// struct MyStruct<'a> {
///     e: MyEnum,
///     s: &'a str,
///     v: Vec<(isize, isize)>,
/// }
///
/// enum MyEnum {
///     Variant1,
///     Variant2(usize)
/// }
///
/// // let's implement MyEnum first
/// // it has no references so can be valid for the static lifetime
/// impl ToKserd<'static> for MyEnum {
///     fn into_kserd(self) -> Result<Kserd<'static>, ToKserdErr> {
///         let r = match self {
///             MyEnum::Variant1 =>
///                 Kserd::with_id("Variant1", Value::Unit)?,
///             // A variant with items is a named Tuple
///             MyEnum::Variant2(i) =>
///                 Kserd::with_id("Variant2", Value::Tuple(vec![
///                     i.into_kserd()?
///                 ]))?,
///         };
///         Ok(r)
///     }
/// }
///
/// // now implement MyStruct
/// // it has a reference so make it last for that long
/// impl<'a> ToKserd<'a> for MyStruct<'a> {
///     fn into_kserd(self) -> Result<Kserd<'a>, ToKserdErr> {
///         Ok(Kserd::with_id("MyStruct", Value::new_cntr(vec![
///             ("e", self.e.into_kserd()?),
///             ("s", self.s.into_kserd()?),
///             ("v", self.v.into_kserd()?),
///         ]).unwrap())?) // notice the ? to propogate the error
///     }
/// }
///
/// // lets test it
/// let mys = MyStruct {
///     e: MyEnum::Variant2(101),
///     s: "Hello, world!",
///     v: vec![
///         (0, 1),
///         (2, 3),
///         (4, 5)
///     ]
/// };
///
/// let kserd = mys.into_kserd().unwrap();
///
/// let s = kserd.as_str();
///
/// assert_eq!(
/// &s,
/// r#"MyStruct (
///     e = Variant2 (101)
///     s = "Hello, world!"
///     v = [(0, 1), (2, 3), (4, 5)]
/// )"#
/// );
/// ```
pub trait ToKserd<'a> {
    /// Consume the object and convert it into a [`Kserd`](Kserd) data object.
    fn into_kserd(self) -> Result<Kserd<'a>, ToKserdErr>;
}

/// Errors that can occur when use [`ToKserd`](ToKserd).
#[derive(Debug, PartialEq)]
pub enum ToKserdErr {
    /// The identity contained invalid characters.
    InvalidId(String),
}

impl error::Error for ToKserdErr {}

impl fmt::Display for ToKserdErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl From<String> for ToKserdErr {
    fn from(err: String) -> Self {
        ToKserdErr::InvalidId(err)
    }
}

impl<'a> ToKserd<'a> for Kserd<'a> {
    fn into_kserd(self) -> Result<Kserd<'a>, ToKserdErr> {
        Ok(self)
    }
}

// ********************* COPY-ABLE PRIMITIVES *********************************

macro_rules! number {
    ( $( $x:ty ) * ) => {
        $(
            impl ToKserd<'static> for $x {
                fn into_kserd(self) -> Result<Kserd<'static>, ToKserdErr> {
                    Ok(Kserd::new_num(self))
                }
            }
        )*
    }
}

number!(
    usize u8 u16 u32 u64 u128
    isize i8 i16 i32 i64 i128
    f32 f64
);

impl ToKserd<'static> for () {
    fn into_kserd(self) -> Result<Kserd<'static>, ToKserdErr> {
        Ok(Kserd::new_unit())
    }
}

impl ToKserd<'static> for bool {
    fn into_kserd(self) -> Result<Kserd<'static>, ToKserdErr> {
        Ok(Kserd::new_bool(self))
    }
}

impl ToKserd<'static> for char {
    fn into_kserd(self) -> Result<Kserd<'static>, ToKserdErr> {
        Ok(Kserd::with_id_unchk(
            "char",
            Value::Str(self.to_string().into()),
        ))
    }
}

// ********************* STRINGS AND BYTE ARRAYS ******************************

impl ToKserd<'static> for String {
    fn into_kserd(self) -> Result<Kserd<'static>, ToKserdErr> {
        Ok(Kserd::new_string(self))
    }
}

impl<'a> ToKserd<'a> for &'a str {
    fn into_kserd(self) -> Result<Kserd<'a>, ToKserdErr> {
        Ok(Kserd::new_str(self))
    }
}

// ********************* TUPLES AND ARRAYS ************************************

impl<'a, A: ToKserd<'a>> ToKserd<'a> for (A,) {
    fn into_kserd(self) -> Result<Kserd<'a>, ToKserdErr> {
        Ok(Kserd::new(Value::Tuple(vec![self.0.into_kserd()?])))
    }
}

impl<'a, A: ToKserd<'a>, B: ToKserd<'a>> ToKserd<'a> for (A, B) {
    fn into_kserd(self) -> Result<Kserd<'a>, ToKserdErr> {
        Ok(Kserd::new(Value::Tuple(vec![
            self.0.into_kserd()?,
            self.1.into_kserd()?,
        ])))
    }
}

macro_rules! array {
    ( $( $x:literal ) * ) => {
        $(
            impl<'a, T: ToKserd<'a>> ToKserd<'a> for [T; $x] {
                fn into_kserd(self) -> Result<Kserd<'a>, ToKserdErr> {
                    let b: Box<[T]> = Box::new(self);
                    let arr = b.into_vec();
                    let mut v = Vec::with_capacity(arr.len());
                    for i in arr.into_iter() {
                        v.push(i.into_kserd()?);
                    }
                    Ok(Kserd::with_id_unchk("array", Value::Seq(v)))
                }
            }
        )*
    }
}

array!(
     1  2  3  4  5  6  7  8
     9 10 11 12 13 14 15 16
    17 18 19 20 21 22 23 24
    25 26 27 28 29 30 31 32
);

// ********************* SEQUENCES AND MAPS ***********************************

impl<'a, T: ToKserd<'a>> ToKserd<'a> for Vec<T> {
    fn into_kserd(self) -> Result<Kserd<'a>, ToKserdErr> {
        let mut v = Vec::with_capacity(self.len());
        for i in self {
            v.push(i.into_kserd()?);
        }
        Ok(Kserd::with_id_unchk("Vec", Value::Seq(v)))
    }
}

// ********************* STRINGS AND BYTE ARRAYS ******************************
// ********************* STRINGS AND BYTE ARRAYS ******************************
// ********************* STRINGS AND BYTE ARRAYS ******************************
// ********************* STRINGS AND BYTE ARRAYS ******************************
