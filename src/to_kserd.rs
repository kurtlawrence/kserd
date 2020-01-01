use crate::{Kserd, Value, ds::{InvalidId, InvalidFieldName}};
use std::{error, fmt};

/// _Convert_ something into a `Kserd`.
///
/// This differs to [_encoding_](crate::encode::Encoder) in that the object is _consumed_. This trait
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
/// # #[cfg(feature = "fmt")] {
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
/// # }
/// ```
pub trait ToKserd<'a> {
    /// Consume the object and convert it into a [`Kserd`](Kserd) data object.
    fn into_kserd(self) -> Result<Kserd<'a>, ToKserdErr>;
}

/// Errors that can occur when using [`ToKserd`](ToKserd).
#[derive(Debug, PartialEq, Clone)]
pub enum ToKserdErr {
    /// The identity contained invalid characters.
    InvalidId(InvalidId),
    /// A container's field name contained invalid characters.
    InvalidFieldName(InvalidFieldName),
}

impl error::Error for ToKserdErr {}

impl fmt::Display for ToKserdErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ToKserdErr::InvalidId(s) => write!(f, "{}", s),
            ToKserdErr::InvalidFieldName(s) => write!(f, "{}", s),
        }
    }
}

impl<'a> ToKserd<'a> for Kserd<'a> {
    fn into_kserd(self) -> Result<Kserd<'a>, ToKserdErr> {
        Ok(self)
    }
}

impl From<InvalidId> for ToKserdErr {
    fn from(id: InvalidId) -> ToKserdErr {
        ToKserdErr::InvalidId(id)
    }
}

impl From<InvalidFieldName> for ToKserdErr {
    fn from(x: InvalidFieldName) -> ToKserdErr {
        ToKserdErr::InvalidFieldName(x)
    }
}

#[cfg(test)]
use rand::{random, thread_rng, Rng};
#[cfg(test)]
fn random_string() -> String {
    let mut rng = rand::thread_rng();
    let len = rng.gen::<u8>() as usize;
    let mut s = String::new();
    for _ in 0..len {
        s.push(rng.gen());
    }
    s
}

#[test]
fn test_kserd_to_kserd() {
    let kserd = (random_string(), rand::random::<usize>())
        .into_kserd()
        .unwrap();
    let kserdnew = kserd.clone().into_kserd().unwrap();
    assert_eq!(kserd, kserdnew);
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

#[test]
fn test_copyable_primitives() {
    assert_eq!(123456.into_kserd(), Ok(Kserd::new_num(123456)));
    assert_eq!((-1234567).into_kserd(), Ok(Kserd::new_num(-1234567)));
    assert_eq!(3.14.into_kserd(), Ok(Kserd::new_num(3.14)));
    assert_eq!(().into_kserd(), Ok(Kserd::new_unit()));
    assert_eq!(true.into_kserd(), Ok(Kserd::new_bool(true)));
    assert_eq!('y'.into_kserd(), Ok(Kserd::new_str("y")));
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

#[test]
fn test_strings() {
    let s = random_string();
    assert_eq!(s.as_str().into_kserd(), Ok(Kserd::new_str(&s)));
    assert_eq!(s.clone().into_kserd(), Ok(Kserd::new_string(s)));
}

// ********************* TUPLES AND ARRAYS ************************************

macro_rules! tuples {
    ( $( $lifetime:tt|$element:tt|$idx:tt ),+ ) => {
        impl<'kserd, $($lifetime: 'kserd,)* $($element,)*> ToKserd<'kserd> for ($($element,)*)
        where
            $(
                $element: ToKserd<$lifetime>,
            )*
        {
            fn into_kserd(self) -> Result<Kserd<'kserd>, ToKserdErr> {
                let v = vec![$(
                    self.$idx.into_kserd()?,
                )*];
                Ok(Kserd::new(Value::Tuple(v)))
            }
        }
    }
}

tuples!('a|A|0);
tuples!('a|A|0,'b|B|1);
tuples!('a|A|0,'b|B|1,'c|C|2);
tuples!('a|A|0,'b|B|1,'c|C|2, 'd|D|3);
tuples!('a|A|0,'b|B|1,'c|C|2, 'd|D|3, 'e|E|4);
tuples!('a|A|0,'b|B|1,'c|C|2, 'd|D|3, 'e|E|4, 'f|F|5);
tuples!('a|A|0,'b|B|1,'c|C|2, 'd|D|3, 'e|E|4, 'f|F|5, 'g|G|6);
tuples!('a|A|0,'b|B|1,'c|C|2, 'd|D|3, 'e|E|4, 'f|F|5, 'g|G|6, 'h|H|7);
tuples!('a|A|0,'b|B|1,'c|C|2, 'd|D|3, 'e|E|4, 'f|F|5, 'g|G|6, 'h|H|7, 'i|I|8);
tuples!('a|A|0,'b|B|1,'c|C|2, 'd|D|3, 'e|E|4, 'f|F|5, 'g|G|6, 'h|H|7, 'i|I|8, 'j|J|9);
tuples!('a|A|0,'b|B|1,'c|C|2, 'd|D|3, 'e|E|4, 'f|F|5, 'g|G|6, 'h|H|7, 'i|I|8, 'j|J|9, 'k|K|10);
tuples!('a|A|0,'b|B|1,'c|C|2, 'd|D|3, 'e|E|4, 'f|F|5, 'g|G|6, 'h|H|7, 'i|I|8, 'j|J|9, 'k|K|10, 'l|L|11);

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

#[test]
fn tuples_and_array_test() {
    let mut rng = thread_rng();

    macro_rules! tester {
        (arrays $( $size:literal ) * ) => {{
            $(
            let mut arr = [0u8; $size];
            rng.fill(&mut arr[..]);
            let v = arr.iter().copied().collect::<Vec<_>>();
            assert_eq!(arr.into_kserd(), v.into_kserd());
            )*
        }};
        (tuples $tuple:expr => $($idx:tt),*) => {{
            let tuple = $tuple;
            let kserd = tuple.clone().into_kserd().unwrap();
            let vec = match kserd.val {
                Value::Tuple(v) => v,
                _ => unreachable!("should be a tuple")
            };
            $(
                assert_eq!(tuple.$idx.clone().into_kserd().unwrap(), vec[$idx]);
            )*
        }}
    };

    tester!(arrays
         1  2  3  4  5  6  7  8
         9 10 11 12 13 14 15 16
        17 18 19 20 21 22 23 24
        25 26 27 28 29 30 31 32
    );

    tester!(tuples (random_string(),) => 0);
    tester!(tuples (random_string(), rng.gen::<usize>()) => 0,1);
    tester!(tuples (random_string(), rng.gen::<usize>(), random_string()) => 0,1,2);
    tester!(tuples (random_string(), rng.gen::<usize>(), random_string(), rng.gen::<char>()) => 0,1,2,3);
    tester!(tuples (random_string(), rng.gen::<usize>(), random_string(), rng.gen::<char>(), random_string()) => 0,1,2,3,4);
    tester!(tuples (random_string(), rng.gen::<usize>(), random_string(), rng.gen::<char>(), random_string(), random_string()) => 0,1,2,3,4,5);
    tester!(tuples (random_string(), rng.gen::<usize>(), random_string(), rng.gen::<char>(), random_string(), random_string(), rng.gen::<isize>()) => 0,1,2,3,4,5,6);
    tester!(tuples (random_string(), random::<usize>(), random_string(), rng.gen::<char>(), random_string(), random_string(), rng.gen::<isize>(), rng.gen::<bool>()) => 0,1,2,3,4,5,6,7);
    tester!(tuples (random_string(), rng.gen::<usize>(), random_string(), rng.gen::<char>(), random_string(), random_string(), rng.gen::<isize>(), rng.gen::<bool>(), rng.gen::<char>()) => 0,1,2,3,4,5,6,7,8);
    tester!(tuples (random_string(), rng.gen::<usize>(), random_string(), rng.gen::<char>(), random_string(), random_string(), rng.gen::<isize>(), rng.gen::<bool>(), rng.gen::<char>(), rng.gen::<f64>()) => 0,1,2,3,4,5,6,7,8,9);
    tester!(tuples (random_string(), rng.gen::<usize>(), random_string(), rng.gen::<char>(), random_string(), random_string(), rng.gen::<isize>(), rng.gen::<bool>(), rng.gen::<char>(), rng.gen::<f64>(), random_string()) => 0,1,2,3,4,5,6,7,8,9,10);
    tester!(tuples (random_string(), rng.gen::<usize>(), random_string(), rng.gen::<char>(), random_string(), random_string(), rng.gen::<isize>(), rng.gen::<bool>(), rng.gen::<char>(), rng.gen::<f64>(), random_string(), random_string()) => 0,1,2,3,4,5,6,7,8,9,10,11);
}

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

// ********************* BLANKET IMPLEMENTATIONS ******************************

impl<'a, T> ToKserd<'a> for Box<T>
where
    T: ToKserd<'a>
{
    fn into_kserd(self) -> Result<Kserd<'a>, ToKserdErr> {
        (*self).into_kserd()
    }
}

/// Blanket implementation for `Option<T>`.
///
/// The implementation _does not wrap `Some` values_. Rather, if there is something, it is just
/// that value, or it will be an empty tuple with the name `None`.
///
/// ```rust
/// # use kserd::*;
/// let option = Some(String::from("Hello, world!"));
/// assert_eq!(option.into_kserd(), Ok(Kserd::new_str("Hello, world!")));
/// let option: Option<String> = None;
/// assert_eq!(option.into_kserd(), Kserd::with_id("None", Value::Tuple(vec![])).map_err(From::from));
/// ```
impl<'a, T> ToKserd<'a> for Option<T>
where
    T: ToKserd<'a>
{
    fn into_kserd(self) -> Result<Kserd<'a>, ToKserdErr> {
        match self {
            Some(x) => x.into_kserd(),
            None => Ok(Kserd::with_id_unchk("None", Value::Tuple(Vec::new()))),
        }
    }
}

#[test]
fn blanket_impls_tests() {
    let boxed = Box::new(String::from("Hello, world!"));
    assert_eq!(boxed.into_kserd(), Ok(Kserd::new_str("Hello, world!")));

    let option = Some(String::from("Hello, world!"));
    assert_eq!(option.into_kserd(), Ok(Kserd::new_str("Hello, world!")));
    let option: Option<String> = None;
    assert_eq!(option.into_kserd(), Kserd::with_id("None", Value::Tuple(vec![])).map_err(From::from));

}

// ********************* STRINGS AND BYTE ARRAYS ******************************
// ********************* STRINGS AND BYTE ARRAYS ******************************
// ********************* STRINGS AND BYTE ARRAYS ******************************
