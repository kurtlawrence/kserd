use super::*;
use serde::de::{
    self,
    value::{MapDeserializer, SeqDeserializer},
    IntoDeserializer, Unexpected, Visitor,
};
use std::borrow::Cow::*;
use std::convert::TryInto;
use std::{error, fmt};

type Res<T> = Result<T, Error>;

/// Decoder to pass to [`Deserialize::deserialize`] to decode a [`Kserd`] into a type.
///
/// `Decoder` _consumes_ the `Kserd` structure.
/// It is important to understand how this interacts with the `Deserialize` implementor.
/// Because `Kserd` tries to reduce copying, using clone-on-write pointers for strings
/// and byte arrays, there are extra cases to be handled to achieve the minimal amount of copying data.
/// The `Decoder` was designed to leverage movement of data as much as possible, however this
/// presents a unique case when decoding back to a type that contains borrowed data.
///
/// The table below captures different memory operations using `Decoder`:
///
/// | Kserd ownership | Rust type | Data transfer |
/// | --------------- | --------- | ------------- |
/// | Owned           | Owned     | _move_        |
/// | Borrowed        | Owned     | _copy_        |
/// | Borrowed        | Borrowed  | _move_ (ptr)  |
/// | Owned           | Borrowed  | **unique**    |
///
/// As the `Kserd` is consumed, owned data can be directly moved, whilst borrowed data can be
/// cloned if needing to be come owned, or the pointer can be moved. The only case where this
/// breaks is if the `Kserd` is owned and an attempt is made to decode it back into borrowed data.
/// The failing occurs because the owned data will be dropped after the `deserialize` call.
///
/// If borrowed data is required, the solution is to decode a `Kserd` which itself is a _reference
/// to the owned `Kserd`_. The method [`.mk_brw()`] does this. There is small overhead as
/// nested structures have to be reallocated, but strings and byte arrays are made to point to the
/// original `Kserd` structure. The 'borrowed' `Kserd` does not really differ that much, expect
/// that it is _gauranteed to not contain owned data_. This makes the fourth case not possible and
/// the decoding to succeed.
///
/// It is worth noting that it may _increase_ the amount of copying occurring, as previously _owned_
/// data is now _borrowed_. It is therefore recommended to _not_ borrow the `Kserd` unless
/// necessary.
///
/// [`Kserd::decode`](crate::Kserd::decode) can be used for conveniance.
///
/// # Examples
/// `Kserd`s containing borrowed data can decode into borrowed or owned types.
/// ```rust
/// # use kserd::*;
/// use kserd::encode::Decoder;
/// use serde::Deserialize;
///
/// let string = String::from("Hello, world!");
///
/// // decode into borrowed type
/// let kserd = Kserd::new_str(&string);
/// let decoder = Decoder(kserd);
/// let s = <&str>::deserialize(decoder).unwrap();
/// assert_eq!(s, "Hello, world!");
///
/// // decode into owned type
/// let kserd = Kserd::new_str(&string);
/// let decoder = Decoder(kserd);
/// let s = <String>::deserialize(decoder).unwrap();
/// assert_eq!(s, string);
/// ```
///
/// `Kserd`s which own their data can decode into owned types, but error on borrowed types.
/// ```rust
/// # use kserd::*;
/// use kserd::encode::Decoder;
/// use serde::Deserialize;
///
/// // decode into owned type works
/// let kserd = Kserd::new_string(String::from("Hello, world!"));
/// let decoder = Decoder(kserd);
/// let s = <String>::deserialize(decoder).unwrap();
/// assert_eq!(s, "Hello, world!".to_string());
///
/// // decode into borrowed type fails
/// let kserd = Kserd::new_string(String::from("Hello, world!"));
/// let decoder = Decoder(kserd);
/// let r = <&str>::deserialize(decoder);
/// assert_eq!(r.is_err(), true);
/// ```
///
/// Borrowing the `Kserd` beforehand extends the lifetime.
/// ```rust
/// # use kserd::*;
/// use kserd::encode::Decoder;
/// use serde::Deserialize;
///
/// let kserd = Kserd::new_string(String::from("Hello, world!"));
/// let decoder = Decoder(kserd.mk_brw());
/// let s = <&str>::deserialize(decoder).unwrap();
/// assert_eq!(s, "Hello, world!");
/// ```
///
/// [`Deserialize::deserialize`]: serde::de::Deserialize::deserialize
/// [`Kserd`]: crate::Kserd
/// [`.mk_brw()`]: crate::Kserd::mk_brw
pub struct Decoder<'de>(pub Kserd<'de>);

impl<'de> de::Deserializer<'de> for Decoder<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Res<V::Value>
    where
        V: Visitor<'de>,
    {
        let Kserd { id, val } = self.0;

        match val {
            Value::Unit => {
                if let Some(id) = id {
                    if id.as_str() == "None" {
                        visitor.visit_none()
                    } else {
                        visitor.visit_unit()
                    }
                } else {
                    visitor.visit_unit()
                }
            }
            Value::Bool(v) => visitor.visit_bool(v),
            Value::Num(num) => {
                // serde's implementation will try to cast
                // integers _from a 64 bit basis_. so if you are expecting
                // u8, it can handle downcasting from u64 to u8 if it fits.
                // Unfortunately it only handles casting _to_ 128 bit, so
                // it will error if given a u128 and expecting a u8.
                // We handle this by doing our own checks and only telling it to
                // visit a 128 bit integer if it won't fit in a 64 bit container

                match num {
                    Number::Uint(v) => match (v).try_into() {
                        Ok(v) => visitor.visit_u64(v),
                        Err(_) => visitor.visit_u128(v),
                    },
                    Number::Int(v) => match (v).try_into() {
                        Ok(v) => visitor.visit_i64(v),
                        Err(_) => visitor.visit_i128(v),
                    },
                    Number::Float(v) => visitor.visit_f64(v),
                }
            }
            Value::Str(v) => match v.inner {
                Borrowed(v) => visitor.visit_borrowed_str(v),
                Owned(v) => visitor.visit_string(v),
            },
            Value::Barr(v) => match v.inner {
                Borrowed(v) => visitor.visit_borrowed_bytes(v),
                Owned(v) => visitor.visit_byte_buf(v),
            },
            Value::Tuple(mut seq) => {
                if let Some(id) = id {
                    if seq.len() == 1 {
                        if id.as_str() == "Some" {
                            visitor.visit_some(Decoder(seq.pop().expect("one should exist")))
                        } else {
                            visitor
                                .visit_newtype_struct(Decoder(seq.pop().expect("one should exist")))
                        }
                    } else {
                        visitor.visit_seq(SeqDeserializer::new(seq.into_iter()))
                    }
                } else {
                    visitor.visit_seq(SeqDeserializer::new(seq.into_iter()))
                }
            }
            Value::Cntr(map) => visitor.visit_map(MapDeserializer::new(
                map.into_iter().map(|(k, v)| (k.inner, v)),
            )),
            Value::Seq(seq) => visitor.visit_seq(SeqDeserializer::new(seq.into_iter())),
            Value::Map(map) => visitor.visit_map(MapDeserializer::new(map.into_iter())),
        }
    }

    forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct identifier ignored_any
    }

    fn deserialize_char<V: Visitor<'de>>(self, visitor: V) -> Res<V::Value> {
        match self.0.val {
            Value::Str(v) => {
                let s = v.as_str();
                let c = s.chars().count();

                if c == 1 {
                    visitor.visit_char(s.chars().next().expect("will be one"))
                } else {
                    Err(de::Error::invalid_type(Unexpected::Str(s), &"char"))
                }
            }
            x => Err(de::Error::invalid_type(unexp_err(&x), &"char")),
        }
    }

    fn deserialize_enum<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Res<V::Value> {
        visitor.visit_enum(self)
    }
}

impl<'de> de::IntoDeserializer<'de, Error> for Kserd<'de> {
    type Deserializer = Decoder<'de>;
    fn into_deserializer(self) -> Decoder<'de> {
        Decoder(self)
    }
}

impl<'de> de::EnumAccess<'de> for Decoder<'de> {
    type Error = Error;
    type Variant = Decoder<'de>;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let id = self
            .0
            .id()
            .ok_or_else(|| Error::Message(String::from("enum but no identity is present")))?;

        seed.deserialize(id.into_deserializer())
            .map(|v| (v, Decoder(self.0)))
    }
}

impl<'de> de::VariantAccess<'de> for Decoder<'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Error> {
        unexp_matching(self.0.val, &"unit value", |val| match val {
            Value::Unit => Ok(Ok(())),
            x => Err(x),
        })
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        unexp_matching(self.0.val, &"newtype enum variant", |val| match val {
            Value::Tuple(mut v) => Ok(if v.len() == 1 {
                seed.deserialize(Decoder(v.pop().expect("one should exist")))
            } else {
                Err(de::Error::invalid_length(
                    v.len(),
                    &"a tuple with a single element was expected",
                ))
            }),
            x => Err(x),
        })
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        unexp_matching(self.0.val, &"tuple variant", |val| match val {
            Value::Tuple(v) => Ok(if v.len() == len {
                visitor.visit_seq(SeqDeserializer::new(v.into_iter()))
            } else {
                let msg = format!("a tuple with {} element(s) was expected", len);
                Err(de::Error::invalid_length(v.len(), &msg.as_str()))
            }),
            x => Err(x),
        })
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        unexp_matching(self.0.val, &"struct variant", |val| match val {
            Value::Cntr(map) => Ok(visitor.visit_map(MapDeserializer::new(
                map.into_iter().map(|(k, v)| (k.inner, v)),
            ))),
            x => Err(x),
        })
    }
}

////// FUNCTIONS ///////////////////////////////////////////////////////////////

fn unexp_err<'a, 'v>(val: &'a Value<'v>) -> Unexpected<'a> {
    match val {
        Value::Unit => Unexpected::Unit,
        Value::Bool(v) => Unexpected::Bool(*v),
        Value::Num(n) => match n {
            Number::Uint(n) => Unexpected::Unsigned((*n).try_into().unwrap_or_default()),
            Number::Int(n) => Unexpected::Signed((*n).try_into().unwrap_or_default()),
            Number::Float(n) => Unexpected::Float(*n),
        },
        Value::Str(v) => Unexpected::Str(v.as_str()),
        Value::Barr(v) => Unexpected::Bytes(v.as_bytes()),
        Value::Tuple(_) => Unexpected::Other("tuple"),
        Value::Cntr(_) => Unexpected::Other("container"),
        Value::Seq(_) => Unexpected::Seq,
        Value::Map(_) => Unexpected::Map,
    }
}

fn unexp_matching<'v, T, E, F, S>(val: Value<'v>, expected: &S, matchfn: F) -> Result<T, E>
where
    F: FnOnce(Value<'v>) -> Result<Result<T, E>, Value<'v>>,
    E: de::Error,
    S: de::Expected,
{
    match matchfn(val) {
        Ok(x) => x,
        Err(val) => Err(de::Error::invalid_type(unexp_err(&val), expected)),
    }
}

////// ERROR ///////////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    Message(String),
}

impl de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Message(s) => write!(f, "custom error: {}", s),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::de::*;

    #[test]
    fn interface_check() {
        // try a string literal
        let kserd = Kserd::new_str("Hello, world!");
        let decoder = Decoder(kserd);
        let r = <&str>::deserialize(decoder);
        assert_eq!(r, Ok("Hello, world!"));
        let kserd = Kserd::new_str("Hello, world!");
        let decoder = Decoder(kserd);
        let r = <String>::deserialize(decoder);
        assert_eq!(r, Ok("Hello, world!".to_string()));

        // try with borrowed string
        let string = String::from("Hello, world!");
        let kserd = Kserd::new_str(&string);
        let decoder = Decoder(kserd);
        let r = <&str>::deserialize(decoder);
        assert_eq!(r, Ok("Hello, world!"));
        let kserd = Kserd::new_str(&string);
        let decoder = Decoder(kserd);
        let r = <String>::deserialize(decoder);
        assert_eq!(r, Ok("Hello, world!".to_string()));

        // try with owned string
        let kserd = Kserd::new_string(String::from("Hello, world!"));
        let decoder = Decoder(kserd.mk_brw()); // NOTICE! Use .mk_brw() to avoid error
        let r = <&str>::deserialize(decoder);
        assert_eq!(r, Ok("Hello, world!"));
        let kserd = Kserd::new_string(String::from("Hello, world!"));
        let decoder = Decoder(kserd);
        let r = <String>::deserialize(decoder);
        assert_eq!(r, Ok("Hello, world!".to_string()));
    }

    #[test]
    fn mem_de_round_trips() {
        macro_rules! round_trip_test {
	    ($t:ty, $($x:expr),*) => {{
		$(
		    let kserd = Kserd::enc(&$x).unwrap();
		    let de = Decoder(kserd);
		    let v: $t = <$t>::deserialize(de)
			.expect(concat!("failed deserializing ", stringify!($t)));
		    assert_eq!($x, v, concat!("failed equaling ", stringify!($t)));
		)*
	    }};
	};

        round_trip_test!((), ());
        round_trip_test!(bool, true, false);
        round_trip_test!(char, 'a', 'b', 'c');

        // TODO impl rng when you have internet
        round_trip_test!(u8, std::u8::MIN, std::u8::MAX);
        round_trip_test!(u16, std::u16::MIN, std::u16::MAX);
        round_trip_test!(u32, std::u32::MIN, std::u32::MAX);
        round_trip_test!(u64, std::u64::MIN, std::u64::MAX);
        round_trip_test!(u128, std::u128::MIN, std::u128::MAX);

        round_trip_test!(i8, std::i8::MIN, 0, std::i8::MAX);
        round_trip_test!(i16, std::i16::MIN, 0, std::i16::MAX);
        round_trip_test!(i32, std::i32::MIN, 0, std::i32::MAX);
        round_trip_test!(i64, std::i64::MIN, 0, std::i64::MAX);
        round_trip_test!(i128, std::i128::MIN, 0, std::i128::MAX);

        round_trip_test!(
            f32,
            0.0,
            std::f32::consts::E,
            std::f32::consts::PI,
            -std::f32::consts::E,
            -std::f32::consts::PI
        );
        round_trip_test!(
            f64,
            0.0,
            std::f64::consts::E,
            std::f64::consts::PI,
            -std::f64::consts::E,
            -std::f64::consts::PI
        );

        //         round_trip_test!(&str, "Hello, world!", "\nThis is me!");
        round_trip_test!(
            String,
            "Hello, world!".to_owned(),
            "\nThis is me!".to_owned()
        );

        // TODO might have to implement serialisation to byte arrays betterer
        // Trying to create sequences by the looks of it.
        // round_trip_test!(&[u8], &b"Hello, world!"[..], &b"\nThis is me!"[..]);
        // round_trip_test!(Vec<u8>, b"Hello, world!".to_vec(), b"\nThis is me!".to_vec());
    }

    #[derive(Debug, PartialEq)]
    struct BarrTestOwned(Vec<u8>);

    impl<'de> Deserialize<'de> for BarrTestOwned {
        fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
            d.deserialize_bytes(BarrTestVisitor)
                .map(|x| x.to_vec())
                .map(BarrTestOwned)
        }
    }

    struct BarrTestVisitor;

    impl<'de> Visitor<'de> for BarrTestVisitor {
        type Value = Vec<u8>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(formatter, "")
        }

        fn visit_borrowed_bytes<E: std::error::Error>(
            self,
            v: &'de [u8],
        ) -> Result<Self::Value, E> {
            Ok(v.to_vec())
        }

        fn visit_byte_buf<E: std::error::Error>(self, v: Vec<u8>) -> Result<Self::Value, E> {
            Ok(v)
        }

        fn visit_newtype_struct<D: Deserializer<'de>>(
            self,
            deserializer: D,
        ) -> Result<Self::Value, D::Error> {
            dbg!();
            deserializer.deserialize_bytes(BarrTestVisitor)
        }
    }

    #[derive(Debug, PartialEq)]
    struct NewTypeTest(Vec<u8>);

    impl<'de> Deserialize<'de> for NewTypeTest {
        fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
            d.deserialize_newtype_struct("", BarrTestVisitor)
                .map(|x| x.to_vec())
                .map(NewTypeTest)
        }
    }

    #[test]
    fn barr_deserializing() {
        let kserd = Kserd::new_barr([0, 1, 2].as_ref());
        let v = vec![0, 1, 2];

        let expected = BarrTestOwned(v.clone());
        let r = kserd.clone().decode::<BarrTestOwned>();
        assert_eq!(r, Ok(expected));

        let expected = BarrTestOwned(v);
        let r = kserd.clone().decode::<BarrTestOwned>();
        assert_eq!(r, Ok(expected));

        let kserd = Kserd::new_barrv(vec![0, 1, 2]);
        let r = kserd.decode::<BarrTestOwned>();
        assert_eq!(r, Ok(BarrTestOwned(vec![0, 1, 2])));
    }

    #[test]
    fn test_newtype_struct() {
        let kserd = Kserd::with_id(
            "NewTypeTest",
            Value::Tuple(vec![Kserd::new_barr([0, 1, 2].as_ref())]),
        )
        .unwrap();
        let expected = NewTypeTest(vec![0, 1, 2]);

        let r = kserd.decode::<NewTypeTest>();
        assert_eq!(r, Ok(expected));
    }

    #[test]
    fn test_deserialize_char() {
        let kserd = Kserd::new_str("x");
        let r = kserd.decode::<char>();
        assert_eq!(r, Ok('x'));

        let kserd = Kserd::new_str("");
        let r = kserd.decode::<char>().map_err(|x| x.to_string());
        assert_eq!(
            r,
            Err("custom error: invalid type: string \"\", expected char".into())
        );
    }

    struct NoImplementVisitor;
    impl<'de> Visitor<'de> for NoImplementVisitor {
        type Value = ();

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(formatter, "")
        }
    }

    #[test]
    fn unexp_char_test() {
        #[derive(Debug, PartialEq)]
        struct Dummy;
        impl<'de> Deserialize<'de> for Dummy {
            fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
                d.deserialize_char(NoImplementVisitor).map(|_| Dummy)
            }
        }

        let kserd = Kserd::new_num(3.14);
        let r = kserd.decode::<Dummy>().map_err(|e| e.to_string());
        assert_eq!(
            r,
            Err("custom error: invalid type: floating point `3.14`, expected char".into())
        );

        let kserd = Kserd::new_num(3.14);
        let r = Decoder(kserd)
            .deserialize_char(NoImplementVisitor)
            .map_err(|e| e.to_string());
        assert_eq!(
            r,
            Err("custom error: invalid type: floating point `3.14`, expected char".into())
        );
    }

    #[test]
    fn unexp_variants() {
        assert_eq!(Decoder(Kserd::new_unit()).unit_variant(), Ok(()));
        assert_eq!(
            Decoder(Kserd::new_num(3))
                .unit_variant()
                .map_err(|e| e.to_string()),
            Err("custom error: invalid type: integer `3`, expected unit value".into())
        );

        let kserd = Kserd::with_id(
            "Test",
            Value::Tuple(vec![Kserd::new_unit(), Kserd::new_unit()]),
        )
        .unwrap();
        let r = Decoder(kserd)
            .newtype_variant::<()>()
            .map_err(|e| e.to_string());
        assert_eq!(r, Err("custom error: invalid length 2, expected a tuple with a single element was expected".into()));

        let kserd = Kserd::new_num(3);
        let r = Decoder(kserd)
            .newtype_variant::<()>()
            .map_err(|e| e.to_string());
        assert_eq!(
            r,
            Err("custom error: invalid type: integer `3`, expected newtype enum variant".into())
        );

        let kserd = Kserd::with_id(
            "Test",
            Value::Tuple(vec![Kserd::new_unit(), Kserd::new_unit()]),
        )
        .unwrap();
        let r = Decoder(kserd)
            .tuple_variant(1, NoImplementVisitor)
            .map_err(|e| e.to_string());
        assert_eq!(
            r,
            Err(
                "custom error: invalid length 2, expected a tuple with 1 element(s) was expected"
                    .into()
            )
        );

        let kserd = Kserd::new_num(3);
        let r = Decoder(kserd)
            .tuple_variant(1, NoImplementVisitor)
            .map_err(|e| e.to_string());
        assert_eq!(
            r,
            Err("custom error: invalid type: integer `3`, expected tuple variant".into())
        );

        let kserd = Kserd::new_num(3);
        let r = Decoder(kserd)
            .struct_variant(&[], NoImplementVisitor)
            .map_err(|e| e.to_string());
        assert_eq!(
            r,
            Err("custom error: invalid type: integer `3`, expected struct variant".into())
        );
    }

    #[test]
    fn unexp_err_test() {
        macro_rules! test {
            ($kserd:expr, $exp:expr) => {{
                assert_eq!(
                    Decoder($kserd)
                        .tuple_variant(1, NoImplementVisitor)
                        .map_err(|e| e.to_string()),
                    Err(format!(
                        "custom error: invalid type: {}, expected tuple variant",
                        $exp
                    ))
                );
            }};
        };

        test!(Kserd::new_unit(), "unit value");
        test!(Kserd::new_bool(true), "boolean `true`");
        test!(Kserd::new_num(123u8), "integer `123`");
        test!(Kserd::new_num(-123i8), "integer `-123`");
        test!(Kserd::new_num(3.14), "floating point `3.14`");
        test!(Kserd::new_str("Hello"), "string \"Hello\"");
        test!(Kserd::new_barr([0, 1, 2].as_ref()), "byte array");
        // handle tuple outside
        assert_eq!(
            Decoder(Kserd::new(Value::Tuple(vec![])))
                .unit_variant()
                .map_err(|e| e.to_string()),
            Err(format!(
                "custom error: invalid type: {}, expected unit value",
                "tuple"
            ))
        );
        test!(
            Kserd::new_cntr(vec![("a", Kserd::new_unit())]).unwrap(),
            "container"
        );
        test!(Kserd::new(Value::Seq(vec![])), "sequence");
        test!(Kserd::new_map(vec![]), "map");
    }
}
