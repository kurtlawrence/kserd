use super::*;
use serde::{ser, Serialize};
use std::{error, fmt};

type Res = Result<Kserd<'static>, Error>;

/// Encoder to pass to [`Serialize::serialize`] to encode a type into a [`Kserd`].
///
/// There is no data associated with the `Encoder`, instead it is used to implement `serde`'s
/// `Serializer` trait. It can be used to encode a data type that implements [`Serialize`] like
/// so:
/// ```rust
/// # use kserd::*;
/// use kserd::encode::Encoder;
/// use kserd::encode::Serialize;
///
/// let data = ("Hello!", 3.14);
/// let expected = Kserd::new(
///     Value::Tuple(vec![
///         Kserd::new_str("Hello!"),
///         Kserd::new_num(3.14),
///     ])
/// );
///
/// let kserd = data.serialize(Encoder);
/// assert_eq!(kserd, Ok(expected));
/// ```
///
/// It is important to note that the signature of the [`Serialize`] trait does not allow
/// propogation of borrowed data. Specifically, when implementing a `Serializer` the values
/// are either passed through copied, or as ephemeral borrows as in the case of strings and byte
/// arrays. This limits the [`Kserd`] to be an _owned_ copy, with a `'static` lifetime parameter.
/// This has implications when decoding as described in the [`Decoder`] documentation.
/// An alternate trait [`ToKserd`] is defined that _consumes_ the data structure and allows
/// borrowed data to be propogated to the [`Kserd`].
///
/// There is also a helper function [`Kserd::enc`] that can encode without importing the `Encoder`.
/// ```rust
/// # use kserd::*;
/// let expected = Kserd::new(
///     Value::Tuple(vec![
///         Kserd::new_str("Hello!"),
///         Kserd::new_num(3.14),
///     ])
/// );
///
/// let kserd = Kserd::enc(&("Hello!", 3.14));
/// assert_eq!(kserd, Ok(expected));
/// ```
///
/// [`Decoder`]: crate::encode::Decoder
/// [`Kserd`]: crate::Kserd
/// [`Kserd::enc`]: crate::Kserd::enc
/// [`Serialize`]: crate::Serialize
/// [`Serialize::serialize`]: crate::Serialize::serialize
/// [`ToKserd`]: crate::ToKserd
pub struct Encoder;

impl ser::Serializer for Encoder {
    type Ok = Kserd<'static>;
    type Error = Error;
    type SerializeSeq = SeqLike;
    type SerializeTuple = TupleLike;
    type SerializeTupleStruct = TupleLike;
    type SerializeTupleVariant = TupleLike;
    type SerializeMap = MapLike;
    type SerializeStruct = CntrLike;
    type SerializeStructVariant = CntrLike;

    fn serialize_bool(self, v: bool) -> Res {
        Ok(Kserd::new_bool(v))
    }

    fn serialize_i8(self, v: i8) -> Res {
        Ok(Kserd::new_num(v))
    }

    fn serialize_i16(self, v: i16) -> Res {
        Ok(Kserd::new_num(v))
    }

    fn serialize_i32(self, v: i32) -> Res {
        Ok(Kserd::new_num(v))
    }

    fn serialize_i64(self, v: i64) -> Res {
        Ok(Kserd::new_num(v))
    }

    fn serialize_i128(self, v: i128) -> Res {
        Ok(Kserd::new_num(v))
    }

    fn serialize_u8(self, v: u8) -> Res {
        Ok(Kserd::new_num(v))
    }

    fn serialize_u16(self, v: u16) -> Res {
        Ok(Kserd::new_num(v))
    }

    fn serialize_u32(self, v: u32) -> Res {
        Ok(Kserd::new_num(v))
    }

    fn serialize_u64(self, v: u64) -> Res {
        Ok(Kserd::new_num(v))
    }

    fn serialize_u128(self, v: u128) -> Res {
        Ok(Kserd::new_num(v))
    }

    fn serialize_f32(self, v: f32) -> Res {
        Ok(Kserd::new_num(v))
    }

    fn serialize_f64(self, v: f64) -> Res {
        Ok(Kserd::new_num(v))
    }

    fn serialize_char(self, v: char) -> Res {
        Ok(Kserd::with_id_unchk(
            "char",
            Value::Str(Kstr::owned(v.to_string())),
        ))
    }

    fn serialize_str(self, v: &str) -> Res {
        Ok(Kserd::new_str(v).to_owned())
    }

    fn serialize_bytes(self, v: &[u8]) -> Res {
        Ok(Kserd::new_barr(v).to_owned())
    }

    fn serialize_none(self) -> Res {
        Ok(Kserd::with_id_unchk("None", Value::Unit))
    }

    fn serialize_some<T: ?Sized + Serialize>(self, v: &T) -> Res {
        Ok(Kserd::with_id_unchk(
            "Some",
            Value::Tuple(vec![v.serialize(self)?]),
        ))
    }

    fn serialize_unit(self) -> Res {
        Ok(Kserd::new_unit())
    }

    fn serialize_unit_struct(self, name: &'static str) -> Res {
        Kserd::with_id(name, Value::Unit).map_err(Error::InvalidName)
    }

    fn serialize_unit_variant(self, _: &'static str, _: u32, variant: &'static str) -> Res {
        Kserd::with_id(variant, Value::Unit).map_err(Error::InvalidName)
    }

    fn serialize_newtype_struct<T: ?Sized + Serialize>(self, name: &'static str, value: &T) -> Res {
        Kserd::with_id(name, Value::Tuple(vec![value.serialize(self)?])).map_err(Error::InvalidName)
    }

    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        _: &'static str,
        _: u32,
        variant: &'static str,
        value: &T,
    ) -> Res {
        Kserd::with_id(variant, Value::Tuple(vec![value.serialize(self)?]))
            .map_err(Error::InvalidName)
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<SeqLike, Error> {
        Ok(SeqLike {
            kserd: Kserd::new(Value::Seq(Vec::with_capacity(len.unwrap_or_default()))),
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<TupleLike, Error> {
        Ok(TupleLike {
            kserd: Kserd::new(Value::Tuple(Vec::with_capacity(len))),
        })
    }

    fn serialize_tuple_struct(self, name: &'static str, len: usize) -> Result<TupleLike, Error> {
        Ok(TupleLike {
            kserd: Kserd::with_id(name, Value::Tuple(Vec::with_capacity(len)))
                .map_err(Error::InvalidName)?,
        })
    }

    fn serialize_tuple_variant(
        self,
        _: &'static str,
        _: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<TupleLike, Error> {
        Ok(TupleLike {
            kserd: Kserd::with_id(variant, Value::Tuple(Vec::with_capacity(len)))
                .map_err(Error::InvalidName)?,
        })
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<MapLike, Error> {
        Ok(MapLike {
            key: None,
            kserd: Kserd::new(Value::Map(BTreeMap::new())),
        })
    }

    fn serialize_struct(self, name: &'static str, _len: usize) -> Result<CntrLike, Error> {
        Ok(CntrLike {
            kserd: Kserd::with_id(name, Value::Cntr(Fields::new())).map_err(Error::InvalidName)?,
        })
    }

    fn serialize_struct_variant(
        self,
        _: &'static str,
        _: u32,
        variant: &'static str,
        _: usize,
    ) -> Result<CntrLike, Error> {
        Ok(CntrLike {
            kserd: Kserd::with_id(variant, Value::Cntr(Fields::new()))
                .map_err(Error::InvalidName)?,
        })
    }

    fn collect_str<T: ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: std::fmt::Display,
    {
        self.serialize_str(&value.to_string())
    }
}

/// Kserd serialization error.
#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    /// There is only one possible error, if an implementor of
    /// `Serialize` when serializing a map calls `serialize_value` before
    /// calling `serialize_key`. This error is extremely rare and would only
    /// occur where the implementor of `Serialize` does not follow the guidance
    /// from `serde`.
    NoKeyAvailable,
    /// The identity string contains invalid characters.
    /// See [`Kserd::with_id`](crate::Kserd::with_id).
    InvalidName(String),
    /// Some `Serialize` implementor error occurred.
    Message(String),
}

impl ser::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::NoKeyAvailable => {
                write!(f, "no key was available when trying to serialize map value")
            }
            Error::InvalidName(s) => write!(f, "name contains invalid characters: {}", s),
            Error::Message(s) => write!(f, "custom error: {}", s),
        }
    }
}

pub struct SeqLike {
    kserd: Kserd<'static>,
}

impl ser::SerializeSeq for SeqLike {
    type Ok = Kserd<'static>;
    type Error = Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Error> {
        match &mut self.kserd.val {
            Value::Seq(v) => v.push(value.serialize(Encoder)?),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn end(self) -> Res {
        Ok(self.kserd)
    }
}

pub struct TupleLike {
    kserd: Kserd<'static>,
}

impl ser::SerializeTuple for TupleLike {
    type Ok = Kserd<'static>;
    type Error = Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Error> {
        match &mut self.kserd.val {
            Value::Tuple(v) => v.push(value.serialize(Encoder)?),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn end(self) -> Res {
        Ok(self.kserd)
    }
}

impl ser::SerializeTupleStruct for TupleLike {
    type Ok = Kserd<'static>;
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Error> {
        match &mut self.kserd.val {
            Value::Tuple(v) => v.push(value.serialize(Encoder)?),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn end(self) -> Res {
        Ok(self.kserd)
    }
}

impl ser::SerializeTupleVariant for TupleLike {
    type Ok = Kserd<'static>;
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Error> {
        match &mut self.kserd.val {
            Value::Tuple(v) => v.push(value.serialize(Encoder)?),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn end(self) -> Res {
        Ok(self.kserd)
    }
}

pub struct MapLike {
    key: Option<Kserd<'static>>,
    kserd: Kserd<'static>,
}

impl ser::SerializeMap for MapLike {
    type Ok = Kserd<'static>;
    type Error = Error;

    fn serialize_key<T: ?Sized + Serialize>(&mut self, key: &T) -> Result<(), Error> {
        self.key = Some(key.serialize(Encoder)?);
        Ok(())
    }

    fn serialize_value<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Error> {
        let key = self.key.take().ok_or(Error::NoKeyAvailable)?;

        match &mut self.kserd.val {
            Value::Map(v) => {
                v.insert(key, value.serialize(Encoder)?);
            }
            _ => unreachable!(),
        }

        Ok(())
    }

    fn serialize_entry<K, V>(&mut self, key: &K, value: &V) -> Result<(), Error>
    where
        K: ?Sized + Serialize,
        V: ?Sized + Serialize,
    {
        match &mut self.kserd.val {
            Value::Map(v) => {
                v.insert(key.serialize(Encoder)?, value.serialize(Encoder)?);
            }
            _ => unreachable!(),
        }

        Ok(())
    }

    fn end(self) -> Res {
        Ok(self.kserd)
    }
}

pub struct CntrLike {
    kserd: Kserd<'static>,
}

impl ser::SerializeStruct for CntrLike {
    type Ok = Kserd<'static>;
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Error> {
        match &mut self.kserd.val {
            Value::Cntr(cntr) => {
                cntr.insert(key.into(), value.serialize(Encoder)?);
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    fn end(self) -> Res {
        Ok(self.kserd)
    }
}

impl ser::SerializeStructVariant for CntrLike {
    type Ok = Kserd<'static>;
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Error> {
        match &mut self.kserd.val {
            Value::Cntr(cntr) => {
                cntr.insert(key.into(), value.serialize(Encoder)?);
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    fn end(self) -> Res {
        Ok(self.kserd)
    }
}
