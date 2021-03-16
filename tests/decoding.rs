#![cfg(feature = "encode")]

use kserd::*;
use serde_derive::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, HashMap},
    f32::consts::PI as PI32,
    f64::consts::PI as PI64,
};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct UnitStruct;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct TupleStruct<'a>(u8, &'a str, f32);

#[test]
fn strings() {
    let kserd = Kserd::enc(&'a').unwrap();
    let r = kserd.decode::<char>();
    assert_eq!(r, Ok('a'));

    let s = "Hello, world!";
    let kserd = Kserd::new_str(s);
    let r = kserd.mk_brw().decode::<&str>();
    assert_eq!(r, Ok(s));
    let r = kserd.decode::<String>();
    assert_eq!(r, Ok(s.to_string()));
}

#[test]
fn optionals() {
    let none = Option::<u8>::None;
    let kserd = Kserd::enc(&none).unwrap();
    let r = kserd.decode::<Option<u8>>();
    assert_eq!(r, Ok(None));

    let some = Some(8);
    let kserd = Kserd::enc(&some).unwrap();
    let r = kserd.decode::<Option<u8>>();
    assert_eq!(r, Ok(Some(8)));
}

#[test]
fn unit_struct() {
    let u = UnitStruct;
    let kserd = Kserd::enc(&u).unwrap();
    let r = kserd.decode::<UnitStruct>();
    assert_eq!(r, Ok(UnitStruct));
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct EmptyStruct();
#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct EmptyStruct2 {}

#[test]
fn empty_struct() {
    let s = EmptyStruct();
    let kserd = Kserd::enc(&s).unwrap();
    let r = kserd.decode::<EmptyStruct>();
    assert_eq!(r, Ok(s));

    let s = EmptyStruct2 {};
    let kserd = Kserd::enc(&s).unwrap();
    let r = kserd.decode::<EmptyStruct2>();
    assert_eq!(r, Ok(s));
}

#[test]
fn tuples() {
    let t = (1,);
    let kserd = Kserd::enc(&t).unwrap();
    let r = kserd.decode::<(u8,)>();
    assert_eq!(r, Ok(t));

    let t = (1, "hello", -PI32);
    let kserd = Kserd::enc(&t).unwrap();
    let r = kserd.mk_brw().decode::<(u8, &str, f32)>();
    assert_eq!(r, Ok(t));

    let t = TupleStruct(1, "hello", -PI32);
    let kserd = Kserd::enc(&t).unwrap();
    dbg!(&kserd);
    let r = kserd.mk_brw().decode::<TupleStruct>();
    assert_eq!(r, Ok(t));
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct SomeStruct {
    n: i32,
    f: f64,
    s: String,
    ns: Option<Box<SomeStruct>>,
    u: (),
    un: UnitStruct,
    rs: Result<Box<SomeStruct>, UnitStruct>,
    en: Enum,
    its: Its,
    itsv: Vec<Its>,
    ens: Vec<Enum>,
    sv: Vec<SomeStruct>,
    map: BTreeMap<String, SomeStruct>,
}

type Its = (Option<Box<SomeStruct>>, i32, f64, String);

#[test]
fn structs() {
    let s = SomeStruct {
        n: -100,
        f: PI64,
        s: "Hello, world!".to_string(),
        ns: None,
        u: (),
        un: UnitStruct,
        en: Enum::One,
        ens: vec![Enum::One, Enum::Two],
        its: (None, 0, 0.0, String::new()),
        itsv: vec![],
        map: Default::default(),
        rs: Err(UnitStruct),
        sv: vec![],
    };

    let kserd = Kserd::enc(&s).unwrap();
    let r = kserd.decode::<SomeStruct>();
    assert_eq!(r, Ok(s));

    let s = SomeStruct {
        n: 10101,
        f: PI64,
        s: "".to_string(),
        ns: Some(Box::new(SomeStruct {
            n: -100,
            f: PI64,
            s: "Hello, world!".to_string(),
            ns: None,
            u: (),
            un: UnitStruct,
            en: Enum::One,
            ens: vec![Enum::One, Enum::Two],
            its: (None, 0, 0.0, String::new()),
            itsv: vec![],
            map: Default::default(),
            rs: Err(UnitStruct),
            sv: vec![],
        })),
        u: (),
        un: UnitStruct,
        en: Enum::One,
        ens: vec![Enum::One, Enum::Two],
        its: (None, 0, 0.0, String::new()),
        itsv: vec![],
        map: Default::default(),
        rs: Err(UnitStruct),
        sv: vec![],
    };

    let kserd = Kserd::enc(&s).unwrap();
    let r = kserd.decode::<SomeStruct>();
    assert_eq!(r, Ok(s));
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
enum Enum {
    One,
    Two,
    NType(u8),
    Tuple(u8, i16, f64),
    Struct { a: u8, b: String, c: f64 },
}

#[test]
fn unit_enum() {
    let kserd = Kserd::enc(&Enum::One).unwrap();
    let r = kserd.decode::<Enum>();
    assert_eq!(r, Ok(Enum::One));

    let kserd = Kserd::enc(&Enum::Two).unwrap();
    let r = kserd.decode::<Enum>();
    assert_eq!(r, Ok(Enum::Two));
}

#[test]
fn newtype_enum_variant() {
    let e = Enum::NType(120);
    let kserd = Kserd::enc(&e).unwrap();
    dbg!(&kserd);
    println!("{}", kserd.as_str());
    let r = kserd.decode::<Enum>();
    assert_eq!(r, Ok(e));
}

#[test]
fn tuple_enum_variant() {
    let e = Enum::Tuple(120, -100, PI64);
    let kserd = Kserd::enc(&e).unwrap();
    dbg!(&kserd);
    println!("{}", kserd.as_str());
    let r = kserd.decode::<Enum>();
    assert_eq!(r, Ok(e));
}

#[test]
fn struct_enum_variant() {
    let e = Enum::Struct {
        a: 120,
        b: "Hello, world!".to_string(),
        c: -PI64,
    };
    let kserd = Kserd::enc(&e).unwrap();
    dbg!(&kserd);
    println!("{}", kserd.as_str());
    let r = kserd.decode::<Enum>();
    assert_eq!(r, Ok(e));
}

#[test]
fn seqs() {
    let v = vec![-1, 0, 1, 5];
    let kserd = Kserd::enc(&v).unwrap();
    let r = kserd.decode::<Vec<i32>>();
    assert_eq!(r, Ok(v));
}

#[test]
fn maps() {
    let m = vec![("a", 0), ("b", 1), ("c", 2)]
        .into_iter()
        .collect::<HashMap<_, _>>();

    let kserd = Kserd::enc(&m).unwrap();
    let r = kserd.mk_brw().decode::<HashMap<&str, u32>>();
    assert_eq!(r, Ok(m));
}

// ###### FUZZING ##############################################################
// Fuzzing is meant for round trip checks of encoding, serialisation, parsing, decoding.
mod fuzzing {
    use super::*;
    use kserd::fmt::FormattingConfig;
    use rand::{rngs::ThreadRng, *};
    use std::error;

    trait Fuzz {
        fn produce(rng: &mut ThreadRng) -> Self;
    }

    macro_rules! impl_fuzz {
        ($($t:ty)*) => {
            $(
            impl Fuzz for $t { fn produce(rng: &mut ThreadRng) -> Self { rng.gen() } }
            )*
        };
    }
    impl_fuzz! {
        u8 u16 u32 u64 u128 usize
        i8 i16 i32 i64 i128 isize
        f32 f64
    }

    const FUZZ_ITERATIONS: u64 = 10_000;

    fn check_round_trip<T>(
        rng: &mut ThreadRng,
    ) -> Result<(), (Box<dyn error::Error>, T, FormattingConfig)>
    where
        T: Fuzz + serde::Serialize + PartialEq,
        T: for<'a> serde::Deserialize<'a>,
    {
        let t = T::produce(rng);
        let fmt_config = FormattingConfig::produce(rng);
        let kserd = match Kserd::enc(&t) {
            Ok(x) => x,
            Err(e) => return Err((format!("Kserd::enc error: {}", e).into(), t, fmt_config)),
        };
        let s = kserd.as_str_with_config(fmt_config);
        let d = match kserd::parse::parse(&s) {
            Ok(x) => x,
            Err(e) => {
                return Err((
                    format!("Kserd::parse error: {}", e.backtrace()).into(),
                    t,
                    fmt_config,
                ))
            }
        };
        if d != kserd {
            return Err((
                "The deserialised Kserd did not match the input Kserd".into(),
                t,
                fmt_config,
            ));
        }
        let tt = match d.decode::<T>() {
            Ok(x) => x,
            Err(e) => return Err((format!("Kserd::decode error: {}", e).into(), t, fmt_config)),
        };
        if t != tt {
            Err((
                "The decoded T does not match in the input T".into(),
                t,
                fmt_config,
            ))
        } else {
            Ok(())
        }
    }

    macro_rules! check_round_trip {
    ($($fn:ident<$type:ty>),*) => {
        $(
            check_round_trip!($fn, $type);
        )*
    };
    ($fn:ident, $type:ty) => {
        #[test]
        fn $fn() {
            let mut rng = rand::thread_rng();
            for _ in 0..FUZZ_ITERATIONS {
                if let Err((e, t, f)) = check_round_trip::<$type>(&mut rng) {
                    println!("---- Failure checking round trip for type `{}`", stringify!($type));
                    println!("---- Error message: {}", e);
                    println!("---- Value that failed:\n{:#?}", t);
                    println!("---- FormattingConfig:\n{:#?}", f);
                    panic!("Failure checking round trip for type `{}`", stringify!($type));
                }
            }
        }
    };
}

    impl Fuzz for FormattingConfig {
        fn produce(rng: &mut ThreadRng) -> Self {
            Self {
                id_on_primitives: rng.gen(),
                id_on_seqs: rng.gen(),
                id_on_maps: rng.gen(),
                width_limit: rng.gen(),
                ..Default::default()
            }
        }
    }

    check_round_trip! {
        fuzz_rt_u8<u8>, fuzz_rt_u16<u16>, fuzz_rt_u32<u32>, fuzz_rt_u64<u64>, fuzz_rt_u128<u128>, fuzz_rt_usize<usize>,
        fuzz_rt_i8<i8>, fuzz_rt_i16<i16>, fuzz_rt_i32<i32>, fuzz_rt_i64<i64>, fuzz_rt_i128<i128>, fuzz_rt_isize<isize>,
        fuzz_rt_f32<f32>, fuzz_rt_f64<f64>
    }
}

#[test]
fn u8_decode_test() {
    let x = 123u8;
    let y = Kserd::enc(&x).unwrap();
    let y = y.as_str();
    let y = kserd::parse::parse(&y).unwrap();
    let y = y.decode::<u8>().unwrap();
    assert_eq!(x, y);
}

#[test]
fn u64_decode_test() {
    let x = 16803534192531604596u64;
    let y = Kserd::enc(&x).unwrap();
    let y = y.as_str();
    println!("Serialized: {}", y);
    let y = kserd::parse::parse(&y).unwrap();
    println!("Parsed: {:?}", y);
    let y = y.decode::<u64>().unwrap();
    assert_eq!(x, y);
}
