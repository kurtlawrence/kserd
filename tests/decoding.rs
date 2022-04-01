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
    map: BTreeMap<String, UnitStruct>,
}

type Its = (Option<Box<String>>, i32, f64, String);

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
        })),
        u: (),
        un: UnitStruct,
        en: Enum::One,
        ens: vec![Enum::One, Enum::Two],
        its: (None, 0, 0.0, String::new()),
        itsv: vec![],
        map: Default::default(),
        rs: Err(UnitStruct),
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
    println!("{}", kserd.as_str());
    let r = kserd.decode::<Enum>();
    assert_eq!(r, Ok(e));
}

#[test]
fn tuple_enum_variant() {
    let e = Enum::Tuple(120, -100, PI64);
    let kserd = Kserd::enc(&e).unwrap();
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
    use std::{error, iter::*};

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
        f32 f64 bool
    }
    impl Fuzz for () {
        fn produce(_: &mut ThreadRng) -> () {
            ()
        }
    }
    impl Fuzz for String {
        fn produce(rng: &mut ThreadRng) -> Self {
            let take = rng.gen_range(0..30);
            repeat_with(|| rng.gen::<char>()).take(take).collect()
        }
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
                width_limit: if rng.gen() {
                    Some(rng.gen_range(0..120))
                } else {
                    None
                },
                ..Default::default()
            }
        }
    }

    check_round_trip! { fuzz_rt_unit<()> } // Unit
    check_round_trip! { fuzz_rt_bool<bool> } // Boolean
                                             // Numbers
    check_round_trip! {
        fuzz_rt_u8<u8>, fuzz_rt_u16<u16>, fuzz_rt_u32<u32>, fuzz_rt_u64<u64>, fuzz_rt_u128<u128>, fuzz_rt_usize<usize>,
        fuzz_rt_i8<i8>, fuzz_rt_i16<i16>, fuzz_rt_i32<i32>, fuzz_rt_i64<i64>, fuzz_rt_i128<i128>, fuzz_rt_isize<isize>,
        fuzz_rt_f32<f32>, fuzz_rt_f64<f64>
    }
    check_round_trip! { fuzz_rt_string<String>, fuzz_rt_bytes<Vec<u8>> } // Bytes

    // Common sum types
    impl<T: Fuzz> Fuzz for Option<T> {
        fn produce(rng: &mut ThreadRng) -> Self {
            if rng.gen() {
                Some(T::produce(rng))
            } else {
                None
            }
        }
    }
    impl<T: Fuzz, E: Fuzz> Fuzz for Result<T, E> {
        fn produce(rng: &mut ThreadRng) -> Self {
            if rng.gen() {
                Ok(T::produce(rng))
            } else {
                Err(E::produce(rng))
            }
        }
    }
    check_round_trip! { fuzz_rt_option<Option<usize>>, fuzz_rt_result<Result<f64, isize>> }

    // Unit structs
    impl Fuzz for UnitStruct {
        fn produce(_: &mut ThreadRng) -> Self {
            Self
        }
    }
    impl Fuzz for EmptyStruct {
        fn produce(_: &mut ThreadRng) -> Self {
            EmptyStruct()
        }
    }
    impl Fuzz for EmptyStruct2 {
        fn produce(_: &mut ThreadRng) -> Self {
            EmptyStruct2 {}
        }
    }
    check_round_trip! { fuzz_rt_unit_struct<UnitStruct>, fuzz_rt_empty_struct<EmptyStruct>, fuzz_rt_empty_struct2<EmptyStruct2> }

    // My structures test
    impl Fuzz for Enum {
        fn produce(rng: &mut ThreadRng) -> Self {
            match rng.gen_range(0..5) {
                0 => Enum::One,
                1 => Enum::Two,
                2 => Enum::NType(rng.gen()),
                3 => Enum::Tuple(rng.gen(), rng.gen(), rng.gen()),
                4 => Enum::Struct {
                    a: rng.gen(),
                    b: String::produce(rng),
                    c: rng.gen(),
                },
                _ => unreachable!(),
            }
        }
    }
    impl<T: Fuzz> Fuzz for Box<T> {
        fn produce(rng: &mut ThreadRng) -> Self {
            Box::new(T::produce(rng))
        }
    }
    impl Fuzz for SomeStruct {
        fn produce(rng: &mut ThreadRng) -> Self {
            // handle the nesting structures manually to avoid massively nested items
            Self {
                n: rng.gen(),
                f: rng.gen(),
                s: Fuzz::produce(rng),
                ns: if rng.gen_range(0..10) == 0 {
                    Fuzz::produce(rng)
                } else {
                    None
                },
                u: (),
                un: UnitStruct,
                rs: if rng.gen_range(0..10) == 0 {
                    Fuzz::produce(rng)
                } else {
                    Err(UnitStruct)
                },
                en: Fuzz::produce(rng),
                its: Fuzz::produce(rng),
                itsv: Fuzz::produce(rng),
                ens: Fuzz::produce(rng),
                map: Fuzz::produce(rng),
            }
        }
    }
    impl Fuzz for Its {
        fn produce(rng: &mut ThreadRng) -> Self {
            (Fuzz::produce(rng), rng.gen(), rng.gen(), Fuzz::produce(rng))
        }
    }
    impl<T: Fuzz> Fuzz for Vec<T> {
        fn produce(rng: &mut ThreadRng) -> Self {
            let i = if rng.gen() { 0 } else { rng.gen_range(0..2) };
            repeat_with(|| T::produce(rng)).take(i).collect()
        }
    }
    impl<K: Fuzz + Ord, V: Fuzz> Fuzz for BTreeMap<K, V> {
        fn produce(rng: &mut ThreadRng) -> Self {
            let i = if rng.gen() { 0 } else { rng.gen_range(0..2) };
            repeat_with(|| (K::produce(rng), V::produce(rng)))
                .take(i)
                .collect()
        }
    }

    check_round_trip! { fuzz_rt_enum<Enum>, fuzz_rt_tuple<Its> }

    #[test]
    fn fuzz_rt_some_struct() {
        std::thread::Builder::new()
            .name("some struct fuzzing".to_string())
            .stack_size(1024 * 1024 * 100)
            .spawn(|| {
                let mut rng = rand::thread_rng();
                for _ in 0..FUZZ_ITERATIONS {
                    if let Err((e, t, f)) = check_round_trip::<SomeStruct>(&mut rng) {
                        println!(
                            "---- Failure checking round trip for type `{}`",
                            "SomeStruct"
                        );
                        println!("---- Error message: {}", e);
                        println!("---- Value that failed:\n{:#?}", t);
                        println!("---- FormattingConfig:\n{:#?}", f);
                        panic!("Failure checking round trip for type `{}`", "SomeStruct");
                    }
                }
            })
            .unwrap()
            .join()
            .unwrap();
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

#[test]
fn str_decode_test() {
    let x = "𦭅\u{f51e9}\u{1621c}\u{147ea}\u{c2706}\u{197af}\u{f644c}\u{84d9d}𢣸\u{9fffb}\u{6cf65}\u{d2541}\u{e7be8}\u{72071}\u{f8030}\u{925b3}\u{67e40}\u{7fa89}";
    println!("String: {}", x);
    let y = Kserd::enc(&x).unwrap();
    let y = y.as_str();
    println!("Serialized: {}", y);
    let y = kserd::parse::parse(&y).unwrap();
    println!("Parsed: {:?}", y);
    let y = y.decode::<String>().unwrap();
    assert_eq!(x, y);
}

#[test]
fn empty_struct2_decode_test() {
    let y = Kserd::enc(&EmptyStruct2 {}).unwrap();
    let y = y.as_str_with_config(kserd::fmt::FormattingConfig {
        width_limit: Some(0),
        ..Default::default()
    });
    println!("Serialized: {}", y);
    let y = kserd::parse::parse(&y).unwrap();
    println!("Parsed: {:?}", y);
    let y = y.decode::<EmptyStruct2>().unwrap();
    assert_eq!(EmptyStruct2 {}, y);
}

#[test]
fn some_struct_decode_test_1() {
    let s = SomeStruct {
            n: -1534911249,
                f: 0.3503985440975186,
                    s: "\u{878b0}\u{eeddb}\u{5dd09}\u{f7a12}씝\u{ccacc}\u{a6a4f}\u{bc0f7}\u{7cf22}\u{70e10}\u{108360}\u{d70bf}\u{75aff}\u{42be3}\u{b2851}\u{1026ea}\u{a1d0c}\u{470f4}\u{50ebe}\u{69fad}\u{87c60}\u{62eed}\u{1487c}\u{77e4d}\u{39624}\u{f09b0}".to_string(),
                        ns: None,
                            u: (),
                                un: UnitStruct,
                                    rs: Err(
                                                UnitStruct,
                                                    ),
                                                        en: Enum::Tuple(
                                                                    213,
                                                                            -15278,
                                                                                    0.5143220283660902,
                                                                                        ),
                                                                                            its: (
                                                                                                        None,
                                                                                                                -1039545731,
                                                                                                                        0.9572395051775555,
                                                                                                                                "".to_string(),
                                                                                                                                    ),
                                                                                                                                        itsv: vec![],
                                                                                                                                            ens: vec![
                                                                                                                                                        Enum::Two,
                                                                                                                                                            ],
                                                                                                                                                                map: Default::default(),
    };

    let y = Kserd::enc(&s).unwrap();
    let y = y.as_str_with_config(kserd::fmt::FormattingConfig {
        id_on_primitives: true,
        id_on_maps: true,
        width_limit: Some(0),
        ..Default::default()
    });
    println!("Serialized: {}", y);
    let y = kserd::parse::parse(&y).unwrap();
    println!("Parsed: {:?}", y);
    let y = y.decode::<SomeStruct>().unwrap();
    assert_eq!(s, y);
}

#[test]
fn some_struct_decode_test_2() {
    let s = SomeStruct {
    n: 1785306414,
    f: 0.9600825275211862,
    s: "\u{d2a3c}\u{c36d9}\u{56c77}紐\u{e15c}\u{f8a54}\u{572ce}\u{10727c}\u{d606c}\u{105a35}\u{3296f}".to_string(),
    ns: None,
    u: (),
    un: UnitStruct,
    rs: Err(
        UnitStruct,
    ),
    en: Enum::NType(
        30,
    ),
    its: (
        None,
        2101028762,
        0.8603156654015199,
        "".to_string(),
    ),
    itsv: vec![],
    ens: vec![
        Enum::Struct {
            a: 187,
            b: "\u{b57a6}\u{f7003}\u{78529}\u{d2a11}\u{3fff9}\u{e486f}\u{3c103}\u{98d3c}\u{42bbe}\u{10cc5f}".to_string(),
            c: 0.5815612461032534,
        },
    ],
    map: vec![(
        "\u{2f520}\u{b1cc9}\u{50f64}\u{f3c5c}\u{f7fb8}\u{c4f59}\u{d55cf}\u{72e88}\u{3ea1a}\u{dbeaa}\u{d6f0b}\u{d2107}\u{10bd17}𰷧\u{5c5a5}".to_string(), UnitStruct)].into_iter().collect(),
};

    let y = Kserd::enc(&s).unwrap();
    let y = y.as_str_with_config(kserd::fmt::FormattingConfig {
        id_on_primitives: true,
        id_on_maps: true,
        id_on_seqs: true,
        width_limit: Some(77),
        ..Default::default()
    });
    println!("Serialized: {}", y);
    let y = kserd::parse::parse(&y).unwrap();
    println!("Parsed: {:?}", y);
    let y = y.decode::<SomeStruct>().unwrap();
    assert_eq!(s, y);
}

// ###### OTHER ################################################################
#[test]
fn structs_encode_as_containers() {
    #[derive(Serialize)]
    struct S {
        a: u8,
        b: String,
    }

    let s = S {
        a: 0,
        b: "hello".to_string(),
    };

    let kserd = Kserd::enc(&s).unwrap();

    let cntr = kserd.cntr().expect("should be a container");
    assert_eq!(cntr.get_num("a"), Some(0.into()));
    assert_eq!(cntr.get_str("b"), Some("hello".into()));

    let v = vec![s];

    let kserd = Kserd::enc(&v).unwrap();

    let seq = kserd.seq().unwrap();
    let x = seq.get(0).unwrap();

    let cntr = x.cntr().expect("should be a container");
    assert_eq!(cntr.get_num("a"), Some(0.into()));
    assert_eq!(cntr.get_str("b"), Some("hello".into()));
}
