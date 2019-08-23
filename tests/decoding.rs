#[macro_use]
extern crate serde_derive;

use kserd::*;
use std::collections::HashMap;

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

#[test]
fn empty_struct() {
    let s = EmptyStruct();
    let kserd = Kserd::enc(&s).unwrap();
    let r = kserd.decode::<EmptyStruct>();
    assert_eq!(r, Ok(s));
}

#[test]
fn tuples() {
    let t = (1,);
    let kserd = Kserd::enc(&t).unwrap();
    let r = kserd.decode::<(u8,)>();
    assert_eq!(r, Ok(t));

    let t = (1, "hello", -3.14);
    let kserd = Kserd::enc(&t).unwrap();
    let r = kserd.mk_brw().decode::<(u8, &str, f32)>();
    assert_eq!(r, Ok(t));

    let t = TupleStruct(1, "hello", -3.14);
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
}

#[test]
fn structs() {
    let s = SomeStruct {
        n: -100,
        f: 3.14,
        s: "Hello, world!".to_string(),
        ns: None,
        u: (),
        un: UnitStruct,
    };

    let kserd = Kserd::enc(&s).unwrap();
    let r = kserd.decode::<SomeStruct>();
    assert_eq!(r, Ok(s));

    let s = SomeStruct {
        n: 10101,
        f: 3.14,
        s: "".to_string(),
        ns: Some(Box::new(SomeStruct {
            n: -100,
            f: 3.14,
            s: "Hello, world!".to_string(),
            ns: None,
            u: (),
            un: UnitStruct,
        })),
        u: (),
        un: UnitStruct,
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
    let e = Enum::Tuple(120, -100, 3.14);
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
        c: -3.14,
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
