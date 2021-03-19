#![cfg(feature = "encode")]
extern crate kserd;
#[macro_use]
extern crate serde_derive;

use kserd::encode::Serialize;
use kserd::fmt::*;
use kserd::*;
use std::collections::BTreeMap;

fn encode_map<'a, I, K, V>(i: I) -> impl Iterator<Item = (Kserd<'a>, Kserd<'a>)>
where
    I: Iterator<Item = &'a (K, V)>,
    K: Serialize + 'a,
    V: Serialize + 'a,
{
    i.map(|(k, v)| (Kserd::enc(&k).unwrap(), Kserd::enc(&v).unwrap()))
}

#[test]
fn fmt_unit() {
    let mut map = BTreeMap::new();
    map.insert("uvalue".into(), Kserd::new_unit());
    map.insert(
        "ustruct".into(),
        Kserd::with_id("AStruct", Value::Unit).unwrap(),
    );
    map.insert(
        "uenum".into(),
        Kserd::with_id("EnumVariant", Value::Unit).unwrap(),
    );
    let kserd = Kserd::with_id("Units", Value::Cntr(map)).unwrap();

    let mut config = FormattingConfig {
        width_limit: None,
        ..Default::default()
    };

    let s = kserd.as_str_with_config(config);
    println!("{}", s);
    assert_eq!(
        &s,
        r#"Units (uenum = EnumVariant(), ustruct = AStruct(), uvalue = ())"#
    );

    config.width_limit = Some(25);
    let s = kserd.as_str_with_config(config);
    println!("{}", s);
    assert_eq!(
        &s,
        r#"Units (
    uenum = EnumVariant()
    ustruct = AStruct()
    uvalue = ()
)"#
    );

    config.width_limit = Some(0);
    let s = kserd.as_str_with_config(config);
    println!("{}", s);
    assert_eq!(
        &s,
        r#":Units
uenum = EnumVariant()
ustruct = AStruct()
uvalue = ()
"#
    );
}

#[test]
fn fmt_bool() {
    let kserd = Kserd::enc(&true).unwrap();

    assert_eq!(
        "true",
        &kserd.as_str_with_config(FormattingConfig {
            id_on_primitives: false,
            ..Default::default()
        })
    );
    assert_eq!(
        "<bool> true",
        &kserd.as_str_with_config(FormattingConfig {
            id_on_primitives: true,
            ..Default::default()
        })
    );

    let kserd = Kserd::enc(&false).unwrap();

    assert_eq!(
        "false",
        &kserd.as_str_with_config(FormattingConfig {
            id_on_primitives: false,
            ..Default::default()
        })
    );
    assert_eq!(
        "<bool> false",
        &kserd.as_str_with_config(FormattingConfig {
            id_on_primitives: true,
            ..Default::default()
        })
    );
}

#[test]
fn fmt_ch() {
    let kserd = Kserd::enc(&'a').unwrap();

    let with_ident = FormattingConfig {
        id_on_primitives: true,
        ..Default::default()
    };

    let without_ident = FormattingConfig {
        id_on_primitives: false,
        ..Default::default()
    };

    assert_eq!("\"a\"", &kserd.as_str_with_config(without_ident));
    assert_eq!("<char> \"a\"", &kserd.as_str_with_config(with_ident));

    let kserd = Kserd::enc(&'㌊').unwrap();

    assert_eq!("\"㌊\"", &kserd.as_str_with_config(without_ident));
    assert_eq!("<char> \"㌊\"", &kserd.as_str_with_config(with_ident));
}

#[test]
fn fmt_float() {
    #[allow(clippy::approx_constant)]
    let kserd = Kserd::enc(&3.14f32).unwrap();

    let with_ident = FormattingConfig {
        id_on_primitives: true,
        ..Default::default()
    };

    let without_ident = FormattingConfig {
        id_on_primitives: false,
        ..Default::default()
    };

    assert_eq!(&kserd.as_str_with_config(without_ident), r#"3.14"#);
    assert_eq!(&kserd.as_str_with_config(with_ident), r#"<f32> 3.14"#);

    let kserd = Kserd::enc(&std::f32::INFINITY).unwrap();
    assert_eq!(&kserd.as_str_with_config(without_ident), r#"inf"#);
    assert_eq!(&kserd.as_str_with_config(with_ident), r#"<f32> inf"#);

    let kserd = Kserd::enc(&std::f32::NEG_INFINITY).unwrap();
    assert_eq!(&kserd.as_str_with_config(without_ident), r#"-inf"#);
    assert_eq!(&kserd.as_str_with_config(with_ident), r#"<f32> -inf"#);

    let kserd = Kserd::enc(&std::f32::NAN).unwrap();
    assert_eq!(&kserd.as_str_with_config(without_ident), r#"NaN"#);
    assert_eq!(&kserd.as_str_with_config(with_ident), r#"<f32> NaN"#);
}

#[test]
fn fmt_str() {
    let kserd = Kserd::enc(&"My ❤ \"beats\",\non a 'new' line!").unwrap();

    let with_ident = FormattingConfig {
        id_on_primitives: true,
        ..Default::default()
    };

    let without_ident = FormattingConfig {
        id_on_primitives: false,
        ..Default::default()
    };

    assert_eq!(
        &kserd.as_str_with_config(without_ident),
        r##"str#My ❤ "beats",
on a 'new' line!#"##
    );
    assert_eq!(
        &kserd.as_str_with_config(with_ident),
        r##"<str> str#My ❤ "beats",
on a 'new' line!#"##
    );
}

#[test]
fn fmt_string() {
    let s = "My ❤ beats,\non a new line!".to_string();
    let kserd = Kserd::enc(&s).unwrap();

    let with_ident = FormattingConfig {
        id_on_primitives: true,
        ..Default::default()
    };

    let without_ident = FormattingConfig {
        id_on_primitives: false,
        ..Default::default()
    };

    assert_eq!(
        &kserd.as_str_with_config(without_ident),
        r#""My ❤ beats,
on a new line!""#
    );
    assert_eq!(
        &kserd.as_str_with_config(with_ident),
        r#"<str> "My ❤ beats,
on a new line!""#
    );
}

#[test]
fn fmt_barr() {
    let kserd = Kserd::new(Value::Barr(
        [0u8, 1, 2, 5, 10, 20, 50, 100, 150, 200, 255][..].into(),
    ));

    assert_eq!(&kserd.as_str(), "b91':C!WEH#L4RHMWL'");
}

#[test]
fn fmt_tuple() {
    let mut config = FormattingConfig {
        id_on_primitives: false,
        id_on_tuples: false,
        width_limit: None,
        ..Default::default()
    };

    let kserd = Kserd::enc(&(100, -101, "Hello, world!")).unwrap();

    let s = kserd.as_str_with_config(config);
    assert_eq!(&s, r#"(100, -101, "Hello, world!")"#);

    config.width_limit = Some(10);
    let s = kserd.as_str_with_config(config);
    assert_eq!(
        &s,
        r#"(
    100
    -101
    "Hello, world!"
)"#
    );

    let kserd = Kserd::enc(&((100, -101), "Hello, world!")).unwrap();

    let s = kserd.as_str_with_config(config);
    assert_eq!(
        &s,
        r#"(
    (
        100
        -101
    )
    "Hello, world!"
)"#
    );
}

#[test]
fn fmt_seq() {
    let mut config = FormattingConfig {
        id_on_primitives: false,
        id_on_tuples: false,
        width_limit: None,
        ..Default::default()
    };

    let var = vec![100, -101, 102, -103];
    let kserd = Kserd::enc(&var).unwrap();

    let s = kserd.as_str_with_config(config);
    assert_eq!(&s, r#"[100, -101, 102, -103]"#);

    config.width_limit = Some(16);

    let s = kserd.as_str_with_config(config);
    assert_eq!(
        &s,
        r#"[
    100
    -101
    102
    -103
]"#
    );

    config.width_limit = Some(0);

    let s = kserd.as_str_with_config(config);
    assert_eq!(
        &s,
        r#"[
    100
    -101
    102
    -103
]"#
    );
}

#[derive(Serialize)]
struct Simple {
    a: u8,
    b: (i32, i32),
    c: Vec<u8>,
}

#[test]
fn fmt_struct() {
    let var = Simple {
        a: 100,
        b: (-1, -2),
        c: vec![100, 101, 102, 103],
    };

    let kserd = Kserd::enc(&var).unwrap();

    let mut config = FormattingConfig {
        id_on_primitives: false,
        id_on_tuples: false,
        id_on_containers: true,
        width_limit: None,
        ..Default::default()
    };

    let s = kserd.as_str_with_config(config);
    assert_eq!(
        &s,
        r#"Simple (a = 100, b = (-1, -2), c = [100, 101, 102, 103])"#
    );

    config.width_limit = Some(50);
    let s = kserd.as_str_with_config(config);
    assert_eq!(
        &s,
        r#"Simple (
    a = 100
    b = (-1, -2)
    c = [100, 101, 102, 103]
)"#
    );

    config.width_limit = Some(0);
    let s = kserd.as_str_with_config(config);
    println!("{}", s);
    assert_eq!(
        &s,
        r#":Simple
a = 100
b = (
        -1
        -2
    )

[[c]]
100

[[c]]
101

[[c]]
102

[[c]]
103
"#
    );

    #[derive(Serialize)]
    struct Seq {
        inner: Vec<Simple>,
    };

    let var = Seq {
        inner: vec![
            Simple {
                a: 100,
                b: (0, 1),
                c: vec![2, 3],
            },
            Simple {
                a: 101,
                b: (1, 2),
                c: vec![4, 5],
            },
            Simple {
                a: 103,
                b: (2, 3),
                c: vec![6, 7],
            },
        ],
    };

    let kserd = Kserd::enc(&var).unwrap();

    let config = FormattingConfig {
        id_on_containers: false,
        id_on_primitives: false,
        id_on_tuples: false,
        id_on_maps: false,
        id_on_seqs: false,
        width_limit: Some(16),
    };

    let s = kserd.as_str_with_config(config);
    println!("{}", s);
    assert_eq!(
        &s,
        r#"
[[inner]]
    a = 100
    b = (0, 1)
    c = [2, 3]

[[inner]]
    a = 101
    b = (1, 2)
    c = [4, 5]

[[inner]]
    a = 103
    b = (2, 3)
    c = [6, 7]
"#
    );
}

use std::collections::HashMap;

#[derive(Serialize)]
struct SimpleMap {
    name: String,
    mapping: HashMap<(u8, u8), Simple>,
}

#[test]
fn fmt_map() {
    let var = SimpleMap {
        name: "A simple mapping".to_string(),
        mapping: vec![
            (
                (1, 2),
                Simple {
                    a: 100,
                    b: (0, 1),
                    c: vec![2, 3],
                },
            ),
            (
                (2, 3),
                Simple {
                    a: 101,
                    b: (1, 2),
                    c: vec![4, 5],
                },
            ),
            (
                (3, 4),
                Simple {
                    a: 103,
                    b: (2, 3),
                    c: vec![6, 7],
                },
            ),
        ]
        .into_iter()
        .collect(),
    };

    let kserd = Kserd::enc(&var).unwrap();

    let mut config = FormattingConfig {
        id_on_containers: false,
        id_on_primitives: false,
        id_on_tuples: false,
        id_on_maps: false,
        id_on_seqs: false,
        width_limit: None,
    };

    let s = kserd.as_str_with_config(config);
    println!("*** INLINE ***\n{}", s);
    assert_eq!(
        &s,
        r#"(mapping = { (1, 2): (a = 100, b = (0, 1), c = [2, 3]), (2, 3): (a = 101, b = (1, 2), c = [4, 5]), (3, 4): (a = 103, b = (2, 3), c = [6, 7]) }, name = "A simple mapping")"#
    );

    config.width_limit = Some(80);
    let s = kserd.as_str_with_config(config);
    println!("*** CONCISE - 80 width ***\n{}", s);
    assert_eq!(
        &s,
        r#"(
    mapping = {
                  (1, 2): (a = 100, b = (0, 1), c = [2, 3])
                  (2, 3): (a = 101, b = (1, 2), c = [4, 5])
                  (3, 4): (a = 103, b = (2, 3), c = [6, 7])
              }
    name = "A simple mapping"
)"#
    );

    config.width_limit = Some(50);
    let s = kserd.as_str_with_config(config);
    println!("*** CONCISE - 50 width ***\n{}", s);
    assert_eq!(
        &s,
        r#"(
    mapping = {
                  (1, 2): (
                              a = 100
                              b = (0, 1)
                              c = [2, 3]
                          )
                  (2, 3): (
                              a = 101
                              b = (1, 2)
                              c = [4, 5]
                          )
                  (3, 4): (
                              a = 103
                              b = (2, 3)
                              c = [6, 7]
                          )
              }
    name = "A simple mapping"
)"#
    );

    config.width_limit = Some(35);
    let s = kserd.as_str_with_config(config);
    println!("*** CONCISE - 35 width ***\n{}", s);
    assert_eq!(
        &s,
        r#"mapping = {
              (1, 2): (
                          a = 100
                          b = (0, 1)
                          c = [2, 3]
                      )
              (2, 3): (
                          a = 101
                          b = (1, 2)
                          c = [4, 5]
                      )
              (3, 4): (
                          a = 103
                          b = (2, 3)
                          c = [6, 7]
                      )
          }
name = "A simple mapping"
"#
    );

    config.width_limit = Some(14);
    config.id_on_containers = true;
    let s = kserd.as_str_with_config(config);
    println!("*** 14 width ***\n{}", s);
    assert_eq!(
        &s,
        r#":SimpleMap

[[mapping]]
(1, 2):
    :Simple
    a = 100
    b = (0, 1)
    c = [2, 3]

[[mapping]]
(2, 3):
    :Simple
    a = 101
    b = (1, 2)
    c = [4, 5]

[[mapping]]
(3, 4):
    :Simple
    a = 103
    b = (2, 3)
    c = [6, 7]

name = "A simple mapping"
"#
    );
}

#[test]
fn map_as_root() {
    let kserd = Kserd::new_map(encode_map(
        [("hello", 1), ("world", 2), ("whats", 3), ("up", 4)].iter(),
    ));

    let mut config = FormattingConfig {
        id_on_containers: false,
        id_on_primitives: false,
        id_on_tuples: false,
        id_on_maps: false,
        id_on_seqs: false,
        width_limit: None,
    };

    assert_eq!(
        &kserd.as_str_with_config(config),
        r#"{ "hello": 1, "up": 4, "whats": 3, "world": 2 }"#
    );

    config.width_limit = Some(0);
    assert_eq!(
        &kserd.as_str_with_config(config),
        r#"{
    "hello": 1
    "up": 4
    "whats": 3
    "world": 2
}"#
    );
}

#[test]
fn map_value_indenting() {
    let kserd = Kserd::with_id(
        "map",
        Value::new_map(encode_map(
            [(('c', 'c'), (3.3, "cc")), (('a', 'a'), (1.1, "aa"))].iter(),
        )),
    )
    .unwrap();

    let config = FormattingConfig {
        width_limit: Some(22),
        ..Default::default()
    };

    let s = kserd.as_str_with_config(config);
    println!("{}", s);
    assert_eq!(
        &s,
        r#"{
    ("a", "a"): (
                    1.1
                    "aa"
                )
    ("c", "c"): (
                    3.3
                    "cc"
                )
}"#
    );
}

#[test]
fn seq_of_seq_fmts_as_concise() {
    // when seqs or maps do not have a field name (such as nested seqs of seqs or maps of maps) the
    // formatting should be concise as not having a field name won't parse back.
    let kserd = Kserd::new_cntr(vec![(
        "seq",
        Kserd::new(Value::Seq(vec![
            Kserd::new(Value::Seq(vec![Kserd::new_num(0)])),
            Kserd::new_map(vec![(Kserd::new_num(1), Kserd::new_num(2))]),
        ])),
    )])
    .unwrap();

    let _s = kserd.as_str_with_config(kserd::fmt::FormattingConfig {
        width_limit: Some(0),
        ..Default::default()
    }); // should work with no panics
}

#[test]
fn bug_1() {
    let kserdstr = r#"render-as = "table"
header = [ "Case","Expr","Desc" ]
data = [
    [1, "benchmark open foo-quals.csv", "Cloning in Table (6 MB)"]
    [2, "benchmark open bar.csv", "Cloning in Table (922 MB)"]
    [3, "open diamonds.csv | benchmark filter { get carat | > 0.3 }", "Filtering _out_ 10% rows"]
    [4, "open diamonds.csv | benchmark filter { get carat | <= 0.3 }", "Filtering _to_ 10% rows"]
]"#;

    let kserd = kserd::parse::parse(kserdstr).unwrap();

    println!("****** KSERD VALUE *******\n{:#?}", kserd);

    let fmtd = kserd.as_str();

    println!("{}", fmtd);
}
