#![cfg(feature = "encode")]
#[macro_use]
extern crate serde_derive;

use kserd::encode::Serialize;
use kserd::*;
use parse::parse;
use std::collections::BTreeMap;

macro_rules! do_test {
    ($s:expr, $ans:expr) => {{
        // use nom::error::convert_error;

        let s: &str = $s;
        let ans: &Kserd = $ans;

        let r = parse(s);

        match r {
            Err(e) => {
                println!("************** KSERD VALUE  **************\n{}", s);
                println!(
                    "************** ERROR OUTPUT **************\n{:?}",
                    // convert_error(s, e) TODO raise bug report on this.
                    e
                );
                panic!("errored");
            }
            Ok(res) => {
                println!("************** KSERD VALUE  **************\n{}", s);
                assert_eq!(&res, ans);
            }
        }
    }};
}

fn encode_map<'a, I, K, V>(i: I) -> impl Iterator<Item = (Kserd<'a>, Kserd<'a>)>
where
    I: Iterator<Item = &'a (K, V)>,
    K: Serialize + 'a,
    V: Serialize + 'a,
{
    i.map(|(k, v)| (Kserd::enc(&k).unwrap(), Kserd::enc(&v).unwrap()))
}

fn as_kserd<T: Serialize>(val: &T) -> Kserd<'static> {
    Kserd::enc(val).unwrap()
}

mod misc {
    use super::*;

    #[test]
    fn bug_1() {
        // a more distilled version
        let s = "a = 101\n\t";
        do_test!(
            s,
            &Kserd::new_cntr(vec![("a", Kserd::new_num(101))]).unwrap()
        );

        let s = "a = 101\n\t\t";
        do_test!(
            s,
            &Kserd::new_cntr(vec![("a", Kserd::new_num(101))]).unwrap()
        );

        let s = "a = 101\n\t\t\nb = 202";
        do_test!(
            s,
            &Kserd::new_cntr(vec![("a", Kserd::new_num(101)), ("b", Kserd::new_num(202))]).unwrap()
        );

        dbg!("here");

        let s = "[[list]]
\"Hello, world!\"

[[list]]
\"This is a list\"

[[list]]
\"as entries\"

[key]
    inner-item = \"This is a container\"
    num = 101
    another = \"more words to write stuff\"
    byte-array = b91'fdsaljfjdsa'
\t";

        let kserd = Kserd::new_cntr(vec![
            (
                "list",
                Kserd::new(Value::Seq(vec![
                    Kserd::new_str("Hello, world!"),
                    Kserd::new_str("This is a list"),
                    Kserd::new_str("as entries"),
                ])),
            ),
            (
                "key",
                Kserd::new_cntr(vec![
                    ("inner-item", Kserd::new_str("This is a container")),
                    ("num", Kserd::new_num(101)),
                    ("another", Kserd::new_str("more words to write stuff")),
                    (
                        "byte-array",
                        Kserd::new_barr(&[110, 74, 45, 89, 50, 72, 22, 252, 52]),
                    ),
                ])
                .unwrap(),
            ),
        ])
        .unwrap();
        do_test!(s, &kserd);
    }
}

mod prims {
    use super::*;

    #[test]
    fn test_uint() {
        let kserd_ = Kserd::new_num(std::u128::MAX);
        let s = kserd_.as_str();
        do_test!(&s, &kserd_);

        do_test!(
            "<something> 123456789",
            &Kserd::with_id("something", Value::new_num(123456789)).unwrap()
        );

        do_test!(
            "<something>   \t\t\t  123456789",
            &Kserd::with_id("something", Value::new_num(123456789)).unwrap()
        );

        do_test!(
            "<something>123456789",
            &Kserd::with_id("something", Value::new_num(123456789)).unwrap()
        );

        do_test!("<u8> 0", &Kserd::new_num(0u8));
    }

    #[test]
    fn test_int() {
        let kserd_ = Kserd::new_num(std::i128::MAX);
        let s = kserd_.as_str();
        do_test!(&s, &as_kserd(&(std::i128::MAX as u128)));

        do_test!(
            "<something> 123456789",
            &Kserd::with_id("something", Value::new_num(123456789)).unwrap()
        );

        do_test!(
            "<something>   \t\t\t  123456789",
            &Kserd::with_id("something", Value::new_num(123456789)).unwrap()
        );

        do_test!(
            "<something>123456789",
            &Kserd::with_id("something", Value::new_num(123456789)).unwrap()
        );

        let kserd_ = Kserd::new_num(std::i128::MIN);
        let s = kserd_.as_str();
        do_test!(&s, &kserd_);

        do_test!(
            "<something> -123456789",
            &Kserd::with_id("something", Value::new_num(-123456789)).unwrap()
        );

        do_test!(
            "<something>   \t\t\t  -123456789",
            &Kserd::with_id("something", Value::new_num(-123456789)).unwrap()
        );

        do_test!(
            "<something>-123456789",
            &Kserd::with_id("something", Value::new_num(-123456789)).unwrap()
        );
    }
}

mod tuples {
    use super::*;

    #[test]
    fn test_tuple_inline() {
        let ans = Kserd::new(Value::Tuple(vec![
            Kserd::with_id("something", Value::new_num(123456789)).unwrap(),
            Kserd::new(Value::new_num(255)),
        ]));

        do_test!("(<something> 123456789, 255)", &ans);
        do_test!("(    <something>  \t\t 123456789  \t,   255 \t\t )", &ans);
        do_test!(
            "A-Name (    <something>  \t\t 123456789  \t,   255 \t\t )",
            &ans
        );
    }

    #[test]
    fn test_tuple_concise() {
        let ans = Kserd::new(Value::Tuple(vec![
            Kserd::with_id("something", Value::new_num(123456789)).unwrap(),
            Kserd::new(Value::new_num(255)),
            Kserd::with_id("last", Value::new_num(100)).unwrap(),
        ]));

        let s = r#"(
<something> 123456789
255
<last> 100
)"#;
        do_test!(s, &ans);

        let s = r#"(
<something> 123456789
255
<last> 100)"#;
        let r = parse(s);
        assert!(r.is_err()); // should fail, need new line after last one.

        let s = r#"(
					<something>    123456789   
					255   
					<last>    100   
)"#;
        do_test!(s, &ans);

        let s = r#"(



					<something>    123456789   



					255   



					<last>    100   



)"#;
        do_test!(s, &ans);
        let s = r#"  Some-Name    (
<something> 123456789
255
<last> 100
)"#;
        do_test!(s, &ans);
    }
}

mod containers {
    use super::*;
    use fmt::FormattingConfig;

    struct ByteArray<'a>(&'a [u8]);

    #[derive(Serialize)]
    struct Primitives<'a> {
        a: (),
        bc: bool,
        def: char,
        ghij: u8,
        klmno: i8,
        pqrstu: f32,
        v: &'a str,
        wx: ByteArray<'a>,
    }

    impl<'a> serde::Serialize for ByteArray<'a> {
        fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            serializer.serialize_bytes(&self.0)
        }
    }

    #[test]
    fn test_cntr_inline() {
        macro_rules! map {
		( $($k:literal $v:expr),* ) => {{
			let mut map = BTreeMap::new();
			$(
				map.insert($k.into(), $v);
			)*
			map
		}};
	};

        // do some manual tests first
        do_test!(
            "(a=1)",
            &Kserd::new(Value::Cntr(map!("a" Kserd::new_num(1))))
        );
        do_test!(
            "(a=1  \t, b \t = \t 2)",
            &Kserd::new(Value::Cntr(
                map!("a" Kserd::new_num(1), "b" Kserd::new_num(2))
            ))
        );
        do_test!(
            "(a   =  ()  \t, b \t = \t 2)",
            &Kserd::new(Value::Cntr(
                map!("a" Kserd::new_unit(), "b" Kserd::new_num(2))
            ))
        );
        do_test!(
            "Some-Name(a   =  ()  \t, b \t = \t 2)",
            &Kserd::new(Value::Cntr(
                map!("a" Kserd::new_unit(), "b" Kserd::new_num(2))
            ))
        );

        let primitives = Primitives {
            a: (),
            bc: true,
            def: 'c',
            ghij: 100,
            klmno: -100,
            pqrstu: 3.14,
            v: "Hello,\r\nworld!",
            wx: ByteArray(&[0, 1, 2, 5, 10, 20, 50, 100]),
        };

        let kserd_ = as_kserd(&primitives);

        let mut config = FormattingConfig {
            width_limit: None,
            ..Default::default()
        };

        let s = kserd_.as_str_with_config(config);
        do_test!(&s, &kserd_);

        config.id_on_primitives = true;
        config.id_on_containers = true;
        let s = kserd_.as_str_with_config(config);
        do_test!(&s, &kserd_);
    }

    #[test]
    fn test_cntr_concise() {
        let primitives = Primitives {
            a: (),
            bc: true,
            def: 'c',
            ghij: 100,
            klmno: -100,
            pqrstu: 3.14,
            v: "Hello,\r\nworld!",
            wx: ByteArray(&[0, 1, 2, 5, 10, 20, 50, 100]),
        };

        let kserd = as_kserd(&primitives);

        let mut config = FormattingConfig {
            id_on_primitives: true,
            id_on_containers: true,
            ..Default::default()
        };

        config.width_limit = Some(80);
        let s = kserd.as_str_with_config(config);
        do_test!(&s, &kserd);

        config.width_limit = Some(60);
        let s = kserd.as_str_with_config(config);
        do_test!(&s, &kserd);

        let s = "A-Name (
        a = 1
        b = 2
        )";
        do_test!(
            &s,
            &Kserd::new_cntr(vec![("a", Kserd::new_num(1)), ("b", Kserd::new_num(2))]).unwrap()
        );
    }

    #[test]
    fn test_cntr_verbose01() {
        let primitives = Primitives {
            a: (),
            bc: true,
            def: 'c',
            ghij: 100,
            klmno: -100,
            pqrstu: 3.14,
            v: "Hello,\r\nworld!",
            wx: ByteArray(&[0, 1, 2, 5, 10, 20, 50, 100]),
        };

        let kserd_ = as_kserd(&primitives);

        let mut config = FormattingConfig {
            id_on_primitives: true,
            id_on_containers: true,
            ..Default::default()
        };

        config.width_limit = Some(0);
        let s = kserd_.as_str_with_config(config).trim_end().to_string();
        do_test!(&s, &kserd_);
    }

    #[test]
    fn test_cntr_verbose02() {
        #[derive(Serialize)]
        struct Test2 {
            a_string: String,
            a_number: f64,
        };
        #[derive(Serialize)]
        struct Test {
            items: Vec<Test2>,
            items2: Vec<(u8, i64)>,
        };

        macro_rules! i {
            ($s:expr, $n:expr) => {{
                Test2 {
                    a_string: $s.to_string(),
                    a_number: $n,
                }
            }};
        };

        let structure = Test {
            items: vec![i!("Hello, world!", 3.14)],
            items2: vec![(0, 0), (1, -1), (2, -2), (3, -3)],
        };

        let kserd_ = as_kserd(&structure);

        let mut config = FormattingConfig {
            id_on_primitives: true,
            id_on_containers: true,
            id_on_seqs: true,
            ..Default::default()
        };

        config.width_limit = Some(0);
        let s = kserd_.as_str_with_config(config);
        do_test!(&s, &kserd_);
    }

    #[test]
    fn test_cntr_verbose03() {
        #[derive(Serialize)]
        struct Test2 {
            a_string: &'static str,
            a_number: f64,
            inner_test_items: Vec<Test>,
        };
        #[derive(Serialize)]
        struct Test {
            items: Vec<Test2>,
        };

        let structure = Test {
            items: vec![Test2 {
                a_string: "Hello, world!",
                a_number: 3.14,
                inner_test_items: vec![
                    Test {
                        items: vec![Test2 {
                            a_string: "Hello,",
                            a_number: 3.00,
                            inner_test_items: vec![],
                        }],
                    },
                    Test {
                        items: vec![Test2 {
                            a_string: "world!",
                            a_number: 0.14,
                            inner_test_items: vec![],
                        }],
                    },
                ],
            }],
        };

        let kserd_ = as_kserd(&structure);

        let mut config = FormattingConfig {
            id_on_primitives: true,
            id_on_containers: true,
            id_on_seqs: true,
            ..Default::default()
        };

        config.width_limit = Some(0);
        let s = kserd_.as_str_with_config(config);
        println!("{}", s);

        let ans = Kserd::new(Value::Cntr(
            vec![(
                Kstr::brwed("items"),
                Kserd::new(Value::Seq(vec![Kserd::new(Value::Cntr(
                    vec![
                        ("a_number", Kserd::new_num(3.14)),
                        ("a_string", Kserd::new_str("Hello, world!")),
                        (
                            "inner_test_items",
                            Kserd::new(Value::Seq(vec![
                                Kserd::new(Value::Cntr(
                                    vec![(
                                        "items",
                                        Kserd::new(Value::Seq(vec![Kserd::new(Value::Cntr(
                                            vec![
                                                ("a_number", Kserd::new_num(3.0)),
                                                ("a_string", Kserd::new_str("Hello,")),
                                            ]
                                            .into_iter()
                                            .map(|(k, v)| (k.into(), v))
                                            .collect(),
                                        ))])),
                                    )]
                                    .into_iter()
                                    .map(|(k, v)| (k.into(), v))
                                    .collect(),
                                )),
                                Kserd::new(Value::Cntr(
                                    vec![(
                                        "items",
                                        Kserd::new(Value::Seq(vec![Kserd::new(Value::Cntr(
                                            vec![
                                                ("a_number", Kserd::new_num(0.14)),
                                                ("a_string", Kserd::new_str("world!")),
                                            ]
                                            .into_iter()
                                            .map(|(k, v)| (k.into(), v))
                                            .collect(),
                                        ))])),
                                    )]
                                    .into_iter()
                                    .map(|(k, v)| (k.into(), v))
                                    .collect(),
                                )),
                            ])),
                        ),
                    ]
                    .into_iter()
                    .map(|(k, v)| (k.into(), v))
                    .collect(),
                ))])),
            )]
            .into_iter()
            .collect(),
        ));

        do_test!(&s, &ans);
    }

    #[test]
    fn test_cntr_verbose04() {
        #[derive(Serialize)]
        struct Test2 {
            a_string: &'static str,
            a_number: f64,
            inner_test_numbers: Test,
        };
        #[derive(Serialize)]
        struct Test {
            items: Vec<u8>,
        };

        let structure = Test2 {
            a_string: "Hello, world!",
            a_number: 3.14,
            inner_test_numbers: Test {
                items: vec![0, 2, 4, 6, 8],
            },
        };

        let kserd_ = as_kserd(&structure);

        let mut config = FormattingConfig {
            id_on_primitives: true,
            id_on_containers: true,
            id_on_seqs: true,
            ..Default::default()
        };

        config.width_limit = Some(0);
        let s = kserd_.as_str_with_config(config);
        do_test!(&s, &kserd_);
    }

    #[test]
    fn test_cntr_verbose05() {
        let mut map = BTreeMap::new();
        map.insert("a".into(), Kserd::new_num(1));
        do_test!("a = 1", &Kserd::new(Value::Cntr(map)));
    }

    #[test]
    fn test_cntr_verbose06() {
        let s = "a = 101
        
        b = 202
        c
        d = 404";
        let err = parse(s).unwrap_err().backtrace();
        assert_eq!(
            err,
            "#0: at 4:9 :: in Eof
        c
        ^"
        );
    }

    #[test]
    fn test_cntr_verbose07() {
        // notice the indent
        let s = "a = 101
        
        b = 202";
        do_test!(
            s,
            &Kserd::new_cntr(vec![("a", Kserd::new_num(101)), ("b", Kserd::new_num(202)),])
                .unwrap()
        );
    }

    #[test]
    fn named_verbose_layouts_test() {
        let s = include_str!("named-verbose.kserd");

        let kserd = parse(s).unwrap();

        assert_eq!(kserd.id(), Some("container-name"));

        let map = kserd.cntr().unwrap();

        assert_eq!(map.get("a").unwrap().id(), Some("A"));
        assert_eq!(map.get("b").unwrap().id(), Some("B"));
        assert_eq!(map.get("c").unwrap().id(), Some("C"));
        assert_eq!(map.get("d").unwrap().id(), Some("D"));
        assert_eq!(map.get("e").unwrap().id(), Some("E"));
        assert_eq!(map.get("f").unwrap().id(), Some("F"));
        assert_eq!(map.get("g").unwrap().id(), Some("G"));
        assert_eq!(map.get("h").unwrap().id(), Some("H"));
        assert_eq!(map.get("i").unwrap().id(), Some("I"));
        assert_eq!(map.get("list").unwrap().id(), Some("list-name"));
        assert_eq!(map.get("map").unwrap().id(), Some("map-name"));
        assert_eq!(map.get("cntr").unwrap().id(), Some("cntr1-name"));
        assert_eq!(map.get("cntr2").unwrap().id(), Some("cntr2name"));

        let nest = map.get_cntr("cntr2").unwrap();
        assert_eq!(nest.get("nest").unwrap().id(), Some("cntr-nested"));

        let map2 = map.get_map("map").unwrap();
        assert_eq!(map2.get(&Kserd::new_str("d")).unwrap().id(), Some("a-name"));
    }

    #[test]
    fn verbose_maps_with_inline() {
        let s = "[[map]]
            <foo>   0   :    <bar>   1  

        [[map]]
    <foo>2:<bar>4";

        do_test!(
            s,
            &Kserd::new_cntr(vec![(
                "map",
                Kserd::new_map(vec![
                    (Kserd::new_num(0), Kserd::new_num(1)),
                    (Kserd::new_num(2), Kserd::new_num(4))
                ])
            )])
            .unwrap()
        );

        let kserd = parse(s).unwrap();
        for (k, v) in kserd.cntr().unwrap().get_map("map").unwrap() {
            assert_eq!(k.id(), Some("foo"));
            assert_eq!(v.id(), Some("bar"));
        }
    }
}

mod seq {
    use super::*;
    use fmt::FormattingConfig;

    #[test]
    fn test_seq_inline() {
        do_test!(
            "[-5,-2,-1,0,1,2,5]",
            &Kserd::new(Value::Seq(vec![
                Kserd::new(Value::new_num(-5)),
                Kserd::new(Value::new_num(-2)),
                Kserd::new(Value::new_num(-1)),
                Kserd::new(Value::new_num(0)),
                Kserd::new(Value::new_num(1)),
                Kserd::new(Value::new_num(2)),
                Kserd::new(Value::new_num(5)),
            ]))
        );

        let ans = Kserd::with_id(
            "list",
            Value::Seq(vec![
                Kserd::new_num(-5),
                Kserd::new_num(-2),
                Kserd::new_num(-1),
                Kserd::new_num(0u8),
                Kserd::new_num(1u8),
                Kserd::new_num(2u8),
                Kserd::new_num(5u8),
            ]),
        )
        .unwrap();
        let s = ans.as_str_with_config(FormattingConfig {
            id_on_primitives: true,
            id_on_seqs: true,
            width_limit: None,
            ..Default::default()
        });
        do_test!(&s, &ans);

        do_test!(
            "Some-name[-5,-2,-1,0,1,2,5]",
            &Kserd::new(Value::Seq(vec![
                Kserd::new(Value::new_num(-5)),
                Kserd::new(Value::new_num(-2)),
                Kserd::new(Value::new_num(-1)),
                Kserd::new(Value::new_num(0)),
                Kserd::new(Value::new_num(1)),
                Kserd::new(Value::new_num(2)),
                Kserd::new(Value::new_num(5)),
            ]))
        );
    }

    #[test]
    fn test_seq_concise() {
        let config = FormattingConfig {
            id_on_primitives: true,
            id_on_seqs: true,
            width_limit: Some(0),
            ..Default::default()
        };

        // no idents
        let ans = Kserd::new(Value::Seq(vec![
            Kserd::new(Value::new_num(-5)),
            Kserd::new(Value::new_num(-2)),
            Kserd::new(Value::new_num(-1)),
            Kserd::new(Value::new_num(0)),
            Kserd::new(Value::new_num(1)),
            Kserd::new(Value::new_num(2)),
            Kserd::new(Value::new_num(5)),
        ]));

        do_test!("[-5,-2,-1,0,1,2,5]", &ans);

        // with idents
        let ans = Kserd::with_id(
            "list",
            Value::Seq(vec![
                Kserd::new_num(-5),
                Kserd::new_num(-2),
                Kserd::new_num(-1),
                Kserd::new_num(0u8),
                Kserd::new_num(1u8),
                Kserd::new_num(2u8),
                Kserd::new_num(5u8),
            ]),
        )
        .unwrap();
        let s = ans.as_str_with_config(config);
        do_test!(&s, &ans);
    }
}

mod map {
    use super::*;
    use fmt::FormattingConfig;

    macro_rules! n {
        ($x:expr) => {{
            Kserd::new(Value::new_num($x))
        }};
    }

    #[test]
    fn test_map_inline() {
        do_test!(
            "{0:1,2:3,4:5}",
            &Kserd::new(Value::new_map(vec![
                (n!(0), n!(1)),
                (n!(2), n!(3)),
                (n!(4), n!(5))
            ]))
        );

        do_test!(
            "{   \t\t 0  \t\t :  \t 1  ,  \t 2 :   \t 3 \t\t , 4 \t\t :\t5\t\t}",
            &Kserd::new(Value::new_map(vec![
                (n!(0), n!(1)),
                (n!(2), n!(3)),
                (n!(4), n!(5))
            ]))
        );

        let ans = Kserd::with_id(
            "map",
            Value::new_map(encode_map([(0, 1), (2, 3), (4, 5)].iter())),
        )
        .unwrap();
        let s = ans.as_str_with_config(FormattingConfig {
            id_on_primitives: true,
            id_on_maps: true,
            width_limit: None,
            ..Default::default()
        });
        do_test!(&s, &ans);

        do_test!(
            "A-Crazy-Map {0:1,2:3,4:5}",
            &Kserd::new(Value::new_map(vec![
                (n!(0), n!(1)),
                (n!(2), n!(3)),
                (n!(4), n!(5))
            ]))
        );
    }

    #[test]
    fn test_map_concise() {
        let kserd_ = Kserd::new(Value::new_map(vec![
            (n!(0), n!(1)),
            (n!(2), n!(3)),
            (n!(4), n!(5)),
        ]));

        do_test!(
            "{
			0: 1
			2: 3
			4: 5
		}",
            &kserd_
        );

        do_test!(
            "{   \t\t 0  \t\t :  \t 1  ,  \t 2 :   \t 3 \t\t , 4 \t\t :\t5\t\t}",
            &Kserd::new(Value::new_map(vec![
                (n!(0), n!(1)),
                (n!(2), n!(3)),
                (n!(4), n!(5))
            ]))
        );

        let ans = Kserd::with_id(
            "map",
            Value::new_map(encode_map([(0, 1), (2, 3), (4, 5)].iter())),
        )
        .unwrap();
        let s = ans.as_str_with_config(FormattingConfig {
            id_on_primitives: true,
            id_on_maps: true,
            width_limit: Some(0),
            ..Default::default()
        });
        do_test!(&s, &ans);

        // This is little more complicated, uses nested tuples for kv.
        let ans = Kserd::with_id(
            "map",
            Value::new_map(encode_map([(('a', 'a'), (1.1, "aa"))].iter())),
        )
        .unwrap();
        let s = ans.as_str_with_config(FormattingConfig {
            id_on_primitives: true,
            id_on_tuples: true,
            id_on_maps: true,
            width_limit: Some(0),
            ..Default::default()
        });
        do_test!(&s, &ans);
    }
}
