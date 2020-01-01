#![cfg(feature = "encode")]
#[macro_use]
extern crate serde_derive;

use kserd::{nav::*, *};

#[derive(Serialize)]
struct Simple {
    a: u8,
    b: String,
    c: Vec<Simple>,
}

#[test]
fn nav_ctor_simple() {
    let simple = Simple {
        a: 100,
        b: "Hello".to_owned(),
        c: vec![
            Simple {
                a: 25,
                b: "world".to_owned(),
                c: Vec::new(),
            },
            Simple {
                a: 0,
                b: "!".to_owned(),
                c: Vec::new(),
            },
        ],
    };

    let kserd = Kserd::enc(&simple).unwrap();

    Navigator::new(&kserd);
}

#[test]
fn tuples() {
    let kserd = Kserd::enc(&(100,)).unwrap();
    Navigator::new(&kserd);

    let kserd = Kserd::enc(&(100, -101)).unwrap();
    Navigator::new(&kserd);

    let kserd = Kserd::enc(&(100, -101, "Hello, world!")).unwrap();
    Navigator::new(&kserd);
}
