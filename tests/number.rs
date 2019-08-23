use kserd::*;
use std::cmp::Ordering::*;

#[test]
fn number_eq() {
    let n1 = Number::from(100u8);
    let n2 = Number::from(100i8);
    let n3 = Number::from(100.0);

    assert_eq!(n1, n2);
    assert_eq!(n2, n3);
    assert_eq!(n3, n1);

    let n1 = Number::from(100u64);
    let n2 = Number::from(-100i64);
    let n3 = Number::from(100.1);

    assert_ne!(n1, n2);
    assert_ne!(n2, n3);
    assert_ne!(n3, n1);
}

#[test]
fn number_cmp_uint() {
    let n = Number::from(100u8);
    let n1 = Number::from(99u64);
    let n2 = Number::from(99i8);
    let n3 = Number::from(99.99);
    let n4 = Number::from(98.0);

    assert_eq!(n.cmp(&n1), Greater);
    assert_eq!(n.cmp(&n2), Greater);
    assert_eq!(n.cmp(&n3), Greater);
    assert_eq!(n.cmp(&n4), Greater);

    assert_eq!(n1.cmp(&n), Less);
    assert_eq!(n2.cmp(&n), Less);
    assert_eq!(n3.cmp(&n), Less);
    assert_eq!(n4.cmp(&n), Less);
}

#[test]
fn number_cmp_int() {
    let n = Number::from(100i8);
    let n1 = Number::from(99u64);
    let n2 = Number::from(99i8);
    let n3 = Number::from(99.99);
    let n4 = Number::from(98.0);

    assert_eq!(n.cmp(&n1), Greater);
    assert_eq!(n.cmp(&n2), Greater);
    assert_eq!(n.cmp(&n3), Greater);
    assert_eq!(n.cmp(&n4), Greater);

    assert_eq!(n1.cmp(&n), Less);
    assert_eq!(n2.cmp(&n), Less);
    assert_eq!(n3.cmp(&n), Less);
    assert_eq!(n4.cmp(&n), Less);
}

#[test]
fn number_cmp_float() {
    let n = Number::from(100.0);
    let n1 = Number::from(99u64);
    let n2 = Number::from(99i8);
    let n3 = Number::from(99.99);
    let n4 = Number::from(98.0);

    assert_eq!(n.cmp(&n1), Greater);
    assert_eq!(n.cmp(&n2), Greater);
    assert_eq!(n.cmp(&n3), Greater);
    assert_eq!(n.cmp(&n4), Greater);

    assert_eq!(n1.cmp(&n), Less);
    assert_eq!(n2.cmp(&n), Less);
    assert_eq!(n3.cmp(&n), Less);
    assert_eq!(n4.cmp(&n), Less);
}
