#![cfg(feature = "encode")]
use kserd::*;
use std::f32::consts::PI;

#[test]
fn eq_unit() {
    let kserd = Kserd::enc(&()).unwrap();
    assert_eq!(kserd.unit(), true);
    assert_eq!(kserd.id(), None);
    let kserd = Kserd::enc(&1).unwrap();
    assert_eq!(kserd.unit(), false);
}

#[test]
fn eq_bool() {
    let kserd = Kserd::enc(&true).unwrap();
    assert_eq!(kserd.bool(), Some(true));
    let kserd = Kserd::enc(&false).unwrap();
    assert_eq!(kserd.bool(), Some(false));
    let kserd = Kserd::enc(&1).unwrap();
    assert_eq!(kserd.bool(), None);
}

#[test]
fn eq_ch() {
    let kserd = Kserd::enc(&'a').unwrap();
    assert_eq!(kserd.ch(), Some('a'));
    let kserd = Kserd::enc(&1).unwrap();
    assert_eq!(kserd.ch(), None);
}

#[test]
fn eq_unsigned_int() {
    macro_rules! t {
        ($t:ident) => {
            let kserd = Kserd::new_num(std::$t::MAX);
            assert_eq!(kserd.uint(), Some(std::$t::MAX as u128));
            let kserd = Kserd::enc(&true).unwrap();
            assert_eq!(kserd.uint(), None);
        };
    }

    t!(usize);
    t!(u8);
    t!(u16);
    t!(u32);
    t!(u64);
    t!(u128);
}

#[test]
fn eq_signed_int() {
    macro_rules! t {
        ($t:ident) => {
            let kserd = Kserd::new_num(std::$t::MAX);
            assert_eq!(kserd.int(), Some(std::$t::MAX as i128));
            let kserd = Kserd::new_num(std::$t::MIN);
            assert_eq!(kserd.int(), Some(std::$t::MIN as i128));
            let kserd = Kserd::enc(&true).unwrap();
            assert_eq!(kserd.int(), None);
        };
    }

    t!(isize);
    t!(i8);
    t!(i16);
    t!(i32);
    t!(i64);
    t!(i128);
}

#[test]
fn eq_float() {
    macro_rules! inner {
        ($v:expr) => {
            let as_f64 = $v.to_string().parse::<f64>().expect("shouldn't fail");
            let kserd = Kserd::new_num($v);
            assert_eq!(kserd.float(), Some(as_f64));
            let kserd = Kserd::enc(&true).unwrap();
            assert_eq!(kserd.float(), None);
        };
    }

    macro_rules! t {
        ($t:ident) => {
            inner!(std::$t::EPSILON);
            inner!(std::$t::MAX);
            inner!(std::$t::MIN);
            inner!(std::$t::MIN_POSITIVE);
            inner!(std::$t::consts::FRAC_1_PI);
            inner!(std::$t::consts::FRAC_2_PI);
            inner!(std::$t::consts::FRAC_2_SQRT_PI);
            inner!(std::$t::consts::FRAC_1_SQRT_2);
            inner!(std::$t::consts::FRAC_PI_2);
            inner!(std::$t::consts::FRAC_PI_3);
            inner!(std::$t::consts::FRAC_PI_4);
            inner!(std::$t::consts::FRAC_PI_6);
            inner!(std::$t::consts::FRAC_PI_8);
            inner!(std::$t::consts::LN_2);
            inner!(std::$t::consts::LN_10);
            inner!(std::$t::consts::LOG2_E);
            inner!(std::$t::consts::LOG10_E);
            inner!(std::$t::consts::PI);
            inner!(std::$t::consts::SQRT_2);
        };
    }

    t!(f32);
    t!(f64);

    // edge case where 3.14f32 as f64 != 3.14f64
    let kserd = Kserd::enc(&1.2345678f32).unwrap();
    assert_eq!(kserd.float(), Some(1.2345678));

    // inifities and nans work
    let kserd = Kserd::new_num(std::f32::INFINITY);
    assert_eq!(kserd.float(), Some(std::f64::INFINITY));
    let kserd = Kserd::new_num(std::f64::INFINITY);
    assert_eq!(kserd.float(), Some(std::f64::INFINITY));

    let kserd = Kserd::new_num(std::f32::NEG_INFINITY);
    assert_eq!(kserd.float(), Some(std::f64::NEG_INFINITY));
    let kserd = Kserd::new_num(std::f64::NEG_INFINITY);
    assert_eq!(kserd.float(), Some(std::f64::NEG_INFINITY));

    let kserd = Kserd::new_num(std::f32::NAN);
    assert!(kserd.float().unwrap().is_nan());
    let kserd = Kserd::new_num(std::f64::NAN);
    assert!(kserd.float().unwrap().is_nan());
}

#[test]
fn eq_str() {
    let s = "Hello, world!".to_string();
    let kserd = Kserd::enc(&s).unwrap();
    assert_eq!(kserd.str(), Some("Hello, world!"));
    let kserd = Kserd::enc(&true).unwrap();
    assert_eq!(kserd.str(), None);
}

#[test]
fn eq_barr() {
    let arr = [1, 2, 3, 4, 5];
    let v = vec![1, 2, 3, 4, 5];
    let kserd = Kserd::new_barr(&v);
    assert_eq!(kserd.barr(), Some(&arr[..]));
    let kserd = Kserd::enc(&true).unwrap();
    assert_eq!(kserd.barr(), None);
}
