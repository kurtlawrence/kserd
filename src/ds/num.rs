use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::convert::TryInto;
use std::fmt;
use Number::*;

/// A numerical value.
///
/// `Number` captures Rust numerical primitives: _unsigned integers_, _signed integers_, and
/// _floating point decimal_ numbers. The data is stored inside an enum housing the maximum size of
/// each numerical type (128 bits for integers, 64 bits for floats). The numbers are also
/// canonicalized, that is `Eq` and `Ord` are implemented and comparisons can be made between
/// integers and floats.
///
/// TODO: Operations (add, subtract, etc) are to be added in the future.
///
/// The number line extends from negative infinity, through zero, to positive infinity. Nan is
/// above positive infinity. All zeroes are treated equally (`-0 == +0`), as well as all Nans.
///
/// `[ -∞, .., 0, .., +∞, NaN ]`
///
/// # Examples
/// `Number` can be constructed straight from any of the Rust numbers using the `From` trait.
/// ```rust
/// # use kserd::*;
/// let n: Number = 123456u32.into();
/// assert_eq!(n, Number::Uint(123456));
/// ```
///
/// Comparisons can be made between different number types.
/// ```rust
/// # use kserd::*;
/// let n = Number::from(100u8);
/// assert_eq!(n, Number::from(100.0f32));
/// assert_eq!(n, Number::from(100i32));
/// assert_ne!(n, Number::from(99.99f64));
/// ```
///
/// `PartialEq` is also implemented for each Rust number.
/// ```rust
/// # use kserd::*;
/// let n = Number::from(100u8);
/// assert_eq!(n, 100.0f32);
/// assert_eq!(n, 100i32);
/// assert_ne!(n, 99.99);
/// ```
///
/// Ordering is likewise implemented.
/// ```rust
/// # use kserd::*;
/// let n = Number::from(100u8);
/// assert!(n < Number::from(101));
/// assert!(n > Number::from(-100));
/// assert!(n > Number::from(99.99));
/// // Likewise with literal numbers
/// assert!(n < 101);
/// assert!(n > -100);
/// assert!(n > 99.99);
/// ```
///
/// An exmaple of the canonicalized ordering.
/// ```rust
/// # use kserd::*;
/// use std::f64::{INFINITY, NEG_INFINITY, NAN};
///
/// // use an ordered set
/// let mut set = std::collections::BTreeSet::new();
///
/// set.insert(Number::from(0));
/// set.insert((-0.0).into());
/// set.insert((-1.0).into());
/// set.insert(0.5.into());
/// set.insert(INFINITY.into());
/// set.insert((-100).into());
/// set.insert(NAN.into());
/// set.insert(NAN.into());
/// set.insert((-0.0).into());
/// set.insert(NEG_INFINITY.into());
/// set.insert(100.0.into());
///
/// let expected: Vec<Number> = vec![
///      NEG_INFINITY,
///      -100.0,
///      -1.0,
///      0.0,
///      0.5,
///      100.0,
///      INFINITY,
///      NAN,
/// ].into_iter().map(Number::from).collect();
///
/// assert_eq!(set.into_iter().collect::<Vec<_>>(), expected);
/// ```
#[derive(Copy, Clone, Debug)]
#[allow(missing_docs)]
pub enum Number {
    Uint(u128),
    Int(i128),
    Float(f64),
}

/// Converting into a signed or unsigned integer can fail if the original number is outside the
/// integer's valid range.
#[derive(Debug, PartialEq)]
pub struct IntoIntError;

impl Number {
    /// Represent `Number` as an unsigned integer.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::ds::IntoIntError;
    /// use std::f64::{INFINITY, NEG_INFINITY, NAN};
    ///
    /// // generally the conversion will work
    /// assert_eq!(Number::from(100i32).as_u128(), Ok(100));
    /// assert_eq!(Number::from(100.0).as_u128(), Ok(100));
    /// // can also work if fractional part of float is smaller than valid amount
    /// assert_eq!(Number::from(3.0 + 5e-11).as_u128(), Ok(3));
    /// // can fail if outside
    /// assert_eq!(Number::from(-100i32).as_u128(), Err(IntoIntError));
    /// assert_eq!(Number::from(0.5).as_u128(), Err(IntoIntError));
    /// assert_eq!(Number::from(INFINITY).as_u128(), Err(IntoIntError));
    /// assert_eq!(Number::from(NEG_INFINITY).as_u128(), Err(IntoIntError));
    /// assert_eq!(Number::from(NAN).as_u128(), Err(IntoIntError));
    /// ```
    pub fn as_u128(&self) -> Result<u128, IntoIntError> {
        match self {
            Uint(x) => Ok(*x),
            Int(x) => (*x).try_into().map_err(|_| IntoIntError),
            Float(x) => {
                if x.is_finite() && x.fract() < 1e-10 {
                    Ok(*x as u128)
                } else {
                    Err(IntoIntError)
                }
            }
        }
    }

    /// Represent `Number` as a signed integer.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::ds::IntoIntError;
    /// use std::f64::{INFINITY, NEG_INFINITY, NAN};
    ///
    /// // generally the conversion will work
    /// assert_eq!(Number::from(100u32).as_i128(), Ok(100));
    /// assert_eq!(Number::from(100.0).as_i128(), Ok(100));
    /// // can also work if fractional part of float is smaller than valid amount
    /// assert_eq!(Number::from(3.0 + 5e-11).as_i128(), Ok(3));
    /// // can fail if outside
    /// assert_eq!(Number::from(0.5).as_i128(), Err(IntoIntError));
    /// assert_eq!(Number::from(INFINITY).as_i128(), Err(IntoIntError));
    /// assert_eq!(Number::from(NEG_INFINITY).as_i128(), Err(IntoIntError));
    /// assert_eq!(Number::from(NAN).as_i128(), Err(IntoIntError));
    /// ```
    pub fn as_i128(&self) -> Result<i128, IntoIntError> {
        match self {
            Uint(x) => (*x).try_into().map_err(|_| IntoIntError),
            Int(x) => Ok(*x),
            Float(x) => {
                if x.is_finite() && x.fract() < 1e-10 {
                    Ok(*x as i128)
                } else {
                    Err(IntoIntError)
                }
            }
        }
    }

    /// Represent `Number` as a floating point decimal.
    /// Does not fail, but is a lossy conversion if an integer.
    ///
    /// # Examples
    /// ```rust
    /// # use kserd::*;
    /// assert_eq!(Number::from(100u8).as_f64(), 100.0);
    /// assert_eq!(Number::from(-100).as_f64(), -100.0);
    /// // lossy conversion if integers get large.
    /// assert_eq!(Number::from(u128::max_value()).as_f64(), 3402823669209385e23);
    /// assert_eq!(Number::from(i128::max_value()).as_f64(), 17014118346046923e22);
    /// assert_eq!(Number::from(i128::min_value()).as_f64(), -17014118346046923e22);
    /// ```
    pub fn as_f64(&self) -> f64 {
        match *self {
            Uint(x) => x as f64,
            Int(x) => x as f64,
            Float(x) => x,
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Uint(x) => write!(f, "{}", x),
            Int(x) => write!(f, "{}", x),
            Float(x) => write!(f, "{}", x),
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Number) -> bool {
        match (self, other) {
            // Three cases where lhs and rhs are same type
            (Uint(lhs), Uint(rhs)) => lhs.eq(rhs),
            (Int(lhs), Int(rhs)) => lhs.eq(rhs),
            (Float(lhs), Float(rhs)) => cmp_float_to_float(*lhs, *rhs) == Ordering::Equal,

            // Integers
            (Uint(lhs), Int(_)) => other.as_u128().map(|rhs| lhs.eq(&rhs)).unwrap_or(false),
            (Uint(lhs), Float(_)) => other.as_u128().map(|rhs| lhs.eq(&rhs)).unwrap_or(false),
            (Int(lhs), Uint(_)) => other.as_i128().map(|rhs| lhs.eq(&rhs)).unwrap_or(false),
            (Int(lhs), Float(_)) => other.as_i128().map(|rhs| lhs.eq(&rhs)).unwrap_or(false),

            // Floats
            (Float(lhs), Uint(_)) => lhs.eq(&other.as_f64()),
            (Float(lhs), Int(_)) => lhs.eq(&other.as_f64()),
        }
    }
}

macro_rules! partial_eq_impl {
    ( $( $t:ty ),* ) => {
	$(
	impl PartialEq<$t> for Number {
	    fn eq(&self, rhs: &$t) -> bool {
		self.eq(&Number::from(*rhs))
	    }
	}
	)*
    };
}

partial_eq_impl!(usize, u8, u16, u32, u64, u128, isize, i8, i16, i32, i64, i128, f32, f64);

impl Eq for Number {}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Number) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

macro_rules! partial_ord_impl {
    ( $( $t:ty ),* ) => {
	$(
	impl PartialOrd<$t> for Number {
	    fn partial_cmp(&self, rhs: &$t) -> Option<Ordering> {
		Some(self.cmp(&Number::from(*rhs)))
	    }
	}
	)*
    };
}

partial_ord_impl!(usize, u8, u16, u32, u64, u128, isize, i8, i16, i32, i64, i128, f32, f64);

impl Ord for Number {
    fn cmp(&self, other: &Number) -> Ordering {
        match (*self, *other) {
            // Three cases where lhs and rhs are same type
            (Uint(lhs), Uint(rhs)) => lhs.cmp(&rhs),
            (Int(lhs), Int(rhs)) => lhs.cmp(&rhs),
            (Float(lhs), Float(rhs)) => cmp_float_to_float(lhs, rhs),

            // Integers
            (Uint(lhs), Int(rhs)) => cmp_uint_to_int(&lhs, rhs),
            (Int(lhs), Uint(rhs)) => rev_ordering(cmp_uint_to_int(&rhs, lhs)),

            // Floats
            (Float(lhs), Uint(rhs)) => cmp_float_to_uint(lhs, rhs),
            (Uint(lhs), Float(rhs)) => rev_ordering(cmp_float_to_uint(rhs, lhs)),
            (Float(lhs), Int(rhs)) => cmp_float_to_int(lhs, rhs),
            (Int(lhs), Float(rhs)) => rev_ordering(cmp_float_to_int(rhs, lhs)),
        }
    }
}

/// `[ -INF | ... | C0 | ... | +INF | CNaN ]`
fn cmp_float_to_float(lhs: f64, rhs: f64) -> Ordering {
    match lhs.partial_cmp(&rhs) {
        Some(ordering) => ordering,
        None => {
            if lhs.is_nan() {
                if rhs.is_nan() {
                    Ordering::Equal
                } else {
                    Ordering::Greater
                }
            } else {
                Ordering::Less
            }
        }
    }
}

fn cmp_uint_to_int(lhs: &u128, rhs: i128) -> Ordering {
    let rhs: Result<u128, _> = rhs.try_into();
    match rhs {
        Ok(rhs) => lhs.cmp(&rhs),
        Err(_) => Ordering::Greater,
    }
}

fn cmp_float_to_uint(lhs: f64, rhs: u128) -> Ordering {
    use Ordering::*;

    if lhs.is_sign_negative() {
        Less
    } else if lhs.is_infinite() || lhs.is_nan() {
        Greater
    } else {
        let (floor, ceil) = float_bounds_uint(lhs);

        match (floor.cmp(&rhs), ceil.cmp(&rhs)) {
            (Less, Less) => Less,
            (Less, Equal) => Less,
            (Equal, Equal) => Equal,
            (Equal, Greater) => Greater,
            (Greater, Greater) => Greater,
            _ => unreachable!("logical error since bounded to floor and ceil"),
        }
    }
}

fn cmp_float_to_int(lhs: f64, rhs: i128) -> Ordering {
    use Ordering::*;

    if lhs.is_sign_negative() && lhs.is_infinite() {
        Less
    } else if lhs.is_infinite() || lhs.is_nan() {
        Greater
    } else {
        let (floor, ceil) = float_bounds_int(lhs);

        match (floor.cmp(&rhs), ceil.cmp(&rhs)) {
            (Less, Less) => Less,
            (Less, Equal) => Less,
            (Equal, Equal) => Equal,
            (Equal, Greater) => Greater,
            (Greater, Greater) => Greater,
            _ => unreachable!("logical error since bounded to floor and ceil"),
        }
    }
}

fn rev_ordering(ord: Ordering) -> Ordering {
    match ord {
        Ordering::Greater => Ordering::Less,
        Ordering::Equal => Ordering::Equal,
        Ordering::Less => Ordering::Greater,
    }
}

/// make sure to check infinities and nan beforehand
/// make sure to check for positivity
fn float_bounds_uint(f: f64) -> (u128, u128) {
    (f.floor() as u128, f.ceil() as u128)
}

/// make sure to check infinities and nan beforehand
fn float_bounds_int(f: f64) -> (i128, i128) {
    (f.floor() as i128, f.ceil() as i128)
}

/// A type that can be converted into a `Number`.
/// The type also has a string type identity.
/// Used primarily in [`Kserd::new_num`] to accept any Rust number type.
///
/// [`Kserd::new_num`]: crate::Kserd::new_num
pub trait NumberType: Into<Number> {
    /// The type name.
    fn identity(&self) -> &'static str;
}

macro_rules! number_type_impl {
	( $($t:ty),* ) => {
		$(
			impl NumberType for $t {
				fn identity(&self) -> &'static str {
					stringify!($t)
				}
			}
		)*
	};
}

number_type_impl!(usize, u8, u16, u32, u64, u128);
number_type_impl!(isize, i8, i16, i32, i64, i128);
number_type_impl!(f32, f64);

macro_rules! fr_uint {
	( $( $t:ty ),* ) => {
		$(
			impl From<$t> for Number {
				fn from(x: $t) -> Self {
					Number::Uint(x as u128)
				}
			}
		)*
	};
}

macro_rules! fr_int {
	( $( $t:ty ),* ) => {
		$(
			impl From<$t> for Number {
				fn from(x: $t) -> Self {
					Number::Int(x as i128)
				}
			}
		)*
	};
}

fr_uint!(usize, u8, u16, u32, u64, u128);
fr_int!(isize, i8, i16, i32, i64, i128);

impl From<f32> for Number {
    fn from(x: f32) -> Self {
        Number::Float(f32_to_f64(x))
    }
}

impl From<f64> for Number {
    fn from(x: f64) -> Self {
        Number::Float(x)
    }
}

fn f32_to_f64(f: f32) -> f64 {
    f.to_string().parse::<f64>().expect("shouldn't fail")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_float_cmp() {
        // Confirm `[ -INF | ... | C0 | ... | +INF | CNaN ]`
        use std::f64::{INFINITY, NAN, NEG_INFINITY};
        use Ordering::*;

        assert_eq!(cmp_float_to_float(0.0, 0.0), Equal);
        assert_eq!(cmp_float_to_float(0.0, -0.0), Equal);
        assert_eq!(cmp_float_to_float(0.1, 0.0), Greater);
        assert_eq!(cmp_float_to_float(0.0, 0.1), Less);
        assert_eq!(cmp_float_to_float(3.14, -3.14), Greater);
        assert_eq!(cmp_float_to_float(-3.14, 3.14), Less);

        assert_eq!(cmp_float_to_float(NAN, 0.0), Greater);
        assert_eq!(cmp_float_to_float(NAN, 1.0), Greater);
        assert_eq!(cmp_float_to_float(NAN, -1.0), Greater);
        assert_eq!(cmp_float_to_float(NAN, INFINITY), Greater);
        assert_eq!(cmp_float_to_float(NAN, NEG_INFINITY), Greater);

        assert_eq!(cmp_float_to_float(0.0, NAN), Less);
        assert_eq!(cmp_float_to_float(1.0, NAN), Less);
        assert_eq!(cmp_float_to_float(-1.0, NAN), Less);
        assert_eq!(cmp_float_to_float(INFINITY, NAN), Less);
        assert_eq!(cmp_float_to_float(NEG_INFINITY, NAN), Less);

        assert_eq!(cmp_float_to_float(INFINITY, 0.0), Greater);
        assert_eq!(cmp_float_to_float(INFINITY, NEG_INFINITY), Greater);
        assert_eq!(cmp_float_to_float(INFINITY, INFINITY), Equal);

        assert_eq!(cmp_float_to_float(0.0, INFINITY), Less);
        assert_eq!(cmp_float_to_float(NEG_INFINITY, INFINITY), Less);
        assert_eq!(cmp_float_to_float(INFINITY, INFINITY), Equal);

        assert_eq!(cmp_float_to_float(NEG_INFINITY, NEG_INFINITY), Equal);
    }

    #[test]
    fn test_float_eq() {
        use std::f64::{INFINITY, NAN, NEG_INFINITY};
        fn f(v: f64) -> Number {
            Number::from(v)
        };

        assert!(f(INFINITY) == f(INFINITY));
        assert!(f(INFINITY) != f(NEG_INFINITY));
        assert!(f(INFINITY) != f(NAN));

        assert!(f(NEG_INFINITY) != f(INFINITY));
        assert!(f(NEG_INFINITY) == f(NEG_INFINITY));
        assert!(f(NEG_INFINITY) != f(NAN));

        assert!(f(NAN) != f(INFINITY));
        assert!(f(NAN) != f(NEG_INFINITY));
        assert!(f(NAN) == f(NAN));
    }

    #[test]
    fn canonicalized_ordering() {
        use std::f64::{INFINITY, NAN, NEG_INFINITY};

        // use an ordered set
        let mut set = std::collections::BTreeSet::new();

        set.insert(Number::from(0));
        set.insert((-0.0).into());
        set.insert((-1.0).into());
        set.insert(0.5.into());
        set.insert(INFINITY.into());
        set.insert((-100).into());
        set.insert(NAN.into());
        set.insert(NAN.into());
        set.insert((-0.0).into());
        set.insert(NEG_INFINITY.into());
        set.insert(100.0.into());

        let expected: Vec<Number> =
            vec![NEG_INFINITY, -100.0, -1.0, 0.0, 0.5, 100.0, INFINITY, NAN]
                .into_iter()
                .map(Number::from)
                .collect();

        assert_eq!(set.into_iter().collect::<Vec<_>>(), expected);
    }

    #[test]
    fn as_u128_test() {
        use std::f64::{INFINITY, NAN, NEG_INFINITY};

        // generally the conversion will work
        assert_eq!(Number::from(100i32).as_u128(), Ok(100));
        assert_eq!(Number::from(100.0).as_u128(), Ok(100));
        // can also work if fractional part of float is smaller than valid amount
        assert_eq!(Number::from(3.0 + 5e-11).as_u128(), Ok(3));
        // can fail if outside
        assert_eq!(Number::from(-100i32).as_u128(), Err(IntoIntError));
        assert_eq!(Number::from(0.5).as_u128(), Err(IntoIntError));
        assert_eq!(Number::from(INFINITY).as_u128(), Err(IntoIntError));
        assert_eq!(Number::from(NEG_INFINITY).as_u128(), Err(IntoIntError));
        assert_eq!(Number::from(NAN).as_u128(), Err(IntoIntError));
    }

    #[test]
    fn partial_eq_and_comp_tests() {
        macro_rules! tester {
            ($( $x:ty ) +) => {{
                $(
                    let t: $x = 0;
                    assert_eq!(Number::from(0u8), t);
                    assert!(Number::from(-0.001) < t);
                )*
            }}
        };
        tester!(
            isize i8 i16 i32 i64 i128
            usize u8 u16 u32 u64 u128
        );
        assert_eq!(Number::from(0u8), 0.0f32);
        assert_eq!(Number::from(0u8), 0.0f64);
        assert!(Number::from(-0.001) < 0.0f32);
        assert!(Number::from(-0.001) < 0.0f64);
    }

    #[test]
    fn large_int_ordering() {
        assert!(Number::from(128u128) > Number::from(-128i128));
    }

    #[test]
    fn float_to_int_ordering() {
        assert!(Number::from(128u128) > Number::from(-3.14));
        assert!(Number::from(std::f64::INFINITY) > Number::from(std::u128::MAX));
        assert!(Number::from(std::f64::NAN) > Number::from(std::u128::MAX));
        assert!(Number::from(128u128) == Number::from(128.0));
        assert!(Number::from(128u128) < Number::from(128.1));

        assert!(Number::from(128i128) > Number::from(-3.14));
        assert!(Number::from(std::f64::INFINITY) > Number::from(std::i128::MAX));
        assert!(Number::from(std::f64::NAN) > Number::from(std::i128::MAX));
        assert!(Number::from(128i128) == Number::from(128.0));
        assert!(Number::from(128i128) < Number::from(128.1));
    }

    #[test]
    fn eq_int_uint() {
        assert_ne!(Number::from(-123), Number::from(123usize));
        assert_eq!(Number::from(123isize), Number::from(123usize));
    }

    #[test]
    fn float_to_int_cmp() {
        use std::f64::{INFINITY, NAN, NEG_INFINITY};
        use Ordering::*;

        assert_eq!(Number::from(123u8).cmp(&Number::from(INFINITY)), Less);
        assert_eq!(Number::from(123u8).cmp(&Number::from(NAN)), Less);
        assert_eq!(Number::from(123u8).cmp(&Number::from(123f32)), Equal);
        assert_eq!(
            Number::from(123u8).cmp(&Number::from(NEG_INFINITY)),
            Greater
        );
    }

    #[test]
    fn rev_ordering_test() {
        use Ordering::*;
        assert_eq!(rev_ordering(Less), Greater);
        assert_eq!(rev_ordering(Greater), Less);
        assert_eq!(rev_ordering(Equal), Equal);
    }
}
