use super::*;

fn from_str<'a, T: FromStr, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, T, E> {
    i.parse::<T>()
        .map(|x| ("", x))
        .map_err(|_| Err::Error(error::make_error(i, ErrorKind::ParseTo)))
}

/// A unit value (`()`). Note can have inline whitespace.
pub fn unit_value<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    context(
        "unit value",
        map(
            preceded(char('('), cut(terminated(inline_whitespace, char(')')))),
            |_| (),
        ),
    )(i)
}

fn boolean<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, bool, E> {
    context(
        "boolean",
        alt((map(tag("true"), |_| true), map(tag("false"), |_| false))),
    )(i)
}

fn num<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Number, E> {
    const ALLOW: &str = "-.einfNa";
    context(
        "number",
        map_parser(
            take_while(|c: char| c.is_ascii_digit() || ALLOW.contains(c)),
            from_str,
        ),
    )(i)
}

fn parse_str<'a, E: ParseError<&'a str>>(
    delimiter: char,
) -> impl Fn(&'a str) -> IResult<&'a str, Kstr<'a>, E> {
    preceded(
        char(delimiter),
        cut(terminated(
            map(take_till(move |c| c == delimiter), Kstr::brwed),
            char(delimiter),
        )),
    )
}

fn string<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Kstr<'a>, E> {
    let (d, i) = if let Some(suff) = i.strip_prefix("str") {
        (suff.chars().next().unwrap_or('"'), suff)
    } else {
        ('"', i)
    };
    context("string", parse_str(d))(i)
}

fn barr<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Barr<'a>, E> {
    context(
        "base 91 byte array",
        preceded(
            tag("b91'"),
            cut(terminated(
                map(take_till(|c| c == '\''), |bytes: &str| {
                    Barr::owned(base91::slice_decode(bytes.as_bytes()))
                }),
                char('\''),
            )),
        ),
    )(i)
}

fn prim_value<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Value<'a>, E> {
    context(
        "primitive value",
        alt((
            map(unit_value, |_| Value::Unit),
            map(boolean, Value::Bool),
            map(num, Value::Num),
            map(string, Value::Str),
            map(barr, Value::Barr),
        )),
    )(i)
}

/// A primitive is always formatted the same way; inline with optional `<ident>` out
/// the front. The value can also have _inline_ whitespace in-between the ident.
pub fn prim<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Kserd<'a>, E> {
    let (i, (ident, value)) = context(
        "primitive",
        separated_pair(opt(ident(true)), inline_whitespace, prim_value),
    )(i)?;

    let kserd = if let Some(ident) = ident {
        Kserd::with_id(ident, value).unwrap()
    } else {
        Kserd::new(value)
    };

    Ok((i, kserd))
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::VerboseError;

    #[test]
    fn test_string() {
        macro_rules! test_str {
	    ( $($parse:literal $res:literal $rem:literal),* ) => {
		$(
		    let r = string::<VerboseError<_>>($parse);
		    assert_eq!(r, Ok(($rem, Kstr::brwed($res))));
		)*
	    };
	}

        test_str!(
            r#""""# "" "",
            r#""abcdef""# "abcdef" "",
            r#""Hello, world!""# "Hello, world!" "",
            "\"Hello\nnewline\"" "Hello\nnewline" "",
            "\" \"" " " "",
            "\"\r\n \n\"" "\r\n \n" "",
            "\"\t\"" "\t" "",
            "str'  \"  '" "  \"  " "",
            "str\"  '  \"" "  '  " "",
            "\"'\t\n\r\n\"" "'\t\n\r\n" ""
        );
    }

    #[test]
    fn test_barr() {
        macro_rules! test_parse_barr {
		( $($bytes:expr),* ) => {
			$(
				let var: &[u8] = $bytes;
				let kserd_ = Kserd::new(Value::Barr(Barr::brwed(var)));
				let s = kserd_.as_str();
				println!("{}", s);
				let r = barr::<VerboseError<_>>(&s);
				assert_eq!(r, Ok(("", Barr::brwed(var))));
			)*
		};
	}

        test_parse_barr!(
            &[],
            &[0],
            &[0, 1, 2, 5, 10, 20, 50, 100, 110, 120, 150, 200, 210, 220, 250]
        );

        let all = (0..255_u8).collect::<Vec<u8>>();

        test_parse_barr!(&all);

        let r = barr::<VerboseError<_>>("b91'',");
        assert_eq!(r, Ok((",", Barr::brwed(&[]))));
        let r = barr::<VerboseError<_>>("b91']',");
        assert_eq!(r, Ok((",", Barr::brwed(&[82]))));
        let r = barr::<VerboseError<_>>("b91'],',");
        assert_eq!(r, Ok((",", Barr::brwed(&[143]))));
    }

    #[test]
    fn unsigned_numbers() {
        let u = |x: u128| {
            assert_eq!(
                Some(x),
                num::<()>(&x.to_string())
                    .ok()
                    .and_then(|x| x.1.as_u128().ok())
            )
        };
        u(0);
        u(1);
        u(std::u8::MAX as u128);
        u(std::u16::MAX as u128);
        u(std::u32::MAX as u128);
        u(std::u64::MAX as u128);
        u(std::u128::MAX as u128);
        u(16803534192531604596);
        u(16803534192531605504);
    }

    #[test]
    fn signed_numbers() {
        let u = |x: i128| {
            assert_eq!(
                Some(x),
                num::<()>(&x.to_string())
                    .ok()
                    .and_then(|x| x.1.as_i128().ok())
            )
        };
        u(0);
        u(1);
        u(-1);
        u(std::i8::MAX as i128);
        u(std::i8::MIN as i128);
        u(std::i16::MAX as i128);
        u(std::i16::MIN as i128);
        u(std::i32::MAX as i128);
        u(std::i32::MIN as i128);
        u(std::i64::MAX as i128);
        u(std::i64::MIN as i128);
        u(std::i128::MAX as i128);
        u(std::i128::MIN as i128);
    }

    #[test]
    fn float_numbers() {
        let u = |x: f64| {
            assert_eq!(
                Some(x),
                num::<()>(&x.to_string()).ok().map(|x| x.1.as_f64())
            )
        };
        use std::f64::consts::*;
        u(0.0);
        u(-0.0);
        u(E);
        u(FRAC_1_PI);
        u(FRAC_1_SQRT_2);
        u(LN_2);
        u(LN_10);
        u(PI);
        u(TAU);
        u(std::f64::INFINITY);
        u(std::f64::NEG_INFINITY);
        // u(std::f64::NAN); can't check NaN != Nan!
    }
}
