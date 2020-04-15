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

fn uint<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, u128, E> {
    context("unsigned integer", map_parser(digit1, from_str))(i)
}

fn int<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, i128, E> {
    context(
        "signed integer",
        map_parser(
            take_while(|c: char| c == '-' || c.is_ascii_digit()),
            from_str,
        ),
    )(i)
}

fn num<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Number, E> {
    context(
        "number",
        alt((
            map(double, Number::from),
            map(int, Number::from),
            map(uint, Number::from),
        )),
    )(i)
}

/// Takes what could be string contents until a string terminator is reached.
/// The terminator is one of [`"`, `'`, `\t`, `\r`, `\n`].
fn take_str_until_terminated<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, &'a str, E> {
    let mut preceded_by_backslash = false;

    for (idx, ch) in i.char_indices() {
        if "\t\r\n".contains(ch) {
            return Ok((&i[idx..], &i[..idx]));
        }
        if !preceded_by_backslash && ch == '\"' {
            return Ok((&i[idx..], &i[..idx]));
        }

        preceded_by_backslash = ch == '\\';
    }

    let to = i.len();

    Ok((&i[to..], &i[..to]))
}

const ESC_QUOTE: &str = r#"\'"#;
const ESC_DBL_QUOTE: &str = r#"\""#;
const ESC_BACKSLASH: &str = r#"\"#;
const ESC_TAB: &str = r#"\t"#;
const ESC_CR: &str = r#"\r"#;
const ESC_LF: &str = r#"\n"#;

fn str_contains_escaped(i: &str) -> bool {
    i.contains(ESC_QUOTE)
        || i.contains(ESC_DBL_QUOTE)
        || i.contains(ESC_BACKSLASH)
        || i.contains(ESC_TAB)
        || i.contains(ESC_CR)
        || i.contains(ESC_LF)
}

fn parse_str<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Kstr<'a>, E> {
    let (i, string) = take_str_until_terminated(i)?;

    let kstr = if str_contains_escaped(string) {
        let string = string
            .replace(ESC_QUOTE, "'")
            .replace(ESC_DBL_QUOTE, "\"")
            .replace(ESC_BACKSLASH, "\\")
            .replace(ESC_TAB, "\t")
            .replace(ESC_CR, "\r")
            .replace(ESC_LF, "\n");

        Kstr::owned(string)
    } else {
        Kstr::brwed(string)
    };

    Ok((i, kstr))
}

fn string<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Kstr<'a>, E> {
    context(
        "string",
        preceded(char('\"'), cut(terminated(parse_str, char('\"')))),
    )(i)
}

#[inline(always)]
pub fn barr_tag<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    tag("b91'")(i)
}

fn barr<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Barr<'a>, E> {
    context(
        "base 91 byte array",
        preceded(
            barr_tag,
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
        macro_rules! test_parse_str {
		( $($str:literal),* ) => {
			$(
				let var: &str = $str;
				let s = format!("{}", var);
				let r = parse_str::<VerboseError<_>>(&s);
				assert_eq!(r, Ok(("", Kstr::brwed(var))));
			)*
		};
	}

        let r = parse_str::<VerboseError<_>>(" \"");
        assert_eq!(r, Ok(("\"", Kstr::brwed(" "))));
        let r = parse_str::<VerboseError<_>>(" \t");
        assert_eq!(r, Ok(("\t", Kstr::brwed(" "))));
        let r = parse_str::<VerboseError<_>>(" \r");
        assert_eq!(r, Ok(("\r", Kstr::brwed(" "))));
        let r = parse_str::<VerboseError<_>>(" \n");
        assert_eq!(r, Ok(("\n", Kstr::brwed(" "))));

        test_parse_str!("", "abcd");

        macro_rules! test_str {
		( $($str:literal),* ) => {
			$(
				let var: &str = $str;
				let s = format!("{:?}", var);
				println!("{}", s);
				let r = string::<VerboseError<_>>(&s);
				assert_eq!(r, Ok(("", Kstr::brwed(var))));
			)*
		};
	}

        test_str!(
            "",
            "abcdef",
            "Hello, world!",
            "Hello\nnewline",
            " ",
            "\r\n \n",
            "\t",
            "  \"  ",
            "  '  ",
            "'\t\n\r\n\""
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
}
