use super::*;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Nonprim {
    Tuple,
    Cntr,
    Seq,
    Map,
    None,
}

/// This only matches `Inline` and `Concise` variants. Looks ahead to guess type
/// based on opening delimiters and field naming. _Always_ returns the original
/// input.
pub fn pattern_match_delimited<'a, E: ParseError<&'a str>>(
    orig: &'a str,
) -> IResult<&'a str, Nonprim, E> {
    #[derive(PartialEq)]
    enum Ident {
        Prim,
        Nonprim,
        None,
    };

    let (i, ident_) = opt(ident(true))(orig)?;
    let (i, ident_) = if ident_.is_some() {
        (i, Ident::Prim)
    } else {
        let (i, ident__) = opt(ident(false))(orig)?;
        if ident__.is_some() {
            (i, Ident::Nonprim)
        } else {
            (orig, Ident::None)
        }
    };

    let (i, _) = multiline_whitespace(i)?;
    let next = i.chars().next().unwrap_or(' ');

    let mut r = if next == '[' {
        Nonprim::Seq
    } else if next == '{' {
        Nonprim::Map
    } else if next == '(' {
        // opening of tuple or container
        if (ident_ == Ident::None || ident_ == Ident::Prim) && unit_value::<E>(i).is_ok() {
            Nonprim::None // a unit value, not a tuple. Can't be named, or would be an empty tuple
        } else if recognise_field_assign(multiline_whitespace(&i[1..])?.0) {
            Nonprim::Cntr
        } else {
            Nonprim::Tuple
        }
    } else {
        Nonprim::None
    };

    if r != Nonprim::None && ident_ == Ident::Prim {
        r = Nonprim::None;
    }

    Ok((orig, r))
}

/// This only matches `Verbose` variants, returning true if it looks like a `Verbose` formatting.
/// _Always_ returns the original input.
pub fn pattern_match_verbose<'a, E: ParseError<&'a str>>(
    orig: &'a str,
) -> IResult<&'a str, bool, E> {
    let (i, _) = multiline_whitespace(orig)?; // ignore _all_ whitespace before hand

    // generally a Verbose format can be recognised by:
    // - A direct mapping (var_name = xxx)
    // - A nested mapping ([var_name])
    // - A sequence or map entry ([[var_name]])

    let is = recognise_field_assign(i);
    let is = is
        || preceded::<_, _, _, (), _, _>(
            char('['),
            terminated(
                ignore_inline_whitespace(valid_name),
                ignore_inline_whitespace(char(']')),
            ),
        )(i)
        .is_ok();
    let is = is
        || preceded::<_, _, _, (), _, _>(
            tag("[["),
            terminated(
                ignore_inline_whitespace(valid_name),
                ignore_inline_whitespace(tag("]]")),
            ),
        )(i)
        .is_ok();

    Ok((orig, is))
}

/// Looks at the next characters, ignoring spaces. If there is a a new line before
/// other characters then we say that this is in `Concise` format.
pub fn recognise_concise(i: &str) -> bool {
    const SKIP: &str = " \t\r";

    i.chars()
        .skip_while(|&c| SKIP.contains(c))
        .next()
        .map(|c| c == '\n')
        .unwrap_or(false)
}

/// Recognises the sequence: `field_name =`.
pub fn recognise_field_assign(i: &str) -> bool {
    preceded::<_, _, _, (), _, _>(field_name, terminated(inline_whitespace, char('=')))(i).is_ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::VerboseError;

    macro_rules! test {
		( $ex:expr, $($x:literal ),* ) => {{
			$(
				let r = pattern_match_delimited::<VerboseError<_>>($x);
				assert_eq!(r, Ok(($x, $ex)));
			)*
		}};
	}

    #[test]
    fn test_pat_match_none() {
        test!(
            Nonprim::None,
            "123456",
            "-123456",
            "-3.14e-314",
            "()",
            "a = 123",
            r#""Hello, world!""#,
            "true",
            "false",
            "b91':C!WEH#L4R'"
        );
    }

    #[test]
    fn test_pat_match_seq() {
        test!(
            Nonprim::Seq,
            "[1,2,3]",
            "[\n1,2,3\n]",
            "Hello [1,2,3]",
            "Hello [\n1,2,3\n]",
            "Hello[1,2,3]",
            "Hello[\n1,2,3\n]"
        );
    }

    #[test]
    fn test_pat_match_map() {
        test!(
            Nonprim::Map,
            "{1:2, 3:4, 5:6}",
            "{\n1:2\n3:4\n5:6\n}",
            "Hello {1:2, 3:4, 5:6}",
            "Hello {\n1:2\n3:4\n5:6\n}",
            "Hello{1:2, 3:4, 5:6}",
            "Hello{\n1:2\n3:4\n5:6\n}"
        );
    }

    #[test]
    fn test_pat_match_tuple() {
        test!(
            Nonprim::Tuple,
            "(1,2,3)",
            "(\n1,2,3\n)",
            "Hello (1,2,3)",
            "Hello (\n1,2,3\n)",
            "Hello(1,2,3)",
            "Hello(\n1,2,3\n)"
        );
    }

    #[test]
    fn test_pat_match_cntr() {
        test!(
            Nonprim::Cntr,
            "(a=1, 2, 3)",
            "(\na  = 1,2,3\n)",
            "Hello (a = 1,2,3)",
            "Hello (\na=1,2,3\n)",
            "Hello(_a=1,2,3)",
            "Hello(\na \t\t\t =1,2,3\n)"
        );
    }
}
