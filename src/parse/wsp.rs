use super::*;

/// A sequence of inline whitespace, which could be any `' '` or `'\t'`.
pub fn inline_wsp<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    const CHARS: &str = " \t";
    take_while(|c| CHARS.contains(c))(i)
}

/// A sequence of multiline whitespace,
/// which could be any `' '`, `'\t'`, `'\r\n'`, or `'\n'`.
pub fn multiline_wsp<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    const CHARS: &str = " \t\r\n";
    take_while(|c| CHARS.contains(c))(i)
}

/// Matches `inline_wsp`, discarding if any, before matching the parser and returning the result.
pub fn ignore_inline_wsp<'a, O, E: ParseError<&'a str>, F>(
    f: F,
) -> impl Fn(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    move |input: &'a str| {
        let (input, _) = opt(inline_wsp)(input)?;
        f(input)
    }
}
