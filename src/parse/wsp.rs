use super::*;

/// A sequence of inline whitespace, which could be any `' '` or `'\t'`.
pub(super) fn inline_whitespace<'a, E: CxErr<'a>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    const CHARS: &str = " \t";
    take_while(|c| CHARS.contains(c))(i)
}

/// A sequence of multiline whitespace,
/// which could be any `' '`, `'\t'`, `'\r\n'`, or `'\n'`.
pub(super) fn multiline_whitespace<'a, E: CxErr<'a>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    const CHARS: &str = " \t\r\n";
    take_while(|c| CHARS.contains(c))(i)
}

/// Matches `inline_whitespace`, discarding if any, before matching the parser and returning the result.
pub(super) fn ignore_inline_whitespace<'a, O, E: CxErr<'a>, F>(
    mut f: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    move |input: &'a str| {
        let (input, _) = opt(inline_whitespace)(input)?;
        f(input)
    }
}
