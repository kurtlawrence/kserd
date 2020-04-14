use super::*;

fn inline_list_kserds<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Vec<Kserd<'a>>, E> {
    context(
        "comma separated kserds",
        separated_list(
            ignore_inline_whitespace(char(',')),
            ignore_inline_whitespace(kserd_inline),
        ),
    )(i)
}

/// Concise lists are separated by new lines. This can become problematic since
/// there may be multiple new lines and whitespace before you get to the next item.
/// The way to control for this is to allow whitespace all around the items.
///
/// TODO: Show one of the tuple examples.
fn concise_list_kserds<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Vec<Kserd<'a>>, E> {
    preceded(
        multiline_whitespace,
        terminated(
            separated_list(multiline_whitespace, kserd_concise),
            multiline_whitespace,
        ),
    )(i)
}

/// Parse as a tuple. Will `Fail` if delimited by opening paren `(`.
/// Tries to determine if the stream is in `Concise` or `Inline` format using
/// simple heuristics. The decision can be overridden by forcing `Inline` format.
pub fn tuple<'a, E: ParseError<&'a str>>(
    force_inline: bool,
) -> impl Fn(&'a str) -> IResult<&'a str, Kserd<'a>, E> {
    move |i: &'a str| {
        let (i, ident) = opt(ident(false))(i)?;

        let (i, _) = ignore_inline_whitespace(char('('))(i)?; // open with paren

        // we manually work out if should be treating as inline or concise
        let concise = recognise_concise(i) && !force_inline;

        let parser = if concise {
            concise_list_kserds
        } else {
            inline_list_kserds
        };

        let ctx = if concise {
            "multi-line (concise) tuple"
        } else {
            "inline tuple"
        };

        let (i, value) = context(
            ctx,
            cut(terminated(parser, ignore_inline_whitespace(char(')')))),
        )(i)?;

        let value = Value::Tuple(value);

        Ok((i, kserd_ctor(ident, value)))
    }
}

/// Parse as a sequence. Will `Fail` if delimited by opening bracket `[`.
/// Tries to determine if the stream is in `Concise` or `Inline` format using
/// simple heuristics. The decision can be overridden by forcing `Inline` format.
pub fn seq_delimited<'a, E: ParseError<&'a str>>(
    force_inline: bool,
) -> impl Fn(&'a str) -> IResult<&'a str, Kserd<'a>, E> {
    move |i: &'a str| {
        let (i, ident) = opt(ident(false))(i)?;

        let (i, _) = ignore_inline_whitespace(char('['))(i)?; // open with bracket

        // we manually work out if should be treating as inline or concise
        let concise = recognise_concise(i) && !force_inline;

        let parser = if concise {
            concise_list_kserds
        } else {
            inline_list_kserds
        };

        let ctx = if concise {
            "multi-line (concise) sequence"
        } else {
            "inline sequence"
        };

        let (i, value) = context(
            ctx,
            cut(terminated(parser, ignore_inline_whitespace(char(']')))),
        )(i)?;

        let value = Value::Seq(value);

        Ok((i, kserd_ctor(ident, value)))
    }
}
