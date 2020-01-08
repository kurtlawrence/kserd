use super::*;

/// Key-Value pair, of `Kserd` to `Kserd`.
fn kvp_kserd_to_kserd<'a, E: ParseError<&'a str>>(
    force_inline: bool,
) -> impl Fn(&'a str) -> IResult<&'a str, (Kserd<'a>, Kserd<'a>), E> {
    move |input: &'a str| {
        let value_parser = if force_inline {
            kserd_inline
        } else {
            kserd_concise
        };

        context(
            "kserd-kserd key-value pair",
            separated_pair(
                alt((kserd_concise, kserd_inline)),
                ignore_inline_whitespace(char(':')),
                ignore_inline_whitespace(value_parser),
            ),
        )(input)
    }
}

/// Comma separated key-value pairs, where key and value are both `Kserd`s.
fn inline_map_kserds<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Vec<(Kserd<'a>, Kserd<'a>)>, E> {
    context(
        "comma separated kserd-kserd pair",
        separated_list(
            ignore_inline_whitespace(char(',')),
            ignore_inline_whitespace(kvp_kserd_to_kserd(true)),
        ),
    )(i)
}

/// Concise maps are separated by new lines.
fn concise_map_kserds<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Vec<(Kserd<'a>, Kserd<'a>)>, E> {
    context(
        "newline separated (concise) kserd-kserd pair",
        preceded(
            multiline_whitespace,
            terminated(
                separated_list(multiline_whitespace, kvp_kserd_to_kserd(false)),
                multiline_whitespace,
            ),
        ),
    )(i)
}

/// Parse as a sequence. Will `Fail` if delimited by opening brace `{`.
/// Tries to determine if the stream is in `Concise` or `Inline` format using
/// simple heuristics. The decision can be overridden by forcing `Inline` format.
pub fn map_delimited<'a, E: ParseError<&'a str>>(
    force_inline: bool,
) -> impl Fn(&'a str) -> IResult<&'a str, Kserd<'a>, E> {
    move |i: &'a str| {
        let (i, ident) = opt(ident(false))(i)?;

        let (i, _) = ignore_inline_whitespace(char('{'))(i)?; // open with bracket

        // we manually work out if should be treating as inline or concise
        let concise = recognise_concise(i) && !force_inline;

        let parser = if concise {
            concise_map_kserds
        } else {
            inline_map_kserds
        };

        let ctx = if concise { "concise map" } else { "inline map" };

        let (i, value) = context(ctx, cut(terminated(parser, ignore_inline_whitespace(char('}')))))(i)?;

        let value = Value::Map(value.into_iter().collect());

        Ok((i, kserd_ctor(ident, value)))
    }
}
