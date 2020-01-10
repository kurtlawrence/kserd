use super::*;

/// Key-Value pair, of `Kstr` to `Kserd`.
fn kvp_kserdstr_to_kserd<'a, E: ParseError<&'a str>>(
    force_inline: bool,
) -> impl Fn(&'a str) -> IResult<&'a str, (Kstr<'a>, Kserd<'a>), E> {
    move |input: &'a str| {
        let value_parser = if force_inline {
            kserd_inline
        } else {
            kserd_concise
        };

        context(
            "kserdstr-kserd key-value pair",
            separated_pair(
                field_name,
                ignore_inline_whitespace(char('=')),
                cut(ignore_inline_whitespace(value_parser)),
            ),
        )(input)
    }
}

/// Comma separated key-value pairs, where key is `Kstr` and value are `Kserd`.
fn inline_cntr_kserds<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Vec<(Kstr<'a>, Kserd<'a>)>, E> {
    context(
        "comma separated kserdstr-kserd pair",
        separated_list(
            ignore_inline_whitespace(char(',')),
            ignore_inline_whitespace(kvp_kserdstr_to_kserd(true)),
        ),
    )(i)
}

/// Concise maps are separated by new lines.
fn concise_cntr_kserds<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Vec<(Kstr<'a>, Kserd<'a>)>, E> {
    context(
        "newline separated (concise) kserdstr-kserd pair",
        preceded(
            multiline_whitespace,
            terminated(
                separated_list(multiline_whitespace, kvp_kserdstr_to_kserd(false)),
                multiline_whitespace,
            ),
        ),
    )(i)
}

pub fn delimited<'a, E: ParseError<&'a str>>(
    force_inline: bool,
) -> impl Fn(&'a str) -> IResult<&'a str, Kserd<'a>, E> {
    use std::iter::FromIterator;
    move |i: &'a str| {
        let (i, ident) = opt(ident(false))(i)?;

        let (i, _) = ignore_inline_whitespace(char('('))(i)?; // open with paren

        // we manually work out if should be treating as inline or concise
        let concise = recognise_concise(i) && !force_inline;

        let parser = if concise {
            concise_cntr_kserds
        } else {
            inline_cntr_kserds
        };

        let ctx = if concise {
            "concise container"
        } else {
            "inline container"
        };

        let (i, value) = context(
            ctx,
            cut(terminated(parser, ignore_inline_whitespace(char(')')))),
        )(i)?;

        let value = Value::Cntr(BTreeMap::from_iter(value));

        Ok((i, kserd_ctor(ident, value)))
    }
}

/// Verbose container.
///
/// Verbose containers can have three field types:
/// 1. An inline field mapping: `var_name = XXXXX`
/// 2. A nested verbose field mapping: `[var_name]`
/// 3. A sequence or map entry: `[[var_name]]`
///
/// The only way a verbose container is ended is via indenting recognisation.
/// A simple rooted container will just end with the end of input. A nested container
/// ends if the element (direct etc) is not at the level of indenting expected.
///
/// There are quite a few error and failure pathways, which for now aren't handled gracefully.
/// TODO: when I get to implementing a custom error type to handle these errors, will work something out.
///
/// `indents` is in number of _indents_ that is expected to be inside _this_ Container.
/// An _indent_ is 4 consectutive spaces, or a single tab character.
pub fn verbose<'a, E: ParseError<&'a str>>(
    indents: usize,
) -> impl Fn(&'a str) -> IResult<&'a str, Kserd<'a>, E> {
    move |i: &'a str| {
        let (i, fields) = many0(verbose_cntr_field(indents))(i)?;

        let mut seqs = BTreeMap::new();
        let mut maps = BTreeMap::new();
        let mut rfields = BTreeMap::new();

        for field in fields {
            match field {
                ContainerField::Direct(name, value) => {
                    rfields.insert(name, value);
                }
                ContainerField::NestedVerbose(name, value) => {
                    rfields.insert(name, value);
                }
                ContainerField::SeqEntry(name, value) => {
                    seqs.entry(name).or_insert_with(Vec::new).push(value);
                }
                ContainerField::MapEntry {
                    field_name,
                    key,
                    value,
                } => {
                    maps.entry(field_name)
                        .or_insert_with(BTreeMap::new)
                        .insert(key, value);
                }
            }
        }

        for (name, seq) in seqs {
            let val = Kserd::new(Value::Seq(seq));
            rfields.insert(name, val);
        }

        for (name, map) in maps {
            let val = Kserd::new(Value::Map(map));
            rfields.insert(name, val);
        }

        Ok((i, Kserd::new(Value::Cntr(rfields))))
    }
}

#[derive(Debug)]
enum ContainerField<'a> {
    Direct(Kstr<'a>, Kserd<'a>),
    NestedVerbose(Kstr<'a>, Kserd<'a>),
    SeqEntry(Kstr<'a>, Kserd<'a>),
    MapEntry {
        field_name: Kstr<'a>,
        key: Kserd<'a>,
        value: Kserd<'a>,
    },
}

fn verbose_cntr_field<'a, E: ParseError<&'a str>>(
    indents: usize,
) -> impl Fn(&'a str) -> IResult<&'a str, ContainerField<'a>, E> {
    move |i: &'a str| {
        // This is little different. The indent is always from the newline
        // but there could be multiple newlines and whitespace before the _final_
        // new line. The only way to know is to look ahead...
        let i = {
            let mut tmp = i;
            loop {
                let (n, yes) = opt(ignore_inline_whitespace(line_ending))(tmp)?;
                if yes.is_some() {
                    tmp = n; // nothing but net
                } else {
                    break;
                }
            }
            tmp
        };

        if i.is_empty() {
            return Err(Err::Error(error::make_error(i, ErrorKind::Eof)));
        }

        let (i, _) = expect_indents(indents)(i)?;

        let (i, _) = inline_whitespace(i)?; // passed the indent threshold, ignore any extra inline space
                                            // the thinking is it won't affect parsing, but could affect formatting
                                            // hopefully can write a formatter to prettify it.

        if recognise_field_assign(i) {
            let (i, (fname, val)) = kvp_kserdstr_to_kserd(false)(i)?;
            Ok((i, ContainerField::Direct(fname, val)))
        } else if tag::<_, _, ()>("[[")(i).is_ok() {
            let (i, (fname, identity)) = preceded(
                tag("[["),
                cut(terminated(verbose_field_name_and_identity, tag("]]"))),
            )(i)?;
            let (i, _) = ignore_inline_whitespace(line_ending)(i)?; // must be a new line after a field name [[]]

            // now work out if it is a map or just sequence.
            // a map as the usual inline kserd, followed by the colon.

            let (i, key) = match terminated::<_, _, _, (), _, _>(
                terminated(kserd_inline, ignore_inline_whitespace(char(':'))),
                ignore_inline_whitespace(line_ending),
            )(i)
            {
                Ok((ii, key)) => (ii, Some(key)),
                Err(_) => (i, None),
            };

            let ctx = if key.is_some() {
                "verbose map entry"
            } else {
                "verbose sequence entry"
            };
            let (i, mut value) = context(ctx, cut(kserd_nested(indents + 1)))(i)?;
            value.id = identity;

            if let Some(key) = key {
                Ok((
                    i,
                    ContainerField::MapEntry {
                        field_name: fname,
                        key,
                        value,
                    },
                ))
            } else {
                Ok((i, ContainerField::SeqEntry(fname, value)))
            }
        } else if tag::<_, _, ()>("[")(i).is_ok() {
            let (i, (fname, identity)) = preceded(
                tag("["),
                cut(terminated(verbose_field_name_and_identity, tag("]"))),
            )(i)?;
            let (i, _) = ignore_inline_whitespace(line_ending)(i)?; // must be a new line after a field name []
            let (i, mut value) = context("nested container", cut(kserd_nested(indents + 1)))(i)?;
            value.id = identity;

            Ok((i, ContainerField::NestedVerbose(fname, value)))
        } else {
            dbg!(i);
            panic!("not recognised");
        }
    }
}

fn expect_indents<'a, E: ParseError<&'a str>>(
    indents: usize,
) -> impl Fn(&'a str) -> IResult<&'a str, (), E> {
    move |i: &'a str| {
        let mut r = i;

        for _ in 0..indents {
            let (r_, _) = alt((tag("    "), tag("\t")))(r)?;
            r = r_;
        }

        Ok((r, ()))
    }
}

fn verbose_field_name_and_identity<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, (Kstr<'a>, Option<Kstr<'a>>), E> {
    context("verbose container field name", |input| {
        let (input, fname) = field_name(input)?;
        let dot = input.chars().next().map(|c| c == '.').unwrap_or(false);
        if dot {
            let (input, id) = ident(false)(&input[1..])?;
            Ok((input, (fname, Some(id))))
        } else {
            Ok((input, (fname, None)))
        }
    })(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kserdit<'a, K, V>(v: Vec<(K, V)>) -> impl Iterator<Item = (Kserd<'a>, Kserd<'a>)>
    where
        K: ToKserd<'a>,
        V: ToKserd<'a>,
    {
        v.into_iter()
            .map(|(k, v)| (k.into_kserd().unwrap(), v.into_kserd().unwrap()))
    }

    #[test]
    fn verbose_cntr_map_entries() {
        let s = r#"
[[map-1]]
"a":
100

[[map-1]]
"b":
200

[[map-2]]
(1,2):
(
    3
    4
)

[[map-2]]
(5,6):
(7,8)
"#;
        let expected = Kserd::new_cntr(vec![
            (
                "map-1",
                Kserd::new_map(kserdit(vec![("a", 100), ("b", 200)])),
            ),
            (
                "map-2",
                Kserd::new_map(kserdit(vec![((1, 2), (3, 4)), ((5, 6), (7, 8))])),
            ),
        ])
        .unwrap();
        assert_eq!(parse(s), Ok(expected));
    }
}
