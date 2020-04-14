use super::*;

/// Key-Value pair, of `Kstr` to `Kserd`.
fn kvp_kserdstr_to_kserd<'a, E: ParseError<&'a str>>(
    force_inline: bool,
) -> impl Fn(&'a str) -> IResult<&'a str, (Kstr<'a>, Kserd<'a>), E> {
    context(
        "name-kserd key value pair",
        separated_pair(
            field_name,
            ignore_inline_whitespace(char('=')),
            ignore_inline_whitespace(cut(if force_inline {
                kserd_inline
            } else {
                kserd_concise
            })),
        ),
    )
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

        let (i, value) = context(ctx, terminated(parser, ignore_inline_whitespace(char(')'))))(i)?;

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
        let mut identity = None;

        for field in fields {
            match field {
                ContainerField::Id(id) => identity = Some(id),
                ContainerField::Direct(name, value) => {
                    rfields.insert(name, value);
                }
                ContainerField::NestedVerbose(name, value) => {
                    rfields.insert(name, value);
                }
                ContainerField::SeqEntry {
                    id,
                    field_name,
                    value,
                } => {
                    let (x, seq) = seqs.entry(field_name).or_insert_with(|| (None, Vec::new()));
                    *x = id.or_else(|| x.take());
                    seq.push(value);
                }
                ContainerField::MapEntry {
                    id,
                    field_name,
                    key,
                    value,
                } => {
                    let (x, map) = maps
                        .entry(field_name)
                        .or_insert_with(|| (None, BTreeMap::new()));
                    *x = id.or_else(|| x.take());
                    map.insert(key, value);
                }
            }
        }

        for (name, (id, seq)) in seqs {
            let mut val = Kserd::new(Value::Seq(seq));
            val.id = id;
            rfields.insert(name, val);
        }

        for (name, (id, map)) in maps {
            let mut val = Kserd::new(Value::Map(map));
            val.id = id;
            rfields.insert(name, val);
        }

        let mut kserd = Kserd::new(Value::Cntr(rfields));
        kserd.id = identity;

        Ok((i, kserd))
    }
}

/// Generally the `Kstr` is the field name, except for maps which have to be a little more
/// explicit.
#[derive(Debug)]
enum ContainerField<'a> {
    Id(Kstr<'a>),
    Direct(Kstr<'a>, Kserd<'a>),
    NestedVerbose(Kstr<'a>, Kserd<'a>),
    SeqEntry {
        id: Option<Kstr<'a>>,
        field_name: Kstr<'a>,
        value: Kserd<'a>,
    },
    MapEntry {
        id: Option<Kstr<'a>>,
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

        let (i, _) = expect_indents(indents)(i)?;
        let (i, _) = inline_whitespace(i)?; // passed the indent threshold, ignore any extra inline space
                                            // the thinking is it won't affect parsing,
                                            // but could affect formatting
                                            // hopefully can write a formatter to prettify it.

        if i.is_empty() || (ignore_inline_whitespace::<_, E, _>(line_ending)(i)).is_ok() {
            // The line had tabs but is empty of other characters.
            return Err(Err::Error(error::make_error(i, ErrorKind::Eof)));
        }

        // can use starts_with as all whitespace should be removed.
        if i.starts_with(':') {
            let (i, identity) = ignore_inline_whitespace(ident(false))(&i[1..])?;
            Ok((i, ContainerField::Id(identity)))
        } else if recognise_field_assign(i) {
            let (i, (fname, val)) = kvp_kserdstr_to_kserd(false)(i)?;
            Ok((i, ContainerField::Direct(fname, val)))
        } else if i.starts_with("[[") {
            let (i, (field_name, id)) = preceded(
                tag("[["),
                terminated(
                    verbose_field_name_and_identity,
                    ignore_inline_whitespace(tag("]]")),
                ),
            )(i)?;
            // must be a new line after a field name [[]]
            let (i, _) = ignore_inline_whitespace(line_ending)(i)?;

            // now work out if it is a map or just sequence.
            // a map as the usual inline kserd, followed by the colon.

            // get the key (kserd-inline:)
            let (i, key) = opt(terminated(
                ignore_inline_whitespace(uncut(kserd_inline)),
                ignore_inline_whitespace(char(':')),
            ))(i)?;

            if let Some(key) = key {
                let (i, value) = context(
                    "verbose map entry",
                    alt((
                        ignore_inline_whitespace(kserd_concise),
                        preceded(
                            ignore_inline_whitespace(line_ending),
                            kserd_nested(indents + 1),
                        ),
                    )),
                )(i)?;
                let entry = ContainerField::MapEntry {
                    id,
                    field_name,
                    key,
                    value,
                };
                Ok((i, entry))
            } else {
                let (i, value) = context("verbose sequence entry", kserd_nested(indents + 1))(i)?;
                Ok((
                    i,
                    ContainerField::SeqEntry {
                        id,
                        field_name,
                        value,
                    },
                ))
            }
        } else if i.starts_with('[') {
            let (i, (fname, identity)) = preceded(
                tag("["),
                terminated(
                    verbose_field_name_and_identity,
                    ignore_inline_whitespace(tag("]")),
                ),
            )(i)?;
            // must be a new line after a field name []
            let (i, _) = ignore_inline_whitespace(line_ending)(i)?;
            let (i, mut value) = context("nested container", kserd_nested(indents + 1))(i)?;
            value.id = value.id.or(identity); // prioritise ids that come as rows

            Ok((i, ContainerField::NestedVerbose(fname, value)))
        } else {
            context(
                "unmatchable container field. not a direct (=), seq, or map entry",
                |i| Err(Err::Error(error::make_error(i, ErrorKind::NoneOf))),
            )(i)
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

/// Ignores all whitespace around name and identity. Field name _must_ exist, and if there is a
/// separator (`:`) then the identity _must_ exist as well.
fn verbose_field_name_and_identity<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, (Kstr<'a>, Option<Kstr<'a>>), E> {
    context("verbose container field name", |i| {
        let (i, fname) = ignore_inline_whitespace(field_name)(i)?;
        match ignore_inline_whitespace::<_, E, _>(char(':'))(i) {
            Ok((i, _)) => {
                let (i, id) = ignore_inline_whitespace(ident(false))(i)?;
                Ok((i, (fname, Some(id))))
            }
            Err(_) => Ok((i, (fname, None))),
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
