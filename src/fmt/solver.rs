use super::*;

pub fn inline_widths(root: &Node, config: &FormattingConfig) -> Vec<usize> {
    let mut widths = vec![None; root.nav().len()];

    for node in root.nav().nodes() {
        if widths.get(node.index()).unwrap().is_none() {
            let w = inline_widths_recurse(&node, config, &mut widths);
            *widths.get_mut(node.index()).unwrap() = Some(w);
        }
    }

    widths
        .into_iter()
        .map(|x| x.expect("all should be some"))
        .collect()
}

/// **Aggregates** self and children widths.
fn inline_widths_recurse(
    node: &Node,
    config: &FormattingConfig,
    widths: &mut Vec<Option<usize>>,
) -> usize {
    let mut width = inline_width(node.kserd(), config);

    for child in node.value_children() {
        if let Some(w) = widths.get(child.index()).unwrap() {
            width += w;
        } else {
            // recursively calculate and update value in widths
            let w = inline_widths_recurse(&child, config, widths);
            *widths.get_mut(child.index()).unwrap() = Some(w);

            width += w;
        }
    }

    for child in node.key_children() {
        if let Some(w) = widths.get(child.index()).unwrap() {
            width += w;
        } else {
            // recursively calculate and update value in widths
            let w = inline_widths_recurse(&child, config, widths);
            *widths.get_mut(child.index()).unwrap() = Some(w);

            width += w;
        }
    }

    width
}

fn inline_width(node: &Kserd, config: &FormattingConfig) -> usize {
    let ident = node.id().map(|x| x.chars().count()).unwrap_or(0);

    let prim_ident_width = if config.id_on_primitives {
        ident + 3 // add three for '<> '
    } else {
        0
    };

    match &node.val {
        Value::Unit => {
            if ident > 0 {
                ident
            } else {
                2 // for ()
            }
        }
        Value::Bool(v) => {
            prim_ident_width
                + if *v {
                    4 // true
                } else {
                    5 // false
                }
        }
        Value::Num(v) => prim_ident_width + v.to_string().chars().count(),
        Value::Str(v) => 2 + prim_ident_width + v.escape_default().count(), // plus two for quotations
        Value::Barr(bytes) => {
            3 + prim_ident_width
                + bytes.len() * 2
                + bytes.len().saturating_sub(1) * FIELDS_SEPARATOR.chars().count()
        }
        Value::Tuple(v) => {
            2 + v.len().saturating_sub(1) * FIELDS_SEPARATOR.chars().count()
                + if ident > 0 && config.id_on_tuples {
                    ident + 1 // for space after name
                } else {
                    0
                }
        }
        Value::Cntr(v) => {
            2 // brackets
			+ v.len().saturating_sub(1) * FIELDS_SEPARATOR.chars().count() // separator
			+ v.keys().map(|x| x.chars().count()).sum::<usize>() // names
			+ v.len() * (FIELDS_ASSIGNER.chars().count() + 1) // assign, plus space after
			+ if ident > 0 &&  config.id_on_containers {
				ident + 1 // for space after name
			} else {
				0
			}
        }
        Value::Seq(v) => {
            2 + v.len().saturating_sub(1) * FIELDS_SEPARATOR.chars().count()
                + if ident > 0 && config.id_on_seqs {
                    ident + 1 // for space after name
                } else {
                    0
                }
        }
        Value::Map(v) => {
            2 // brackets
			+ v.len().saturating_sub(1) * FIELDS_SEPARATOR.chars().count() // separator
			+ v.len() * (KEYS_ASSIGNER.chars().count() + 1) // assign, plus space after			
			+ if ident > 0 &&  config.id_on_maps {
				ident + 1 // for space after name
			} else {
				0
			}
        }
    }
}

pub fn concise_widths(
    root: &Node,
    config: &FormattingConfig,
    inline_widths: &[usize],
) -> Vec<usize> {
    let mut widths = vec![None; root.nav().len()];

    for node in root.nav().nodes() {
        if widths.get(node.index()).unwrap().is_none() {
            let w = concise_widths_recurse(&node, config, &mut widths, inline_widths);
            *widths.get_mut(node.index()).unwrap() = Some(w);
        }
    }

    widths
        .into_iter()
        .map(|x| x.expect("all should be some"))
        .collect()
}

/// Takes the **maximum** width of self and children
fn concise_widths_recurse(
    node: &Node,
    config: &FormattingConfig,
    widths: &mut Vec<Option<usize>>,
    inline_widths: &[usize],
) -> usize {
    use std::cmp::max;

    let mut width = concise_width(node.kserd(), config);

    match node.value() {
        NodeValue::Primitive => (),
        NodeValue::Tuple(children) => {
            for child in children {
                let w = if let Some(w) = widths.get(child.index()).unwrap() {
                    *w
                } else {
                    // recursively calculate and update value in widths
                    let w = concise_widths_recurse(&child, config, widths, inline_widths);
                    *widths.get_mut(child.index()).unwrap() = Some(w);
                    w
                };

                width = max(width, INDENT + w);
            }
        }
        NodeValue::Cntr(map) => {
            for (name, child) in map {
                let w = if let Some(w) = widths.get(child.index()).unwrap() {
                    *w
                } else {
                    // recursively calculate and update value in widths
                    let w = concise_widths_recurse(&child, config, widths, inline_widths);
                    *widths.get_mut(child.index()).unwrap() = Some(w);
                    w
                };

                width = max(
                    width,
                    INDENT + name.chars().count() + FIELDS_ASSIGNER.chars().count() + 1 + w,
                );
            }
        }
        NodeValue::Seq(children) => {
            for child in children {
                let w = if let Some(w) = widths.get(child.index()).unwrap() {
                    *w
                } else {
                    // recursively calculate and update value in widths
                    let w = concise_widths_recurse(&child, config, widths, inline_widths);
                    *widths.get_mut(child.index()).unwrap() = Some(w);
                    w
                };

                width = max(width, INDENT + w);
            }
        }
        NodeValue::Map(map) => {
            for (key, value) in map {
                let w = if let Some(w) = widths.get(value.index()).unwrap() {
                    *w
                } else {
                    // recursively calculate and update value in widths
                    let w = concise_widths_recurse(&value, config, widths, inline_widths);
                    *widths.get_mut(value.index()).unwrap() = Some(w);
                    w
                };

                width = max(
                    width,
                    INDENT
                        + inline_widths
                            .get(key.index())
                            .expect("inline widths vector must be from the same navigator")
                        + KEYS_ASSIGNER.chars().count()
                        + 1
                        + w,
                );
            }
        }
    }

    width
}

fn concise_width(node: &Kserd, config: &FormattingConfig) -> usize {
    let ident = node.id().map(|x| x.chars().count()).unwrap_or(0);

    match &node.val {
        Value::Unit | Value::Bool(_) | Value::Num(_) | Value::Str(_) | Value::Barr(_) => {
            inline_width(node, config)
        } // same as inline width
        Value::Tuple(_) => {
            1 + if config.id_on_tuples {
                ident + 1 // for space after name
            } else {
                0
            }
        }
        Value::Cntr(_) => {
            1 + if config.id_on_containers {
                ident + 1 // for space after name
            } else {
                0
            }
        }
        Value::Seq(_) => {
            1 + if config.id_on_seqs {
                ident + 1 // for space after name
            } else {
                0
            }
        }
        Value::Map(_) => {
            1 + if config.id_on_maps {
                ident + 1 // for space after name
            } else {
                0
            }
        }
    }
}

// make a note that this DOES NOT RESET formatting
pub fn apply_config(fmtr: &mut Formatter, config: FormattingConfig) {
    // first run through and just apply the displaying of identities, pretty easy
    for i in 0..fmtr.len() {
        let id = match fmtr.get(i).unwrap().value() {
            NodeValue::Primitive => config.id_on_primitives,
            NodeValue::Tuple(_) => config.id_on_tuples,
            NodeValue::Cntr(_) => config.id_on_containers,
            NodeValue::Seq(_) => config.id_on_seqs,
            NodeValue::Map(_) => config.id_on_maps,
        };
        fmtr.id(i, id).ok();
    }

    // now actually apply the line repr for each node
    let root = fmtr.root();
    let inline_widths = solver::inline_widths(&root, &config);
    let concise_widths = solver::concise_widths(&root, &config, &inline_widths);

    let repr = min_line_repr(0, config.width_limit, &inline_widths, &concise_widths);

    let decendants = root
        .descendants_breadth()
        .into_iter()
        .map(|x| x.index())
        .collect::<Vec<_>>();

    // failures here don't matter
    // either it succeeds in setting the line repr, or the line repr is already
    // pretty limited.
    fmtr.request_repr_alteration(0, repr).ok();

    for c in decendants {
        let offset = calculate_offset(fmtr.get(c).unwrap(), fmtr.fmts(), &inline_widths);

        let lim = config.width_limit.map(|x| x.saturating_sub(offset));
        let repr = min_line_repr(c, lim, &inline_widths, &concise_widths);

        // manually change some
        use NodeValue::*;
        use Repr::*;;
        let child = fmtr.get(c).unwrap();
        let repr = match (repr, child.value(), child.parent().unwrap().value()) {
            (Concise, Cntr(_), Seq(_)) => Verbose, // child containers in a sequence won't go as verbose as concise is generally just as wide
            (x, _, _) => x,
        };

        fmtr.request_repr_alteration(c, repr).ok(); // again we let this silently fail
    }
}

fn calculate_offset(node: Node, fmts: &[Fmt], inline_widths: &[usize]) -> usize {
    use Repr::*;

    if let Some(parent) = node.parent() {
        // the first one is concise _or_ inline
        let mut offset = match fmts.get(parent.index()).unwrap().line {
            Inline => 0,
            Concise | Verbose => {
                concise_parent_child_column(parent.value(), &node, fmts, inline_widths)
            }
        };

        // aggregate the _concrete_ parent fmts
        let mut child = parent;
        while let Some(parent) = child.parent() {
            let add = match fmts.get(parent.index()).unwrap().line {
                Inline => 0,
                Concise => concise_parent_child_column(parent.value(), &child, fmts, inline_widths),
                Verbose => verbose_parent_child_column(parent.value(), &child),
            };

            offset += add;
            child = parent;
        }

        if fmts.get(child.index()).unwrap().line == Repr::Verbose {
            offset.saturating_sub(INDENT)
        } else {
            offset
        }
    } else {
        0
    }
}

fn concise_parent_child_column(
    pvalue: NodeValue,
    child: &Node,
    fmts: &[Fmt],
    inline_widths: &[usize],
) -> usize {
    use NodeValue::*;

    match pvalue {
        Primitive => unreachable!("primitie only inline"),
        Tuple(_) | Seq(_) => INDENT,
        Cntr(m) => INDENT + cntr_field_offset(m, child.idx_in_parent()),
        Map(m) => INDENT + map_entry_offset(m, child, fmts, inline_widths),
    }
}

fn verbose_parent_child_column(pvalue: NodeValue, child: &Node) -> usize {
    use NodeValue::*;

    match (pvalue, child.value()) {
        (Primitive, _) => unreachable!("primitive cannot be verbose"),
        (Tuple(_), _) => unreachable!("tuple cannot be verbose"),
        (Seq(_), _) => 0,
        (Cntr(_), Cntr(_)) => INDENT,
        (Cntr(_), _) => 0,
        (Map(_), _) => {
            if child.is_key() {
                0
            } else {
                INDENT
            }
        }
    }
}

fn cntr_field_offset<'a>(mut map: NamedIter<'a, '_, 'a, 'a>, index: usize) -> usize {
    let (k, _) = map.nth(index).unwrap();
    k.chars().count() + FIELDS_ASSIGNER.chars().count() + 1 // space
}

fn map_entry_offset<'a>(
    mut map: KeyedIter<'a, '_, 'a, 'a>,
    child: &Node,
    fmts: &[Fmt],
    inline_widths: &[usize],
) -> usize {
    use Repr::*;

    if child.is_key() {
        0
    } else {
        let (k, _) = map.nth(child.idx_in_parent()).unwrap();

        let kfmt = fmts.get(k.index()).unwrap();
        let klen = match kfmt.line {
            Inline => inline_widths[k.index()],
            Concise => 1, // closing bracket
            Verbose => unreachable!("map keys cannot be verbose"),
        };

        klen + KEYS_ASSIGNER.chars().count() + 1 // space
    }
}

fn min_line_repr(
    idx: usize,
    lim: Option<usize>,
    inline_widths: &[usize],
    concise_widths: &[usize],
) -> Repr {
    if let Some(lim) = lim {
        if concise_widths[idx] > lim {
            Repr::Verbose
        } else if inline_widths[idx] > lim {
            Repr::Concise
        } else {
            Repr::Inline
        }
    } else {
        Repr::Inline
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_inline_widths() {
        let kserd = Kserd::enc(&(100, -101, "Hello\n")).unwrap();

        let nav = Navigator::new(&kserd);
        let root = nav.root();

        let widths = inline_widths(
            &root,
            &FormattingConfig {
                id_on_primitives: false,
                id_on_tuples: false,
                ..Default::default()
            },
        );

        assert_eq!(widths.len(), 4);

        // The navigator works in a depth first approach
        assert_eq!(widths[0], 6 + 3 + 4 + 9); // (100, -101, "Hello\n")
        assert_eq!(widths[1], 3); // 100
        assert_eq!(widths[2], 4); // -101
        assert_eq!(widths[3], 9); // "Hello\n"
    }

    #[test]
    fn test_offset_calc_01() {
        let kserd = Kserd::new_cntr(vec![(
            "field-name",
            Kserd::new_map(vec![(Kserd::new_num(1234), Kserd::new_num(1234567))]),
        )])
        .unwrap();

        let config = FormattingConfig {
            id_on_containers: false,
            id_on_maps: false,
            id_on_primitives: false,
            ..Default::default()
        };

        let mut fmtr = Formatter::new(&kserd);

        let inline_widths = solver::inline_widths(&fmtr.root(), &config);

        fmtr.concise(0).unwrap(); // make root concise
        fmtr.concise(1).unwrap(); // field is concise (the map)
        fmtr.inline(2).unwrap(); // key is inline
        fmtr.inline(3).unwrap(); // value is inline

        assert_eq!(fmtr.len(), 4); // double check covered all bases

        let f = |idx| calculate_offset(fmtr.get(idx).unwrap(), fmtr.fmts(), &inline_widths);

        assert_eq!(f(0), 0); //  (                                   root
        assert_eq!(f(1), 17); //     field-name = {                  map
        assert_eq!(f(2), 21); //                      1234:          key
        assert_eq!(f(3), 27); //                            1234567  value
    }

    #[test]
    fn test_offset_calc_02() {
        let kserd = Kserd::new_cntr(vec![(
            "mapping",
            Kserd::new_map(vec![(
                Kserd::new(Value::Tuple(vec![Kserd::new_num(1), Kserd::new_num(2)])),
                Kserd::new_cntr(vec![
                    ("a", Kserd::new_num(100)),
                    (
                        "b",
                        Kserd::new(Value::Tuple(vec![Kserd::new_num(0), Kserd::new_num(1)])),
                    ),
                    (
                        "c",
                        Kserd::new(Value::Seq(vec![Kserd::new_num(2), Kserd::new_num(3)])),
                    ),
                ])
                .unwrap(),
            )]),
        )])
        .unwrap();

        let config = FormattingConfig {
            id_on_containers: false,
            id_on_maps: false,
            id_on_seqs: false,
            id_on_primitives: false,
            id_on_tuples: false,
            ..Default::default()
        };

        let mut fmtr = Formatter::new(&kserd);

        let inline_widths = solver::inline_widths(&fmtr.root(), &config);

        fmtr.verbose(0).unwrap(); // make root verbose
        fmtr.verbose(1).unwrap(); // field is verbose (the map)
        fmtr.inline(2).unwrap(); // key is inline

        assert_eq!(fmtr.len(), 13); // double check covered all bases

        let f = |idx| calculate_offset(fmtr.get(idx).unwrap(), fmtr.fmts(), &inline_widths);

        println!("{}", fmtr.write_string(String::new()));

        assert_eq!(
            verbose_parent_child_column(fmtr.get(1).unwrap().value(), &fmtr.get(2).unwrap(),),
            0
        ); // verbose keys are 0 offset

        assert_eq!(f(0), 0); // root
        assert_eq!(f(1), 10); // map gets tested as concise
        assert_eq!(f(2), 0); // key sits on column 0
        assert_eq!(f(3), 0); // key values are inline (usually just zero)
        assert_eq!(f(4), 0);
        assert_eq!(f(5), 8); // the value gets tested as if it is just after key '(1, 2): '
        assert_eq!(f(6), 8); // 'a' field assignment
        assert_eq!(f(7), 8); // 'b' field assignment
        assert_eq!(f(10), 8); // 'c' field assignment
    }

    #[test]
    fn test_offset_calc_03() {
        let kserd = Kserd::new_cntr(vec![(
            "un",
            Kserd::new_cntr(vec![(
                "dos",
                Kserd::new_cntr(vec![(
                    "treis",
                    Kserd::new_cntr(vec![("quattro", Kserd::new_unit())]).unwrap(),
                )])
                .unwrap(),
            )])
            .unwrap(),
        )])
        .unwrap();

        let config = FormattingConfig {
            id_on_containers: false,
            id_on_maps: false,
            id_on_seqs: false,
            id_on_primitives: false,
            id_on_tuples: false,
            ..Default::default()
        };

        let mut fmtr = Formatter::new(&kserd);

        let inline_widths = solver::inline_widths(&fmtr.root(), &config);

        //         fmtr.concise(2).unwrap();

        assert_eq!(fmtr.len(), 5); // double check covered all bases

        println!("{}", fmtr.write_string(String::new()));

        let f = |idx| calculate_offset(fmtr.get(idx).unwrap(), fmtr.fmts(), &inline_widths);

        assert_eq!(f(0), 0); // root
        assert_eq!(f(1), 5); // 'un = ' (5)
        assert_eq!(f(2), 10); // '    dos = ' (10)
        assert_eq!(f(3), 16); // '        treis = ' (16)
        assert_eq!(f(4), 22); // '            quattro = ' (27)
    }

    #[test]
    fn test_offset_calc_04() {
        let kserd = Kserd::new_map(vec![(Kserd::new_num(1234), Kserd::new_num(123456))]);

        let mut fmtr = Formatter::new(&kserd);
        let inline_widths = solver::inline_widths(&fmtr.root(), &FormattingConfig::default());

        fmtr.concise(0).unwrap();

        let f = |idx| calculate_offset(fmtr.get(idx).unwrap(), fmtr.fmts(), &inline_widths);

        assert_eq!(f(0), 0); // {
        assert_eq!(f(1), 4); //     1234:
        assert_eq!(f(2), 10); //          123456
    }

}
