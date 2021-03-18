use super::*;

pub(super) fn write(
    mut buf: String,
    node: Node,
    fmts: &[Fmt],
    col: usize,
    map: NamedIter,
) -> String {
    let fmt = fmts.get(node.index()).unwrap();

    let id = node.kserd().id();

    let prefix = |mut buf| {
        maybe_write_nonprim_ident(&mut buf, fmt.id, id);
        buf.push('(');
        buf
    };
    let suffix = |mut buf: String| {
        buf.push(')');
        buf
    };

    let write_node_with_field_mapping = |mut buf: String, name, value, col| {
        let buf_lo = buf.len();

        buf.push_str(name);
        buf.push_str(FIELDS_ASSIGNER);
        buf.push(' ');

        let buf_hi = buf.len();

        // offset the column to the end of the name, makes things neater
        let name_indent = buf[buf_lo..buf_hi].chars().count();
        let c = col + name_indent;

        buf = write_node(buf, value, fmts, c); // write value
        buf.push('\n');

        buf
    };

    match fmt.line {
        Repr::Inline => {
            delim_writer(buf, prefix, suffix, |mut buf| {
                let rm_trailing = !map.is_empty();

                for (name, v) in map {
                    buf.push_str(name);
                    buf.push_str(FIELDS_ASSIGNER);
                    buf.push(' ');
                    buf = write_node(buf, v, fmts, col); // write value
                    buf.push_str(FIELDS_SEPARATOR);
                }

                if rm_trailing {
                    FIELDS_SEPARATOR.chars().for_each(|_| {
                        buf.pop();
                    });
                }
                buf
            })
        }
        Repr::Concise => delim_writer(buf, prefix, suffix, |mut buf| {
            buf.push('\n');

            let c = col + INDENT;

            for (name, v) in map {
                write_indent(&mut buf, c);
                buf = write_node_with_field_mapping(buf, name, v, c);
            }

            write_indent(&mut buf, col);
            buf
        }),
        Repr::Verbose => {
            // Names for containers follow :<id>
            if let Some(id) = id {
                write_indent(&mut buf, col);
                buf.push(':');
                buf.push_str(id);
                buf.push('\n');
            }

            for (name, v) in map {
                // the formatting follows the following rules
                // 1. if the child value is _not_ verbose then the value is prefixed with
                //    'field_name = '
                // 2. if the child value _is_ verbose then we write out specialised naming
                let vfmt = fmts.get(v.index()).unwrap();

                let (vrepr, val) = (vfmt.line, v.value());

                buf = match (vrepr, val) {
                    (Repr::Verbose, NodeValue::Cntr(map)) => {
                        // write the [field_name] then the map below
                        buf.push('\n');
                        write_indent(&mut buf, col);
                        buf.push('[');
                        buf.push_str(name);
                        buf.push_str("]\n");
                        // notice the added INDENT!!! cntr's indent children
                        cntr::write(buf, v, fmts, col + INDENT, map)
                    }
                    (Repr::Verbose, NodeValue::Seq(seq)) => {
                        buf.push('\n');
                        buf = seqs::write(buf, v, fmts, col, seq, Some(name));
                        buf.pop(); // pop last new line
                        buf
                    }
                    (Repr::Verbose, NodeValue::Map(map)) => {
                        buf.push('\n');
                        maps::write(buf, v, fmts, col, map, Some(name))
                    }
                    _ => {
                        write_indent(&mut buf, col);
                        write_node_with_field_mapping(buf, name, v, col)
                    }
                };
            }

            buf
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[allow(clippy::all)]
    fn cntr_fmting() {
        let kserd = Kserd::with_id(
            "Struct",
            Value::new_cntr(vec![
                ("a", Kserd::new_num(101)),
                (
                    "b",
                    Kserd::new(Value::Tuple(vec![Kserd::new_num(0), Kserd::new_num(10101)])),
                ),
                (
                    "c",
                    Kserd::new(Value::Seq(vec![Kserd::new_num(111), Kserd::new_num(3.14)])),
                ),
            ])
            .unwrap(),
        )
        .unwrap();

        let mut config = FormattingConfig {
            width_limit: None,
            ..Default::default()
        };

        let s = kserd.as_str_with_config(config);
        println!("{}", s);
        assert_eq!(&s, "Struct (a = 101, b = (0, 10101), c = [111, 3.14])");

        config.width_limit = Some(20);

        let s = kserd.as_str_with_config(config);
        println!("{}", s);
        assert_eq!(
            &s,
            "Struct (
    a = 101
    b = (0, 10101)
    c = [111, 3.14]
)"
        );

        config.width_limit = Some(0); // if map is root, can only be concise

        let s = kserd.as_str_with_config(config);
        println!("{}", s);
        assert_eq!(
            &s,
            ":Struct
a = 101
b = (
        0
        10101
    )

[[c]]
111

[[c]]
3.14
"
        );
    }

    #[test]
    fn field_structs() {
        let kserd =
            Kserd::new_cntr(vec![("a", Kserd::new_num(1)), ("b", Kserd::new_num(2))]).unwrap();

        let mut fmtr = Formatter::new(&kserd);
        fmtr.verbose(0).unwrap();
        fmtr.id(1, false).unwrap();
        fmtr.id(2, false).unwrap();

        let s = fmtr.write_string(String::new());
        println!("{}", s);
        assert_eq!(&s, "a = 1\nb = 2\n");
    }

    #[test]
    fn nested_cntrs() {
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

        let s = kserd.as_str_with_config(FormattingConfig {
            width_limit: Some(0),
            ..Default::default()
        });

        println!("{}", s);
        assert_eq!(
            &s,
            "
[un]

    [dos]

        [treis]
            quattro = ()
"
        );
    }

    #[test]
    fn empty_cntr() {
        let kserd = Kserd::with_id(
            "something",
            Value::new_cntr(Vec::<(&str, _)>::new()).unwrap(),
        )
        .unwrap();

        let mut fmtr = Formatter::new(&kserd);

        // with id
        fmtr.id(0, true).unwrap();

        assert!(fmtr.verbose(0).is_err()); // can't verbose empty
        assert!(fmtr.concise(0).is_err()); // can't concise empty

        fmtr.inline(0).unwrap(); // inline
        assert_eq!(&fmtr.write_string(String::new()), "something ()");

        // no id
        fmtr.id(0, false).unwrap();

        fmtr.inline(0).unwrap(); // inline
        assert_eq!(&fmtr.write_string(String::new()), "()");
    }
}
