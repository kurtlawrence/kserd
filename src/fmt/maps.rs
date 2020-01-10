use super::*;

pub(super) fn write(
    mut buf: String,
    node: Node,
    fmts: &[Fmt],
    col: usize,
    map: KeyedIter,
    field_name: Option<&str>,
) -> String {
    let fmt = fmts.get(node.index()).unwrap();

    let id = node.kserd().id();

    let prefix = |mut buf| {
        maybe_write_nonprim_ident(&mut buf, fmt.id, id);
        buf.push('{');
        buf
    };
    let suffix = |mut buf: String| {
        buf.push('}');
        buf
    };

    let write_key_and_value_concise = |mut buf: String, key: Node, value: Node, col| {
        let buf_lo = buf.len();
        let key_is_inline = fmts.get(key.index()).unwrap().line == Repr::Inline;

        buf = write_node(buf, key, fmts, col); // write key
        buf.push_str(KEYS_ASSIGNER);
        buf.push(' ');

        let name_indent = if key_is_inline {
            let buf_hi = buf.len();
            buf[buf_lo..buf_hi].chars().count()
        } else {
            2 + KEYS_ASSIGNER.chars().count() // for '): '
        };

        buf = write_node(buf, value, fmts, col + name_indent); // write value
        buf.push('\n');
        buf
    };

    match fmt.line {
        Repr::Inline => {
            delim_writer(buf, prefix, suffix, |mut buf| {
                let rm_trailing = !map.is_empty();

                buf.push(' ');

                for (k, v) in map {
                    buf = write_node(buf, k, fmts, col); // write key
                    buf.push_str(KEYS_ASSIGNER);
                    buf.push(' ');
                    buf = write_node(buf, v, fmts, col); // write value
                    buf.push_str(FIELDS_SEPARATOR);
                }

                if rm_trailing {
                    FIELDS_SEPARATOR.chars().for_each(|_| {
                        buf.pop();
                    });
                }

                buf.push(' ');
                buf
            })
        }
        Repr::Concise => delim_writer(buf, prefix, suffix, |mut buf| {
            buf.push('\n');

            let c = col + INDENT;

            for (k, v) in map {
                write_indent(&mut buf, c);
                buf = write_key_and_value_concise(buf, k, v, c);
            }

            write_indent(&mut buf, col);
            buf
        }),
        Repr::Verbose => {
            let field_name = field_name.expect("Verbose Maps require a field name");
            let val_indent = col + INDENT;

            for (k, v) in map {
                // write field name
                buf.push_str("[[");
                buf.push_str(field_name);
                buf.push_str("]]\n");

                // if verbose write on new line, otherwise it is just concise like above
                match fmts.get(v.index()).unwrap().line {
                    Repr::Verbose => {
                        // write key
                        buf = write_node(buf, k, fmts, col);
                        buf.push_str(KEYS_ASSIGNER);
                        buf.push('\n');

                        // write value
                        buf = write_node(buf, v, fmts, val_indent);
                        buf.push_str("\n");
                    }
                    _ => {
                        buf = write_key_and_value_concise(buf, k, v, col);
                    }
                }
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
    fn map_fmting() {
        let kserd = Kserd::new_map(vec![
            (Kserd::new_unit(), Kserd::new_num(100)),
            (Kserd::new_num(1), Kserd::new_num(-101)),
            (
                Kserd::new(Value::Tuple(vec![Kserd::new_num(0), Kserd::new_num(1)])),
                Kserd::new_num(3.14),
            ),
        ]);

        let mut config = FormattingConfig {
            width_limit: None,
            ..Default::default()
        };

        let s = kserd.as_str_with_config(config);
        println!("{}", s);
        assert_eq!(&s, "{ (): 100, 1: -101, (0, 1): 3.14 }");

        config.width_limit = Some(20);

        let s = kserd.as_str_with_config(config);
        println!("{}", s);
        assert_eq!(
            &s,
            "{
    (): 100
    1: -101
    (0, 1): 3.14
}"
        );

        config.width_limit = Some(0); // if map is root, can only be concise

        let s = kserd.as_str_with_config(config);
        println!("{}", s);
        assert_eq!(
            &s,
            "{
    (): 100
    1: -101
    (
        0
        1
    ): 3.14
}"
        );
    }

    #[test]
    fn concise_keys_formatting() {
        let kserd = Kserd::new_map(vec![(
            Kserd::new(Value::Tuple(vec![Kserd::new_num(0), Kserd::new_num(1)])),
            Kserd::new(Value::Tuple(vec![Kserd::new_num(2), Kserd::new_num(3)])),
        )]);

        let fmtr = Formatter::new(&kserd);

        let s = fmtr.write_string(String::new());

        println!("{}", s);

        assert_eq!(
            &s,
            "{
    (
        <i32> 0
        <i32> 1
    ): (
           <i32> 2
           <i32> 3
       )
}"
        );
    }

    #[test]
    fn empty_map() {
        let kserd = Kserd::with_id("something", Value::new_map(vec![])).unwrap();

        let mut fmtr = Formatter::new(&kserd);

        // with id
        fmtr.id(0, true).unwrap();

        fmtr.concise(0).unwrap(); // concise
        assert_eq!(&fmtr.write_string(String::new()), "something {\n}");

        fmtr.inline(0).unwrap(); // inline
        assert_eq!(&fmtr.write_string(String::new()), "something {  }");

        // no id
        fmtr.id(0, false).unwrap();

        fmtr.concise(0).unwrap(); // concise
        assert_eq!(&fmtr.write_string(String::new()), "{\n}");

        fmtr.inline(0).unwrap(); // inline
        assert_eq!(&fmtr.write_string(String::new()), "{  }");
    }
}
