use super::*;

pub(super) fn write(
    mut buf: String,
    node: Node,
    fmts: &[Fmt],
    col: usize,
    seq: SeqIter,
    field_name: Option<&str>,
) -> String {
    let fmt = fmts.get(node.index()).unwrap();

    let id = node.kserd().id();

    let prefix = |mut buf| {
        maybe_write_nonprim_ident(&mut buf, fmt.id, id);
        buf.push('[');
        buf
    };
    let suffix = |mut buf: String| {
        buf.push(']');
        buf
    };

    match fmt.line {
        Repr::Inline => {
            delim_writer(buf, prefix, suffix, |mut buf| {
                let rm_trailing = seq.len() > 0;

                for n in seq {
                    buf = write_node(buf, n, fmts, col);
                    buf.push_str(FIELDS_SEPARATOR);
                }
                if rm_trailing {
                    FIELDS_SEPARATOR.chars().for_each(|_| {
                        buf.pop();
                    }); // remove trailing separator
                }
                buf
            })
        }
        Repr::Concise => delim_writer(buf, prefix, suffix, |mut buf| {
            buf.push('\n');

            let c = col + INDENT;

            for n in seq {
                write_indent(&mut buf, c);
                buf = write_node(buf, n, fmts, c);
                buf.push('\n');
            }

            write_indent(&mut buf, col);

            buf
        }),
        Repr::Verbose => {
            let field_name = field_name.expect("Verbose Seqs require a field name");

            for n in seq {
                // write field name
                write_indent(&mut buf, col);
                buf.push_str("[[");
                buf.push_str(field_name);
                buf.push_str("]]\n");

                // write value
                match fmts.get(n.index()).unwrap().line {
                    Repr::Verbose => {
                        buf = write_node(buf, n, fmts, col + INDENT);
                    }
                    _ => {
                        write_indent(&mut buf, col);
                        buf = write_node(buf, n, fmts, col);
                        buf.push('\n');
                    }
                }
                buf.push('\n');
            }

            buf
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn seq_fmting() {
        let kserd = Kserd::with_id(
            "hello",
            Value::Seq(vec![
                Kserd::new_num(100),
                Kserd::new_num(-101),
                Kserd::new_num(3.14),
            ]),
        )
        .unwrap();

        let mut config = FormattingConfig {
            id_on_primitives: true,
            id_on_seqs: true,
            width_limit: None,
            ..Default::default()
        };

        let s = kserd.as_str_with_config(config);
        println!("{}", s);
        assert_eq!(&s, "hello [<i32> 100, <i32> -101, <f64> 3.14]");

        config.width_limit = Some(20);

        let s = kserd.as_str_with_config(config);
        println!("{}", s);
        assert_eq!(
            &s,
            "hello [
    <i32> 100
    <i32> -101
    <f64> 3.14
]"
        );

        config.width_limit = Some(0); // if seq is root, can only be concise

        let s = kserd.as_str_with_config(config);
        println!("{}", s);
        assert_eq!(
            &s,
            "hello [
    <i32> 100
    <i32> -101
    <f64> 3.14
]"
        );
    }

    #[test]
    fn empty_seq() {
        let kserd = Kserd::with_id("something", Value::Seq(vec![])).unwrap();

        let mut fmtr = Formatter::new(&kserd);

        // with id
        fmtr.id(0, true).unwrap();

        fmtr.concise(0).unwrap(); // concise
        assert_eq!(&fmtr.write_string(String::new()), "something [\n]");

        fmtr.inline(0).unwrap(); // inline
        assert_eq!(&fmtr.write_string(String::new()), "something []");

        // no id
        fmtr.id(0, false).unwrap();

        fmtr.concise(0).unwrap(); // concise
        assert_eq!(&fmtr.write_string(String::new()), "[\n]");

        fmtr.inline(0).unwrap(); // inline
        assert_eq!(&fmtr.write_string(String::new()), "[]");
    }

}
