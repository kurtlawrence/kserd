use super::*;
use nom::error::VerboseError;
use std::collections::BTreeMap;
use std::fmt;

impl<'a> Error<'a> {
    pub(super) fn new(src: &'a str, verr: VerboseError<&'a str>) -> Self {
        Self {
            errs: verr
                .errors
                .into_iter()
                .map(|(i, e)| (offset(src, i), e))
                .collect(),
            lines: lines_to_map(src),
            empty_input: src.is_empty(),
        }
    }

    /// Get a [`Trace`] at the hierarchal error index.
    ///
    /// [`Trace`]: parse::Trace
    pub fn get(&self, index: usize) -> Option<Trace<'a>> {
        let (offset, ekind) = self.errs.get(index)?;

        let trace = if self.empty_input {
            make_empty_trace(ekind)
        } else {
            let (line_offset, line, linestr) =
                get_line(&self.lines, *offset).expect("not empty input");
            let col = offset.saturating_sub(line_offset);
            let msg = make_trace_msg(linestr, col, ekind);
            let col = col + 1; // col is one-based
            Trace {
                line,
                col,
                linestr,
                msg,
            }
        };

        Some(trace)
    }

    /// Iterate over the error hierarchy.
    pub fn iter<'i>(&'i self) -> impl Iterator<Item = Trace<'a>> + 'i {
        (0..self.len()).filter_map(move |i| self.get(i))
    }

    /// The number of errors in the error hierarchy.
    pub fn len(&self) -> usize {
        self.errs.len()
    }

    /// Combine the error into a backtrace.
    ///
    /// # Example
    /// ```rust
    /// let string = "
    /// list = [
    ///            (0,1)
    ///            (2,3]
    ///        ]
    /// ";
    ///
    /// let fail = kserd::parse::parse(string).unwrap_err();
    ///
    /// assert_eq!(
    /// &fail.backtrace(),
    /// r##"#0: at 4:16 :: expected ')', found ']'
    ///            (2,3]
    ///                ^
    ///
    /// #1: at 4:13 :: in inline tuple
    ///            (2,3]
    ///             ^
    ///
    /// #2: at 2:9 :: in newline separated (concise) kserds
    /// list = [
    ///         ^
    ///
    /// #3: at 2:9 :: in concise sequence
    /// list = [
    ///         ^
    ///
    /// #4: at 2:1 :: in kserdstr-kserd key-value pair
    /// list = [
    /// ^"##
    /// );
    /// ```
    pub fn backtrace(&self) -> String {
        use std::io::Write;
        let mut buf = Vec::<u8>::new();

        let last = self.len().saturating_sub(1);

        self.iter().enumerate().for_each(|(idx, trace)| {
            let e = "won't fail in memory";
            let mut w = || write!(&mut buf, "#{}: {}", idx, trace).expect(e);
            if idx == last {
                w();
            } else {
                w();
                writeln!(&mut buf, "\n").expect(e);
            }
        });

        String::from_utf8(buf).expect("all str so should not be invalid")
    }
}

impl<'a> fmt::Debug for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.backtrace())
    }
}

impl<'a> fmt::Display for Trace<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "at {}:{} :: {}", self.line, self.col, self.msg)?;
        writeln!(f, "{}", self.linestr)?;
        if self.col > 0 {
            for _ in 0..self.col.saturating_sub(1) {
                write!(f, " ")?;
            }
            write!(f, "^")?;
        }

        Ok(())
    }
}

fn offset(src: &str, sub: &str) -> usize {
    (sub.as_ptr() as usize) - (src.as_ptr() as usize)
}

/// Format `(line_offset, line_idx, line)`.
fn get_line<'a>(
    lines: &BTreeMap<usize, (usize, &'a str)>,
    offset: usize,
) -> Option<(usize, usize, &'a str)> {
    lines
        .range(..=offset)
        .last()
        .map(|x| (*x.0, (x.1).0, (x.1).1))
}

/// Lines indices are one-based.
fn lines_to_map(src: &str) -> BTreeMap<usize, (usize, &str)> {
    src.lines()
        .enumerate()
        .map(|(idx, line)| (offset(src, line), (idx + 1, line)))
        .collect()
}

fn make_empty_trace(ekind: &VerboseErrorKind) -> Trace<'static> {
    use VerboseErrorKind::*;
    let msg = match ekind {
        Char(c) => format!("expected '{}', got empty input", c),
        Context(s) => format!("in {}, got empty input", s),
        Nom(e) => format!("in {:?}, got empty input", e),
    };

    Trace {
        line: 0,
        col: 0,
        linestr: "",
        msg,
    }
}

fn make_trace_msg<'a>(linestr: &str, col: usize, ekind: &VerboseErrorKind) -> String {
    use VerboseErrorKind::*;
    match ekind {
        Char(c) => {
            if col >= linestr.len() {
                format!("expected '{}', reached end of line or end of file first", c)
            } else {
                format!(
                    "expected '{}', found '{}'",
                    c,
                    linestr[col..]
                        .chars()
                        .next()
                        .expect("character should exist at col")
                )
            }
        }
        Context(s) => format!("in {}", s),
        Nom(e) => format!("in {:?}", e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset() {
        let s = "Hello, world!";
        let slice = &s[5..];
        assert_eq!(offset(s, slice), 5);

        let slice = &s[..];
        assert_eq!(offset(s, slice), 0);

        let slice = &s[..5];
        assert_eq!(offset(s, slice), 0);
    }

    #[test]
    fn test_lines_to_map() {
        let s = "Hello\nA\r\nBrave\r\nWorld";
        let map = lines_to_map(s);
        let mut iter = map.iter();
        assert_eq!(iter.next(), Some((&0, &(1, "Hello"))));
        assert_eq!(iter.next(), Some((&6, &(2, "A"))));
        assert_eq!(iter.next(), Some((&9, &(3, "Brave"))));
        assert_eq!(iter.next(), Some((&16, &(4, "World"))));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_get_line() {
        // test empty
        let s = "";
        let map = lines_to_map(s);
        assert_eq!(get_line(&map, 0), None);
        assert_eq!(get_line(&map, 1), None);

        // test full
        let s = "Hello\nA\r\nBrave\r\nWorld";
        let map = lines_to_map(s);
        assert_eq!(get_line(&map, 0), Some((0, 1, "Hello")));
        assert_eq!(get_line(&map, 1), Some((0, 1, "Hello")));
        assert_eq!(get_line(&map, 2), Some((0, 1, "Hello")));
        assert_eq!(get_line(&map, 3), Some((0, 1, "Hello")));
        assert_eq!(get_line(&map, 4), Some((0, 1, "Hello")));
        assert_eq!(get_line(&map, 5), Some((0, 1, "Hello")));
        assert_eq!(get_line(&map, 6), Some((6, 2, "A")));
        assert_eq!(get_line(&map, 7), Some((6, 2, "A")));
        assert_eq!(get_line(&map, 8), Some((6, 2, "A")));
        assert_eq!(get_line(&map, 9), Some((9, 3, "Brave")));
        assert_eq!(get_line(&map, 10), Some((9, 3, "Brave")));
        assert_eq!(get_line(&map, 11), Some((9, 3, "Brave")));
        assert_eq!(get_line(&map, 12), Some((9, 3, "Brave")));
        assert_eq!(get_line(&map, 13), Some((9, 3, "Brave")));
        assert_eq!(get_line(&map, 14), Some((9, 3, "Brave")));
        assert_eq!(get_line(&map, 15), Some((9, 3, "Brave")));
        assert_eq!(get_line(&map, 16), Some((16, 4, "World")));
        assert_eq!(get_line(&map, 17), Some((16, 4, "World")));
        assert_eq!(get_line(&map, 18), Some((16, 4, "World")));
        assert_eq!(get_line(&map, 19), Some((16, 4, "World")));
        assert_eq!(get_line(&map, 20), Some((16, 4, "World")));
        assert_eq!(get_line(&map, 21), Some((16, 4, "World")));
        assert_eq!(get_line(&map, 22), Some((16, 4, "World")));
        assert_eq!(get_line(&map, 23), Some((16, 4, "World")));
        assert_eq!(get_line(&map, 24), Some((16, 4, "World")));
    }

    #[test]
    fn str_parse() {
        let s = r#"<str> "Hello, world!"#;
        let err = parse(s).unwrap_err();
        println!("{}", err.backtrace());

        assert_eq!(err.len(), 4);
        assert_eq!(
            err.get(0),
            Some(Trace {
                line: 1,
                col: 21,
                linestr: r#"<str> "Hello, world!"#,
                msg: format!(r#"expected '"', reached end of line or end of file first"#),
            })
        );
        assert_eq!(
            err.get(1),
            Some(Trace {
                line: 1,
                col: 7,
                linestr: r#"<str> "Hello, world!"#,
                msg: format!(r#"in string"#),
            })
        );
        assert_eq!(
            err.get(2),
            Some(Trace {
                line: 1,
                col: 7,
                linestr: r#"<str> "Hello, world!"#,
                msg: format!(r#"in primitive value"#),
            })
        );
        assert_eq!(
            err.get(3),
            Some(Trace {
                line: 1,
                col: 1,
                linestr: r#"<str> "Hello, world!"#,
                msg: format!(r#"in primitive"#),
            })
        );
        assert_eq!(err.get(4), None);
    }

    #[test]
    fn test_parse_err_get() {
        let s = r#"
[[a-map]]
("key, 1):
    [1,2,3,,4]
"#;
        let err = parse(s).unwrap_err();

        println!("{}", err.backtrace());

        assert_eq!(
            err.get(0),
            Some(Trace {
                line: 3,
                col: 11,
                linestr: r#"("key, 1):"#,
                msg: format!(r#"expected '"', reached end of line or end of file first"#),
            })
        );
    }

    #[test]
    fn test_empty_trace() {
        let err = parse("").unwrap_err();

        println!("{}", err.backtrace());

        for trace in err.iter() {
            assert_eq!(trace.msg.contains("got empty input"), true);
        }
    }

    #[test]
    fn debug_err_fmt() {
        let err = parse("(1,2,5]").unwrap_err();
        assert_eq!(format!("{:?}", err), err.backtrace());
    }

    #[test]
    fn make_empty_trace_test() {
        assert_eq!(
            make_empty_trace(&VerboseErrorKind::Char('x')),
            Trace {
                line: 0,
                col: 0,
                linestr: "",
                msg: r#"expected 'x', got empty input"#.into()
            }
        );
        assert_eq!(
            make_empty_trace(&VerboseErrorKind::Context("some-context")),
            Trace {
                line: 0,
                col: 0,
                linestr: "",
                msg: r#"in some-context, got empty input"#.into()
            }
        );
        assert_eq!(
            make_empty_trace(&VerboseErrorKind::Nom(nom::error::ErrorKind::Alt)),
            Trace {
                line: 0,
                col: 0,
                linestr: "",
                msg: r#"in Alt, got empty input"#.into()
            }
        );
    }

    #[test]
    fn make_trace_msg_test() {
        assert_eq!(
            &make_trace_msg("a line string", 5, &VerboseErrorKind::Char('x')),
            "expected 'x', found 'e'"
        );
        assert_eq!(
            &make_trace_msg(
                "a line string",
                5,
                &VerboseErrorKind::Context("some-context")
            ),
            "in some-context"
        );
        assert_eq!(
            &make_trace_msg(
                "a line string",
                5,
                &VerboseErrorKind::Nom(nom::error::ErrorKind::Alt)
            ),
            "in Alt"
        );
    }
}
