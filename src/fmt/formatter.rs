use super::*;
use crate::{
    nav::{Navigator, NodeValue},
    Kserd,
};
use std::{error, fmt as stdfmt, ops::Deref};

/// A structure to format individual [`Kserd`] nodes.
///
/// The formatter implements `Deref` to [`Navigator`] so movement around the structure is possible.
/// A formatter can provide finer formatting control over a [`Kserd`] than using a
/// [`FormattingConfig`]. There are four main methods, `id(), inline(), concise(), verbose()` which
/// control the representations. A line representation has constraints based on its parent
/// representation, along with the type of [`Value`] it contains.
///
/// 1. Representations are ordered: `Inline < Concise < Verbose`
/// 2. A child cannot be formatted with a line representation greater than its parent. That is if
///    the parent is formatted as `Inline`, the child cannot be formatted as `Verbose` or
///    `Concise`.
/// 3. Tuples cannot be formatted as `Verbose`
/// 4. Primitives can _only_ be formatted as `Inline`
/// 5. A sequence or map which is at the root cannot be formatted as `Verbose`
///
/// If a request is made that fails one of these conditions, an error result is returned.
///
/// When a request is made, subsequent formatting of descendant nodes is also carried out to ensure
/// the above conditions are met. So if a sequence is formatted as concise, all descendants will be
/// formatted a `min(fmt, Concise)` where `fmt` is the current format. An important detail to note
/// is that inverse operation will _not_ result in the same [`Kserd`] formatting. To extend the
/// sequence example, if it is request to format as inline, all descendants will be formatted as
/// inline as well (as inline is the minimum representation). If a request is then made to format
/// as concise, all the descendants will remain formatted as inline.
///
/// It is best practice to format from maximum representations to minimums.
///
/// # Example
/// ```rust
/// # use kserd::*;
/// use kserd::fmt::*;
///
/// // indices can be found in various ways, the Navigator stores the Kserd
/// // as a flattened vector in depth-first order
///
/// let kserd = Kserd::new_cntr(vec![ // root, idx 0
///     ("a", Kserd::new_num(0)), // idx 1
///     ("b", Kserd::new(Value::Seq(vec![ // seq idx 2
///         Kserd::new(Value::Tuple(vec![ // tuple idx 3
///             Kserd::new_num(1), // idx 4
///             Kserd::new_num(3.14) // idx 5
///         ]))
///     ])))
/// ]).unwrap();
///
/// let mut fmtr = Formatter::new(&kserd);
///
/// // the Formatter default is at each node's maximum line representation, and the
/// // id's are all display (all true). Basically the maximal verbosity.
///
/// // check that the sequence is verbose
/// assert_eq!(fmtr.fmts()[2], Fmt { id: true, line: Repr::Verbose });
///
/// fmtr.concise(0).unwrap(); // make the root concise, this makes the seq concise as well
///
/// assert_eq!(fmtr.fmts()[0], Fmt { id: true, line: Repr::Concise });
/// assert_eq!(fmtr.fmts()[2], Fmt { id: true, line: Repr::Concise });
///
/// fmtr.verbose(0).unwrap(); // we can set the root back to verbose
///                           // but sequence will still be concise
///
/// assert_eq!(fmtr.fmts()[0], Fmt { id: true, line: Repr::Verbose });
/// assert_eq!(fmtr.fmts()[2], Fmt { id: true, line: Repr::Concise });
///
/// fmtr.inline(3).unwrap(); // make the tuple inline
///
/// let formatted = fmtr.write_string(String::new());
///
/// // check the formatted string
///
/// assert_eq!(&formatted,
/// "a = <i32> 0
/// b = [
///         (<i32> 1, <f64> 3.14)
///     ]
/// ");
/// ```
///
/// [`FormattingConfig`]: crate::fmt::FormattingConfig
/// [`Kserd`]: crate::Kserd
/// [`Navigator`]: crate::nav::Navigator
/// [`Value`]: crate::Value
pub struct Formatter<'a, 'k> {
    nav: Navigator<'a, 'k>,
    fmts: Vec<Fmt>,
}

impl<'a, 'k> Formatter<'a, 'k> {
    /// Construct a new `Formatter` from a [`Kserd`](crate::Kserd).
    pub fn new(root: &'a Kserd<'k>) -> Self {
        let nav = Navigator::new(root);

        let mut fmts = Vec::with_capacity(nav.len());

        let mut indices = Vec::with_capacity(nav.len()); // will use this later

        for node in nav.nodes() {
            indices.push(node.index());
            let mut line = max_line_repr(&node.value());

            if node.is_key() {
                line = std::cmp::min(line, Repr::Concise);
            }

            let id = true;

            fmts.push(Fmt { id, line });
        }

        let mut fmtr = Self { nav, fmts };

        // if a map or a sequence is at the root, it can be at most a concise
        // TODO consider changing this for more verbosity of arrays??
        match fmtr.nav.root().value() {
            NodeValue::Seq(_) | NodeValue::Map(_) => {
                fmtr.request_repr_alteration(0, Repr::Concise).ok()
            }
            _ => Some(()),
        };

        // The fmts are all set at the max, however, there may be knock-on effects if the parent
        // is lower than the node.
        for i in indices {
            if let Some(parent_idx) = fmtr.get(i).and_then(|x| x.parent()).map(|x| x.index()) {
                let node_line = fmtr.fmts.get(i).unwrap().line;
                let parent_line = fmtr.fmts.get(parent_idx).unwrap().line;

                if parent_line < node_line {
                    fmtr.request_repr_alteration(i, parent_line).ok();
                }
            }
        }

        fmtr
    }

    /// Apply a [`FormattingConfig`](crate::fmt::FormattingConfig) to the `Formatter`.
    ///
    /// This applies the width limitations and identity display that `FormattingConfig` captures.
    /// It _does not_ reset formatting before applying, which means already defined formats may
    /// make application fail. Failures are silently ignored and the application continues to try
    /// to alter the formatting based on the width constraints.
    pub fn apply_config(&mut self, config: FormattingConfig) -> &mut Self {
        solver::apply_config(self, config);
        self
    }

    /// The current [`Fmt`](crate::fmt::Fmt)s of each `Kserd` node.
    ///
    /// The order of formats are the same as the underlying `Navigator`.
    pub fn fmts(&self) -> &[Fmt] {
        self.fmts.as_slice()
    }

    /// Change whether to display the identity of a `Kserd`, if one exists.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::fmt::*;
    ///
    /// let kserd = Kserd::new_num(101);
    ///
    /// let mut fmtr = Formatter::new(&kserd);
    ///
    /// assert_eq!(fmtr.fmts()[0].id, true);
    ///
    /// fmtr.id(0, false).unwrap();
    ///
    /// assert_eq!(fmtr.fmts()[0].id, false);
    /// ```
    pub fn id(&mut self, node_idx: usize, write: bool) -> Result<(), FmtError> {
        self.fmts
            .get_mut(node_idx)
            .map(|fmt| fmt.id = write)
            .ok_or(FmtError::IdxOutOfBounds)
    }

    /// Request that the `Kserd` is formatted as `Inline`.
    /// Will make descendants `Inline` as well.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::fmt::*;
    ///
    /// let kserd = Kserd::new(Value::Tuple(vec![
    ///     Kserd::new_num(0)
    /// ]));
    ///
    /// let mut fmtr = Formatter::new(&kserd);
    ///
    /// assert_eq!(fmtr.fmts()[0].line, Repr::Concise);
    ///
    /// fmtr.inline(0).unwrap();
    ///
    /// assert_eq!(fmtr.fmts()[0].line, Repr::Inline);
    /// ```
    pub fn inline(&mut self, node_idx: usize) -> Result<(), FmtError> {
        self.request_repr_alteration(node_idx, Repr::Inline)
    }

    /// Request that the `Kserd` is formatted as `Concise`.
    /// Will make descendants `Concise` if they are `Verbose`.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::fmt::*;
    ///
    /// let kserd = Kserd::new_cntr(vec![
    ///     ("a", Kserd::new(Value::Seq(vec![Kserd::new_num(0)])))
    /// ]).unwrap();
    ///
    /// let mut fmtr = Formatter::new(&kserd);
    ///
    /// assert_eq!(fmtr.fmts()[0].line, Repr::Verbose);
    /// assert_eq!(fmtr.fmts()[1].line, Repr::Verbose); // seq is also verbose
    ///
    /// fmtr.concise(0).unwrap();
    ///
    /// assert_eq!(fmtr.fmts()[0].line, Repr::Concise);
    /// assert_eq!(fmtr.fmts()[1].line, Repr::Concise); // seq is now concise
    /// ```
    pub fn concise(&mut self, node_idx: usize) -> Result<(), FmtError> {
        self.request_repr_alteration(node_idx, Repr::Concise)
    }

    /// Request that the `Kserd` is formatted as `Verbose`.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::fmt::*;
    ///
    /// let kserd = Kserd::new_cntr(vec![
    ///     ("a", Kserd::new(Value::Seq(vec![Kserd::new_num(0)])))
    /// ]).unwrap();
    ///
    /// let mut fmtr = Formatter::new(&kserd);
    ///
    /// assert_eq!(fmtr.fmts()[0].line, Repr::Verbose);
    /// assert_eq!(fmtr.fmts()[1].line, Repr::Verbose); // seq is also verbose
    ///
    /// fmtr.concise(0).unwrap();
    ///
    /// assert_eq!(fmtr.fmts()[0].line, Repr::Concise);
    /// assert_eq!(fmtr.fmts()[1].line, Repr::Concise); // seq is now concise
    ///
    /// fmtr.verbose(0).unwrap();
    ///
    /// assert_eq!(fmtr.fmts()[0].line, Repr::Verbose);
    /// assert_eq!(fmtr.fmts()[1].line, Repr::Concise); // seq is still concise
    /// ```
    pub fn verbose(&mut self, node_idx: usize) -> Result<(), FmtError> {
        self.request_repr_alteration(node_idx, Repr::Verbose)
    }

    /// Write the `Kserd` as a string using the current formatting state.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::fmt::*;
    ///
    /// let kserd = Kserd::new_cntr(vec![
    ///     ("a", Kserd::new_num(3.14)),
    ///     ("b", Kserd::new(Value::Seq(vec![
    ///         Kserd::new_num(0),
    ///         Kserd::new_num(100),
    ///     ])))
    /// ]).unwrap();
    ///
    /// let mut fmtr = Formatter::new(&kserd);
    ///
    /// fmtr.concise(0).unwrap();
    /// fmtr.inline(2).unwrap();
    ///
    /// for i in 0..fmtr.len() {
    ///     fmtr.id(i, false).unwrap(); // set all ids to false
    /// }
    ///
    /// assert_eq!(
    /// fmtr.write_string(String::new()),
    /// "(
    ///     a = 3.14
    ///     b = [0, 100]
    /// )");
    /// ```
    pub fn write_string(&self, buf: String) -> String {
        write_node(buf, self.nav.root(), &self.fmts, 0)
    }

    fn line_repr_constraints(&self, node: &Node) -> (Repr, Option<Repr>) {
        let max_line_repr = max_line_repr(&node.value());
        let parent_line_repr = node
            .parent()
            .and_then(|p| self.fmts.get(p.index()))
            .map(|f| f.line);

        (max_line_repr, parent_line_repr)
    }

    pub(super) fn request_repr_alteration(
        &mut self,
        idx: usize,
        requested: Repr,
    ) -> Result<(), FmtError> {
        let node = self.nav.get(idx).ok_or(FmtError::IdxOutOfBounds)?;

        let prev_repr = self.fmts.get(idx).expect("should exist").line;
        if requested == prev_repr {
            return Ok(()); // if the requested is already what it is on, don't do anything
        }

        if idx == 0 && requested == Repr::Verbose {
            match node.value() {
                NodeValue::Seq(_) | NodeValue::Map(_) => return Err(FmtError::SeqMapAtRoot),
                _ => (),
            }
        }

        let (max, parent) = self.line_repr_constraints(&node);

        let repr = validate_requst_repr(max, parent, requested)?;

        if let Some(x) = self.fmts.get_mut(idx) {
            x.line = repr;
        }

        for d in node.descendants_depth() {
            let current = self.fmts.get_mut(d.index()).unwrap();

            if repr < current.line {
                current.line = repr;
            }
        }

        Ok(())
    }
}

impl<'a, 'k> AsRef<Navigator<'a, 'k>> for Formatter<'a, 'k> {
    fn as_ref(&self) -> &Navigator<'a, 'k> {
        &self.nav
    }
}

impl<'a, 'k> Deref for Formatter<'a, 'k> {
    type Target = Navigator<'a, 'k>;
    fn deref(&self) -> &Navigator<'a, 'k> {
        &self.nav
    }
}

/// Highest line representation a node value can take, ie Primitives are only allowed to be Inline.
fn max_line_repr(nv: &NodeValue) -> Repr {
    use NodeValue::*;
    match nv {
        Primitive => Repr::Inline, // primitives are always inline
        // Empty tuples, seq, map, cntrs should be inlined
        Tuple(x) if x.is_empty() => Repr::Inline,
        Seq(x) if x.is_empty() => Repr::Inline,
        Map(x) if x.is_empty() => Repr::Inline,
        Cntr(x) if x.is_empty() => Repr::Inline,
        Tuple(_) => Repr::Concise, // tuples can be at most concise
        _ => Repr::Verbose,        // everything else can be from verbose
    }
}

/// Validates that the request can be made.
/// Will return the `Repr` if it can, otherwise the error if not.
fn validate_requst_repr(
    max_allowed: Repr,
    parent: Option<Repr>,
    requested: Repr,
) -> Result<Repr, FmtError> {
    let parent = parent.unwrap_or(Repr::Verbose);

    if requested > max_allowed {
        // if request is higher than allowed, error
        Err(FmtError::mk_request_too_high(max_allowed, requested))
    } else if requested > parent {
        let e = match parent {
            Repr::Inline => FmtError::ParentIsInline,
            Repr::Concise => FmtError::ParentIsConcise,
            Repr::Verbose => unreachable!("requested can't be higher than Verbose"),
        };
        Err(e)
    } else {
        Ok(requested)
    }
}

/// Error variants that can occur when trying to format a [`Kserd`](crate::Kserd).
#[derive(Debug, PartialEq)]
pub enum FmtError {
    /// The index is outside the [`Navigator`](crate::nav::Navigator) range.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::fmt::{FmtError, Formatter};
    ///
    /// let kserd = Kserd::new_num(0);
    /// let mut fmtr = Formatter::new(&kserd);
    ///
    /// assert_eq!(fmtr.concise(1), Err(FmtError::IdxOutOfBounds));
    /// ```
    IdxOutOfBounds,

    /// The parent is [`Inline`](crate::fmt::Repr::Inline), and the format request is higher.
    ParentIsInline,

    /// The parent is [`Concise`](crate::fmt::Repr::Concise), and the format request is higher.
    ParentIsConcise,

    /// The requested line formatter was higher than the value variant supports.
    ///
    /// An example is primitive values which can only be formatted as `inline` so should a request
    /// to format as `concise` or `verbose` occur this error will be returned.
    ///
    /// Contains the `(maximum, request)` line representations.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::fmt::{FmtError, Formatter};
    ///
    /// let kserd = Kserd::new_num(0);
    /// let mut fmtr = Formatter::new(&kserd);
    ///
    /// assert_eq!(
    ///     fmtr.verbose(0),
    ///     Err(FmtError::RequestTooHigh("inline", "verbose"))
    /// );
    /// ```
    RequestTooHigh(&'static str, &'static str),

    /// A sequence or a map is the root `Kserd` and cannot be formatted as `verbose`.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::fmt::{FmtError, Formatter};
    ///
    /// let kserd = Kserd::new(Value::Seq(vec![]));
    /// let mut fmtr = Formatter::new(&kserd);
    ///
    /// assert_eq!(
    ///     fmtr.verbose(0),
    ///     Err(FmtError::SeqMapAtRoot),
    /// );
    /// ```
    SeqMapAtRoot,
}

impl FmtError {
    fn mk_request_too_high(max: Repr, request: Repr) -> Self {
        fn n(r: Repr) -> &'static str {
            match r {
                Repr::Inline => "inline",
                Repr::Concise => "concise",
                Repr::Verbose => "verbose",
            }
        }
        FmtError::RequestTooHigh(n(max), n(request))
    }
}

impl error::Error for FmtError {}

impl stdfmt::Display for FmtError {
    fn fmt(&self, f: &mut stdfmt::Formatter) -> stdfmt::Result {
        match self {
            FmtError::IdxOutOfBounds => write!(
                f,
                "the requested item index is outside the range of the formatter"
            ),
            FmtError::ParentIsInline => {
                write!(f, "parent is 'inline', child must be formatted as 'inline'")
            }
            FmtError::ParentIsConcise => write!(
                f,
                "parent is 'concise', child must be formatted as 'inline' or 'concise'"
            ),
            FmtError::RequestTooHigh(max, req) => write!(
                f,
                "requested format '{}' is higher than the value maximally allows '{}'",
                req, max
            ),
            FmtError::SeqMapAtRoot => write!(
                f,
                "requested to format root as verbose but the root is a sequence or a map"
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use FmtError::*;
    use Repr::*;

    #[test]
    fn test_validate_request_repr() {
        use validate_requst_repr as val;

        assert_eq!(val(Inline, None, Inline), Ok(Inline));
        assert_eq!(
            val(Inline, None, Concise),
            Err(FmtError::mk_request_too_high(Inline, Concise))
        );
        assert_eq!(val(Concise, Some(Inline), Concise), Err(ParentIsInline));
        assert_eq!(val(Verbose, Some(Concise), Verbose), Err(ParentIsConcise));
        assert_eq!(val(Verbose, Some(Verbose), Concise), Ok(Concise));
    }

    fn lines(fmts: &[Fmt]) -> Vec<Repr> {
        fmts.iter().map(|x| x.line).collect()
    }

    #[test]
    fn test_fmt_new() {
        let kserd = Kserd::new_str("Hello");
        assert_eq!(&lines(&Formatter::new(&kserd).fmts), &[Inline]);

        let kserd = Kserd::new(Value::Tuple(vec![]));
        assert_eq!(&lines(&Formatter::new(&kserd).fmts), &[Inline]);

        let kserd = Kserd::new_cntr(Vec::<(String, _)>::new()).unwrap();
        assert_eq!(&lines(&Formatter::new(&kserd).fmts), &[Inline]);

        let kserd = Kserd::new(Value::Seq(vec![]));
        assert_eq!(&lines(&Formatter::new(&kserd).fmts), &[Inline]);

        let kserd = Kserd::new_map(vec![]);
        assert_eq!(&lines(&Formatter::new(&kserd).fmts), &[Inline]);

        let kserd = Kserd::new(Value::Tuple(vec![
            Kserd::new_cntr(Vec::<(String, _)>::new()).unwrap(),
            Kserd::new(Value::Seq(vec![])),
            Kserd::new_map(vec![]),
        ]));
        assert_eq!(
            &lines(&Formatter::new(&kserd).fmts),
            &[Concise, Inline, Inline, Inline]
        );
    }

    #[test]
    fn test_inlining() {
        let kserd = Kserd::new(Value::Tuple(vec![
            Kserd::new_unit(),
            Kserd::new(Value::Seq(vec![
                Kserd::new_num(1.01),
                Kserd::new_str("Hello, world!"),
            ])),
            Kserd::new_num(10101),
        ]));

        let mut fmtr = Formatter::new(&kserd);

        assert_eq!(
            &lines(&fmtr.fmts),
            &[Concise, Inline, Concise, Inline, Inline, Inline]
        );

        fmtr.inline(0).unwrap();

        assert_eq!(
            &lines(&fmtr.fmts),
            &[Inline, Inline, Inline, Inline, Inline, Inline]
        );

        let kserd = Kserd::new_cntr(vec![
            // Verbose
            ("a", Kserd::new_num(1.01)), // Inline
            ("b", Kserd::new(Value::Seq(vec![Kserd::new_num(1)]))), // Verbose, Inline
        ])
        .unwrap();

        let mut fmtr = Formatter::new(&kserd);

        assert_eq!(&lines(&fmtr.fmts), &[Verbose, Inline, Verbose, Inline]);

        fmtr.inline(2).unwrap();

        assert_eq!(&lines(&fmtr.fmts), &[Verbose, Inline, Inline, Inline]);

        fmtr.inline(0).unwrap();

        assert_eq!(&lines(&fmtr.fmts), &[Inline, Inline, Inline, Inline]);
    }

    #[test]
    fn test_concising() {
        let kserd = Kserd::new_cntr(vec![
            // verbose
            (
                "a",
                Kserd::new(Value::Seq(vec![
                    // verbose
                    Kserd::new_num(0), // inline
                    Kserd::new_num(1), // inline
                ])),
            ),
            (
                "b",
                Kserd::new(Value::Tuple(vec![
                    // concise
                    Kserd::new_num(1),       // inline
                    Kserd::new_str("Hello"), // inline
                ])),
            ),
            (
                "c",
                Kserd::new_map(vec![
                    // verbose
                    (
                        Kserd::new(Value::Tuple(vec![
                            // concise
                            Kserd::new_unit(), // inline
                        ])),
                        Kserd::new_unit(), // inline
                    ),
                    (
                        Kserd::new(Value::Seq(vec![
                            // concise?
                            Kserd::new_unit(), // inline
                        ])),
                        Kserd::new_unit(), // inline
                    ),
                ]),
            ),
            ("d", Kserd::new_str("asdf")), // inline
        ])
        .unwrap();

        let mut fmtr = Formatter::new(&kserd);

        assert_eq!(
            &lines(&fmtr.fmts),
            &[
                Verbose, Verbose, Inline, Inline, Concise, Inline, Inline, Verbose, Concise,
                Inline, Inline, Concise, Inline, Inline, Inline
            ]
        );

        assert_eq!(fmtr.concise(1), Ok(()));

        assert_eq!(
            &lines(&fmtr.fmts),
            &[
                Verbose, Concise, Inline, Inline, Concise, Inline, Inline, Verbose, Concise,
                Inline, Inline, Concise, Inline, Inline, Inline
            ]
        );

        assert_eq!(fmtr.concise(0), Ok(()));
        assert_eq!(
            &lines(&fmtr.fmts),
            &[
                Concise, Concise, Inline, Inline, Concise, Inline, Inline, Concise, Concise,
                Inline, Inline, Concise, Inline, Inline, Inline
            ]
        );
    }

    #[test]
    fn test_verbosing() {
        let kserd = Kserd::new_cntr(vec![
            // verbose
            (
                "a",
                Kserd::new(Value::Seq(vec![
                    // verbose
                    Kserd::new_num(0), // inline
                    Kserd::new_num(1), // inline
                ])),
            ),
            (
                "b",
                Kserd::new(Value::Tuple(vec![
                    // concise
                    Kserd::new_num(1),       // inline
                    Kserd::new_str("Hello"), // inline
                ])),
            ),
            (
                "c",
                Kserd::new_map(vec![
                    // verbose
                    (
                        Kserd::new(Value::Tuple(vec![
                            // concise
                            Kserd::new_unit(), // inline
                        ])),
                        Kserd::new_unit(), // inline
                    ),
                    (
                        Kserd::new(Value::Seq(vec![
                            // concise?
                            Kserd::new_unit(), // inline
                        ])),
                        Kserd::new_unit(), // inline
                    ),
                ]),
            ),
            ("d", Kserd::new_str("asdf")), // inline
        ])
        .unwrap();

        let mut fmtr = Formatter::new(&kserd);

        assert_eq!(
            &lines(&fmtr.fmts),
            &[
                Verbose, Verbose, Inline, Inline, Concise, Inline, Inline, Verbose, Concise,
                Inline, Inline, Concise, Inline, Inline, Inline
            ]
        );

        assert_eq!(fmtr.inline(0), Ok(())); // set everything to inline

        assert_eq!(&lines(&fmtr.fmts), &[Inline; 15]);

        fmtr.concise(0).unwrap();

        assert_eq!(
            &lines(&fmtr.fmts),
            &[
                Concise, Inline, Inline, Inline, Inline, Inline, Inline, Inline, Inline, Inline,
                Inline, Inline, Inline, Inline, Inline
            ]
        );

        fmtr.concise(1).unwrap();
        assert_eq!(
            &lines(&fmtr.fmts),
            &[
                Concise, Concise, Inline, Inline, Inline, Inline, Inline, Inline, Inline, Inline,
                Inline, Inline, Inline, Inline, Inline
            ]
        );

        fmtr.verbose(0).unwrap();
        assert_eq!(
            &lines(&fmtr.fmts),
            &[
                Verbose, Concise, Inline, Inline, Inline, Inline, Inline, Inline, Inline, Inline,
                Inline, Inline, Inline, Inline, Inline
            ]
        );
    }
}
