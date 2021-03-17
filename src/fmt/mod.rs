//! Format a [`Kserd`] into a human-readable string.
//!
//! Requires the _format_ feature.
//!
//! Formatting a [`Kserd`] can be done to easily read and understand a data structure, or be used as a
//! data storage format to store serialized data. The easiest way to format a `Kserd` is to use the
//! [`.as_str()`] and [`.as_str_with_config()`] methods. These methods use default formatting, or a
//! heuristic based [`FormattingConfig`] to control the display of a data structure.
//!
//! For more fine control over a format, [`Formatter`] can be used to individually format `Kserd`
//! nodes.
//!
//! For each `Kserd` node, a format can exist which displays the _identity_, along with a line
//! representation of the [`Value`]. There are three line representations, `Inline`, `Concise`, and
//! `Verbose`. `Inline` and `Concise` representations are delimited, the `Verbose` formatting takes
//! inspiration from TOML to reduce column widths, nesting, and break apart sequences and maps.
//!
//! For example, suppose a `Kserd` is built that represents the contents of a `Cargo.toml` file.
//!
//! ```rust
//! # use kserd::*;
//! use kserd::fmt::*;
//!
//! // we will manually construct a Kserd, but you could use Serialize
//! // to do this instead.
//! let cargotoml = Kserd::with_id(
//!    "my-crate",
//!     Value::new_cntr(vec![
//!         (
//!             "package",
//!             Kserd::with_id(
//!                "Package",
//!                Value::new_cntr(vec![
//!                     ("name", Kserd::new_str("a-crate")),
//!                     ("version", Kserd::new_str("0.1.0")),
//!                 ]).unwrap(),
//!             )
//!             .unwrap(),
//!         ),
//!         (
//!             "dependencies",
//!             Kserd::new(Value::Seq(vec![
//!                 Kserd::new_cntr(vec![
//!                     ("name", Kserd::new_str("serde")),
//!                     ("version", Kserd::new_str("1")),
//!                 ]).unwrap(),
//!                 Kserd::new_cntr(vec![
//!                     ("name", Kserd::new_str("rand")),
//!                     ("version", Kserd::new_str("0.5")),
//!                 ]).unwrap(),
//!             ])),
//!         ),
//!     ]).unwrap(),
//! )
//! .unwrap();
//!
//! // let's look at the default formatting
//! let s = cargotoml.as_str();
//!
//! assert_eq!(
//!     &s,
//! r#"my-crate (
//!     dependencies = [
//!                        (name = "serde", version = "1")
//!                        (name = "rand", version = "0.5")
//!                    ]
//!     package = Package (name = "a-crate", version = "0.1.0")
//! )"#
//! );
//!
//! // we can format it much more verbosely
//! // it becomes more readable
//! let config = FormattingConfig {
//!     width_limit: Some(0),
//!     ..Default::default()
//! };
//! let s = cargotoml.as_str_with_config(config);
//!
//! assert_eq!(
//!     &s,
//! r#"
//! [[dependencies]]
//!     name = "serde"
//!     version = "1"
//!
//! [[dependencies]]
//!     name = "rand"
//!     version = "0.5"
//!
//! [package]
//!     name = "a-crate"
//!     version = "0.1.0"
//! "#
//! );
//!
//! // maybe we want the package to be inline.
//!
//! let mut fmtr = Formatter::new(&cargotoml);
//!
//! fmtr.apply_config(config); // apply the config as before
//!
//! // we get the index of the Package by filtering on id
//! fmtr.inline(
//!     fmtr.nodes()
//!         .filter(|n| n.kserd().id() == Some("Package"))
//!         .map(|n| n.index())
//!         .next()
//!         .unwrap(),
//! )
//! .unwrap();
//!
//! let s = fmtr.write_string(String::new());
//!
//! assert_eq!(
//!     &s,
//! r#"
//! [[dependencies]]
//!     name = "serde"
//!     version = "1"
//!
//! [[dependencies]]
//!     name = "rand"
//!     version = "0.5"
//! package = Package (name = "a-crate", version = "0.1.0")
//! "#
//! );
//! ```
//!
//! [`.as_str()`]: crate::Kserd::as_str
//! [`.as_str_with_config()`]: crate::Kserd::as_str_with_config
//! [`Formatter`]: Formatter
//! [`FormattingConfig`]: FormattingConfig
//! [`Kserd`]: crate::Kserd
//! [`Value`]: crate::Value
use super::*;
use nav::*;
use std::fmt::{self as stdfmt, Debug, Display};

mod cntr;
mod formatter;
mod maps;
mod prims;
mod seqs;
mod solver;
mod tuples;

pub use self::formatter::{FmtError, Formatter};

const FIELDS_ASSIGNER: &str = " =";
const FIELDS_SEPARATOR: &str = ", ";
const KEYS_ASSIGNER: &str = ":";
/// The number of spaces an indent is worth.
const INDENT: usize = 4;

/// Configuration of how a [`Kserd`] is to be formatted.
///
/// Generally the default values should be used when displaying a [`Kserd`]. If the [`Kserd`] is being
/// used for serialisation of data then the default is _not_ recommended. There are constructor
/// methods that can be used that better capture round-trip data conversions.
///
/// The likely field that would be changed is `width_limit`. Altering this value can allow for more
/// or less verbosity as to the formatter's taste.
///
/// # Example
/// ```rust
/// # use kserd::*;
/// use kserd::fmt::FormattingConfig;
///
/// let kserd = Kserd::new_cntr(vec![
///     ("a", Kserd::new_num(101)),
///     ("b", Kserd::new_num(3.14)),
/// ]).unwrap();
///
/// let fmt = kserd.as_str_with_config(FormattingConfig {
///     width_limit: None, // None means no limit, so all inline
///     ..Default::default()
/// });
/// assert_eq!(&fmt, "(a = 101, b = 3.14)");
///
/// let fmt = kserd.as_str_with_config(FormattingConfig {
///     width_limit: Some(16), // This force a concise layout
///     ..Default::default()
/// });
/// assert_eq!(&fmt, "(
///     a = 101
///     b = 3.14
/// )"
/// );
///
/// let fmt = kserd.as_str_with_config(FormattingConfig {
///     width_limit: Some(0), // Always will be verbose where applicable
///     ..Default::default()
/// });
/// assert_eq!(&fmt, "a = 101
/// b = 3.14
/// ");
/// ```
///
/// [`Kserd`]: crate::Kserd
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FormattingConfig {
    /// Display the identity on primitive values.
    /// _Default is false._
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::fmt::FormattingConfig;
    ///
    /// let kserd = Kserd::new_cntr(vec![
    ///     ("unit", Kserd::new_unit()),
    ///     ("bool", Kserd::new_bool(true)),
    ///     ("number", Kserd::new_num(1)),
    ///     ("float", Kserd::new_num(3.14)),
    ///     ("str", Kserd::new_str("Hello")),
    ///     ("barr", Kserd::new_barr(&[0, 1, 2])),
    ///     ]).unwrap();
    ///
    /// let show_prims = FormattingConfig {
    ///     id_on_primitives: true,
    ///     width_limit: Some(0),
    ///     ..Default::default()
    /// };
    ///
    /// assert_eq!(&kserd.as_str_with_config(show_prims),
    /// r#"barr = <barr> b91':CQA'
    /// bool = <bool> true
    /// float = <f64> 3.14
    /// number = <i32> 1
    /// str = <str> "Hello"
    /// unit = ()
    /// "#);
    /// ```
    pub id_on_primitives: bool,

    /// Display the identity on tuple values.
    /// _Default is true._
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::fmt::FormattingConfig;
    ///
    /// let kserd = Kserd::with_id("a-tuple", Value::Tuple(vec![])).unwrap();
    ///
    /// let show_tuples = FormattingConfig {
    ///     id_on_tuples: true,
    ///     ..Default::default()
    /// };
    ///
    /// assert_eq!(
    ///     &kserd.as_str_with_config(show_tuples),
    ///     "a-tuple ()"
    /// );
    /// ```
    pub id_on_tuples: bool,

    /// Display the identity on container values.
    /// _Default is true._
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::fmt::FormattingConfig;
    ///
    /// let kserd = Kserd::with_id("a-cntr",
    ///     Value::new_cntr(Vec::<(&str, _)>::new()).unwrap())
    ///     .unwrap();
    ///
    /// let show_containers = FormattingConfig {
    ///     id_on_containers: true,
    ///     ..Default::default()
    /// };
    ///
    /// assert_eq!(
    ///     &kserd.as_str_with_config(show_containers),
    ///     "a-cntr ()"
    /// );
    /// ```
    pub id_on_containers: bool,

    /// Display the identity on sequences.
    /// _Default is false._
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::fmt::FormattingConfig;
    ///
    /// let kserd = Kserd::with_id("a-seq", Value::Seq(vec![])).unwrap();
    ///
    /// let show_seqs = FormattingConfig {
    ///     id_on_seqs: true,
    ///     ..Default::default()
    /// };
    ///
    /// assert_eq!(
    ///     &kserd.as_str_with_config(show_seqs),
    ///     "a-seq []"
    /// );
    /// ```
    pub id_on_seqs: bool,

    /// Display the identity on maps.
    /// _Default is false._
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::fmt::FormattingConfig;
    ///
    /// let kserd = Kserd::with_id("a-map", Value::new_map(vec![])).unwrap();
    ///
    /// let show_maps = FormattingConfig {
    ///     id_on_maps: true,
    ///     ..Default::default()
    /// };
    ///
    /// assert_eq!(
    ///     &kserd.as_str_with_config(show_maps),
    ///     "a-map {  }"
    /// );
    /// ```
    pub id_on_maps: bool,

    /// The column limit in characters.
    /// _The default is `Some(60)`._
    ///
    /// When a [`FormattingConfig`] is applied to a [`Kserd`], the contents tries to use inline
    /// formatting as much as possible. If the inline format is too wide, then a concise format is
    /// used. If the concise format is still too wide, the verbose formatting is used.
    ///
    /// Setting to `None` means a limit is never reached and _all_ formatting is done inline.
    /// Setting to `Some(0)` means the formatting _always_ reaches a limit and each [`Kserd`] will
    /// be formatted at it's maximum verbosity.
    ///
    /// [`FormattingConfig`]: FormattingConfig
    /// [`Kserd`]: crate::Kserd
    pub width_limit: Option<u16>,
    // pub line_limit: Option<usize>, // TODO: Include this on stabilisation
}

impl Default for FormattingConfig {
    fn default() -> Self {
        Self {
            id_on_primitives: false,
            id_on_tuples: true,
            id_on_containers: true,
            id_on_seqs: false,
            id_on_maps: false,
            width_limit: Some(60),
            // line_limit: Some(5), // TODO: Include this on stabilisation
        }
    }
}

/// The line formatting representation of a [`Kserd`](crate::Kserd).
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Repr {
    /// The `Kserd` will be formatted on a single line.
    Inline,
    /// The `Kserd` will be formatted across multiple lines and is delimited.
    Concise,
    /// The `Kserd` is formatted through a verbose mechanism which breaks down fields, sequences,
    /// and maps.
    Verbose,
}

/// A formatting representation of a [`Kserd`](crate::Kserd).
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Fmt {
    /// Display the identity if there is one.
    pub id: bool,
    /// The line formatting representation.
    pub line: Repr,
}

/// String representation.
impl<'a> Kserd<'a> {
    /// Format the [`Kserd`](crate::Kserd) as a string using the default
    /// [`FormattingConfig`](FormattingConfig).
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let kserd = Kserd::with_id("AStruct", Value::new_cntr(vec![
    ///     ("a", Kserd::new_num(1_010_101)),
    ///     ("b", Kserd::new_num(3.14)),
    ///     ("c", Kserd::new_str("Hello, world!")),
    ///     ("d", Kserd::new_barrv(vec![100,150,200,225]))
    /// ]).unwrap()).unwrap();
    ///
    /// let s = kserd.as_str();
    /// assert_eq!(
    ///     &s,
    ///     r#"AStruct (
    ///     a = 1010101
    ///     b = 3.14
    ///     c = "Hello, world!"
    ///     d = b91'"!Mo4'
    /// )"#);
    /// ```
    pub fn as_str(&self) -> String {
        let config = FormattingConfig::default();
        self.as_str_with_config(config)
    }

    /// Format the [`Kserd`](crate::Kserd) as a string using the specified
    /// [`FormattingConfig`](FormattingConfig).
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::fmt::FormattingConfig;
    ///
    /// let kserd = Kserd::with_id("AStruct", Value::new_cntr(vec![
    ///     ("a", Kserd::new_num(1_010_101)),
    ///     ("b", Kserd::new_num(3.14)),
    ///     ("c", Kserd::new_str("Hello, world!")),
    ///     ("d", Kserd::new_barrv(vec![100,150,200,225]))
    /// ]).unwrap()).unwrap();
    ///
    /// let config = FormattingConfig {
    ///     id_on_containers: false, // don't give container name
    ///     width_limit: None, // all inline
    ///     ..Default::default()
    /// };
    ///
    /// let s = kserd.as_str_with_config(config);
    /// assert_eq!(
    ///     &s,
    ///     r#"(a = 1010101, b = 3.14, c = "Hello, world!", d = b91'"!Mo4')"#
    /// );
    /// ```
    pub fn as_str_with_config(&self, config: FormattingConfig) -> String {
        let mut formatter = Formatter::new(self);
        formatter.apply_config(config);
        formatter.write_string(String::new())
    }
}

fn write_indent(buf: &mut String, columns: usize) {
    (0..columns).for_each(|_| buf.push(' '));
}

/// Maybe writes the identity for primitive values.
/// These are always in '<Name> ' format.
fn maybe_write_prim_ident(buf: &mut String, print: bool, ident: Option<&str>) {
    if print {
        if let Some(ident) = ident {
            buf.push('<');
            buf.push_str(ident);
            buf.push_str("> ");
        }
    }
}

/// Maybe writes the identity for non-primitive values.
/// These are always in 'Name ' format.
fn maybe_write_nonprim_ident(buf: &mut String, print: bool, ident: Option<&str>) {
    if print {
        if let Some(ident) = ident {
            buf.push_str(ident);
            buf.push(' ');
        }
    }
}

fn delim_writer<P, B, S>(buf: String, prefix: P, suffix: S, body: B) -> String
where
    P: FnOnce(String) -> String,
    B: FnOnce(String) -> String,
    S: FnOnce(String) -> String,
{
    suffix(body(prefix(buf)))
}

fn write_node(buf: String, node: Node, fmts: &[Fmt], col: usize) -> String {
    match node.value() {
        NodeValue::Primitive => prims::write(buf, node.kserd(), fmts.get(node.index()).unwrap().id),
        NodeValue::Tuple(seq) => tuples::write(buf, node, fmts, col, seq),
        NodeValue::Cntr(map) => cntr::write(buf, node, fmts, col, map),
        NodeValue::Seq(seq) => seqs::write(buf, node, fmts, col, seq, None),
        NodeValue::Map(map) => maps::write(buf, node, fmts, col, map, None),
    }
}

impl<'a> Display for Kserd<'a> {
    fn fmt(&self, f: &mut stdfmt::Formatter<'_>) -> stdfmt::Result {
        let s = self.as_str_with_config(FormattingConfig::default());
        write!(f, "{}", s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mod_docs_example() {
        // we will manually construct a Kserd, but you could use Serialize
        // to do this instead.
        let cargotoml = Kserd::with_id(
            "my-crate",
            Value::new_cntr(vec![
                (
                    "package",
                    Kserd::with_id(
                        "Package",
                        Value::new_cntr(vec![
                            ("name", Kserd::new_str("a-crate")),
                            ("version", Kserd::new_str("0.1.0")),
                        ])
                        .unwrap(),
                    )
                    .unwrap(),
                ),
                (
                    "dependencies",
                    Kserd::new(Value::Seq(vec![
                        Kserd::new_cntr(vec![
                            ("name", Kserd::new_str("serde")),
                            ("version", Kserd::new_str("1")),
                        ])
                        .unwrap(),
                        Kserd::new_cntr(vec![
                            ("name", Kserd::new_str("rand")),
                            ("version", Kserd::new_str("0.5")),
                        ])
                        .unwrap(),
                    ])),
                ),
            ])
            .unwrap(),
        )
        .unwrap();

        // let's look at the default formatting
        let s = cargotoml.as_str();

        assert_eq!(
            &s,
            r#"my-crate (
    dependencies = [
                       (name = "serde", version = "1")
                       (name = "rand", version = "0.5")
                   ]
    package = Package (name = "a-crate", version = "0.1.0")
)"#
        );

        // we can format it much more verbosely
        // it becomes more readable
        let config = FormattingConfig {
            width_limit: Some(0),
            ..Default::default()
        };
        let s = cargotoml.as_str_with_config(config);

        assert_eq!(
            &s,
            r#"
[[dependencies]]
    name = "serde"
    version = "1"

[[dependencies]]
    name = "rand"
    version = "0.5"

[package]
    name = "a-crate"
    version = "0.1.0"
"#
        );

        // maybe we want the package to be inline.

        let mut fmtr = Formatter::new(&cargotoml);

        fmtr.apply_config(config); // apply the config as before

        // we get the index of the Package by filtering on id
        fmtr.inline(
            fmtr.nodes()
                .filter(|n| n.kserd().id() == Some("Package"))
                .map(|n| n.index())
                .next()
                .unwrap(),
        )
        .unwrap();

        let s = fmtr.write_string(String::new());

        assert_eq!(
            &s,
            r#"
[[dependencies]]
    name = "serde"
    version = "1"

[[dependencies]]
    name = "rand"
    version = "0.5"
package = Package (name = "a-crate", version = "0.1.0")
"#
        );
    }

    #[test]
    fn string_formatting_test() {
        let s = |s| Kserd::new_str(s).as_str();

        assert_eq!(s(""), r#""""#);
        assert_eq!(s("Hello, world!"), r#""Hello, world!""#);
        assert_eq!(
            s("Hello\nWorld"),
            r#""Hello
World""#
        );
        assert_eq!(s("Hello \"World\""), r#"str'Hello "World"'"#);
        assert_eq!(
            s("'Hello'\n\"World\""),
            r##"str#'Hello'
"World"#"##
        );
        assert_eq!(s("#Hello# \"World\""), r#"str'#Hello# "World"'"#);
    }
}
