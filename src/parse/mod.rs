mod cntr;
mod err;
mod heuristics;
mod list;
mod map;
mod names;
mod prims;
mod wsp;

use super::*;
use heuristics::*;
use list::*;
use names::*;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_while, take_while1},
    character::complete::{alpha1, char, line_ending},
    combinator::{all_consuming, cut, map, map_parser, opt},
    error::VerboseErrorKind,
    error::{self, context, ErrorKind, ParseError},
    multi::{many0, separated_list},
    sequence::{preceded, separated_pair, terminated},
    Err, IResult,
};
use prims::*;
use std::collections::BTreeMap;
use std::str::FromStr;
use wsp::*;

/// A hierarchy of errors which provide a trace of where the error originates.
///
/// As `Kserd`s are nested, errors may occur inside other structures, creating a hierarchy of
/// locations where errors originate from. `ParseErr` holds this hierarchy and can provide traces
/// and a backtrace string to better understand the error.
#[derive(PartialEq)]
pub struct Error<'a> {
    /// Format `(offset, error_kind)`.
    errs: Vec<(usize, VerboseErrorKind)>,
    /// Format `(line_offset, (line_idx, line))`.
    lines: BTreeMap<usize, (usize, &'a str)>,
    empty_input: bool,
}

/// A location where an error occurred.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Trace<'a> {
    /// The line number that the trace is on. One-based, but can be zero if the input was empty.
    pub line: usize,
    /// The column number that the trace is on. One-based, but can be zero if the input was empty.
    pub col: usize,
    /// The line contents as a string.
    pub linestr: &'a str,
    /// The error message.
    pub msg: String,
}

fn kserd_ctor<'a>(ident: Option<Kstr<'a>>, value: Value<'a>) -> Kserd<'a> {
    if let Some(ident) = ident {
        Kserd::with_id(ident, value).unwrap()
    } else {
        Kserd::new(value)
    }
}

fn kserd_delimited<'a, E: ParseError<&'a str>>(
    force_inline: bool,
) -> impl Fn(&'a str) -> IResult<&'a str, Kserd<'a>, E> {
    move |i: &'a str| {
        let (i, pat) = pattern_match_delimited(i)?;

        match pat {
            Nonprim::Tuple => tuple(force_inline)(i),
            Nonprim::Cntr => cntr::delimited(force_inline)(i),
            Nonprim::Seq => seq_delimited(force_inline)(i),
            Nonprim::Map => map::delimited(force_inline)(i),
            Nonprim::None => prim(i),
        }
    }
}

fn kserd_inline<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Kserd<'a>, E> {
    kserd_delimited(true)(i)
}

fn kserd_concise<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Kserd<'a>, E> {
    kserd_delimited(false)(i)
}

fn kserd_nested<'a, E: ParseError<&'a str>>(
    indents: usize,
) -> impl Fn(&'a str) -> IResult<&'a str, Kserd<'a>, E> {
    move |i: &'a str| {
        let (i, verbose) = pattern_match_verbose(i)?;
        if verbose {
            // verbose can only be a Container type!
            cntr::verbose(indents)(i)
        } else {
            ignore_inline_whitespace(kserd_concise)(i)
        }
    }
}

fn kserd_root<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Kserd<'a>, E> {
    if i.is_empty() {
        context("empty input", |i| {
            Err(Err::Error(error::make_error(i, ErrorKind::Eof)))
        })(i)
    } else {
        all_consuming(terminated(kserd_nested(0), multiline_whitespace))(i)
    }
}

/// Turn a failure error back into a regular error.
fn uncut<'a, E: ParseError<&'a str>, O, F>(f: F) -> impl Fn(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    move |i: &'a str| {
        f(i).map_err(|e| match e {
            Err::Failure(e) => Err::Error(e),
            x => x,
        })
    }
}

/// Attemp to parse a string into a [`Kserd`] object.
///
/// Requires the _parse_ feature.
///
/// Parsing can fail, and will return a [`Error`] with trace information if it does.
///
/// # Example
///
/// Successfully parse.
///
/// ```rust
/// # use kserd::*;
/// use kserd::parse::parse;
///
/// let string = r#"
/// words = "Hello, world!"
/// pi = 3.14
/// a-list = [0, 1, 2]
/// "#;
///
/// let expected = Kserd::new_cntr(vec![
///     ("words", Kserd::new_str("Hello, world!")),
///     ("pi", Kserd::new_num(3.14)),
///     ("a-list", Kserd::new(Value::Seq(vec![
///         Kserd::new_num(0),
///         Kserd::new_num(1),
///         Kserd::new_num(2),
///     ])))
/// ]).unwrap();
///
/// assert_eq!(parse(string), Ok(expected));
/// ```
///
/// Fail to parse.
///
/// ```rust
/// # use kserd::*;
/// use kserd::parse::parse;
///
/// let string = "(wrong]";
///
/// let parse = parse(string);
///
/// assert!(parse.is_err());
///
/// // output a backtrace
/// assert_eq!(
/// parse.unwrap_err().backtrace(),
/// r##"#0: at 1:2 :: expected ')', found 'w'
/// (wrong]
///  ^
///
/// #1: at 1:2 :: in inline tuple
/// (wrong]
///  ^"##);
/// ```
///
/// [`Kserd`]: crate::Kserd
/// [`Error`]: Error
pub fn parse(s: &str) -> Result<Kserd, Error> {
    kserd_root::<nom::error::VerboseError<_>>(s)
        .map(|x| x.1)
        .map_err(|e| match e {
            Err::Error(x) | Err::Failure(x) => Error::new(s, x),
            Err::Incomplete(_) => {
                unreachable!("all parsers use complete versions so no incomplete possible")
            }
        })
}
