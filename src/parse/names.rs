use super::*;

pub(super) fn valid_name<'a, E: CxErr<'a>>(i: &'a str) -> IResult<&'a str, Kstr<'a>, E> {
    alt((alpha1, tag("_")))(i)?; // can only start with a (ASCII) letter or underscore

    context(
        "valid name",
        map(
            cut(take_while1(|c: char| {
                c.is_ascii_alphanumeric() || c == '-' || c == '_'
            })),
            Kstr::brwed,
        ),
    )(i)
}

/// Parses an identity.
///
/// Specify if angle brackets delimiting it are required (will `Fail` if they do and they don't exist).
pub(super) fn ident<'a, E: CxErr<'a>>(
    req_angles: bool,
) -> impl Fn(&'a str) -> IResult<&'a str, Kstr<'a>, E> {
    move |i: &'a str| {
        if req_angles {
            context(
                "identity",
                preceded(char('<'), cut(terminated(valid_name, char('>')))),
            )(i)
        } else {
            context("identity", valid_name)(i)
        }
    }
}

pub(super) fn field_name<'a, E: CxErr<'a>>(i: &'a str) -> IResult<&'a str, Kstr<'a>, E> {
    map(
        take_while(|c: char| c.is_ascii_alphanumeric() || c == '-' || c == '_'),
        Kstr::brwed,
    )(i)
}
