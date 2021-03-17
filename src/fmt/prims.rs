use super::*;
use std::fmt::Write;

pub fn write(mut buf: String, kserd: &Kserd, print_id: bool) -> String {
    let id = kserd.id();

    match &kserd.val {
        Value::Unit => fmt_unit(&mut buf, id),
        Value::Bool(v) => fmt_display(&mut buf, v, print_id, id),
        Value::Num(v) => fmt_display(&mut buf, v, print_id, id),
        Value::Str(v) => fmt_str(&mut buf, v, print_id, id),
        Value::Barr(bytes) => fmt_byte_array(&mut buf, bytes, print_id, id),
        _ => unreachable!("can only format primitive values"),
    }

    buf
}

fn fmt_unit(buf: &mut String, ident: Option<&str>) {
    if let Some(ident) = ident {
        if ident.is_empty() {
            buf.push_str("()");
        } else {
            write!(buf, "{}()", ident).ok();
        }
    } else {
        buf.push_str("()");
    }
}

fn fmt_str(buf: &mut String, val: &str, fmt_ident: bool, ident: Option<&str>) {
    maybe_write_prim_ident(buf, fmt_ident, ident);
    let used = Used::parse(val);
    if !used.contains(Used::DBL_QUOTE) {
        write!(buf, "\"{}\"", val)
    } else if !used.contains(Used::QUOTE) {
        write!(buf, "str'{}'", val)
    } else if !used.contains(Used::HASH) {
        write!(buf, "str#{}#", val)
    } else if !used.contains(Used::BACKTICK) {
        write!(buf, "str`{}`", val)
    } else if !used.contains(Used::EQ) {
        write!(buf, "str={}=", val)
    } else if !used.contains(Used::PIPE) {
        write!(buf, "str|{}|", val)
    } else if !used.contains(Used::STAR) {
        write!(buf, "str*{}*", val)
    } else if !used.contains(Used::HAT) {
        write!(buf, "str^{}^", val)
    } else if !used.contains(Used::BANG) {
        write!(buf, "str!{}!", val)
    } else if !used.contains(Used::PERCENT) {
        write!(buf, "str%{}%", val)
    } else if !used.contains(Used::TILDE) {
        write!(buf, "str~{}~", val)
    } else if !used.contains(Used::AMP) {
        write!(buf, "str&{}&", val)
    } else if !used.contains(Used::UNDERSCORE) {
        write!(buf, "str_{}_", val)
    } else if !used.contains(Used::BACKSLASH) {
        write!(buf, "str\\{}\\", val)
    } else if !used.contains(Used::SLASH) {
        write!(buf, "str/{}/", val)
    } else if !used.contains(Used::COLON) {
        write!(buf, "str:{}:", val)
    } else if !used.contains(Used::L_ANGLE) {
        write!(buf, "str<{}<", val)
    } else if !used.contains(Used::R_ANGLE) {
        write!(buf, "str>{}>", val)
    } else if !used.contains(Used::L_PAREN) {
        write!(buf, "str({}(", val)
    } else if !used.contains(Used::R_PAREN) {
        write!(buf, "str){})", val)
    } else {
        panic!("exhausted delimiter options for formatting a string");
    }
    .ok();
}

fn fmt_display<D: Display>(buf: &mut String, val: D, fmt_ident: bool, ident: Option<&str>) {
    maybe_write_prim_ident(buf, fmt_ident, ident);
    write!(buf, "{}", val).ok();
}

fn fmt_byte_array(buf: &mut String, val: &[u8], fmt_ident: bool, ident: Option<&str>) {
    maybe_write_prim_ident(buf, fmt_ident, ident);

    buf.push_str("b91'");
    base91::iter_encode(val.iter().copied(), |byte| buf.push(byte.into()));
    buf.push('\'');
}

::bitflags::bitflags! {
    struct Used: u32 {
        const DBL_QUOTE = 0x1;
        const QUOTE = 0x2;
        const HASH = 0x4;
        const BACKTICK = 0x8;
        const EQ = 0x10;
        const PIPE = 0x20;
        const STAR = 0x40;
        const HAT = 0x80;
        const BANG = 0x100;
        const PERCENT = 0x200;
        const TILDE = 0x400;
        const AMP = 0x800;
        const UNDERSCORE = 0x1000;
        const BACKSLASH = 0x2000;
        const SLASH = 0x4000;
        const COLON = 0x8000;
        const L_ANGLE = 0x10000;
        const R_ANGLE = 0x20000;
        const L_PAREN = 0x40000;
        const R_PAREN = 0x80000;
    }
}

impl Used {
    fn parse(s: &str) -> Self {
        let mut u = Used::empty();
        for ch in s.chars() {
            u |= Self::is(ch);
        }
        u
    }

    fn is(c: char) -> Self {
        match c {
            '"' => Self::DBL_QUOTE,
            '\'' => Self::QUOTE,
            '#' => Self::HASH,
            '`' => Self::BACKTICK,
            '=' => Self::EQ,
            '|' => Self::PIPE,
            '*' => Self::STAR,
            '^' => Self::HAT,
            '!' => Self::BANG,
            '%' => Self::PERCENT,
            '~' => Self::TILDE,
            '&' => Self::AMP,
            '_' => Self::UNDERSCORE,
            '\\' => Self::BACKSLASH,
            '/' => Self::SLASH,
            ':' => Self::COLON,
            '<' => Self::L_ANGLE,
            '>' => Self::R_ANGLE,
            '(' => Self::L_PAREN,
            ')' => Self::R_PAREN,
            _ => Self::empty(),
        }
    }
}
