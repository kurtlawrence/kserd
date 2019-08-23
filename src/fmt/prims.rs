use super::*;

pub fn write(mut buf: String, kserd: &Kserd, print_id: bool) -> String {
    let id = kserd.id();

    match &kserd.val {
        Value::Unit => fmt_unit(&mut buf, id),
        Value::Bool(v) => fmt_display(&mut buf, v, print_id, id),
        Value::Num(v) => fmt_display(&mut buf, v, print_id, id),
        Value::Str(v) => fmt_debug(&mut buf, v, print_id, id),
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
            buf.push_str(ident);
        }
    } else {
        buf.push_str("()");
    }
}

fn fmt_debug<D: Debug>(buf: &mut String, val: D, fmt_ident: bool, ident: Option<&str>) {
    maybe_write_prim_ident(buf, fmt_ident, ident);
    buf.push_str(&format!("{:?}", val));
}

fn fmt_display<D: Display>(buf: &mut String, val: D, fmt_ident: bool, ident: Option<&str>) {
    maybe_write_prim_ident(buf, fmt_ident, ident);
    buf.push_str(&format!("{}", val));
}

fn fmt_byte_array(buf: &mut String, val: &[u8], fmt_ident: bool, ident: Option<&str>) {
    maybe_write_prim_ident(buf, fmt_ident, ident);

    buf.push_str("b91'");
    base91::iter_encode(val.iter().map(|x| *x), |byte| buf.push(byte.into()));
    buf.push('\'');
}
