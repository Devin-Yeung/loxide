use std::borrow::Cow;

pub enum Value<'src> {
    Number(f64),
    String(Cow<'src, str>),
    Boolean(bool),
    Nil,
}
