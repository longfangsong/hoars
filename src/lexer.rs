use chumsky::prelude::*;

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Null,
    Bool(bool),
    Int(String),
    Text(String),
    Identifier(String),
    Alias(String),
    Header(String),
    Op(char),
    Ctrl(char),
    Fin,
    Inf,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Null => write!(f, "null"),
            Token::Bool(b) => write!(f, "{}", b),
            Token::Int(n) => write!(f, "{}", n),
            Token::Text(txt) => write!(f, "{}", txt),
            Token::Identifier(id) => write!(f, "{}", id),
            Token::Alias(alias) => write!(f, "@{}", alias),
            Token::Header(hdr) => write!(f, "{}:", hdr),
            Token::Op(o) => write!(f, "{}", o),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Fin => write!(f, "Fin"),
            Token::Inf => write!(f, "Inf"),
        }
    }
}

pub fn tokenizer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let int = text::int(10).map(Token::Int);

    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Text);

    let bool = one_of("tf").try_map(|s, span| match s {
        't' => Ok(Token::Bool(true)),
        'f' => Ok(Token::Bool(false)),
        _ => unreachable!(),
    });

    let op = one_of("!|&").map(Token::Op);

    let paren = one_of(r#"(){}[]"#).map(Token::Ctrl);

    let raw_ident = filter(|c: &char| c.is_ascii_alphabetic() || *c == '_')
        .chain(filter(|c: &char| c.is_ascii_alphanumeric() || *c == '_' || *c == '-').repeated())
        .collect::<String>();

    let ident = raw_ident.map(|ident: String| match ident.as_str() {
        "Fin" => Token::Fin,
        "Inf" => Token::Inf,
        _ => Token::Identifier(ident),
    });

    let alias = just('@').ignore_then(raw_ident).map(Token::Alias);

    let header = ident
        .then_ignore(just(':'))
        .map(|header_name| Token::Header(header_name.to_string()));

    let token = int
        .or(header)
        .or(str_)
        .or(op)
        .or(paren)
        .or(ident)
        .or(bool)
        .or(alias);

    let comment = just("/*").then(take_until(just("*/"))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}
