use chumsky::{prelude::*, select};

use crate::{format::LabelExpression, AcceptanceInfo, Id, Token};

pub fn header() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    select! {
        Token::Header(hdr) => hdr.clone(),
    }
}

pub fn boolean() -> impl Parser<Token, bool, Error = Simple<Token>> + Clone {
    select! {
        Token::Bool(b) => b,
    }
}

pub fn integer() -> impl Parser<Token, Id, Error = Simple<Token>> + Clone {
    select! {
        Token::Int(n) => n.parse().unwrap(),
    }
}

pub fn text() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    select! {
        Token::Text(txt) => txt.clone(),
    }
}

pub fn identifier() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    select! { Token::Identifier(ident) => ident.clone() }
}

pub fn alias_name() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    select! { Token::Alias(aname) => aname.clone() }
}

impl LabelExpression {
    pub fn boolean(b: bool) -> Self {
        Self::Boolean(b)
    }

    pub fn alias<I: std::fmt::Display>(name: I) -> Self {
        Self::Alias(name.to_string())
    }

    pub fn not(expr: Self) -> Self {
        Self::Not(Box::new(expr))
    }

    pub fn int(i: u32) -> Self {
        Self::Integer(i)
    }
}

pub fn acceptance_info() -> impl Parser<Token, AcceptanceInfo, Error = Simple<Token>> {
    select! {
        Token::Identifier(ident) => AcceptanceInfo::Identifier(ident),
        Token::Int(n) => AcceptanceInfo::Int(n.parse().unwrap())
    }
}

pub fn label_expression() -> impl Parser<Token, LabelExpression, Error = Simple<Token>> {
    recursive(|label_expression| {
        let value = boolean()
            .map(|b| LabelExpression::Boolean(b))
            .or(integer().map(|n| LabelExpression::Integer(n)))
            .or(alias_name().map(|aname| LabelExpression::Alias(aname)));

        let atom =
            value.or(label_expression.delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))));

        let unary = just(Token::Op('!'))
            .or_not()
            .then(atom)
            .map(|(negated, expr)| {
                if negated.is_some() {
                    LabelExpression::Not(Box::new(expr))
                } else {
                    expr
                }
            });

        let conjunction = unary
            .clone()
            .separated_by(just(Token::Op('&')))
            .map(|conjuncts| {
                if conjuncts.len() > 1 {
                    LabelExpression::And(conjuncts)
                } else {
                    conjuncts.first().unwrap().clone()
                }
            });

        let disjunction = conjunction
            .clone()
            .separated_by(just(Token::Op('|')))
            .map(|disjuncts| {
                if disjuncts.len() > 1 {
                    LabelExpression::Or(disjuncts)
                } else {
                    disjuncts.first().unwrap().clone()
                }
            });

        disjunction
    })
}
