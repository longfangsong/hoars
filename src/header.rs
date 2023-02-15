use chumsky::prelude::*;

use crate::{
    format::{AtomicProposition, LabelExpression, StateConjunction},
    value, AcceptanceCondition, AcceptanceInfo, AcceptanceName, AliasName, Id, Property, Token,
};

#[derive(Debug, PartialEq, Eq)]
pub enum HeaderItem {
    States(Id),
    Start(StateConjunction),
    AP(Vec<AtomicProposition>),
    Alias(AliasName, LabelExpression),
    Acceptance(Id, AcceptanceCondition),
    AcceptanceName(AcceptanceName, Vec<AcceptanceInfo>),
    // Correspond to tool name and optional version number
    Tool(String, Option<String>),
    Name(String),
    Properties(Vec<Property>),
}

pub fn item() -> impl Parser<Token, HeaderItem, Error = Simple<Token>> {
    let states = just(Token::Header("States".to_string()))
        .ignore_then(value::integer())
        .map(HeaderItem::States);

    let acceptance_name = just(Token::Header("acc-name".to_string()))
        .ignore_then(value::identifier())
        .try_map(|identifier, span| {
            AcceptanceName::try_from(identifier).map_err(|e| Simple::custom(span, e))
        })
        .then(value::acceptance_info().repeated())
        .map(|(name, info)| HeaderItem::AcceptanceName(name, info));

    let start = just(Token::Header("Start".to_string()))
        .ignore_then(value::integer().separated_by(just(Token::Op('&'))))
        .map(HeaderItem::Start);

    let aps = just(Token::Header("AP".to_string()))
        .ignore_then(value::integer())
        .then(value::text().repeated())
        .try_map(|(num, prop_labels), span| {
            if (num as usize) == prop_labels.len() {
                Ok(HeaderItem::AP(prop_labels))
            } else {
                Err(Simple::custom(
                    span,
                    format!("Expected {} aps but got {}", num, prop_labels.len()),
                ))
            }
        });

    let alias = just(Token::Header("Alias".to_string()))
        .ignore_then(value::alias_name())
        .then(value::label_expression())
        .map(|(aname, expression)| HeaderItem::Alias(aname, expression));

    let name = just(Token::Header("name".to_string()))
        .ignore_then(value::text())
        .map(|s| HeaderItem::Name(s.clone()));

    let tool = just(Token::Header("tool".to_string()))
        .ignore_then(value::text())
        .then(value::text().or_not())
        .map(|(tool, version)| HeaderItem::Tool(tool, version));

    let properties = just(Token::Header("properties".to_string()))
        .ignore_then(
            value::identifier()
                .try_map(|p, span| {
                    Property::try_from(p).map_err(|err| Simple::custom(span, err.clone()))
                })
                .repeated()
                .at_least(1),
        )
        .map(HeaderItem::Properties);

    states
        .or(acceptance_name)
        .or(start)
        .or(aps)
        .or(alias)
        .or(name)
        .or(tool)
        .or(properties)
}
