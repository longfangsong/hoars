use chumsky::prelude::*;

use crate::{lexer::Token, value, AcceptanceSignature, Id, LabelExpression, StateConjunction};

#[derive(Clone, Debug)]
pub struct Label(LabelExpression);

#[derive(Clone, Debug)]
pub struct RawEdge(Option<Label>, StateConjunction, Option<AcceptanceSignature>);

#[derive(Clone, Debug)]
pub struct RawState(
    Option<Label>,
    Id,
    Option<String>,
    Option<AcceptanceSignature>,
);

#[derive(Clone, Debug)]
pub struct Edge(Label, StateConjunction, AcceptanceSignature);
#[derive(Clone, Debug)]
pub struct State(Id, Option<String>, Vec<Edge>);

impl TryFrom<(Option<Label>, Option<AcceptanceSignature>, RawEdge)> for Edge {
    type Error = String;

    fn try_from(
        (raw_label, raw_acc, raw_edge): (Option<Label>, Option<AcceptanceSignature>, RawEdge),
    ) -> Result<Self, Self::Error> {
        let RawEdge(edge_label, conjunction, edge_acc) = raw_edge;
        let label = match (raw_label, edge_label) {
            (None, Some(lbl)) => Ok(lbl),
            (Some(lbl), None) => Ok(lbl),
            (None, None) => Err("State has no label, so edges should".to_string()),
            _ => Err("State and edges cannot both have labels".to_string()),
        };

        let acc = match (raw_acc, edge_acc) {
            (None, Some(acc)) => Ok(acc),
            (None, None) => Ok(Vec::new()),
            (Some(acc), None) => Ok(acc),
            (Some(_), Some(_)) => Err("State and edges cannot both have acceptance signature"),
        };

        Ok(Edge(label?, conjunction, acc?))
    }
}

impl TryFrom<(RawState, Vec<RawEdge>)> for State {
    type Error = String;

    fn try_from((state, edges): (RawState, Vec<RawEdge>)) -> Result<Self, Self::Error> {
        let mut out_edges = vec![];
        let RawState(state_label, id, state_text, state_acc) = state.clone();

        for raw_edge in edges {
            out_edges.push(Edge::try_from((
                state_label.clone(),
                state_acc.clone(),
                raw_edge,
            ))?);
        }

        Ok(State(id, state_text, out_edges))
    }
}

pub fn label() -> impl Parser<Token, Label, Error = Simple<Token>> {
    value::label_expression()
        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
        .map(Label)
}

pub fn edge() -> impl Parser<Token, RawEdge, Error = Simple<Token>> {
    label()
        .or_not()
        .then(value::state_conjunction())
        .then(value::acceptance_signature().or_not())
        .map(|((label, state_conjunction), acceptance_signature)| {
            RawEdge(label, state_conjunction, acceptance_signature)
        })
}

pub fn state() -> impl Parser<Token, State, Error = Simple<Token>> {
    just(Token::Header("State".to_string()))
        .ignore_then(
            label()
                .or_not()
                .then(value::integer())
                .then(value::text().or_not())
                .then(value::acceptance_signature().or_not())
                .map(|(((l, i), t), a)| RawState(l, i, t, a)),
        )
        .then(edge().repeated())
        .try_map(|output, span| {
            State::try_from(output)
                .map_err(|err| Simple::custom(span, format!("Problem parsing state {}", err)))
        })
}

#[cfg(test)]
mod tests {
    #[test]
    fn state_test() {}
}
