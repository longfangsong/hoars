use chumsky::prelude::*;

use crate::{lexer::Token, value, AcceptanceSignature, Id, LabelExpression, StateConjunction};

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Edge(Label, StateConjunction, AcceptanceSignature);

impl Edge {
    pub fn new(
        label_expression: Label,
        state_conjunction: StateConjunction,
        acceptance_signature: AcceptanceSignature,
    ) -> Self {
        Self(label_expression, state_conjunction, acceptance_signature)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct State(Id, Option<String>, Vec<Edge>);

impl State {
    pub fn new(id: Id, comment: Option<String>, edges: Vec<Edge>) -> Self {
        Self(id, comment, edges)
    }
}

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
    just(Token::Paren('['))
        .ignore_then(value::label_expression())
        .then_ignore(just(Token::Paren(']')))
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

pub fn raw_state() -> impl Parser<Token, (RawState, Vec<RawEdge>), Error = Simple<Token>> {
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
}

pub struct Body(Vec<State>);

// TODO REMOVE
pub struct RawBody(Vec<(RawState, Vec<RawEdge>)>);

impl RawBody {
    pub fn parser() -> impl Parser<Token, Self, Error = Simple<Token>> {
        just(Token::BodyStart)
            .ignore_then(raw_state().repeated())
            .map(RawBody)
            .then_ignore(just(Token::BodyEnd))
    }
}

#[cfg(test)]
mod tests {
    use chumsky::{primitive::end, Parser, Stream};

    use crate::{build_error_report, header, lexer, LabelExpression};

    use super::{Edge, Label, State};

    pub fn in_tags(input: &str) -> String {
        format!("--BODY--\n{}\n--END--", input)
    }

    #[cfg(test)]
    pub fn process_body(input: &str) -> Result<Vec<State>, ()> {
        use crate::{body::RawBody, print_error_report};

        use super::Body;

        let tokens = lexer::tokenizer().parse(input).map_err(|error_list| {
            print_error_report(
                input,
                error_list.into_iter().map(|err| err.map(|c| c.to_string())),
            )
        })?;

        for tok in &tokens {
            print!("{}", tok.0);
        }
        let len = input.chars().count();
        let ast = RawBody::parser()
            .then_ignore(end())
            .parse(Stream::from_iter(len..len + 1, tokens.into_iter()))
            .map_err(|error_list| {
                print_error_report(
                    input,
                    error_list.into_iter().map(|err| err.map(|c| c.to_string())),
                )
            })?;
        let res: Result<Vec<State>, String> = ast
            .0
            .into_iter()
            .map(|(raw_state, raw_edges)| State::try_from((raw_state, raw_edges)))
            .collect();
        Ok(res.map_err(|err| panic!("{}", err))?)
    }

    #[test]
    fn named_state() {
        let hoa = r#"State: 0 "a U b"   /* An example of named state */
        [0 & !1] 0 {0}
        [1] 1 {0}"#;
        let t0 = Edge::new(
            Label(LabelExpression::And(vec![
                LabelExpression::Integer(0),
                LabelExpression::Not(Box::new(LabelExpression::Integer(1))),
            ])),
            vec![0],
            vec![0],
        );
        let t1 = Edge::new(Label(LabelExpression::Integer(1)), vec![1], vec![0]);
        let q0 = State::new(0, Some("a U b".to_string()), vec![t0, t1]);
        assert_eq!(process_body(&in_tags(hoa)), Ok(vec![q0]));
    }

    #[test]
    fn named_state2() {
        let hoa = r#"State: 1"#;
        let t0 = Edge::new(
            Label(LabelExpression::And(vec![
                LabelExpression::Integer(0),
                LabelExpression::Not(Box::new(LabelExpression::Integer(1))),
            ])),
            vec![0],
            vec![0],
        );
        let t1 = Edge::new(Label(LabelExpression::Integer(1)), vec![1], vec![0]);
        let q0 = State::new(0, Some("a U b".to_string()), vec![t0, t1]);
        assert_eq!(process_body(&in_tags(hoa)), Ok(vec![q0]));
    }

    #[test]
    fn named_state3() {
        let hoa = r#"State: 1"#;
        let t0 = Edge::new(
            Label(LabelExpression::And(vec![
                LabelExpression::Integer(0),
                LabelExpression::Not(Box::new(LabelExpression::Integer(1))),
            ])),
            vec![0],
            vec![0],
        );
        let t1 = Edge::new(Label(LabelExpression::Integer(1)), vec![1], vec![0]);
        let q0 = State::new(0, Some("a U b".to_string()), vec![t0, t1]);
        assert_eq!(process_body(&in_tags(hoa)), Ok(vec![q0]));
    }
}
