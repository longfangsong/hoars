use crate::lexer::Token::*;
use crate::lexer::{header_name_token, Token};
use crate::parser::ParserError::*;
use crate::parser::{
    AcceptanceCondition, AcceptanceIdent, BooleanAtomAlias, BooleanExpressionAlias, ParserError,
};

fn parse_expr_alias_conjunct(
    tokens: &Vec<&Token>,
    pos: usize,
) -> Result<(BooleanExpressionAlias, usize), ParserError> {
    let (node_atom, next_pos) = parse_expr_alias_term(tokens, pos)?;
    let token = tokens.get(next_pos);
    match token {
        Some(&TokenAnd) => {
            let (rhs, i) = parse_expr_alias(tokens, next_pos + 1)?;
            Ok((node_atom * rhs, i))
        }
        _ => Ok((node_atom, next_pos)),
    }
}

fn parse_expr_acceptance_conjunct(
    tokens: &Vec<&Token>,
    pos: usize,
) -> Result<(AcceptanceCondition, usize), ParserError> {
    let (node_atom, next_pos) = parse_expr_acceptance_term(tokens, pos)?;
    let token = tokens.get(next_pos);
    match token {
        Some(&TokenAnd) => {
            let (rhs, i) = parse_expr_acceptance(tokens, next_pos + 1)?;
            let res = node_atom * rhs;
            Ok((res, i))
        }
        _ => Ok((node_atom, next_pos)),
    }
}

fn parse_expr_alias_term(
    tokens: &Vec<&Token>,
    pos: usize,
) -> Result<(BooleanExpressionAlias, usize), ParserError> {
    if let Some(token) = tokens.get(pos) {
        match token {
            TokenInt(ap) => Ok((BooleanAtomAlias::bint(*ap).into(), pos + 1)),
            TokenAliasName(aname) => Ok((BooleanAtomAlias::balias(aname.clone()).into(), pos + 1)),
            TokenNot => {
                // todo darf nicht weiter parsen wenn keine Klammern
                parse_expr_alias_term(tokens, pos + 1)
                    .and_then(|(node, next_pos)| Ok((node.not(), next_pos)))
            }
            TokenLparenth => {
                parse_expr_alias(tokens, pos + 1).and_then(|(node, next_pos)| {
                    let next_token = tokens.get(next_pos);
                    match next_token {
                        None => Err(UnexpectedEnd {
                            message: "Expected closing paren".to_string(),
                        }),
                        Some(nt) => {
                            if *nt == &TokenRparenth {
                                // we have a matching bracket
                                Ok((node, next_pos + 1))
                            } else {
                                Err(UnexpectedEnd {
                                    message: "expected closing paren".to_string(),
                                })
                            }
                        }
                    }
                })
            }
            TokenTrue => Ok((BooleanAtomAlias::btrue().into(), pos + 1)),
            TokenFalse => Ok((BooleanAtomAlias::bfalse().into(), pos + 1)),
            _ => Err(UnexpectedEnd {
                message: "expected atom, not what we got".to_string(),
            }),
        }
    } else {
        Err(UnexpectedEnd {
            message: String::from("Unexpected end of input, expected parenteses or identifier"),
        })
    }
}

fn parse_expr_acceptance_term(
    tokens: &Vec<&Token>,
    pos: usize,
) -> Result<(AcceptanceCondition, usize), ParserError> {
    return if let Some(token) = tokens.get(pos) {
        match token {
            TokenIdent(ident) => {
                let ident_func: fn(usize) -> AcceptanceIdent;
                match ident.as_str() {
                    "Fin" => ident_func = AcceptanceIdent::Fin,
                    "Inf" => ident_func = AcceptanceIdent::Inf,
                    _ => {
                        return Err(UnknownToken {
                            message: ident.to_string(),
                        })
                    }
                }
                // we need to have an opening bracket now
                match tokens.get(pos + 1) {
                    Some(&TokenLparenth) => {}
                    Some(t) => {
                        return Err(MismatchingToken {
                            expected: "opening paren".to_string(),
                            actual: t.to_string(),
                            context: "acceptance condition parsing".to_string(),
                        })
                    }
                    _ => {
                        return Err(UnexpectedEnd {
                            message: "expected acceptance set".to_string(),
                        })
                    }
                }

                // see if there is a negation in front of the set
                return if let Some(next_symbol) = tokens.get(pos + 2) {
                    match next_symbol {
                        TokenNot => match tokens.get(pos + 3) {
                            Some(TokenInt(set_identifier)) => Ok((
                                Into::<AcceptanceCondition>::into(!ident_func(*set_identifier)),
                                pos + 5,
                            )),
                            _ => Err(UnexpectedEnd {
                                message:
                                    "Negation in Fin or Inf needs to be followed by an integer"
                                        .to_string(),
                            }),
                        },
                        TokenInt(set_identifier) => {
                            Ok((ident_func(*set_identifier).into(), pos + 4))
                        }
                        _ => Err(UnexpectedEnd {
                            message: "Inf or Fin need to be followed by Negation symbol or INTEGER"
                                .to_string(),
                        }),
                    }
                } else {
                    Err(UnexpectedEnd {
                        message: "Fin or Inf need to be followed by ! INTEGER or just INTEGER"
                            .to_string(),
                    })
                };
            }
            TokenLparenth => parse_expr_acceptance(tokens, pos + 1).and_then(|(node, next_pos)| {
                let next_token = tokens.get(next_pos);
                match next_token {
                    Some(&TokenRparenth) => Ok((node, next_pos + 1)),
                    _ => Err(UnexpectedEnd {
                        message: "Expected closing param".to_string(),
                    }),
                }
            }),
            TokenTrue => Ok((AcceptanceCondition::BooleanValue(true), pos + 1)),
            TokenFalse => Ok((AcceptanceCondition::BooleanValue(false), pos + 1)),
            _ => Err(UnexpectedEnd {
                message: "Expected atom, not whatever we got".to_string(),
            }),
        }
    } else {
        Err(MissingToken {
            expected: "Fin or Neg expression".to_string(),
            context: "parse_expr_acceptance_term".to_string(),
        })
    };
}

fn parse_expr_alias(
    tokens: &Vec<&Token>,
    pos: usize,
) -> Result<(BooleanExpressionAlias, usize), ParserError> {
    let (node_atom, next_pos) = parse_expr_alias_conjunct(tokens, pos)?;
    let token = tokens.get(next_pos);
    match token {
        Some(TokenOr) => {
            let (rhs, i) = parse_expr_alias(tokens, next_pos + 1)?;
            Ok((node_atom + rhs, i))
        }
        _ => Ok((node_atom, next_pos)),
    }
}

fn parse_expr_acceptance(
    tokens: &Vec<&Token>,
    pos: usize,
) -> Result<(AcceptanceCondition, usize), ParserError> {
    match tokens.get(pos) {
        Some(TokenLparenth) => {
            let res = parse_expr_acceptance(tokens, pos + 1);
            return if let Some(TokenRparenth) = tokens.get(tokens.len() - 1) {
                res
            } else {
                Err(MissingToken {
                    expected: "Closing parentheses".to_string(),
                    context: "acceptance extraction".to_string(),
                })
            };
        }
        _ => {
            let (node_atom, next_pos) = parse_expr_acceptance_conjunct(tokens, pos)?;
            let token = tokens.get(next_pos);
            match token {
                Some(TokenOr) => {
                    let (rhs, i) = parse_expr_acceptance(tokens, next_pos + 1)?;
                    Ok((node_atom + rhs, i))
                }
                _ => Ok((node_atom, next_pos)),
            }
        }
    }
}

pub fn parse_alias_expression(tokens: &Vec<&Token>) -> Result<BooleanExpressionAlias, ParserError> {
    Ok(parse_expr_alias(tokens, 0)?.0)
}

pub fn parse_acceptance_expression(
    tokens: &Vec<&Token>,
) -> Result<AcceptanceCondition, ParserError> {
    Ok(parse_expr_acceptance(tokens, 0)?.0)
}

pub fn parse_state_conjunction(tokens: &Vec<&Token>) -> Result<Vec<usize>, ParserError> {
    let mut conj_states = Vec::new();
    let mut it = tokens.iter();
    if let Some(TokenInt(first_state)) = it.next() {
        conj_states.push(*first_state);
    } else {
        return Err(MissingToken {
            expected: "integer".to_string(),
            context: "state conjunction".to_string(),
        });
    }

    loop {
        match it.next() {
            None => break,
            Some(TokenAnd) => match it.next() {
                Some(TokenInt(next_state)) => conj_states.push(*next_state),
                None => {
                    return Err(UnexpectedEnd {
                        message: "in state conjunction a & has to \
                    be followed by an integer"
                            .to_string(),
                    })
                }
                Some(token) => {
                    return Err(MismatchingToken {
                        expected: "integer".to_string(),
                        actual: token.to_string(),
                        context: "state conjunction".to_string(),
                    })
                }
            },
            Some(token) => {
                return Err(MismatchingToken {
                    expected: "&".to_string(),
                    actual: token.to_string(),
                    context: "state conjunction conjunct".to_string(),
                })
            }
        }
    }

    Ok(conj_states)
}

pub fn is_header_token(token: &Token) -> bool {
    vec![
        TokenEof,
        TokenBody,
        TokenEnd,
        TokenAbort,
        TokenHoa,
        TokenState,
        TokenStates,
        TokenStart,
        TokenAp,
        TokenAlias,
        TokenAcceptance,
        TokenAccname,
        TokenTool,
        TokenName,
        TokenProperties,
        header_name_token(),
    ]
    .contains(token)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::AcceptanceIdent::{Fin, InfNeg};

    #[test]
    fn state_conj() {
        let input = vec![
            &TokenInt(2),
            &TokenAnd,
            &TokenInt(0),
            &TokenAnd,
            &TokenInt(1),
        ];
        assert_eq!(
            parse_state_conjunction(&input).expect("could not parse state conjunction"),
            vec![2, 0, 1]
        )
    }

    #[test]
    fn parse_acceptance() {
        let fintoken = TokenIdent("Fin".to_string());
        let inftoken = TokenIdent("Inf".to_string());
        let input = vec![
            &fintoken,
            &TokenLparenth,
            &TokenInt(0),
            &TokenRparenth,
            &TokenAnd,
            &inftoken,
            &TokenLparenth,
            &TokenNot,
            &TokenInt(0),
            &TokenRparenth,
        ];

        assert_eq!(
            parse_acceptance_expression(&input).expect("could not parse input"),
            AcceptanceCondition::Atom(Fin(0)) * AcceptanceCondition::Atom(InfNeg(0))
        );
    }

    #[test]
    fn parse_alias_not_binding_test() {
        let input = vec![&TokenNot, &TokenTrue, &TokenOr, &TokenFalse];
        assert_eq!(
            parse_alias_expression(&input).expect("could not parse input"),
            !BooleanAtomAlias::btrue() + BooleanAtomAlias::bfalse()
        )
    }

    #[test]
    fn parse_alias_binding_test() {
        let input = vec![&TokenTrue, &TokenOr, &TokenFalse, &TokenAnd, &TokenFalse];
        assert_eq!(
            parse_alias_expression(&input).expect("could not parse input"),
            BooleanAtomAlias::btrue() + (BooleanAtomAlias::bfalse() * BooleanAtomAlias::bfalse())
        )
    }

    #[test]
    fn parse_alias_complete_test_with_params() {
        let aliastoken = TokenAliasName("dkf".into());
        let input = vec![
            &aliastoken,
            &TokenOr,
            &TokenNot,
            &TokenLparenth,
            &TokenInt(238),
            &TokenAnd,
            &TokenInt(1),
            &TokenRparenth,
        ];
        println!("{}", parse_expr_alias(&input, 0).ok().unwrap().0);
    }

    #[test]
    fn parse_alias_complete_test() {
        let aliastoken = TokenAliasName("dkf".into());
        let input = vec![
            &aliastoken,
            &TokenOr,
            &TokenNot,
            &TokenInt(238),
            &TokenAnd,
            &TokenInt(1),
        ];
        println!("{}", parse_expr_alias(&input, 0).ok().unwrap().0);
    }

    #[test]
    fn parse_alias_complete_test_should_fail() {
        let aliastoken = TokenAliasName("dkf".into());
        let input = vec![
            &aliastoken,
            &TokenOr,
            &TokenNot,
            &TokenInt(238),
            &TokenAnd,
            &TokenInt(1),
        ];
        println!("{}", parse_expr_alias(&input, 0).ok().unwrap().0);
    }
}
