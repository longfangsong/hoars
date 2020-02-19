use crate::lexer::Token::*;
use crate::lexer::{Token, ALIAS_NAME, BOOLEAN_COMBINATORS, INTEGER};
use crate::parser::ParserError::*;
use crate::parser::{
    AcceptanceCondition, AcceptanceIdent, BooleanAtomAlias, BooleanExpressionAlias, ParserError,
};

fn parse_expr_alias_conjunct<'a>(
    tokens: &Vec<Token<'a>>,
    pos: usize,
) -> Result<(BooleanExpressionAlias<'a>, usize), ParserError> {
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
    tokens: &Vec<Token>,
    pos: usize,
) -> Result<(AcceptanceCondition, usize), ParserError> {
    let (node_atom, next_pos) = parse_expr_acceptance_term(tokens, pos)?;
    unimplemented!()
}

fn parse_expr_alias_term<'a>(
    tokens: &Vec<Token<'a>>,
    pos: usize,
) -> Result<(BooleanExpressionAlias<'a>, usize), ParserError> {
    if let Some(token) = tokens.get(pos) {
        match token {
            TokenInt(ap) => Ok((BooleanAtomAlias::bint(*ap).into(), pos + 1)),
            TokenAliasName(aname) => Ok((BooleanAtomAlias::balias(*aname).into(), pos + 1)),
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
                            if nt == &TokenRparenth {
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
    tokens: &Vec<Token>,
    pos: usize,
) -> Result<(AcceptanceCondition, usize), ParserError> {
    if let Some(token) = tokens.get(pos) {
        match token {
            TokenIdent(ident) => {
                let ident_func: fn(usize) -> AcceptanceIdent;
                match *ident {
                    "Fin" => ident_func = AcceptanceIdent::Fin,
                    "Inf" => ident_func = AcceptanceIdent::Inf,
                    _ => {
                        return Err(UnknownToken {
                            message: ident.to_string(),
                        })
                    }
                }
                // see if there is a negation in front of the set
                return if let Some(next_symbol) = tokens.get(pos + 1) {
                    match next_symbol {
                        TokenNot => match tokens.get(pos + 2) {
                            Some(TokenInt(set_identifier)) => Ok((
                                Into::<AcceptanceCondition>::into(!ident_func(*set_identifier)),
                                pos + 3,
                            )),
                            _ => Err(UnexpectedEnd {
                                message:
                                    "Negation in Fin or Inf needs to be followed by an integer"
                                        .to_string(),
                            }),
                        },
                        TokenInt(set_identifier) => {
                            Ok((ident_func(*set_identifier).into(), pos + 2))
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
            TokenLparenth => {}
            TokenTrue => {}
            TokenFalse => {}
            _ => {}
        }
    }
    Ok((AcceptanceCondition::BooleanValue(true), 0))
}

fn parse_expr_alias<'a>(
    tokens: &Vec<Token<'a>>,
    pos: usize,
) -> Result<(BooleanExpressionAlias<'a>, usize), ParserError> {
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

pub fn parse_alias_expression<'a>(
    tokens: &Vec<Token<'a>>,
) -> Result<BooleanExpressionAlias<'a>, ParserError> {
    Ok(parse_expr_alias(tokens, 0)?.0)
}

pub fn is_alias_expression_token(token: &Token) -> bool {
    BOOLEAN_COMBINATORS.to_vec().contains(&token) || vec![ALIAS_NAME, INTEGER].contains(&token)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_alias_not_binding_test() {
        let input = vec![TokenNot, TokenTrue, TokenOr, TokenFalse];
        println!("{}", parse_expr_alias(&input, 0).ok().unwrap().0);
    }

    #[test]
    fn parse_alias_atom_test() {
        let input = vec![TokenInt(7)];
        println!("{}", parse_expr_alias(&input, 0).ok().unwrap().0);
    }

    #[test]
    fn parse_alias_negation_test() {
        let input = vec![TokenNot, TokenInt(17)];
        println!("{}", parse_expr_alias(&input, 0).ok().unwrap().0);
    }

    #[test]
    fn parse_alias_conjunction_test() {
        let input = vec![
            TokenAliasName("adf"),
            TokenAnd,
            TokenInt(17),
            TokenAnd,
            TokenTrue,
        ];
        println!("{}", parse_expr_alias(&input, 0).ok().unwrap().0);
    }

    #[test]
    fn parse_alias_binding_test() {
        let input = vec![TokenTrue, TokenOr, TokenFalse, TokenAnd, TokenFalse];
        println!("{}", parse_expr_alias(&input, 0).ok().unwrap().0);
    }

    #[test]
    fn parse_alias_complete_test_with_params() {
        let input = vec![
            TokenAliasName("dkf"),
            TokenOr,
            TokenNot,
            TokenLparenth,
            TokenInt(238),
            TokenAnd,
            TokenInt(1),
            TokenRparenth,
        ];
        println!("{}", parse_expr_alias(&input, 0).ok().unwrap().0);
    }

    #[test]
    fn parse_alias_complete_test() {
        let input = vec![
            TokenAliasName("dkf"),
            TokenOr,
            TokenNot,
            TokenInt(238),
            TokenAnd,
            TokenInt(1),
        ];
        println!("{}", parse_expr_alias(&input, 0).ok().unwrap().0);
    }

    #[test]
    fn parse_alias_complete_test_should_fail() {
        let input = vec![
            TokenAliasName("dkf"),
            TokenOr,
            TokenNot,
            TokenInt(238),
            TokenAnd,
            TokenInt(1),
        ];
        println!("{}", parse_expr_alias(&input, 0).ok().unwrap().0);
    }
}
