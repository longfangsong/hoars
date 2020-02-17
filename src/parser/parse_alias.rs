use crate::lexer::Token;
use crate::lexer::Token::*;
use crate::parser::ParserError::*;
use crate::parser::{BooleanAtom, BooleanExpression, ParserError};

fn match_alias_atom<'a>(
    input: &'a [Token<'a>],
) -> Result<(&'a [Token<'a>], BooleanAtom<'a>), &'a str> {
    match input.first() {
        None => Err("expected to find an alias expression atom"),
        Some(token) => match token {
            // true and false can be atoms
            TokenTrue => Ok((&input[1..], BooleanAtom::btrue())),
            TokenFalse => Ok((&input[1..], BooleanAtom::bfalse())),
            // integers represent atomic propositions
            TokenInt(ap) => Ok((&input[1..], BooleanAtom::bint(*ap))),
            // we could also find an alias name
            TokenAliasName(aname) => Ok((&input[1..], BooleanAtom::balias(*aname))),
            // those are the only possible options, so we return an err for all other patterns
            _ => Err("expected to find an atom, found something else instead"),
        },
    }
}

fn parse_expr_conjunct<'a>(
    tokens: &Vec<Token<'a>>,
    pos: usize,
) -> Result<(BooleanExpression<'a>, usize), ParserError<'a>> {
    let (node_atom, next_pos) = parse_expr_term(tokens, pos)?;
    let token = tokens.get(next_pos);
    match token {
        Some(&TokenAnd) => {
            let (rhs, i) = parse_expr(tokens, next_pos + 1)?;
            Ok((node_atom.and(rhs), i))
        }
        _ => Ok((node_atom, next_pos)),
    }
}

fn parse_expr_term<'a>(
    tokens: &Vec<Token<'a>>,
    pos: usize,
) -> Result<(BooleanExpression<'a>, usize), ParserError<'a>> {
    if let Some(token) = tokens.get(pos) {
        match token {
            TokenInt(ap) => Ok((BooleanAtom::bint(*ap).into(), pos + 1)),
            TokenAliasName(aname) => Ok((BooleanAtom::balias(*aname).into(), pos + 1)),
            TokenNot => {
                // todo darf nicht weiter parsen wenn keine Klammern
                parse_expr_term(tokens, pos + 1)
                    .and_then(|(node, next_pos)| Ok((node.not(), next_pos)))
            }
            TokenLparenth => {
                parse_expr(tokens, pos + 1).and_then(|(node, next_pos)| {
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
            TokenTrue => Ok((BooleanAtom::btrue().into(), pos + 1)),
            TokenFalse => Ok((BooleanAtom::bfalse().into(), pos + 1)),
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

fn parse_expr<'a>(
    tokens: &Vec<Token<'a>>,
    pos: usize,
) -> Result<(BooleanExpression<'a>, usize), ParserError<'a>> {
    let (node_atom, next_pos) = parse_expr_conjunct(tokens, pos)?;
    let token = tokens.get(next_pos);
    match token {
        Some(TokenOr) => {
            let (rhs, i) = parse_expr(tokens, next_pos + 1)?;
            Ok((node_atom.or(rhs), i))
        }
        _ => Ok((node_atom, next_pos)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_alias_atom_test() {
        let input = vec![TokenInt(7)];
        println!("{:#?}", parse_expr(&input, 0).unwrap().0);
    }

    #[test]
    fn parse_alias_negation_test() {
        let input = vec![TokenNot, TokenInt(17)];
        println!("{}", parse_expr(&input, 0).unwrap().0);
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
        println!("{}", parse_expr(&input, 0).unwrap().0);
    }

    #[test]
    fn parse_alias_binding_test() {
        let input = vec![TokenTrue, TokenAnd, TokenFalse, TokenOr, TokenFalse];
        println!("{}", parse_expr(&input, 0).unwrap().0);
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
        println!("{}", parse_expr(&input, 0).unwrap().0);
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
        println!("{}", parse_expr(&input, 0).unwrap().0);
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
        println!("{}", parse_expr(&input, 0).unwrap().0);
    }
}
