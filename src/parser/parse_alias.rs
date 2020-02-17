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
            TokenTrue(_, _) => Ok((&input[1..], BooleanAtom::btrue())),
            TokenFalse(_, _) => Ok((&input[1..], BooleanAtom::bfalse())),
            // integers represent atomic propositions
            TokenInt(_, _, ap) => Ok((&input[1..], BooleanAtom::bint(*ap))),
            // we could also find an alias name
            TokenAliasName(_, _, aname) => Ok((&input[1..], BooleanAtom::balias(*aname))),
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
        Some(&TokenAnd(_, _)) => {
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
            TokenInt(_, _, ap) => Ok((BooleanAtom::bint(*ap).into(), pos + 1)),
            TokenAliasName(_, _, aname) => Ok((BooleanAtom::balias(*aname).into(), pos + 1)),
            TokenNot(_, _) => {
                // todo darf nicht weiter parsen wenn keine Klammern
                parse_expr_term(tokens, pos + 1)
                    .and_then(|(node, next_pos)| Ok((node.not(), next_pos)))
            }
            TokenLparenth(_, _) => {
                parse_expr(tokens, pos + 1).and_then(|(node, next_pos)| {
                    let next_token = tokens.get(next_pos);
                    match next_token {
                        None => Err(UnexpectedEnd {
                            message: "Expected closing paren".to_string(),
                        }),
                        Some(nt) => {
                            if std::mem::discriminant(nt)
                                == std::mem::discriminant(&TokenRparenth(0, 0))
                            {
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
            TokenTrue(_, _) => Ok((BooleanAtom::btrue().into(), pos + 1)),
            TokenFalse(_, _) => Ok((BooleanAtom::bfalse().into(), pos + 1)),
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
        Some(&TokenOr(_, _)) => {
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
        let input = vec![TokenInt(0, 0, 7)];
        println!("{:#?}", parse_expr(&input, 0).unwrap().0);
    }

    #[test]
    fn parse_alias_negation_test() {
        let input = vec![TokenNot(0, 0), TokenInt(0, 0, 17)];
        println!("{}", parse_expr(&input, 0).unwrap().0);
    }

    #[test]
    fn parse_alias_conjunction_test() {
        let input = vec![
            TokenAliasName(0, 0, "adf"),
            TokenAnd(0, 0),
            TokenInt(0, 0, 17),
            TokenAnd(0, 0),
            TokenTrue(0, 0),
        ];
        println!("{}", parse_expr(&input, 0).unwrap().0);
    }

    #[test]
    fn parse_alias_binding_test() {
        let input = vec![
            TokenTrue(0, 0),
            TokenAnd(0, 0),
            TokenFalse(0, 0),
            TokenOr(0, 0),
            TokenFalse(0, 0),
        ];
        println!("{}", parse_expr(&input, 0).unwrap().0);
    }

    #[test]
    fn parse_alias_complete_test_with_params() {
        let input = vec![
            TokenAliasName(0, 0, "dkf"),
            TokenOr(0, 0),
            TokenNot(0, 0),
            TokenLparenth(0, 0),
            TokenInt(0, 0, 238),
            TokenAnd(0, 0),
            TokenInt(0, 0, 1),
            TokenRparenth(0, 0),
        ];
        println!("{}", parse_expr(&input, 0).unwrap().0);
    }

    #[test]
    fn parse_alias_complete_test() {
        let input = vec![
            TokenAliasName(0, 0, "dkf"),
            TokenOr(0, 0),
            TokenNot(0, 0),
            TokenInt(0, 0, 238),
            TokenAnd(0, 0),
            TokenInt(0, 0, 1),
        ];
        println!("{}", parse_expr(&input, 0).unwrap().0);
    }

    #[test]
    fn parse_alias_complete_test_should_fail() {
        let input = vec![
            TokenAliasName(0, 0, "dkf"),
            TokenOr(0, 0),
            TokenNot(0, 0),
            TokenInt(0, 0, 238),
            TokenAnd(0, 0),
            TokenInt(0, 0, 1),
        ];
        println!("{}", parse_expr(&input, 0).unwrap().0);
    }
}
