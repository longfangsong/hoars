use crate::consumer::HoaConsumer;
use crate::expressions::*;
use crate::lexer::TokenType::*;
use crate::lexer::{HoaLexer, Token, TokenType};

use std::borrow::Cow;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Display, Error, Formatter};
use std::iter::Peekable;
use std::slice::Iter;
use std::{error, fmt};
use ParserError::*;

type Result<T, E = ParserError> = std::result::Result<T, E>;

#[derive(Debug)]
pub enum ParserError {
    ExpressionParsingError {
        expected: u8,
        found: u8,
        line: usize,
        col: usize,
    },
    MissingToken {
        expected: TokenType,
    },
    MismatchToken {
        expected: TokenType,
        actual: TokenType,
        line: usize,
        col: usize,
    },
}
pub struct HoaParser<'a, C: HoaConsumer> {
    consumer: C,
    lexer: HoaLexer,
    input: &'a [u8],
}

fn expect<'a>(expected: TokenType, possible_token: Option<&'a Token>) -> Result<&'a Token<'a>> {
    match possible_token {
        Some(actual) => {
            if expected != actual.kind {
                return Err(MismatchToken {
                    expected,
                    actual: actual.kind,
                    line: actual.line,
                    col: actual.col,
                });
            } else {
                Ok(actual)
            }
        }
        None => return Err(MissingToken { expected }),
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            ExpressionParsingError {
                expected,
                found,
                line,
                col,
            } => write!(
                f,
                "Expected {} but found {} in line {} column {}",
                expected, found, line, col
            ),
            MissingToken { expected } => write!(f, "Necessary token {:?} is missing", expected),
            MismatchToken {
                expected,
                actual,
                line,
                col,
            } => write!(
                f,
                "Syntax error, expected token {:?}, got {:?} in line {} column {}",
                expected, actual, line, col
            ),
        }
    }
}

fn matching(c: u8) -> u8 {
    match c {
        b'(' => b')',
        b')' => b'(',
        b'[' => b']',
        b']' => b'[',
        b'{' => b'}',
        b'}' => b'{',
        _ => panic!("should have been parenthesis!"),
    }
}

fn label_expr_for_alias<'a>(
    tokens: &Vec<Token<'a>>,
    pos: usize,
) -> Result<(BooleanExpression<'a>, usize), ParserError> {
    let c = tokens.get(pos)?;
    match c.kind {
        TokenInt => BooleanAtom::bint(c.int.unwrap()).into(),
        TokenTrue => BooleanAtom::btrue().into(),
        TokenFalse => BooleanAtom::bfalse().into(),
        TokenAliasName => BooleanAtom::balias(c.string.unwrap()).into(),
        TokenLparenth => label_expr_for_alias(tokens, pos + 1).and_then(|(node, next_pos)| {
            if let Some(c2) = tokens.get(next_pos) {
                if c2.kind == TokenRparenth {
                    Ok((node, next_pos + 1))
                } else {
                    Err(MissingToken {
                        expected: TokenRparenth,
                    })
                }
            } else {
                Err(MissingToken {
                    expected: TokenRparenth,
                })
            }
        }),
    }
}

impl<'a> TryFrom<&Vec<Token<'a>>> for BooleanExpression<'a> {
    type Error = ();

    fn try_from(value: &Vec<Token<'a>>) -> Result<Self, Self::Error> {
        let try_from_rec = |tokens: &Vec<Token<'a>>, pos: usize| {};
    }
}

impl<'a, C: HoaConsumer> HoaParser<'a, C> {
    fn new(consumer: C, input: &'a [u8]) -> Self {
        HoaParser {
            consumer,
            input,
            lexer: HoaLexer::try_from(input).ok().unwrap(),
        }
    }

    pub fn testt(&mut self) {
        let tokens = self.lexer.tokenize();
    }

    pub fn automaton(&mut self) -> Result<()> {
        let tokens = match self.lexer.tokenize() {
            Ok(tokens) => tokens,
            Err(e) => return Err(MissingToken { expected: TokenEof }),
        };
        let mut it = tokens.iter().peekable();

        // ensure that the HOA header is present
        let hoa = expect(TokenHoa, it.next())?;
        self.consumer.notify_header_start(hoa.string.unwrap());

        // loop other possible header items
        'header_items: loop {
            let next = it.peek();
            match next {
                None => break 'header_items,
                Some(&token) => {
                    match token.kind {
                        TokenStates => {
                            // consume label
                            expect(TokenStates, it.next())?;
                            let states_count = expect(TokenInt, it.next())?;
                            self.consumer
                                .set_number_of_states(states_count.int.unwrap());
                        }
                        TokenStart => {
                            // allocate a vec for the start states and consume token
                            let mut start_states = Vec::new();
                            expect(TokenStart, it.next())?;
                            // we need at least one state that is the start state
                            start_states.push(expect(TokenInt, it.next())?.int.unwrap());
                            // loop to see if there are more states left
                            'extract_starts: loop {
                                match it.peek() {
                                    Some(state) if state.kind == TokenInt => {
                                        // found another initial state
                                        start_states
                                            .push(expect(TokenInt, it.next())?.int.unwrap());
                                    }
                                    _ => break 'extract_starts,
                                }
                            }
                            // no need to reset peek as we are not using multipeek, notify consumer
                            self.consumer.add_start_states(start_states);
                            // todo: test this
                        }
                        TokenAp => {
                            // consume the token and extract the number of atomic propositions
                            expect(TokenAp, it.next())?;
                            let num_aps = expect(TokenInt, it.next())?.int.unwrap();
                            // ensure that there is at least one ap
                            if num_aps < 1 {
                                return Err(MissingToken { expected: TokenInt });
                            }
                            // allocate a vec and extract the exact number of aps
                            let mut aps = Vec::new();
                            for _ in 0..num_aps {
                                aps.push(String::from(
                                    expect(TokenIdent, it.next())?.string.unwrap(),
                                ));
                            }
                            self.consumer.set_aps(aps);
                            // todo: test this
                        }
                        TokenAlias => {
                            // consume token and extract alias
                            expect(TokenAlias, it.next())?;
                            // first we get the alias name
                            let alias_name =
                                String::from(expect(TokenAliasName, it.next())?.string.unwrap());
                            // now the label-expr

                            todo!("alias and name extraction");
                        }
                        TokenAcceptance => {
                            todo!("acceptance condition");
                        }
                        TokenAccname => {
                            todo!("acceptance name");
                        }
                        TokenTool => {
                            todo!("tool name extraction");
                        }
                        TokenName => {
                            todo!("item name");
                        }
                        TokenProperties => {
                            todo!("extract properties");
                        }
                        TokenHeaderName => {
                            todo!("header misc item");
                        }
                        _ => break 'header_items,
                    }
                }
            }
            // consume the item as it was indeed a known header
            // this is now done in the branches itself
            // it.next();
        }

        // now we need to encounter a body token
        expect(TokenBody, it.next())?;
        self.consumer.notify_body_start();

        // the body of the automaton

        // finally we expect an end token
        expect(TokenEnd, it.next())?;

        // todo: should we also check if we're still in the midst of announcing a state

        // we notify the receiver of the end
        self.consumer.notify_end();

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trait_test() {
        let v = vec![1, 2, 3];
        let mut it = v.iter().peekable();
        println!("{:?}", it.peek().unwrap());
        println!("{:?}", it.peek().unwrap());
    }
}
