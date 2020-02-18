mod expression;

pub use expression::*;

mod combinators;
mod parse_alias;

use crate::consumer::HoaConsumer;
use crate::lexer::Token::*;
use crate::lexer::{HoaLexer, LexerError, PositionedToken, Token, IDENTIFIER, INTEGER};

use std::borrow::Cow;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Display, Error, Formatter};
use std::iter::Peekable;
use std::slice::Iter;
use std::{error, fmt};
use ParserError::*;

//type Result<'a, T, E = ParserError<'a>> = std::result::Result<T, E>;

pub enum ParserError {
    MismatchingToken { expected: String, actual: String },
    MissingToken { expected: String },
    UnexpectedEnd { message: String },
    ExpressionParsingError { expected: String, found: String },
    LexingError { message: String },
    ZeroAtomicPropositions,
}

// #[derive(Debug)]
// pub enum ParserError<'a> {
//     LexingError {
//         le: LexerError<'a>,
//     },
//     ExpressionParsingError {
//         expected: u8,
//         found: u8,
//         line: usize,
//         col: usize,
//     },
//     MissingToken {
//         expected: Token<'a>,
//     },
//     MismatchToken {
//         expected: Token<'a>,
//         line: usize,
//         col: usize,
//     },
//     UnexpectedEnd {
//         message: String,
//     },
// }

/// The structure holding all relevant information for parsing a HOA encoded automaton.
pub struct HoaParser<'a, C: HoaConsumer> {
    /// the consumer which receives the automaton
    consumer: C,
    /// a lexer that tokenizes the input
    lexer: HoaLexer<'a>,
    /// the actual input which is passed in when the parser is constructed. It also determines
    /// the lifetime of a parser.
    input: &'a [u8],
}

fn expect<'a>(
    expected: Token<'a>,
    possible_token: Option<&'a PositionedToken<'a>>,
) -> Result<&'a PositionedToken<'a>, ParserError> {
    match possible_token {
        Some(actual) => {
            if std::mem::discriminant(&expected) == std::mem::discriminant(&actual.token) {
                return Err(MismatchingToken {
                    expected: expected.to_string(),
                    actual: actual.token.to_string(),
                });
            } else {
                Ok(actual)
            }
        }
        None => {
            return Err(MissingToken {
                expected: expected.to_string(),
            })
        }
    }
}

impl<'a> fmt::Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            MissingToken { expected } => write!(f, "Necessary token {} is missing", expected),
            MismatchingToken { expected, actual } => write!(
                f,
                "Syntax error, expected token {} but got {}",
                expected, actual
            ),
            ZeroAtomicPropositions => write!(f, "At least one atomic proposition is needed"),
            _ => write!(f, "unexpected end"),
        }
    }
}

impl<'a> From<LexerError<'a>> for ParserError {
    fn from(err: LexerError<'a>) -> Self {
        LexingError {
            message: err.to_string(),
        }
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

    fn automaton(&mut self) -> Result<(), ParserError> {
        let tokens = self.lexer.tokenize()?;
        let mut it = tokens.iter().peekable();

        // todo hoa token extraction
        let hoa = expect(TokenHoa, it.next())?;
        self.consumer.notify_header_start("");

        'header_items: loop {
            let next = it.peek();
            match next {
                None => break 'header_items,
                Some(&token) => match token.token {
                    TokenStates => {
                        // consume token
                        expect(TokenStates, it.next())?;

                        // expect next token to be integer, consume it and unwrap the contained integer
                        self.consumer.set_number_of_states(
                            expect(TokenInt(0), it.next())?.token.unwrap_int(),
                        );
                    }
                    TokenStart => {
                        // allocate a vec for the start states and consume the token
                        let mut start_states = Vec::new();
                        expect(TokenStart, it.next())?;

                        // there has to be at least one state so as above we expect an int, consume and unwrap it
                        start_states.push(expect(TokenInt(0), it.next())?.token.unwrap_int());

                        // loop through any further integer tokens to obtain all start states
                        'extract_start_states: loop {
                            match it.peek() {
                                Some(state) if state.token == INTEGER => {
                                    start_states
                                        .push(expect(INTEGER, it.next())?.token.unwrap_int());
                                }
                                _ => break 'extract_start_states,
                            }
                        }

                        self.consumer.add_start_states(start_states);
                        // todo needs testing...
                    }
                    TokenAp => {
                        expect(TokenAp, it.next())?;
                        let num_aps = expect(INTEGER, it.next())?.token.unwrap_int();
                        if num_aps < 1 {
                            return Err(ZeroAtomicPropositions);
                        }

                        // allocate space and extract atomic propositions
                        let mut aps = Vec::new();
                        for _ in 0..num_aps {
                            aps.push(String::from(
                                expect(IDENTIFIER, it.next())?.token.unwap_str(),
                            ));
                        }
                        self.consumer.set_aps(aps);
                    }
                    _ => unimplemented!(),
                },
            }
        }

        // finally return unit type as we have not encountered an error
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
