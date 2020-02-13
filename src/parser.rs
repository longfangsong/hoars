use crate::consumer::HoaConsumer;
use crate::lexer::TokenType::*;
use crate::lexer::{HoaLexer, Token, TokenType};
use std::convert::{TryFrom, TryInto};
use std::fmt::{Error, Formatter};
use std::iter::Peekable;
use std::slice::Iter;
use ParserError::*;

type Result<T, E = ParserError> = std::result::Result<T, E>;

#[derive(Debug)]
pub enum ParserError {
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
    tokens: Vec<Token<'a>>,
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

impl<'a, C: HoaConsumer> HoaParser<'a, C> {
    fn new(consumer: C, hl: &'a mut HoaLexer) -> Result<Self, &'static str> {
        Ok(HoaParser {
            consumer,
            tokens: hl.tokenize()?,
        })
    }

    pub fn automaton(&mut self) -> Result<()> {
        let mut it = self.tokens.iter().peekable();
        // ensure that the HOA header is present
        let hoa = expect(TokenHoa, it.next())?;
        self.consumer.notify_header_start(hoa.string.unwrap());

        // loop other possible header items

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trait_test() {}
}
