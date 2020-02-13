use crate::consumer::HoaConsumer;
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

    //    pub fn automaton(&mut self) -> Result<()> {
    //        let tokens = match self.lexer.tokenize() {
    //            Ok(tokens) => tokens,
    //            Err(e) => return Err(MissingToken { expected: TokenEof }),
    //        };
    //        let mut it = tokens.iter().peekable();
    //
    //        // ensure that the HOA header is present
    //        let hoa = expect(TokenHoa, it.next())?;
    //        self.consumer.notify_header_start(hoa.string.unwrap());
    //
    //        // loop other possible header items
    //        'header_items: loop {
    //            let next = it.peek();
    //            match next {
    //                None => break 'header_items,
    //                Some(&token) => match token {
    //                    TokenStates => self.consumer.set_number_of_states(token.int.unwrap()),
    //                    TokenStart => {
    //                        todo!("extract start state");
    //                    }
    //                    TokenAp => {
    //                        todo!("atomic propositions");
    //                    }
    //                    TokenAlias => {
    //                        todo!("alias and name extraction");
    //                    }
    //                    TokenAcceptance => {
    //                        todo!("acceptance condition");
    //                    }
    //                    TokenAccname => {
    //                        todo!("acceptance name");
    //                    }
    //                    TokenTool => {
    //                        todo!("tool name extraction");
    //                    }
    //                    TokenName => {
    //                        todo!("item name");
    //                    }
    //                    TokenProperties => {
    //                        todo!("extract properties");
    //                    }
    //                    TokenHeaderName => {
    //                        todo!("header misc item");
    //                    }
    //                    _ => break 'header_items,
    //                },
    //            }
    //            // consume the item as it was indeed a known header
    //            it.next();
    //        }
    //
    //        // now we need to encounter a body token
    //        expect(TokenBody, it.next())?;
    //        self.consumer.notify_body_start();
    //
    //        // the body of the automaton
    //
    //        // finally we expect an end token
    //        expect(TokenEnd, it.next())?;
    //
    //        // todo: should we also check if we're still in the midst of announcing a state
    //
    //        // we notify the receiver of the end
    //        self.consumer.notify_end();
    //
    //        Ok(())
    //    }
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
