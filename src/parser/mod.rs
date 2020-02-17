mod expression;
pub use expression::*;

mod combinators;
mod parse_alias;

use crate::consumer::HoaConsumer;
use crate::lexer::Token::*;
use crate::lexer::{HoaLexer, Token};

use std::borrow::Cow;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Display, Error, Formatter};
use std::iter::Peekable;
use std::slice::Iter;
use std::{error, fmt};
use ParserError::*;

//type Result<'a, T, E = ParserError<'a>> = std::result::Result<T, E>;

#[derive(Debug)]
pub enum ParserError<'a> {
    ExpressionParsingError {
        expected: u8,
        found: u8,
        line: usize,
        col: usize,
    },
    MissingToken {
        expected: Token<'a>,
    },
    MismatchToken {
        expected: Token<'a>,
        line: usize,
        col: usize,
    },
    UnexpectedEnd {
        message: String,
    },
}
pub struct HoaParser<'a, C: HoaConsumer> {
    consumer: C,
    lexer: HoaLexer<'a>,
    input: &'a [u8],
}

//fn expect<'a>(
//    expected: Token<'a>,
//    possible_token: Option<&Token<'a>>,
//) -> Result<'a, &'a Token<'a>> {
//    match possible_token {
//        Some(actual) => {
//            if std::mem::discriminant(&expected) == std::mem::discriminant(&actual) {
//                return Err(MismatchToken {
//                    expected,
//                    line: actual.line,
//                    col: actual.col,
//                });
//            } else {
//                Ok(actual)
//            }
//        }
//        None => return Err(MissingToken { expected }),
//    }
//}

impl<'a> fmt::Display for ParserError<'a> {
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
                line,
                col,
            } => write!(
                f,
                "Syntax error, expected token {:?} in line {} column {}",
                expected, line, col
            ),
            _ => write!(f, "unexpected end"),
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
    //                Some(&token) => {
    //                    match token.kind {
    //                        TokenStates => {
    //                            // consume label
    //                            expect(TokenStates, it.next())?;
    //                            let states_count = expect(TokenInt, it.next())?;
    //                            self.consumer
    //                                .set_number_of_states(states_count.int.unwrap());
    //                        }
    //                        TokenStart => {
    //                            // allocate a vec for the start states and consume token
    //                            let mut start_states = Vec::new();
    //                            expect(TokenStart, it.next())?;
    //                            // we need at least one state that is the start state
    //                            start_states.push(expect(TokenInt, it.next())?.int.unwrap());
    //                            // loop to see if there are more states left
    //                            'extract_starts: loop {
    //                                match it.peek() {
    //                                    Some(state) if state.kind == TokenInt => {
    //                                        // found another initial state
    //                                        start_states
    //                                            .push(expect(TokenInt, it.next())?.int.unwrap());
    //                                    }
    //                                    _ => break 'extract_starts,
    //                                }
    //                            }
    //                            // no need to reset peek as we are not using multipeek, notify consumer
    //                            self.consumer.add_start_states(start_states);
    //                            // todo: test this
    //                        }
    //                        TokenAp => {
    //                            // consume the token and extract the number of atomic propositions
    //                            expect(TokenAp, it.next())?;
    //                            let num_aps = expect(TokenInt, it.next())?.int.unwrap();
    //                            // ensure that there is at least one ap
    //                            if num_aps < 1 {
    //                                return Err(MissingToken { expected: TokenInt });
    //                            }
    //                            // allocate a vec and extract the exact number of aps
    //                            let mut aps = Vec::new();
    //                            for _ in 0..num_aps {
    //                                aps.push(String::from(
    //                                    expect(TokenIdent, it.next())?.string.unwrap(),
    //                                ));
    //                            }
    //                            self.consumer.set_aps(aps);
    //                            // todo: test this
    //                        }
    //                        TokenAlias => {
    //                            // consume token and extract alias
    //                            expect(TokenAlias, it.next())?;
    //                            // first we get the alias name
    //                            let alias_name =
    //                                String::from(expect(TokenAliasName, it.next())?.string.unwrap());
    //                            // now the label-expr
    //
    //                            todo!("alias and name extraction");
    //                        }
    //                        TokenAcceptance => {
    //                            todo!("acceptance condition");
    //                        }
    //                        TokenAccname => {
    //                            todo!("acceptance name");
    //                        }
    //                        TokenTool => {
    //                            todo!("tool name extraction");
    //                        }
    //                        TokenName => {
    //                            todo!("item name");
    //                        }
    //                        TokenProperties => {
    //                            todo!("extract properties");
    //                        }
    //                        TokenHeaderName => {
    //                            todo!("header misc item");
    //                        }
    //                        _ => break 'header_items,
    //                    }
    //                }
    //            }
    //            // consume the item as it was indeed a known header
    //            // this is now done in the branches itself
    //            // it.next();
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
