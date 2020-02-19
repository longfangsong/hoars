mod expression;

pub use expression::*;

mod combinators;
mod expression_parser;

use crate::consumer::HoaConsumer;
use crate::lexer::Token::*;
use crate::lexer::{
    HoaLexer, LexerError, PositionedToken, Token, ALIAS_NAME, IDENTIFIER, INTEGER, STRING,
};

use crate::parser::expression_parser::{is_alias_expression_token, parse_alias_expression};
use itertools::Itertools;
use std::borrow::Cow;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Display, Error, Formatter};
use std::iter::Peekable;
use std::slice::Iter;
use std::{error, fmt};
use ParserError::*;

pub enum ParserError {
    MismatchingToken {
        expected: String,
        actual: String,
        context: String,
    },
    MissingToken {
        expected: String,
        context: String,
    },
    UnexpectedEnd {
        message: String,
    },
    ExpressionParsingError {
        expected: String,
        found: String,
    },
    LexingError {
        message: String,
    },
    UnknownToken {
        message: String,
    },
    ZeroAtomicPropositions,
}

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
    context: String,
) -> Result<&'a PositionedToken<'a>, ParserError> {
    match possible_token {
        Some(actual) => {
            // println!("expecting {:#?}, getting {:#?}", expected, actual.token);
            if expected != actual.token {
                return Err(MismatchingToken {
                    expected: expected.to_string(),
                    actual: actual.token.to_string(),
                    context,
                });
            } else {
                Ok(actual)
            }
        }
        None => {
            return Err(MissingToken {
                expected: expected.to_string(),
                context,
            })
        }
    }
}

impl<'a> fmt::Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            MissingToken { expected, context } => {
                write!(f, "Necessary token {} is missing in {}", expected, context)
            }
            MismatchingToken {
                expected,
                actual,
                context,
            } => write!(
                f,
                "Syntax error, expected token {} but got {} in {}",
                expected, actual, context
            ),
            ZeroAtomicPropositions => write!(f, "At least one atomic proposition is needed"),
            UnknownToken { message } => write!(f, "Unexpected token {}", message),
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

    fn automaton(&mut self) -> Result<(), ParserError> {
        let tokens = self.lexer.tokenize()?;
        let mut it = tokens.iter().peekable();

        // todo hoa token extraction
        let _hoa = expect(TokenHoa, it.next(), "HOA header extraction".to_string())?;
        let hoa_version = expect(IDENTIFIER, it.next(), "HOA version".to_string())?
            .token
            .unwap_str();
        self.consumer
            .notify_header_start(&String::from(hoa_version));

        'header_items: loop {
            let next = it.peek();
            match next {
                None => break 'header_items,
                Some(&token) => {
                    match token.token {
                        TokenStates => {
                            // consume token
                            expect(
                                TokenStates,
                                it.next(),
                                "state number extraction".to_string(),
                            )?;

                            // expect next token to be integer, consume it and unwrap the contained integer
                            self.consumer.set_number_of_states(
                                expect(
                                    TokenInt(0),
                                    it.next(),
                                    "state number extraction (int)".to_string(),
                                )?
                                .token
                                .unwrap_int(),
                            );
                        }
                        TokenStart => {
                            // allocate a vec for the start states and consume the token
                            let mut start_states = Vec::new();
                            expect(
                                TokenStart,
                                it.next(),
                                "initial state extraction".to_string(),
                            )?;

                            // there has to be at least one state so as above we expect an int, consume and unwrap it
                            start_states.push(
                                expect(TokenInt(0), it.next(), "first initial state".to_string())?
                                    .token
                                    .unwrap_int(),
                            );

                            // loop through any further integer tokens to obtain all start states
                            'extract_start_states: loop {
                                match it.peek() {
                                    Some(state) if state.token == INTEGER => {
                                        start_states.push(
                                            expect(
                                                INTEGER,
                                                it.next(),
                                                "subsequent initial states".to_string(),
                                            )?
                                            .token
                                            .unwrap_int(),
                                        );
                                    }
                                    _ => break 'extract_start_states,
                                }
                            }

                            self.consumer.add_start_states(start_states);
                            // todo needs testing...
                        }
                        TokenAp => {
                            expect(TokenAp, it.next(), "ap header".to_string())?;
                            let num_aps = expect(INTEGER, it.next(), "num_aps".to_string())?
                                .token
                                .unwrap_int();
                            if num_aps < 1 {
                                return Err(ZeroAtomicPropositions);
                            }

                            // allocate space and extract atomic propositions
                            let mut aps = Vec::new();
                            for _ in 0..num_aps {
                                aps.push(String::from(
                                    expect(STRING, it.next(), "ap extraction".to_string())?
                                        .token
                                        .unwap_str(),
                                ));
                            }
                            self.consumer.set_aps(aps);
                        }
                        TokenAlias => {
                            expect(TokenAlias, it.next(), "alias header".to_string())?;

                            //extract alias name and label-expr
                            let alias_name = String::from(
                                expect(ALIAS_NAME, it.next(), "alias_name".to_string())?
                                    .token
                                    .unwap_str(),
                            );

                            let alias_expr_tokens: Vec<Token> = it
                                .peeking_take_while(|token| is_alias_expression_token(&token.token))
                                .map(|token| token.token)
                                .collect();

                            let alias_expr = parse_alias_expression(&alias_expr_tokens)?;
                            self.consumer.add_alias(&alias_name, &alias_expr);
                            println!("{}", alias_expr);
                        }
                        TokenAcceptance => {
                            expect(TokenAcceptance, it.next(), "acceptance header".to_string())?;

                            let num_acceptance_sets = expect(
                                INTEGER,
                                it.next(),
                                "number of acceptance sets".to_string(),
                            )?
                            .token
                            .unwrap_int();

                            let acceptance_expr_tokens: Vec<Token> = it
                                .peeking_take_while(|token| {
                                    vec![
                                        INTEGER,
                                        TokenAnd,
                                        TokenNot,
                                        TokenOr,
                                        TokenLparenth,
                                        TokenRparenth,
                                        TokenTrue,
                                        TokenFalse,
                                    ]
                                    .contains(&token.token)
                                })
                                .map(|token| token.token)
                                .collect();
                        }
                        _ => {
                            it.next();
                        }
                    }
                }
            }
        }

        // finally return unit type as we have not encountered an error
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::consumer::NopConsumer;

    #[test]
    fn real_automaton_test() {
        let contents = b"HOA: v1\nStates: 2\nAlias: @a 0\nAlias: @ab 0 & !1\nStart: 0\nacc-name: Rabin 1\nAcceptance: 2 (Fin(0) & Inf(1))\nAP: 2 \"a\" \"b\"\n--BODY--\nState: 0 \"a U b\"   /* An example of named state */\n  [0 & !1] 0 {0}\n  [1] 1 {0}\nState: 1\n  [t] 1 {1}\n--END--\n\n";
        let mut hp = HoaParser::new(NopConsumer {}, contents as &[u8]);

        match hp.automaton() {
            Ok(_) => println!("hooray"),
            Err(err) => println!("{}", err),
        }
    }

    #[test]
    fn trait_test() {
        let v = vec![1, 2, 3];
        let mut it = v.iter().peekable();
        println!("{:?}", it.peek().unwrap());
        println!("{:?}", it.peek().unwrap());
    }
}
