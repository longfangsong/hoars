mod token;
pub use token::PositionedToken;
use token::Token::*;
pub use token::*;

extern crate itertools;

use super::utils::*;

use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::iter::{once, FromIterator, Peekable};
use std::str::{from_utf8, Utf8Error};
use std::string::String;
use std::{env, fmt};

use self::itertools::{multipeek, MultiPeek};
use itertools::Itertools;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt::Formatter;
use std::num::ParseIntError;

use crate::lexer::LexerError::*;

#[derive(Debug)]
pub enum LexerError<'a> {
    ParseInputError {},
    IntParseError {
        line: usize,
        col: usize,
    },
    MissingSymbol {
        expected: u8,
    },
    UnrecognizedToken {
        expected: Option<Token<'a>>,
        last: u8,
    },
    PrematureEnd {
        line: usize,
        col: usize,
    },
}

#[derive(Clone, Debug)]
pub struct HoaLexer<'a> {
    line: usize,
    col: usize,
    curr: char,
    known_headers: HashMap<&'static str, Token<'a>>,
    input: String,
    lines: Vec<String>,
    is_eof: bool,
}

impl fmt::Display for LexerError<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            ParseInputError {} => write!(f, "could not parse input data"),
            IntParseError { line, col } => write!(
                f,
                "could not parse integer in line {}, column {}",
                line, col
            ),
            MissingSymbol { expected } => {
                write!(f, "unexpected/missing symbol, expected {}", expected)
            }
            UnrecognizedToken { expected, last } => match expected {
                Some(token) => write!(f, "expected token {:?}, last symbol was {}", token, last),
                None => write!(f, "last symbol was {}", last),
            },
            PrematureEnd { line, col } => {
                write!(f, "premature end in line {}, column {}", line, col)
            }
        }
    }
}

impl<'a> HoaLexer<'a> {
    fn build_known_headers() -> HashMap<&'static str, Token<'a>> {
        let mut hm: HashMap<&'static str, Token<'a>> = HashMap::new();
        hm.insert("HOA:", TokenHoa);
        hm.insert("State:", TokenState);
        hm.insert("States:", TokenStates);
        hm.insert("Start:", TokenStart);
        hm.insert("AP:", TokenAp);
        hm.insert("Alias:", TokenAlias);
        hm.insert("Acceptance", TokenAcceptance);
        hm.insert("acc-name:", TokenAccname);
        hm.insert("tool:", TokenTool);
        hm.insert("name:", TokenName);
        hm.insert("properties:", TokenProperties);
        hm
    }

    pub fn tokenize(&mut self) -> Result<Vec<PositionedToken>, LexerError> {
        let mut tokens = Vec::new();
        // create an iterator that is able to peek multiple times so we can determine tokens
        let mut it = itertools::multipeek(self.iterator_from(0, 0));
        // label the outer loop so that we can break in case an error occurs or we're done
        'outer: loop {
            // we look at the first char to determine what is going to happen
            let chr = it.peek();
            match chr {
                // there are no characters left
                None => {
                    // add an EOF token and calculate position based on lines and length of last line
                    tokens
                        .push(TokenEof.at(self.lines.len() - 1, self.lines.last().unwrap().len()));
                    // exit the loop
                    break 'outer;
                }
                // we found the next character
                Some(&(c, line, col)) => {
                    // reset the peek in case we need encounter a longer token
                    it.reset_peek();
                    match c {
                        // we advance the iterator as long as we encounter whitespaces
                        c if (c as char).is_whitespace() => {
                            it.next();
                        }

                        // handle all simple syntactic elements
                        b'!' => {
                            tokens.push(TokenNot.at(line, col));
                            it.next();
                        }
                        b'&' => {
                            tokens.push(TokenAnd.at(line, col));
                            it.next();
                        }
                        b'|' => {
                            tokens.push(TokenOr.at(line, col));
                            it.next();
                        }
                        b'(' => {
                            tokens.push(TokenLparenth.at(line, col));
                            it.next();
                        }
                        b')' => {
                            tokens.push(TokenRparenth.at(line, col));
                            it.next();
                        }
                        b'[' => {
                            tokens.push(TokenLbracket.at(line, col));
                            it.next();
                        }
                        b']' => {
                            tokens.push(TokenRbracket.at(line, col));
                            it.next();
                        }
                        b'{' => {
                            tokens.push(TokenLcurly.at(line, col));
                            it.next();
                        }
                        b'}' => {
                            tokens.push(TokenRcurly.at(line, col));
                            it.next();
                        }

                        // hanlde --XYZ-- style markers
                        b'-' => {
                            it.next();
                            match it.next() {
                                Some((b'-', _, _)) => {}
                                _ => {
                                    return Err(MissingSymbol { expected: b'-' });
                                }
                            }
                            match it.next() {
                                Some((b'A', _, _)) => {
                                    // try to obtain the rest of the token
                                    let abort_rest = "BORT--".bytes().collect::<Vec<_>>();
                                    match take_n(&mut it, abort_rest.len()) {
                                        Some(word) if word == abort_rest => {
                                            tokens.push(TokenAbort.at(line, col));
                                        }
                                        _ => {
                                            return Err(UnrecognizedToken {
                                                expected: Some(TokenAbort),
                                                last: b'A',
                                            });
                                        }
                                    }
                                }
                                Some((b'B', _, _)) => {
                                    let body_rest = "ODY--".bytes().collect::<Vec<_>>();
                                    match take_n(&mut it, body_rest.len()) {
                                        Some(word) if word == body_rest => {
                                            tokens.push(TokenBody.at(line, col));
                                        }
                                        _ => {
                                            return Err(UnrecognizedToken {
                                                expected: Some(TokenBody),
                                                last: b'B',
                                            });
                                        }
                                    }
                                }
                                Some((b'E', _, _)) => {
                                    let end_rest = "ND--".bytes().collect::<Vec<_>>();
                                    match take_n(&mut it, end_rest.len()) {
                                        Some(word) if word == end_rest => {
                                            tokens.push(TokenEnd.at(line, col));
                                        }
                                        _ => {
                                            return Err(UnrecognizedToken {
                                                expected: Some(TokenEnd),
                                                last: b'E',
                                            });
                                        }
                                    }
                                }
                                Some((c, _, _)) => {
                                    return Err(UnrecognizedToken {
                                        expected: None,
                                        last: c,
                                    })
                                }
                                _ => return Err(PrematureEnd { line, col }),
                            }
                        }

                        // handle quoted strings
                        b'"' => {
                            // advance to behind the "
                            it.next();
                            let start = self.line_col_to_pos(line, col + 1);
                            let mut len = 0usize;

                            // allocate memory for the string
                            let mut string = String::new();
                            'extract_string: loop {
                                match it.next() {
                                    Some((b'"', _, _)) => {
                                        break 'extract_string;
                                    }
                                    Some((c, _, _)) => {
                                        string.push(char::from(c));
                                        len += 1;
                                    }
                                    None => {
                                        return Err(PrematureEnd { line, col });
                                    }
                                };
                            }
                            tokens
                                .push(TokenString(&self.input[start..(start + len)]).at(line, col));
                        }

                        // extract numbers
                        n if (n as char).is_numeric() => {
                            // allocate memory for the chars representing the number
                            let mut number_string = String::new();
                            // labelled loop so we can break when the number is over
                            'extract_number: loop {
                                match it.peek() {
                                    // as long as the peeked char is numeric we add it to the buffer
                                    Some((c, _, _)) if (*c as char).is_numeric() => {
                                        number_string.push(char::from(*c));
                                    }
                                    // otherwise we leave the loop
                                    _ => break 'extract_number,
                                };
                            }
                            // since we only peeked at the digits, we need to advance our iterator
                            // by the number of digits we collected
                            advance_by(&mut it, number_string.len());
                            // try to convert the buffer to a number
                            match number_string.parse::<usize>() {
                                Ok(num) => {
                                    tokens.push(TokenInt(num).at(line, col));
                                }
                                Err(_) => {
                                    return Err(IntParseError { line, col });
                                }
                            }
                        }

                        // handle aliases
                        b'@' => {
                            // skip the @
                            it.next();
                            let mut buffer = String::new();
                            let start = self.line_col_to_pos(line, col + 1);
                            let mut len = 0usize;

                            'extract_alias: loop {
                                let pk = it.peek();
                                println!("{:#?}", (pk.unwrap().0 as char));
                                match pk {
                                    Some((c, _, _))
                                        if (c.is_ascii_alphanumeric()
                                            || *c == b'_'
                                            || *c == b'-') =>
                                    {
                                        buffer.push(char::from(*c));
                                        len += 1;
                                    }
                                    _ => break 'extract_alias,
                                }
                            }
                            // advance by the number of peeked characters
                            advance_by(&mut it, buffer.len());
                            tokens.push(
                                TokenAliasName(&self.input[start..(start + len)]).at(line, col),
                            );
                        }

                        // handle identifiers, headers, t and f
                        c if (c.is_ascii_alphabetic() || c == b'_') => {
                            let mut buffer = String::new();
                            let start = self.line_col_to_pos(line, col);
                            let mut len = 0usize;

                            'extract_ident: loop {
                                match it.peek() {
                                    Some((c, _, _))
                                        if (c.is_ascii_alphanumeric()
                                            || *c == b'_'
                                            || *c == b'-') =>
                                    {
                                        buffer.push(char::from(*c));
                                        len += 1;
                                    }
                                    Some((b':', _, _)) => {
                                        buffer.push(':');
                                        len += 1;
                                    }
                                    _ => break 'extract_ident,
                                }
                            }
                            // advance by number of peeked chars
                            advance_by(&mut it, buffer.len());
                            // check if we have a header, i.e. last char is :
                            if buffer.chars().last().unwrap() == ':' {
                                match self.known_headers.get(buffer.as_str()) {
                                    Some(tokentype) => {
                                        tokens.push(tokentype.at(line, col));
                                    }
                                    None => {
                                        tokens.push(
                                            TokenHeaderName(&self.input[start..(start + len)])
                                                .at(line, col),
                                        );
                                    }
                                }
                            } else {
                                if buffer == "t".to_string() {
                                    tokens.push(TokenTrue.at(line, col));
                                } else if buffer == "f".to_string() {
                                    tokens.push(TokenFalse.at(line, col));
                                } else {
                                    tokens.push(
                                        TokenIdent(&self.input[start..(start + len)]).at(line, col),
                                    );
                                }
                            }
                        }

                        // handle block comments
                        b'/' => 'block_comments: loop {
                            let wrd = peek_n(&mut it, 2);
                            match wrd {
                                None => return Err(PrematureEnd { line, col }),
                                Some(chrs) => {
                                    if chrs != vec![b'*', b'/'] {
                                        it.reset_peek();
                                        it.next();
                                    } else {
                                        it.reset_peek();
                                        advance_by(&mut it, 2);
                                        break 'block_comments;
                                    }
                                }
                            }
                        },

                        // todo: entfernen?
                        d => {
                            println!("got: {:#?}", d as char);
                            it.next();
                        }
                    }
                }
            }
        }
        Ok(tokens)
    }

    fn line_col_to_pos(&self, line: usize, col: usize) -> usize {
        self.lines[0..line]
            .iter()
            // we need to add 1 since the \n is not present in lines, but in the byte array it is
            .map(|l| l.len() + 1)
            .sum::<usize>()
            + col
    }

    fn iterator_annotated(&self) -> impl Iterator<Item = (u8, usize, usize)> + '_ {
        self.input.lines().enumerate().flat_map(|(n_line, line)| {
            line.bytes()
                .chain(once(b'\n'))
                .enumerate()
                .map(move |(n_col, chr)| (chr, n_line, n_col))
        })
    }

    pub fn iterator_from(
        &self,
        l: usize,
        col: usize,
    ) -> impl Iterator<Item = (u8, usize, usize)> + '_ {
        self.iterator_annotated()
            .filter(move |(_, n_line, n_col)| *n_line > l || (*n_line >= l && *n_col >= col))
    }
}

impl<'a> TryFrom<&[u8]> for HoaLexer<'a> {
    type Error = LexerError<'a>;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        let contents = match from_utf8(value) {
            Ok(txt) => txt,
            Err(_) => return Err(ParseInputError {}),
        };

        Ok(HoaLexer {
            line: 0,
            col: 0,
            curr: '\t',
            is_eof: false,
            lines: String::from(contents)
                .lines()
                .map(|s| s.to_string())
                .collect::<Vec<String>>(),
            input: String::from(contents),
            known_headers: HoaLexer::build_known_headers(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_token_test() {
        let testbytes = b"[\n(]";
        let mut hl = HoaLexer::try_from(testbytes as &[u8]).ok().unwrap();
        let tokens = hl.tokenize().ok().unwrap();
        assert_eq!(tokens.len(), 4);
        // todo test fixen
        //        assert_eq!(tokens[0], Token::new(TokenLbracket, 0, 0));
        //        assert_eq!(tokens[1], Token::new(TokenLparenth, 1, 0));
        //        assert_eq!(tokens[2], Token::new(TokenRbracket, 1, 1));
        //        assert_eq!(tokens[3], Token::new(TokenEof, 1, 2));
    }

    #[test]
    fn new_lexer_test() {
        let filename = "/home/leon/tdoc".to_string();
        let contents = fs::read(filename).expect("file not found?");
        let mut hl = HoaLexer::try_from(contents.as_slice()).expect("shiat");
        let tokens = hl.tokenize();
        println!("{:#?}", tokens);
    }

    #[test]
    fn real_automaton_test() {
        let filename = "/home/leon/aut1.hoa".to_string();
        let contents = fs::read(filename).expect("file not found?");
        let mut hl = HoaLexer::try_from(contents.as_slice()).expect("shit");
        for token in hl.tokenize().expect("again shit") {
            println!("{:#?}", token);
        }
    }

    #[test]
    fn newlinetest() {
        let text = "asd\ndfkjdfj".to_string();
        let it = text.bytes();
        for b in it {
            println!("{:?}", char::from(b));
        }
    }
}
