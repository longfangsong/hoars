extern crate itertools;

use super::utils::*;

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;
use std::iter::{once, FromIterator, Peekable};
use std::str::{from_utf8, Utf8Error};
use std::string::String;

use self::itertools::{multipeek, MultiPeek};
use itertools::Itertools;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt::Formatter;
use std::num::ParseIntError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    TokenInt,
    TokenIdent,
    TokenString,
    TokenHeaderName,
    TokenAliasName,

    TokenEof,

    TokenBody,
    TokenEnd,
    TokenAbort,
    TokenHoa,
    TokenState,
    TokenStates,
    TokenStart,
    TokenAp,
    TokenAlias,
    TokenAcceptance,
    TokenAccname,
    TokenTool,
    TokenName,
    TokenProperties,

    // Punctuation, etc.
    TokenNot,
    TokenAnd,
    TokenOr,
    TokenLparenth,
    TokenRparenth,
    TokenLbracket,
    TokenRbracket,
    TokenLcurly,
    TokenRcurly,
    TokenTrue,
    TokenFalse,
}

impl ToString for TokenType {
    fn to_string(&self) -> String {
        match self {
            TokenType::TokenInt => "INT".to_string(),
            TokenType::TokenIdent => "IDENT".to_string(),
            TokenType::TokenString => "STRING".to_string(),
            TokenType::TokenHeaderName => "HEADER_NAME".to_string(),
            TokenType::TokenAliasName => "ALIAS_NAME".to_string(),

            TokenType::TokenEof => "EOF".to_string(),

            TokenType::TokenBody => "BODY".to_string(),
            TokenType::TokenEnd => "END".to_string(),
            TokenType::TokenAbort => "ABORT".to_string(),
            TokenType::TokenHoa => "HOA".to_string(),
            TokenType::TokenState => "STATE".to_string(),
            TokenType::TokenStates => "STATES".to_string(),
            TokenType::TokenStart => "START".to_string(),
            TokenType::TokenAp => "AP".to_string(),
            TokenType::TokenAlias => "ALIAS".to_string(),
            TokenType::TokenAcceptance => "ACCEPTANCE".to_string(),
            TokenType::TokenAccname => "ACCNAME".to_string(),
            TokenType::TokenTool => "TOOL".to_string(),
            TokenType::TokenName => "NAME".to_string(),
            TokenType::TokenProperties => "PROPERTIES".to_string(),

            TokenType::TokenNot => "NOT".to_string(),
            TokenType::TokenAnd => "AND".to_string(),
            TokenType::TokenOr => "OR".to_string(),
            TokenType::TokenLparenth => "LPARENTH".to_string(),
            TokenType::TokenRparenth => "RPARENTH".to_string(),
            TokenType::TokenLbracket => "LBRACHEKT".to_string(),
            TokenType::TokenRbracket => "RBRACKET".to_string(),
            TokenType::TokenLcurly => "LCURLY".to_string(),
            TokenType::TokenRcurly => "RCURLY".to_string(),
            TokenType::TokenTrue => "TRUE".to_string(),
            TokenType::TokenFalse => "FALSE".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenType,
    pub string: Option<&'a str>,
    pub int: Option<usize>,

    pub line: usize,
    pub col: usize,
}

pub struct HoaLexer {
    line: usize,
    col: usize,
    curr: char,
    known_headers: HashMap<String, TokenType>,
    input: String,
    lines: Vec<String>,
    is_eof: bool,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenType, line: usize, col: usize) -> Token<'a> {
        Token {
            kind,
            string: None,
            int: None,
            line,
            col,
        }
    }

    pub fn new_with_string(kind: TokenType, line: usize, col: usize, string: &'a str) -> Token<'a> {
        Token {
            kind,
            string: Some(string),
            int: None,
            line,
            col,
        }
    }

    pub fn new_with_int(kind: TokenType, line: usize, col: usize, integer: usize) -> Token<'a> {
        Token {
            kind,
            string: None,
            int: Some(integer),
            line,
            col,
        }
    }

    pub fn is_eof(&self) -> bool {
        unimplemented!();
    }

    pub fn type_as_string(kind: TokenType) -> String {
        match kind {
            TokenType::TokenInt => "INT".to_string(),
            TokenType::TokenIdent => "IDENT".to_string(),
            TokenType::TokenString => "STRING".to_string(),
            TokenType::TokenHeaderName => "HEADER_NAME".to_string(),
            TokenType::TokenAliasName => "ALIAS_NAME".to_string(),

            TokenType::TokenEof => "EOF".to_string(),

            TokenType::TokenBody => "BODY".to_string(),
            TokenType::TokenEnd => "END".to_string(),
            TokenType::TokenAbort => "ABORT".to_string(),
            TokenType::TokenHoa => "HOA".to_string(),
            TokenType::TokenState => "STATE".to_string(),
            TokenType::TokenStates => "STATES".to_string(),
            TokenType::TokenStart => "START".to_string(),
            TokenType::TokenAp => "AP".to_string(),
            TokenType::TokenAlias => "ALIAS".to_string(),
            TokenType::TokenAcceptance => "ACCEPTANCE".to_string(),
            TokenType::TokenAccname => "ACCNAME".to_string(),
            TokenType::TokenTool => "TOOL".to_string(),
            TokenType::TokenName => "NAME".to_string(),
            TokenType::TokenProperties => "PROPERTIES".to_string(),

            TokenType::TokenNot => "NOT".to_string(),
            TokenType::TokenAnd => "AND".to_string(),
            TokenType::TokenOr => "OR".to_string(),
            TokenType::TokenLparenth => "LPARENTH".to_string(),
            TokenType::TokenRparenth => "RPARENTH".to_string(),
            TokenType::TokenLbracket => "LBRACHEKT".to_string(),
            TokenType::TokenRbracket => "RBRACKET".to_string(),
            TokenType::TokenLcurly => "LCURLY".to_string(),
            TokenType::TokenRcurly => "RCURLY".to_string(),
            TokenType::TokenTrue => "TRUE".to_string(),
            TokenType::TokenFalse => "FALSE".to_string(),
        }
    }
}

impl<'a> ToString for Token<'a> {
    fn to_string(&self) -> String {
        match self.kind {
            TokenType::TokenInt => format!("INTEGER {}", self.int.as_ref().unwrap()),
            TokenType::TokenIdent => format!("IDENTIFIER {}", self.string.as_ref().unwrap()),
            TokenType::TokenString => format!("STRING {}", self.string.as_ref().unwrap()),
            TokenType::TokenHeaderName => format!("HEADER {}", self.string.as_ref().unwrap()),
            TokenType::TokenAliasName => format!("ALIAS {}", self.string.as_ref().unwrap()),

            TokenType::TokenEof => "END-OF-FILE".to_string(),

            TokenType::TokenBody => "--BODY--".to_string(),
            TokenType::TokenEnd => "--END--".to_string(),
            TokenType::TokenAbort => "--ABORT--".to_string(),
            TokenType::TokenHoa => "HEADER HOA".to_string(),
            TokenType::TokenStates => "HEADER States".to_string(),
            TokenType::TokenStart => "HEADER Start".to_string(),
            TokenType::TokenAp => "HEADER AP".to_string(),
            TokenType::TokenAlias => "HEADER Alias".to_string(),
            TokenType::TokenAcceptance => "HEADER Acceptance".to_string(),
            TokenType::TokenAccname => "HEADER acc-name".to_string(),
            TokenType::TokenTool => "HEADER tool".to_string(),
            TokenType::TokenName => "HEADER name".to_string(),
            TokenType::TokenProperties => "HEADER properties".to_string(),

            TokenType::TokenState => "DEFINITION State".to_string(),

            TokenType::TokenNot => "!".to_string(),
            TokenType::TokenAnd => "&".to_string(),
            TokenType::TokenOr => "|".to_string(),
            TokenType::TokenLparenth => "(".to_string(),
            TokenType::TokenRparenth => ")".to_string(),
            TokenType::TokenLbracket => "[".to_string(),
            TokenType::TokenRbracket => "]".to_string(),
            TokenType::TokenLcurly => "{".to_string(),
            TokenType::TokenRcurly => "}".to_string(),
            TokenType::TokenTrue => "TRUE t".to_string(),
            TokenType::TokenFalse => "FALSE f".to_string(),
        }
    }
}

impl HoaLexer {
    fn build_known_headers() -> HashMap<String, TokenType> {
        HashMap::from_iter(vec![
            ("HOA:".to_string(), TokenType::TokenHoa),
            ("State:".to_string(), TokenType::TokenState),
            ("States:".to_string(), TokenType::TokenStates),
            ("Start:".to_string(), TokenType::TokenStart),
            ("AP:".to_string(), TokenType::TokenAp),
            ("Alias:".to_string(), TokenType::TokenAlias),
            ("Acceptance".to_string(), TokenType::TokenAcceptance),
            ("acc-name:".to_string(), TokenType::TokenAccname),
            ("tool:".to_string(), TokenType::TokenTool),
            ("name:".to_string(), TokenType::TokenName),
            ("properties:".to_string(), TokenType::TokenProperties),
        ])
    }

    fn from_file(filename: String) -> HoaLexer {
        if let Some(mut file) = File::open(filename).ok() {
            let mut contents = String::new();
            if file.read_to_string(&mut contents).is_ok() {
                HoaLexer {
                    line: 0,
                    col: 0,
                    curr: '\t',
                    is_eof: false,
                    lines: contents
                        .lines()
                        .map(|s| s.to_string())
                        .collect::<Vec<String>>(),
                    input: contents,
                    known_headers: HoaLexer::build_known_headers(),
                }
            } else {
                panic!("aasdf");
            }
        } else {
            panic!("asdf");
        }
    }

    fn next_char(&mut self) {
        if self.line >= self.lines.len() {
            self.is_eof = true;
        }
        if self.is_eof {
            self.curr = '\x1b';
            return;
        }
        self.curr = match self.lines[self.line].chars().nth(self.col) {
            Some(c) => c,
            None => {
                self.is_eof = true;
                '\x1b'
            }
        };
        self.col += 1;
        if self.col >= self.lines[self.line].len() {
            self.col = 0;
            self.line += 1;
        }
    }

    fn peek_word(&mut self) -> Option<String> {
        let mut word = String::new();
        let col = self.col;
        let line = self.line;
        let curr = self.curr;
        loop {
            if !self.curr.is_alphabetic() {
                break;
            }
            word.push(self.curr);
            self.next_char();
        }
        self.col = col;
        self.line = line;
        self.curr = curr;
        Some(word)
    }

    fn skip_whitespace(&mut self) {
        loop {
            if self.curr.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, &'static str> {
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
                    tokens.push(Token::new(
                        TokenType::TokenEof,
                        self.lines.len() - 1,
                        self.lines.last().unwrap().len(),
                    ));
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
                            tokens.push(Token::new(TokenType::TokenNot, line, col));
                            it.next();
                        }
                        b'&' => {
                            tokens.push(Token::new(TokenType::TokenAnd, line, col));
                            it.next();
                        }
                        b'|' => {
                            tokens.push(Token::new(TokenType::TokenOr, line, col));
                            it.next();
                        }
                        b'(' => {
                            tokens.push(Token::new(TokenType::TokenLparenth, line, col));
                            it.next();
                        }
                        b')' => {
                            tokens.push(Token::new(TokenType::TokenRparenth, line, col));
                            it.next();
                        }
                        b'[' => {
                            tokens.push(Token::new(TokenType::TokenLbracket, line, col));
                            it.next();
                        }
                        b']' => {
                            tokens.push(Token::new(TokenType::TokenRbracket, line, col));
                            it.next();
                        }
                        b'{' => {
                            tokens.push(Token::new(TokenType::TokenLcurly, line, col));
                            it.next();
                        }
                        b'}' => {
                            tokens.push(Token::new(TokenType::TokenRcurly, line, col));
                            it.next();
                        }

                        // hanlde --XYZ-- style markers
                        b'-' => {
                            it.next();
                            match it.next() {
                                Some((b'-', _, _)) => {}
                                _ => {
                                    return Err("tokens need two dashes (--)");
                                }
                            }
                            match it.next() {
                                Some((b'A', _, _)) => {
                                    // try to obtain the rest of the token
                                    let abort_rest = "BORT--".bytes().collect::<Vec<_>>();
                                    match take_n(&mut it, abort_rest.len()) {
                                        Some(word) if word == abort_rest => {
                                            tokens.push(Token::new(
                                                TokenType::TokenAbort,
                                                line,
                                                col,
                                            ));
                                        }
                                        _ => {
                                            return Err("unrecognized token, expected ABORT");
                                        }
                                    }
                                }
                                Some((b'B', _, _)) => {
                                    let body_rest = "ODY--".bytes().collect::<Vec<_>>();
                                    match take_n(&mut it, body_rest.len()) {
                                        Some(word) if word == body_rest => {
                                            tokens.push(Token::new(
                                                TokenType::TokenBody,
                                                line,
                                                col,
                                            ));
                                        }
                                        _ => {
                                            return Err("unrecognized token, expected BODY");
                                        }
                                    }
                                }
                                Some((b'E', _, _)) => {
                                    let end_rest = "ND--".bytes().collect::<Vec<_>>();
                                    match take_n(&mut it, end_rest.len()) {
                                        Some(word) if word == end_rest => {
                                            tokens.push(Token::new(TokenType::TokenEnd, line, col));
                                        }
                                        _ => {
                                            return Err("unrecognized token, expected END");
                                        }
                                    }
                                }
                                _ => return Err("unexpected token, can be ABORT, BODY and END"),
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
                                        return Err("premature end of file in quoted string");
                                    }
                                };
                            }
                            tokens.push(Token::new_with_string(
                                TokenType::TokenString,
                                line,
                                col,
                                &self.input[start..(start + len)],
                            ));
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
                                    tokens.push(Token::new_with_int(
                                        TokenType::TokenInt,
                                        line,
                                        col,
                                        num,
                                    ));
                                }
                                Err(_) => {
                                    return Err("error while parsing integer");
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
                            tokens.push(Token::new_with_string(
                                TokenType::TokenAliasName,
                                line,
                                col,
                                &self.input[start..(start + len)],
                            ));
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
                                        tokens.push(Token::new(*tokentype, line, col));
                                    }
                                    None => {
                                        tokens.push(Token::new_with_string(
                                            TokenType::TokenHeaderName,
                                            line,
                                            col,
                                            &self.input[start..(start + len)],
                                        ));
                                    }
                                }
                            } else {
                                if buffer == "t".to_string() {
                                    tokens.push(Token::new_with_string(
                                        TokenType::TokenTrue,
                                        line,
                                        col,
                                        &self.input[start..(start + len)],
                                    ));
                                } else if buffer == "f".to_string() {
                                    tokens.push(Token::new_with_string(
                                        TokenType::TokenFalse,
                                        line,
                                        col,
                                        &self.input[start..(start + len)],
                                    ));
                                } else {
                                    tokens.push(Token::new_with_string(
                                        TokenType::TokenIdent,
                                        line,
                                        col,
                                        &self.input[start..(start + len)],
                                    ));
                                }
                            }
                        }
                        _ => {
                            unimplemented!("any other tokens? error handling?");
                        }
                    }
                }
            }
        }
        Ok(tokens)
    }

    fn line_col_to_pos(&self, line: usize, col: usize) -> usize {
        let mut res = 0usize;
        for l in 0..line {
            res += self.lines[l].len();
        }

        res + col
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

impl TryFrom<&[u8]> for HoaLexer {
    type Error = &'static str;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        let contents = match from_utf8(value) {
            Ok(txt) => txt,
            Err(_) => return Err("could not parse input data"),
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
        assert_eq!(tokens[0], Token::new(TokenType::TokenLbracket, 0, 0));
        assert_eq!(tokens[1], Token::new(TokenType::TokenLparenth, 1, 0));
        assert_eq!(tokens[2], Token::new(TokenType::TokenRbracket, 1, 1));
        assert_eq!(tokens[3], Token::new(TokenType::TokenEof, 1, 2));
    }

    #[test]
    fn new_lexer_test() {
        let filename = "/home/leon/tdoc".to_string();
        let mut hl = HoaLexer::from_file(filename);
        let tokens = hl.tokenize();
        println!("{:#?}", tokens);
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
