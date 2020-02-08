use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;
use std::iter::FromIterator;
use std::string::String;

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenType,
    pub string: Option<String>,
    pub int: Option<usize>,

    line: usize,
    col: usize,
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

impl Token {
    pub fn new(kind: TokenType, line: usize, col: usize) -> Token {
        Token {
            kind: kind,
            string: None,
            int: None,
            line: line,
            col: col,
        }
    }

    pub fn new_with_string(kind: TokenType, line: usize, col: usize, string: String) -> Token {
        Token {
            kind: kind,
            string: Some(string),
            int: None,
            line: line,
            col: col,
        }
    }

    pub fn new_with_int(kind: TokenType, line: usize, col: usize, integer: usize) -> Token {
        Token {
            kind: kind,
            string: None,
            int: Some(integer),
            line: line,
            col: col,
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

impl ToString for Token {
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
    fn from_file(filename: String) -> HoaLexer {
        if let Some(mut file) = File::open(filename).ok() {
            let mut contents = String::new();
            if file.read_to_string(&mut contents).is_ok() {
                let headers = HashMap::from_iter(vec![
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
                ]);
                let hl = HoaLexer {
                    line: 0,
                    col: 0,
                    curr: '\t',
                    is_eof: false,
                    lines: contents
                        .lines()
                        .map(|s| s.to_string())
                        .collect::<Vec<String>>(),
                    input: contents,
                    known_headers: headers,
                };
                return hl;
            }
        }
        panic!("asldkfjkldsjfk\nsdlkfjdlkfjkl");
    }

    fn next_char(&mut self) {
        if self.line >= self.lines.len() {
            self.is_eof = true;
        }
        if self.is_eof { return; }
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

    fn skip_whitespace(&mut self) {
        loop {
            if self.curr.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        match self.curr {
            c if c.is_alphabetic() => Token::new_with_string(TokenType::TokenString, self.line, self.col, self.curr.to_string()),
            f => Token::new(TokenType::TokenIdent, self.line, self.col),
        }
    }

    // !TODO: remove
    fn it_works(&mut self) -> &String {
        loop {
            println!("{:?}", self.next_token());
            self.next_char();
            if self.is_eof {
                break;
            }
        }
        &self.input
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn lexer_has_ownership() {
        let filename = "/home/leon/tdoc".to_string();
        let mut hl = HoaLexer::from_file(filename);
        hl.it_works();
    }
}
