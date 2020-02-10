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

    fn peek_char_line(&self) -> Option<char> {
        self.lines[self.line].chars().nth(self.col)
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

    pub fn next_token(&mut self) -> Result<Token, &'static str> {
        self.skip_whitespace();
        match self.curr {
            '!' => Ok(Token::new(TokenType::TokenNot, self.line, self.col)),
            '&' => Ok(Token::new(TokenType::TokenAnd, self.line, self.col)),
            '|' => Ok(Token::new(TokenType::TokenOr, self.line, self.col)),
            '(' => Ok(Token::new(TokenType::TokenLparenth, self.line, self.col)),
            ')' => Ok(Token::new(TokenType::TokenRparenth, self.line, self.col)),
            '[' => Ok(Token::new(TokenType::TokenLbracket, self.line, self.col)),
            ']' => Ok(Token::new(TokenType::TokenRbracket, self.line, self.col)),
            '{' => Ok(Token::new(TokenType::TokenLcurly, self.line, self.col)),
            '}' => Ok(Token::new(TokenType::TokenRcurly, self.line, self.col)),
            '-' => {
                self.next_char();
                if self.curr == '-' {
                    match &self.peek_word().unwrap() as &str {
                        "ABORT" => Ok(Token::new(TokenType::TokenAbort, self.line, self.col)),
                        "BODY" => Ok(Token::new(TokenType::TokenBody, self.line, self.col)),
                        "END" => Ok(Token::new(TokenType::TokenEnd, self.line, self.col)),
                        _ => Err("lexical error: token started with - but did not match any of ABORT, ERROR or END"),
                    }
                } else {
                    Err("lexical error: token started with -, expected a second -")
                }
            }
            '"' => {
                let mut txt = String::new();
                loop {
                    if self.curr == '"' {
                        break;
                    }
                    if self.is_eof {
                        return Err("premature end of file in quoted string");
                    }
                    if self.curr != '\\' {
                        txt.push(self.curr);
                    }
                    self.next_char();
                }
                Ok(Token::new_with_string(
                    TokenType::TokenString,
                    self.line,
                    self.col,
                    txt,
                ))
            },
            _n if _n.is_numeric() => {
                let mut txt = String::new();
                loop {
                    if !self.curr.is_numeric() || self.col == 0 {
                        break;
                    }
                    if self.is_eof {
                        return Err("premature end of file in integer");
                    }
                    txt.push(self.curr);
                    self.next_char();
                }
                let i = match txt.parse::<usize>() {
                    Ok(i) => i,
                    Err(_) => {
                        return Err("could not parse integer");
                    },
                };
                Ok(Token::new_with_int(TokenType::TokenInt, self.line, self.col, i))
            },
            _ => Ok(Token::new(TokenType::TokenIdent, self.line, self.col)),
        }
    }

    // !TODO: remove
    fn it_works(&self) -> &String {
        &self.input
    }

    fn one_indexed<T>((n, x): (usize, T)) -> (usize, T) {
        (n+1, x)
    }

    fn iterator_annotated(&self) -> impl Iterator<Item = (char, usize, usize)> + '_ {
        self.input.lines().enumerate().flat_map(|(n_line, line)| {
            line.chars().enumerate().map(move |(n_col, chr)| {
                (chr, n_line, n_col)
            })
        })
    }

    pub fn iterator_from(&self, l: usize, col: usize) -> impl Iterator<Item = (char, usize, usize)> + '_ {
        self.iterator_annotated().filter(move |(c, n_line, n_col)| {
            *n_line > l || (*n_line >= l && *n_col >= col)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn lexer_has_ownership() {
        let filename = "/home/leon/tdoc".to_string();
        let mut hl = HoaLexer::from_file(filename);
        for (c, i, ii) in hl.iterator_from(0,0) {
            print!("({:?}{:?}{:?})", c, i, ii);
        }
        println!("\n{:?}", hl.it_works());
        let it = hl.iterator_from(0,3);
        for (c, i, ii) in it {
            print!("({:?}{:?}{:?})", c, i, ii);
        }
        print!("\n");
    }
}
