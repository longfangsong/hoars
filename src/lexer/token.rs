use Token::*;

/// Annotates a Token with additional data, for now that is only its position in the source
#[derive(Clone, Debug, Copy, PartialEq, Hash)]
pub struct PositionedToken<'a> {
    pub token: Token<'a>,
    pub line: usize,
    pub col: usize,
}

/// A Token in the input is represented here. It can potentially hold relevant data like string.
#[derive(Clone, Debug, Copy, Hash)]
pub enum Token<'a> {
    TokenInt(usize),
    TokenIdent(&'a str),
    TokenString(&'a str),
    TokenHeaderName(&'a str),
    TokenAliasName(&'a str),
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

pub const INTEGER: Token<'static> = TokenInt(0);
pub const IDENTIFIER: Token<'static> = TokenIdent("");
pub const STRING: Token<'static> = TokenString("");
pub const HEADER_NAME: Token<'static> = TokenHeaderName("");
pub const ALIAS_NAME: Token<'static> = TokenAliasName("");
pub const BOOLEAN_COMBINATORS: [Token; 7] = [
    TokenAnd,
    TokenNot,
    TokenOr,
    TokenLparenth,
    TokenRparenth,
    TokenTrue,
    TokenFalse,
];

impl<'a> PartialEq for Token<'a> {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

impl<'a> Eq for Token<'a> {}

impl<'a> Token<'a> {
    pub fn unwrap_int(&self) -> usize {
        match self {
            TokenInt(int) => *int,
            _ => panic!("expected TokenInt"),
        }
    }

    pub fn unwap_str(&self) -> &'a str {
        match self {
            TokenIdent(content) => *content,
            TokenString(content) => *content,
            TokenHeaderName(content) => *content,
            TokenAliasName(content) => *content,
            _ => panic!("expected token containing string"),
        }
    }

    pub fn at(self, line: usize, col: usize) -> PositionedToken<'a> {
        PositionedToken {
            token: self,
            line,
            col,
        }
    }
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenInt(ap) => write!(f, "{}", ap),
            TokenIdent(ident) => write!(f, "'{}'", ident),
            TokenString(string) => write!(f, "\"{}\"", string),
            TokenHeaderName(name) => write!(f, "HEADER({})", name),
            TokenAliasName(alias) => write!(f, "@{}", alias),

            TokenNot => write!(f, "!"),
            TokenAnd => write!(f, "&"),
            TokenOr => write!(f, "|"),
            TokenLparenth => write!(f, "("),
            TokenRparenth => write!(f, ")"),
            TokenLbracket => write!(f, "["),
            TokenRbracket => write!(f, "]"),
            TokenLcurly => write!(f, "{{"),
            TokenRcurly => write!(f, "}}"),
            TokenTrue => write!(f, "t"),
            TokenFalse => write!(f, "f"),

            TokenEof => write!(f, "EOF"),
            TokenBody => write!(f, "--BODY--"),
            TokenEnd => write!(f, "--END--"),
            TokenAbort => write!(f, "--ABORT--"),
            TokenHoa => write!(f, "HOA: "),
            TokenState => write!(f, "State: "),
            TokenStates => write!(f, "States: "),
            TokenStart => write!(f, "Start: "),
            TokenAp => write!(f, "AP: "),
            TokenAlias => write!(f, "Alias: "),
            TokenAcceptance => write!(f, "Acceptance: "),
            TokenAccname => write!(f, "acc-name: "),
            TokenTool => write!(f, "tool: "),
            TokenName => write!(f, "name: "),
            TokenProperties => write!(f, "properties: "),
        }
    }
}

impl<'a> std::fmt::Display for PositionedToken<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.token {
            TokenInt(int) => write!(f, "INT({}) at {},{}", int, self.line, self.col),
            TokenIdent(ident) => write!(f, "IDENT({}) at {},{}", ident, self.line, self.col),
            TokenString(string) => write!(f, "STR({}) at {},{}", string, self.line, self.col),
            TokenHeaderName(name) => write!(f, "HEADER({}) at {},{}", name, self.line, self.col),
            TokenAliasName(alias) => write!(f, "ALIAS(@{}) at {},{}", alias, self.line, self.col),

            TokenEof => write!(f, "EOF at {}, {}", self.line, self.col),
            TokenBody => write!(f, "--BODY-- at {}, {}", self.line, self.col),
            TokenEnd => write!(f, "--END-- at {}, {}", self.line, self.col),
            TokenAbort => write!(f, "--ABORT-- at {}, {}", self.line, self.col),
            TokenHoa => write!(f, "HOA: at {}, {}", self.line, self.col),
            TokenState => write!(f, "State: at {}, {}", self.line, self.col),
            TokenStates => write!(f, "States: at {}, {}", self.line, self.col),
            TokenStart => write!(f, "Start: at {}, {}", self.line, self.col),
            TokenAp => write!(f, "Ap: at {}, {}", self.line, self.col),
            TokenAlias => write!(f, "Alias: at {}, {}", self.line, self.col),
            TokenAcceptance => write!(f, "Acceptance: at {}, {}", self.line, self.col),
            TokenAccname => write!(f, "Accname: at {}, {}", self.line, self.col),
            TokenTool => write!(f, "tool: at {}, {}", self.line, self.col),
            TokenName => write!(f, "name: at {}, {}", self.line, self.col),
            TokenProperties => write!(f, "properties: at {}, {}", self.line, self.col),
            TokenNot => write!(f, "! at {}, {}", self.line, self.col),
            TokenAnd => write!(f, "& at {}, {}", self.line, self.col),
            TokenOr => write!(f, "| at {}, {}", self.line, self.col),
            TokenLparenth => write!(f, "( at {}, {}", self.line, self.col),
            TokenRparenth => write!(f, ") at {}, {}", self.line, self.col),
            TokenLbracket => write!(f, "[ at {}, {}", self.line, self.col),
            TokenRbracket => write!(f, "] at {}, {}", self.line, self.col),
            TokenLcurly => write!(f, "{{ at {}, {}", self.line, self.col),
            TokenRcurly => write!(f, "}} at {}, {}", self.line, self.col),
            TokenTrue => write!(f, "t at {}, {}", self.line, self.col),
            TokenFalse => write!(f, "f at {}, {}", self.line, self.col),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn equality_token_test() {
        assert_eq!(INTEGER, TokenInt(7));
    }

    #[test]
    fn simple_token_print_test() {
        let tkn = TokenAliasName("hallO");
        println!("{}", tkn);
    }
}
