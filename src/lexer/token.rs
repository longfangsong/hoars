use Token::*;

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub enum Token<'a> {
    TokenInt(usize, usize, usize),
    TokenIdent(usize, usize, &'a str),
    TokenString(usize, usize, &'a str),
    TokenHeaderName(usize, usize, &'a str),
    TokenAliasName(usize, usize, &'a str),
    TokenEof(usize, usize),
    TokenBody(usize, usize),
    TokenEnd(usize, usize),
    TokenAbort(usize, usize),
    TokenHoa(usize, usize),
    TokenState(usize, usize),
    TokenStates(usize, usize),
    TokenStart(usize, usize),
    TokenAp(usize, usize),
    TokenAlias(usize, usize),
    TokenAcceptance(usize, usize),
    TokenAccname(usize, usize),
    TokenTool(usize, usize),
    TokenName(usize, usize),
    TokenProperties(usize, usize),
    TokenNot(usize, usize),
    TokenAnd(usize, usize),
    TokenOr(usize, usize),
    TokenLparenth(usize, usize),
    TokenRparenth(usize, usize),
    TokenLbracket(usize, usize),
    TokenRbracket(usize, usize),
    TokenLcurly(usize, usize),
    TokenRcurly(usize, usize),
    TokenTrue(usize, usize),
    TokenFalse(usize, usize),
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenInt(line, col, int) => write!(f, "INT({}) at {},{}", int, line, col),
            TokenIdent(line, col, ident) => write!(f, "IDENT({}) at {},{}", ident, line, col),
            TokenString(line, col, string) => write!(f, "STR({}) at {},{}", string, line, col),
            TokenHeaderName(line, col, name) => write!(f, "HEADER({}) at {},{}", name, line, col),
            TokenAliasName(line, col, alias) => write!(f, "ALIAS(@{}) at {},{}", alias, line, col),

            TokenEof(line, col) => write!(f, "EOF at {}, {}", line, col),
            TokenBody(line, col) => write!(f, "--BODY-- at {}, {}", line, col),
            TokenEnd(line, col) => write!(f, "--END-- at {}, {}", line, col),
            TokenAbort(line, col) => write!(f, "--ABORT-- at {}, {}", line, col),
            TokenHoa(line, col) => write!(f, "HOA: at {}, {}", line, col),
            TokenState(line, col) => write!(f, "State: at {}, {}", line, col),
            TokenStates(line, col) => write!(f, "States: at {}, {}", line, col),
            TokenStart(line, col) => write!(f, "Start: at {}, {}", line, col),
            TokenAp(line, col) => write!(f, "Ap: at {}, {}", line, col),
            TokenAlias(line, col) => write!(f, "Alias: at {}, {}", line, col),
            TokenAcceptance(line, col) => write!(f, "Acceptance: at {}, {}", line, col),
            TokenAccname(line, col) => write!(f, "Accname: at {}, {}", line, col),
            TokenTool(line, col) => write!(f, "tool: at {}, {}", line, col),
            TokenName(line, col) => write!(f, "name: at {}, {}", line, col),
            TokenProperties(line, col) => write!(f, "properties: at {}, {}", line, col),
            TokenNot(line, col) => write!(f, "! at {}, {}", line, col),
            TokenAnd(line, col) => write!(f, "& at {}, {}", line, col),
            TokenOr(line, col) => write!(f, "| at {}, {}", line, col),
            TokenLparenth(line, col) => write!(f, "( at {}, {}", line, col),
            TokenRparenth(line, col) => write!(f, ") at {}, {}", line, col),
            TokenLbracket(line, col) => write!(f, "[ at {}, {}", line, col),
            TokenRbracket(line, col) => write!(f, "] at {}, {}", line, col),
            TokenLcurly(line, col) => write!(f, "{{ at {}, {}", line, col),
            TokenRcurly(line, col) => write!(f, "}} at {}, {}", line, col),
            TokenTrue(line, col) => write!(f, "t at {}, {}", line, col),
            TokenFalse(line, col) => write!(f, "f at {}, {}", line, col),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_token_print_test() {
        let tkn = TokenAliasName(2, 7, "hallO");
        println!("{}", tkn);
    }
}
