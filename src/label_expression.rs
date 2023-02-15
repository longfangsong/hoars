pub fn boolean() -> impl Parser<Token, bool, Error = Simple<Token>> {
    select! {
        Token::Bool(b) => b,
    }
}

pub fn integer() -> impl Parser<Token, Id, Error = Simple<Token>> {
    select! {
        Token::Int(n) => n.parse().unwrap(),
    }
}

pub fn text() -> impl Parser<Token, String, Error = Simple<Token>> {
    select! {
        Token::Text(txt) => txt.clone(),
    }
}

pub fn identifier() -> impl Parser<Token, String, Error = Simple<Token>> {
    select! { Token::Identifier(ident) => ident.clone() }
}

pub fn alias() -> impl Parser<Token, String, Error = Simple<Token>> {
    select! { Token::Alias(aname) => aname.clone() }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LabelExpression {
    Boolean(bool),
    Integer(u32),
    Alias(AliasName),
    Not(Box<LabelExpression>),
    And(Vec<LabelExpression>),
    Or(Vec<LabelExpression>),
}

impl LabelExpression {
    pub fn boolean(b: bool) -> Self {
        Self::Boolean(b)
    }

    pub fn alias<I: std::fmt::Display>(name: I) -> Self {
        Self::Alias(name.to_string())
    }

    pub fn not(expr: Self) -> Self {
        Self::Not(Box::new(expr))
    }

    pub fn int(i: u32) -> Self {
        Self::Integer(i)
    }
}

pub fn label_expression() -> impl Parser<Token, LabelExpression, Error = Simple<Token>> {
    recursive(|label_expression| {
        let value = Value::boolean()
            .map(|b| LabelExpression::Boolean(b))
            .or(Value::integer().map(|n| LabelExpression::Integer(n)))
            .or(alias_name.map(|aname| LabelExpression::Alias(aname)));

        let atom =
            value.or(label_expression.delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))));

        let unary = just(Token::Op('!'))
            .or_not()
            .then(atom)
            .map(|(negated, expr)| {
                if negated.is_some() {
                    LabelExpression::Not(Box::new(expr))
                } else {
                    expr
                }
            });

        let conjunction = unary
            .clone()
            .separated_by(just(Token::Op('&')))
            .map(|conjuncts| {
                if conjuncts.len() > 1 {
                    LabelExpression::And(conjuncts)
                } else {
                    conjuncts.first().unwrap().clone()
                }
            });

        let disjunction = conjunction
            .clone()
            .separated_by(just(Token::Op('|')))
            .map(|disjuncts| {
                if disjuncts.len() > 1 {
                    LabelExpression::Or(disjuncts)
                } else {
                    disjuncts.first().unwrap().clone()
                }
            });

        disjunction
    })
}
