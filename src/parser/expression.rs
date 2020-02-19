use crate::lexer::Token;
use std::convert::TryFrom;
use std::fmt::{Debug, Error, Formatter};
use std::ops::{Add, Mul, Neg, Not};
use BooleanAtomAlias::*;
use BooleanExpressionAlias::*;

type StartStates = Vec<usize>;

/// A boolean atom represents the pieces of which a BooleanExpression is made up of. It can be one
/// of true (t), false (f), an integer value representing an atomic proposition or an alias name
#[derive(Debug, Eq, PartialEq)]
pub enum BooleanAtomAlias {
    BooleanValue(bool),
    IntegerValue(usize),
    AliasName(String),
}

/// Boolean expressions are made up of boolean atoms that can be negated or combined with the
/// junctor 'or' and 'and'
#[derive(Debug, Eq, PartialEq)]
pub enum BooleanExpressionAlias {
    Atom(BooleanAtomAlias),
    Negation(Box<BooleanExpressionAlias>),
    Conjunction(Box<BooleanExpressionAlias>, Box<BooleanExpressionAlias>),
    Disjunction(Box<BooleanExpressionAlias>, Box<BooleanExpressionAlias>),
}

#[derive(Debug, Eq, PartialEq)]
/// Represents an identifier for the acceptance component
pub enum AcceptanceIdent {
    Fin(usize),
    FinNeg(usize),
    Inf(usize),
    InfNeg(usize),
}

/// Acceptance conditions are made up of boolean combinations of acceptance identifiers
#[derive(Debug, Eq, PartialEq)]
pub enum AcceptanceCondition {
    Atom(AcceptanceIdent),
    Conjunction(Box<AcceptanceCondition>, Box<AcceptanceCondition>),
    Disjunction(Box<AcceptanceCondition>, Box<AcceptanceCondition>),
    BooleanValue(bool),
}

impl Mul for BooleanExpressionAlias {
    type Output = BooleanExpressionAlias;

    fn mul(self, rhs: Self) -> Self::Output {
        self.and(rhs)
    }
}

impl Mul<BooleanAtomAlias> for BooleanExpressionAlias {
    type Output = BooleanExpressionAlias;

    fn mul(self, rhs: BooleanAtomAlias) -> Self::Output {
        self * BooleanExpressionAlias::Atom(rhs)
    }
}

impl Mul for BooleanAtomAlias {
    type Output = BooleanExpressionAlias;

    fn mul(self, rhs: Self) -> Self::Output {
        BooleanExpressionAlias::Atom(self) * BooleanExpressionAlias::Atom(rhs)
    }
}

impl Mul<BooleanExpressionAlias> for BooleanAtomAlias {
    type Output = BooleanExpressionAlias;

    fn mul(self, rhs: BooleanExpressionAlias) -> Self::Output {
        BooleanExpressionAlias::Atom(self) * rhs
    }
}

impl Mul for AcceptanceCondition {
    type Output = AcceptanceCondition;

    fn mul(self, rhs: Self) -> Self::Output {
        AcceptanceCondition::Conjunction(Box::new(self), Box::new(rhs))
    }
}

impl Add for BooleanExpressionAlias {
    type Output = BooleanExpressionAlias;

    fn add(self, rhs: Self) -> Self::Output {
        self.or(rhs)
    }
}

impl Add<BooleanAtomAlias> for BooleanExpressionAlias {
    type Output = BooleanExpressionAlias;

    fn add(self, rhs: BooleanAtomAlias) -> Self::Output {
        self + BooleanExpressionAlias::Atom(rhs)
    }
}

impl Add for BooleanAtomAlias {
    type Output = BooleanExpressionAlias;

    fn add(self, rhs: Self) -> Self::Output {
        BooleanExpressionAlias::Atom(self) + rhs
    }
}

impl Add<BooleanExpressionAlias> for BooleanAtomAlias {
    type Output = BooleanExpressionAlias;

    fn add(self, rhs: BooleanExpressionAlias) -> Self::Output {
        BooleanExpressionAlias::Atom(self) + rhs
    }
}

impl Add for AcceptanceCondition {
    type Output = AcceptanceCondition;

    fn add(self, rhs: Self) -> Self::Output {
        AcceptanceCondition::Disjunction(Box::new(self), Box::new(rhs))
    }
}

impl Not for BooleanExpressionAlias {
    type Output = BooleanExpressionAlias;

    fn not(self) -> Self::Output {
        self.not()
    }
}

impl Not for BooleanAtomAlias {
    type Output = BooleanExpressionAlias;

    fn not(self) -> Self::Output {
        BooleanExpressionAlias::Negation(Box::new(BooleanExpressionAlias::Atom(self)))
    }
}

impl Not for AcceptanceIdent {
    type Output = AcceptanceIdent;

    fn not(self) -> Self::Output {
        match self {
            AcceptanceIdent::Fin(x) => AcceptanceIdent::FinNeg(x),
            AcceptanceIdent::Inf(x) => AcceptanceIdent::InfNeg(x),
            _ => unreachable!(),
        }
    }
}

impl From<BooleanAtomAlias> for BooleanExpressionAlias {
    fn from(atom: BooleanAtomAlias) -> Self {
        Atom(atom)
    }
}

impl From<AcceptanceIdent> for AcceptanceCondition {
    fn from(atom: AcceptanceIdent) -> Self {
        AcceptanceCondition::Atom(atom)
    }
}

impl<'a> BooleanAtomAlias {
    pub fn btrue() -> BooleanAtomAlias {
        BooleanValue(true)
    }

    pub fn bfalse() -> BooleanAtomAlias {
        BooleanValue(false)
    }

    pub fn bint(int: usize) -> BooleanAtomAlias {
        IntegerValue(int)
    }

    pub fn balias(name: String) -> BooleanAtomAlias {
        AliasName(name)
    }
}

impl BooleanExpressionAlias {
    pub fn and(self, other: BooleanExpressionAlias) -> BooleanExpressionAlias {
        Conjunction(Box::new(self), Box::new(other))
    }

    pub fn or(self, other: BooleanExpressionAlias) -> BooleanExpressionAlias {
        Disjunction(Box::new(self), Box::new(other))
    }

    pub fn not(self) -> BooleanExpressionAlias {
        Negation(Box::new(self))
    }
}

impl std::fmt::Display for AcceptanceIdent {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            AcceptanceIdent::Fin(int) => write!(f, "Fin({})", int),
            AcceptanceIdent::FinNeg(int) => write!(f, "Fin(!{})", int),
            AcceptanceIdent::Inf(int) => write!(f, "Inf({})", int),
            AcceptanceIdent::InfNeg(int) => write!(f, "Inf(!{})", int),
        }
    }
}

impl std::fmt::Display for AcceptanceCondition {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            AcceptanceCondition::Atom(atom) => write!(f, "{}", atom),
            AcceptanceCondition::Conjunction(left, right) => write!(f, "({} & {})", left, right),
            AcceptanceCondition::Disjunction(left, right) => write!(f, "({} | {})", left, right),
            AcceptanceCondition::BooleanValue(b) => write!(
                f,
                "{}",
                match b {
                    true => "t",
                    false => "f",
                }
            ),
        }
    }
}

impl std::fmt::Display for BooleanAtomAlias {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            BooleanValue(b) => match b {
                true => write!(f, "t"),
                false => write!(f, "f"),
            },
            IntegerValue(int) => write!(f, "{}", int),
            AliasName(name) => write!(f, "@{}", name),
        }
    }
}

impl std::fmt::Display for BooleanExpressionAlias {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Atom(atom) => write!(f, "{}", atom),
            Negation(subexpr) => write!(f, "!{}", *subexpr),
            Conjunction(left, right) => write!(f, "({} & {})", *left, *right),
            Disjunction(left, right) => write!(f, "({} | {})", *left, *right),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alias_test() {
        let a: BooleanExpressionAlias = BooleanAtomAlias::balias("asdf".into()).into();
        let t: BooleanExpressionAlias = BooleanAtomAlias::btrue().into();

        let be = a.not().or(t.not());
        println!("{}", be);
    }

    #[test]
    fn expression_methods_construction_test() {
        let t: BooleanExpressionAlias = BooleanAtomAlias::btrue().into();
        let f: BooleanExpressionAlias = BooleanAtomAlias::bfalse().into();

        let be = t.not().and(f);
        println!("{}", be);
    }

    #[test]
    fn expression_construction_test() {
        let be = Conjunction(
            Box::new(Atom(BooleanValue(true))),
            Box::new(Atom(BooleanValue(false))),
        );
        println!("{}", be);
    }
}
