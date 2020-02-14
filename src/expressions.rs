use std::fmt::{Error, Formatter};
use BooleanAtom::*;
use BooleanExpression::*;

#[derive(Debug)]
enum BooleanAtom<'a> {
    BoolValue(bool),
    IntegerValue(usize),
    AliasName(&'a str),
}

#[derive(Debug)]
enum BooleanExpression<'a> {
    Atom(BooleanAtom<'a>),
    Negation(Box<BooleanExpression<'a>>),
    Conjunction(Box<BooleanExpression<'a>>, Box<BooleanExpression<'a>>),
    Disjunction(Box<BooleanExpression<'a>>, Box<BooleanExpression<'a>>),
}

impl<'a> From<BooleanAtom<'a>> for BooleanExpression<'a> {
    fn from(atom: BooleanAtom<'a>) -> Self {
        Atom(atom)
    }
}

impl<'a> BooleanAtom<'a> {
    fn btrue() -> BooleanAtom<'a> {
        BoolValue(true)
    }

    fn bfalse() -> BooleanAtom<'a> {
        BoolValue(false)
    }

    fn bint(int: usize) -> BooleanAtom<'a> {
        IntegerValue(int)
    }

    fn balias(name: &'a str) -> BooleanAtom<'a> {
        AliasName(name)
    }
}

impl<'a> BooleanExpression<'a> {
    fn and(self, other: BooleanExpression<'a>) -> BooleanExpression<'a> {
        Conjunction(Box::new(self), Box::new(other))
    }

    fn or(self, other: BooleanExpression<'a>) -> BooleanExpression<'a> {
        Disjunction(Box::new(self), Box::new(other))
    }

    fn not(self) -> BooleanExpression<'a> {
        Negation(Box::new(self))
    }
}

impl<'a> std::fmt::Display for BooleanAtom<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            BoolValue(b) => match b {
                true => write!(f, "t"),
                false => write!(f, "f"),
            },
            IntegerValue(int) => write!(f, "{}", int),
            AliasName(name) => write!(f, "@{}", name),
        }
    }
}

impl<'a> std::fmt::Display for BooleanExpression<'a> {
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
        let a: BooleanExpression = BooleanAtom::balias("asdf").into();
        let t: BooleanExpression = BooleanAtom::btrue().into();

        let be = a.not().or(t.not());
        println!("{}", be);
    }

    #[test]
    fn expression_methods_construction_test() {
        let t: BooleanExpression = BooleanAtom::btrue().into();
        let f: BooleanExpression = BooleanAtom::bfalse().into();

        let be = t.not().and(f);
        println!("{}", be);
    }

    #[test]
    fn expression_construction_test() {
        let be = Conjunction(
            Box::new(Atom(BoolValue(true))),
            Box::new(Atom(BoolValue(false))),
        );
        println!("{}", be);
    }
}
