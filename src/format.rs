use std::fmt::Display;

pub type Id = u32;

pub type StateConjunction = Vec<Id>;

pub type AtomicProposition = String;

pub type AliasName = String;

#[derive(Debug, PartialEq, Eq)]
pub enum LabelExpression {
    Boolean(bool),
    Integer(u32),
    Alias(AliasName),
    Not(Box<LabelExpression>),
    And(Box<LabelExpression>, Box<LabelExpression>),
    Or(Box<LabelExpression>, Box<LabelExpression>),
}

impl LabelExpression {
    pub fn boolean(b: bool) -> Self {
        Self::Boolean(b)
    }

    pub fn alias<I: Display>(name: I) -> Self {
        Self::Alias(name.to_string())
    }

    pub fn or(lhs: Self, rhs: Self) -> Self {
        Self::Or(Box::new(lhs), Box::new(rhs))
    }
    pub fn and(lhs: Self, rhs: Self) -> Self {
        Self::And(Box::new(lhs), Box::new(rhs))
    }
    pub fn not(expr: Self) -> Self {
        Self::Not(Box::new(expr))
    }

    pub fn int(i: u32) -> Self {
        Self::Integer(i)
    }
}

pub type AcceptanceAtom = (bool, Id);

#[derive(Debug, PartialEq, Eq)]
pub enum AcceptanceCondition {
    Fin(AcceptanceAtom),
    Inf(AcceptanceAtom),
    And(Box<AcceptanceCondition>, Box<AcceptanceCondition>),
    Or(Box<AcceptanceCondition>, Box<AcceptanceCondition>),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AcceptanceName {
    Buchi,
    GeneralizedBuchi,
    CoBuchi,
    GeneralizedCoBuchi,
    Streett,
    Rabin,
    GeneralizedRabin,
    Parity,
    All,
    None,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Property {
    StateLabel,
    TransLabels,
    ImplicitLabels,
    ExplicitLabels,
    StateAcceptance,
    TransitionAcceptance,
    UniversalBranching,
    NoUniversalBranching,
    Deterministic,
    Complete,
    Unambiguous,
    StutterInvariant,
    Weak,
    VeryWeak,
    InherentlyWeak,
    Terminal,
    Tight,
    Colored,
}

#[derive(Debug, PartialEq, Eq)]
pub enum HeaderItem {
    States(Id),
    Start(StateConjunction),
    AP(Vec<AtomicProposition>),
    Alias(AliasName, LabelExpression),
    Acceptance(AcceptanceCondition),
    AcceptanceName(AcceptanceName),
    // Correspond to tool name and optional version number
    Tool(String, Option<String>),
    Name(String),
    Properties(Vec<Property>),
}

impl HeaderItem {
    pub fn alias<I: Display>(name: I, expr: LabelExpression) -> Self {
        Self::Alias(name.to_string(), expr)
    }

    pub fn acceptance_name(name: AcceptanceName) -> Self {
        Self::AcceptanceName(name)
    }
}

#[derive(Debug)]
pub struct HoaAutomaton {
    version: String,
    headers: Vec<HeaderItem>,
}

impl HoaAutomaton {
    pub fn new((version, headers): (String, Vec<HeaderItem>)) -> Self {
        Self { version, headers }
    }

    #[cfg(test)]
    pub fn assert_single_header(&self, header: HeaderItem) {
        assert!(self.headers.eq(&vec![header]));
    }

    #[cfg(test)]
    pub fn assert_headers(&self, header: Vec<HeaderItem>) {
        assert!(self.headers.eq(&header));
    }
}
