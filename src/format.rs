use std::fmt::Display;

pub type Id = u32;

pub type StateConjunction = Vec<Id>;

pub type AtomicProposition = String;

pub type AliasName = String;

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

    pub fn alias<I: Display>(name: I) -> Self {
        Self::Alias(name.to_string())
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
    And(Vec<AcceptanceCondition>),
    Or(Vec<AcceptanceCondition>),
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

impl TryFrom<String> for AcceptanceName {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "Buchi" => Ok(AcceptanceName::Buchi),
            "generalized-Buchi" => Ok(AcceptanceName::GeneralizedBuchi),
            "co-Buchi" => Ok(AcceptanceName::CoBuchi),
            "generalized-co-Buchi" => Ok(AcceptanceName::GeneralizedCoBuchi),
            "Streett" => Ok(AcceptanceName::Streett),
            "Rabin" => Ok(AcceptanceName::Rabin),
            "generalized-Rabin" => Ok(AcceptanceName::GeneralizedRabin),
            "parity" => Ok(AcceptanceName::Parity),
            "all" => Ok(AcceptanceName::All),
            "none" => Ok(AcceptanceName::None),
            val => Err(format!("Unknown acceptance type: {}", val)),
        }
    }
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
pub enum AcceptanceInfo {
    Int(Id),
    Identifier(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum HeaderItem {
    States(Id),
    Start(StateConjunction),
    AP(Vec<AtomicProposition>),
    Alias(AliasName, LabelExpression),
    Acceptance(Id, AcceptanceCondition),
    AcceptanceName(AcceptanceName, Vec<AcceptanceInfo>),
    // Correspond to tool name and optional version number
    Tool(String, Option<String>),
    Name(String),
    Properties(Vec<Property>),
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
