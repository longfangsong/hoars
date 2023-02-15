mod format;

use std::{fmt::Display, hash::Hash};

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
#[allow(unused_imports)]
use chumsky::prelude::*;

use chumsky::{
    prelude::Simple,
    primitive::{filter, just},
    text::{self, Character, TextParser},
    Parser, Stream,
};
use format::{
    AcceptanceCondition, AcceptanceInfo, AcceptanceName, AliasName, HeaderItem, HoaAutomaton,
    LabelExpression, Property,
};

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    Null,
    Bool(bool),
    Int(String),
    Text(String),
    Identifier(String),
    Alias(String),
    Header(String),
    Op(char),
    Ctrl(char),
    Fin,
    Inf,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Null => write!(f, "null"),
            Token::Bool(b) => write!(f, "{}", b),
            Token::Int(n) => write!(f, "{}", n),
            Token::Text(txt) => write!(f, "{}", txt),
            Token::Identifier(id) => write!(f, "{}", id),
            Token::Alias(alias) => write!(f, "@{}", alias),
            Token::Header(hdr) => write!(f, "{}:", hdr),
            Token::Op(o) => write!(f, "{}", o),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Fin => write!(f, "Fin"),
            Token::Inf => write!(f, "Inf"),
        }
    }
}

fn header<I: Display>(name: I) -> Token {
    Token::Header(name.to_string())
}

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let int = text::int(10).map(Token::Int);

    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Text);

    let bool = one_of("tf").try_map(|s, span| match s {
        't' => Ok(Token::Bool(true)),
        'f' => Ok(Token::Bool(false)),
        _ => unreachable!(),
    });

    let op = one_of("!|&").map(Token::Op);

    let paren = one_of(r#"(){}[]"#).map(Token::Ctrl);

    let raw_ident = filter(|c: &char| c.is_ascii_alphabetic() || *c == '_')
        .chain(filter(|c: &char| c.is_ascii_alphanumeric() || *c == '_' || *c == '-').repeated())
        .collect::<String>();

    let ident = raw_ident.map(|ident: String| match ident.as_str() {
        "Fin" => Token::Fin,
        "Inf" => Token::Inf,
        _ => Token::Identifier(ident),
    });

    let alias = just('@').ignore_then(raw_ident).map(Token::Alias);

    let header = ident
        .then_ignore(just(':'))
        .map(|header_name| Token::Header(header_name.to_string()));

    let token = int
        .or(header)
        .or(str_)
        .or(op)
        .or(paren)
        .or(ident)
        .or(bool)
        .or(alias)
        .recover_with(skip_then_retry_until([]));

    let comment = just("/*").then(take_until(just("*/"))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}

fn single_header_parser() -> impl Parser<Token, HeaderItem, Error = Simple<Token>> {
    let int = select! {
        Token::Int(n) => n.parse().unwrap()
    };
    let boolean = select! {
        Token::Bool(b) => b,
    };
    let ident = select! { Token::Identifier(ident) => ident.clone() };
    let string = select! { Token::Text(txt) => txt.clone() };
    let alias_name = select! { Token::Alias(aname) => aname.clone() };

    let acceptance_info = select! {
        Token::Identifier(ident) => AcceptanceInfo::Identifier(ident),
        Token::Int(n) => AcceptanceInfo::Int(n.parse().unwrap()),
    };

    let states = just(header("States"))
        .ignore_then(int)
        .map(HeaderItem::States);

    let acceptance_name = just(header("acc-name"))
        .ignore_then(ident)
        .try_map(|identifier, span| {
            AcceptanceName::try_from(identifier).map_err(|e| Simple::custom(span, e))
        })
        .then(acceptance_info.repeated())
        .map(|(name, info)| HeaderItem::AcceptanceName(name, info));

    let start = just(header("Start"))
        .ignore_then(int.separated_by(just(Token::Op('&'))))
        .map(HeaderItem::Start);

    let aps = just(header("AP"))
        .ignore_then(int)
        .then(string.repeated())
        .try_map(|(num, prop_labels), span| {
            if (num as usize) == prop_labels.len() {
                Ok(HeaderItem::AP(prop_labels))
            } else {
                Err(Simple::custom(
                    span,
                    format!("Expected {} aps but got {}", num, prop_labels.len()),
                ))
            }
        });

    let label_expression = recursive(|label_expression| {
        let value = boolean
            .map(|b| LabelExpression::Boolean(b))
            .or(int.map(|n| LabelExpression::Integer(n)))
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
    });

    let alias = just(header("Alias"))
        .ignore_then(alias_name)
        .then(label_expression)
        .map(|(aname, expression)| HeaderItem::Alias(aname, expression));

    let name = just(header("name"))
        .ignore_then(string)
        .map(|s| HeaderItem::Name(s.clone()));

    let tool = just(header("tool"))
        .ignore_then(string)
        .then(string.or_not())
        .map(|(tool, version)| HeaderItem::Tool(tool, version));

    let properties = just(header("properties"))
        .ignore_then(
            ident
                .try_map(|p, span| {
                    Property::try_from(p).map_err(|err| Simple::custom(span, err.clone()))
                })
                .repeated()
                .at_least(1),
        )
        .map(HeaderItem::Properties);

    states
        .or(acceptance_name)
        .or(start)
        .or(aps)
        .or(alias)
        .or(name)
        .or(tool)
        .or(properties)
}

fn header_parser() -> impl Parser<Token, Vec<HeaderItem>, Error = Simple<Token>> {
    let required_hoa = just(header("HOA")).ignore_then(just(Token::Identifier("v1".to_string())));

    required_hoa
        .ignore_then(single_header_parser().repeated().at_least(1))
        .then_ignore(end())
}

fn process(input: &str) -> Result<Vec<HeaderItem>, Vec<String>> {
    let (tokens, errs) = lexer().parse_recovery(input);
    if let Some(tokens) = tokens {
        let len = input.chars().count();
        let (ast, parse_errs) =
            header_parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        if parse_errs.is_empty() && ast.is_some() {
            let out = ast.unwrap();
            Ok(out)
        } else {
            Err(make_report(
                input,
                errs.into_iter()
                    .map(|err| err.map(|c| c.to_string()))
                    .chain(parse_errs.into_iter().map(|err| err.map(|c| c.to_string()))),
            ))
        }
    } else {
        Err(make_report(
            input,
            errs.into_iter().map(|err| err.map(|c| c.to_string())),
        ))
    }
}

fn make_report<I: Iterator<Item = Simple<String>>>(input: &str, errs: I) -> Vec<String> {
    errs.into_iter()
        .map(|e| {
            let report = ariadne::Report::build(ReportKind::Error, (), e.span().start);

            let report = match e.reason() {
                chumsky::error::SimpleReason::Unexpected => report
                    .with_message(format!(
                        "{}, expected {}",
                        if e.found().is_some() {
                            "Unexpected token in input"
                        } else {
                            "Unexpected end of input"
                        },
                        if e.expected().len() == 0 {
                            "something else".to_string()
                        } else {
                            e.expected()
                                .map(|expected| match expected {
                                    Some(expected) => expected.to_string(),
                                    None => "end of input".to_string(),
                                })
                                .collect::<Vec<_>>()
                                .join(", ")
                        }
                    ))
                    .with_label(
                        Label::new(e.span())
                            .with_message(format!(
                                "Unexpected token {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                    .with_message(format!(
                        "Unclosed delimiter {}",
                        delimiter.fg(Color::Yellow)
                    ))
                    .with_label(
                        Label::new(span.clone())
                            .with_message(format!(
                                "Unclosed delimiter {}",
                                delimiter.fg(Color::Yellow)
                            ))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new(e.span())
                            .with_message(format!(
                                "Must be closed before this {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                    Label::new(e.span())
                        .with_message(format!("{}", msg.fg(Color::Red)))
                        .with_color(Color::Red),
                ),
            };

            let mut report_output = Vec::new();
            report
                .finish()
                .write(Source::from(input), &mut report_output)
                .unwrap();

            std::str::from_utf8(&report_output)
                .unwrap_or_else(|_| "Could not parse error report")
                .to_string()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;

    fn assert_header(input: &str, cmp: &[HeaderItem]) {
        match process(&format!("HOA: v1 {}", input)) {
            Ok(res) => assert_eq!(res, cmp),
            Err(err) => {
                eprintln!("{}", err.into_iter().join("\n"));
                assert!(false)
            }
        }
    }

    fn assert_fails(input: &str) {
        assert!(process(&format!("HOA: v1 {}", input)).is_err())
    }

    #[test]
    fn property() {
        assert_header(
            r#"properties: trans-labels state-labels"#,
            &[HeaderItem::Properties(vec![
                Property::TransLabels,
                Property::StateLabels,
            ])],
        );
        assert_fails(r#"properties: trans-labels statelabels"#);
        assert_fails(r#"properties: "#);
    }

    #[test]
    fn tool_and_name() {
        assert_header(
            r#"
                tool: "ltl-translate" "1.2-alpha"
                name: "BA for GFa & GFb"
            "#,
            &[
                HeaderItem::Tool("ltl-translate".to_string(), Some("1.2-alpha".to_string())),
                HeaderItem::Name("BA for GFa & GFb".to_string()),
            ],
        );
        assert_fails("tool: ltl-translate \"1.2-alpha\"");
    }

    #[test]
    fn ariadne() {
        assert_header(
            "acc-name: Rabin 3",
            &[HeaderItem::AcceptanceName(
                AcceptanceName::Rabin,
                vec![AcceptanceInfo::Int(3)],
            )],
        );
        assert_header("Start: 0 & 7", &[HeaderItem::Start(vec![0, 7])]);
    }

    #[test]
    fn aps() {
        assert_header(
            r#"AP: 3 "a" "proc@state" "a[x] >= 2""#,
            &[HeaderItem::AP(vec![
                "a".to_string(),
                "proc@state".to_string(),
                "a[x] >= 2".to_string(),
            ])],
        )
    }

    #[test]
    fn alias() {
        assert_header(
            "Alias: @a 0",
            &[HeaderItem::Alias(
                "a".to_string(),
                LabelExpression::Integer(0),
            )],
        );
        assert_header(
            "Alias: @a 0 & 1",
            &[HeaderItem::Alias(
                "a".to_string(),
                LabelExpression::And(vec![
                    LabelExpression::Integer(0),
                    LabelExpression::Integer(1),
                ]),
            )],
        );
        assert_header(
            "Alias: @a 0 & 1 | 2",
            &[HeaderItem::Alias(
                "a".to_string(),
                LabelExpression::Or(vec![
                    LabelExpression::And(vec![
                        LabelExpression::Integer(0),
                        LabelExpression::Integer(1),
                    ]),
                    LabelExpression::Integer(2),
                ]),
            )],
        );
        assert_header(
            "Alias: @a 0 | 1 & 2",
            &[HeaderItem::Alias(
                "a".to_string(),
                LabelExpression::Or(vec![
                    LabelExpression::Integer(0),
                    LabelExpression::And(vec![
                        LabelExpression::Integer(1),
                        LabelExpression::Integer(2),
                    ]),
                ]),
            )],
        );
        assert_header(
            "Alias: @a (0 | 1) & 2",
            &[HeaderItem::Alias(
                "a".to_string(),
                LabelExpression::And(vec![
                    LabelExpression::Or(vec![
                        LabelExpression::Integer(0),
                        LabelExpression::Integer(1),
                    ]),
                    LabelExpression::Integer(2),
                ]),
            )],
        )
    }

    #[test]
    fn multiple_headers() {}

    // fn parse_header(input: &str) -> HeaderItem {
    //     header_parser()
    //         .parse(lexer().parse(input).unwrap())
    //         .unwrap()
    // }

    // #[test]
    // fn states() {
    //     assert_eq!(parse_header("States: 7"), HeaderItem::States(7));
    // }

    // #[test]
    // fn acceptance_name() {
    //     assert_eq!(
    //         parse_header("acc-name: Rabin 3"),
    //         HeaderItem::AcceptanceName(AcceptanceName::Rabin, vec![AcceptanceInfo::Int(3)])
    //     )
    // }

    // #[test]
    // fn aps() {
    //     parse_test(
    //         "AP: 2 \"a\" \"bb\"",
    //         HeaderItem::AP(vec!["a".to_string(), "bb".to_string()]),
    //     );

    //     assert!(parser().parse("AP: 1 \"a\" \"b\"").is_err());
    //     assert!(parser().parse("AP: 3 \"a\" \"b\"").is_err());
    // }

    // #[test]
    // fn acceptance() {
    //     parse_test(
    //         "Acceptance: 2 Fin(!0) & Inf(1)",
    //         HeaderItem::Acceptance(
    //             2,
    //             AcceptanceCondition::And(vec![
    //                 AcceptanceCondition::Fin((false, 0)),
    //                 AcceptanceCondition::Inf((true, 1)),
    //             ]),
    //         ),
    //     )
    // }

    // #[test]
    // fn acc_name() {
    //     parse_test(
    //         "acc-name: Buchi",
    //         HeaderItem::acceptance_name(AcceptanceName::Buchi),
    //     );
    //     fails("acc-name::");
    //     fails("acc-nme:");
    //     fails("acc-name: Buch");
    // }

    // #[test]
    // fn aliases() {
    //     parse_test(
    //         "Alias: @a 0",
    //         HeaderItem::alias("a", LabelExpression::int(0)),
    //     );

    //     parse_test(
    //         "Alias: @c @ps|@a2",
    //         HeaderItem::alias(
    //             "c",
    //             LabelExpression::or(LabelExpression::alias("ps"), LabelExpression::alias("a2")),
    //         ),
    //     )
    // }

    // #[test]
    // fn real_test_1() {
    //     let contents = r#"HOA: v1
    //          AP: 2 "a"
    //          States: 3
    //          Start: 0
    //          acc-name: Buchi
    //          Acceptance: 1 Inf(0)
    //          --BODY--
    //          State: 0 {0}
    //           [0] 1
    //           [!0]  2
    //          State: 1  /* former state 0 */
    //           [0] 1
    //           [!0] 2
    //          State: 2 {0}
    //           [0] 1
    //           [!0] 2
    //          --END--
    //          "#;
    //     println!("{:?}", parser().parse(contents));
    // }
}
