mod format;
mod header;
mod lexer;
mod value;

use ariadne::{Color, Fmt, Label, ReportKind, Source};

#[allow(unused_imports)]
use chumsky::prelude::*;
pub use format::*;

use chumsky::{prelude::Simple, primitive::just, Parser, Stream};
pub use format::{
    AcceptanceCondition, AcceptanceInfo, AcceptanceName, AliasName, LabelExpression, Property,
};
pub use header::HeaderItem;
use lexer::Token;

pub type Id = u32;

#[derive(Debug)]
pub struct HoaAutomaton {
    version: String,
    headers: Vec<HeaderItem>,
}

impl HoaAutomaton {
    pub fn new((version, headers): (String, Vec<HeaderItem>)) -> Self {
        Self { version, headers }
    }
}

fn header_parser() -> impl Parser<Token, Vec<HeaderItem>, Error = Simple<Token>> {
    let required_hoa = just(Token::Header("HOA".to_string()))
        .ignore_then(just(Token::Identifier("v1".to_string())));

    required_hoa
        .ignore_then(header::item().repeated().at_least(1))
        .then_ignore(end())
}

fn process(input: &str) -> Result<Vec<HeaderItem>, Vec<String>> {
    let (tokens, errs) = lexer::tokenizer().parse_recovery(input);
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
