mod body;
mod format;
mod header;
mod lexer;
mod value;

use ariadne::{Color, Fmt, Label, ReportKind, Source};

#[allow(unused_imports)]
use chumsky::prelude::*;
pub use format::*;

use chumsky::{prelude::Simple, Parser, Stream};
pub use format::{
    AcceptanceCondition, AcceptanceInfo, AcceptanceName, AcceptanceSignature, AliasName,
    LabelExpression, Property,
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

fn process(input: &str) -> Result<Vec<HeaderItem>, Vec<String>> {
    let (tokens, errs) = lexer::tokenizer().parse_recovery(input);
    if let Some(tokens) = tokens {
        let len = input.chars().count();
        let (ast, parse_errs) = header::parser()
            .then_ignore(end())
            .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

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
