mod format;

#[allow(unused_imports)]
use chumsky::prelude::*;

use chumsky::{
    prelude::Simple,
    primitive::{filter, just},
    text::{self, TextParser},
    Parser,
};
use format::{AcceptanceName, AliasName, HeaderItem, HoaAutomaton, LabelExpression};

pub fn parser() -> impl Parser<char, HoaAutomaton, Error = Simple<char>> {
    let string = just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .labelled("string");

    let int = text::int(10).map(|s: String| s.parse().unwrap()).padded();

    let version = just("HOA:").padded().ignore_then(text::ident().padded());

    let states = just("States:")
        .padded()
        .ignore_then(
            text::int(10)
                .padded()
                .map(|s: String| HeaderItem::States(s.parse().unwrap())),
        )
        .padded();

    let ap = just("AP:")
        .ignore_then(int)
        .then(string.padded().repeated())
        .try_map(|(len, aps), span| {
            if aps.len() == (len as usize) {
                Ok(aps)
            } else {
                Err(Simple::custom(span, "Number of APs does not match"))
            }
        })
        .map(|aps| HeaderItem::AP(aps))
        .padded();

    let op = |c| just(c).padded();

    let alias_name = just("@").ignore_then(text::ident()).padded();

    let boolean = filter(|c| *c == 't' || *c == 't')
        .map(|sym: char| {
            if sym == 't' {
                true
            } else if sym == 'f' {
                false
            } else {
                unreachable!()
            }
        })
        .padded();

    let label_expression = recursive(|expr| {
        let atom = int
            .map(|i: u32| LabelExpression::Integer(i))
            .or(boolean.map(|b: bool| LabelExpression::Boolean(b)))
            .or(alias_name.map(|an: AliasName| LabelExpression::Alias(an)));

        let negation = just("!")
            .padded()
            .ignore_then(expr)
            .map(|e| LabelExpression::Not(Box::new(e)));

        let subformula = atom.clone().or(negation.clone());

        let binary = subformula
            .clone()
            .then(
                op("|")
                    .to(LabelExpression::Or as fn(_, _) -> _)
                    .or(op("&").to(LabelExpression::And as fn(_, _) -> _))
                    .then(subformula)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        binary
    });

    let alias = just("Alias:")
        .ignore_then(alias_name.then(label_expression))
        .map(|(alias_name, expression)| HeaderItem::Alias(alias_name, expression))
        .padded();

    let acceptance_identifier = just("Buchi")
        .to(AcceptanceName::Buchi)
        .or(just("generalized-Buchi").to(AcceptanceName::GeneralizedBuchi))
        .or(just("co-Buchi").to(AcceptanceName::CoBuchi))
        .or(just("generalized-co-Buchi").to(AcceptanceName::GeneralizedCoBuchi))
        .or(just("Streett").to(AcceptanceName::Streett))
        .or(just("Rabin").to(AcceptanceName::Rabin))
        .or(just("generalized-Rabin").to(AcceptanceName::GeneralizedRabin))
        .or(just("parity").to(AcceptanceName::Parity))
        .or(just("all").to(AcceptanceName::All))
        .or(just("none").to(AcceptanceName::None))
        .padded();

    let acceptance_name = just("acc-name:")
        .ignore_then(acceptance_identifier)
        .map(|name| HeaderItem::acceptance_name(name))
        .padded();

    version
        .then(states.or(alias).or(ap).or(acceptance_name).repeated())
        .map(HoaAutomaton::new)
        .then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_test_vec(input: &str, headers: Vec<HeaderItem>) {
        let hoa = format!("HOA: v1 \n{}", input);
        match parser().parse_recovery_verbose(hoa.as_str()) {
            (Some(aut), _) => aut.assert_headers(headers),
            (None, errs) => assert!(false, "{:?}", errs),
        }
    }

    fn parse_test(input: &str, header: HeaderItem) {
        parse_test_vec(input, vec![header])
    }

    fn fails(input: &str) {
        let hoa = format!("HOA: v1 \n {}", input);
        assert!(parser().parse(hoa.as_str()).is_err());
    }

    #[test]
    fn aps() {
        parse_test(
            r#"AP: 2 "a" "bb""#,
            HeaderItem::AP(vec!["a".to_string(), "bb".to_string()]),
        );

        assert!(parser().parse(r#"AP: 1 "a" "b""#).is_err());
        assert!(parser().parse(r#"AP: 3 "a" "b""#).is_err());
    }

    #[test]
    fn acc_name() {
        parse_test(
            r#"acc-name: Buchi"#,
            HeaderItem::acceptance_name(AcceptanceName::Buchi),
        );
        fails(r#"acc-name::"#);
        fails(r#"acc-nme:"#);
        fails(r#"acc-name: Buch"#);
    }

    #[test]
    fn aliases() {
        parse_test(
            r#"Alias: @a 0"#,
            HeaderItem::alias("a", LabelExpression::int(0)),
        );

        parse_test(
            r#"Alias: @c @ps|@a2"#,
            HeaderItem::alias(
                "c",
                LabelExpression::or(LabelExpression::alias("ps"), LabelExpression::alias("a2")),
            ),
        )
    }

    #[test]
    fn real_test_1() {
        let contents = r#"HOA: v1
             AP: 2 "a"
             States: 3
             Start: 0
             acc-name: Buchi
             Acceptance: 1 Inf(0)
             --BODY--
             State: 0 {0}
              [0] 1
              [!0]  2
             State: 1  /* former state 0 */
              [0] 1
              [!0] 2
             State: 2 {0}
              [0] 1
              [!0] 2
             --END--
             "#;
        println!("{:?}", parser().parse(contents));
    }
}
