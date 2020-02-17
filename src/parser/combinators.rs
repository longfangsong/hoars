use crate::lexer::Token;

type ParseResult<'a, Output> = Result<(&'a [Token<'a>], Output), &'a str>;

trait CParser<'a, Output> {
    fn parse(&self, input: &'a [Token<'a>]) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> CParser<'a, Output> for F
where
    F: Fn(&'a [Token<'a>]) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a [Token<'a>]) -> ParseResult<'a, Output> {
        self(input)
    }
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl CParser<'a, (R1, R2)>
where
    P1: CParser<'a, R1>,
    P2: CParser<'a, R2>,
{
    move |input| match parser1.parse(input) {
        Ok((next_input, result1)) => match parser2.parse(next_input) {
            Ok((final_input, result2)) => Ok((final_input, (result1, result2))),
            Err(err) => Err(err),
        },
        Err(err) => Err(err),
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl CParser<'a, B>
where
    P: CParser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl CParser<'a, R1>
where
    P1: CParser<'a, R1>,
    P2: CParser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl CParser<'a, R2>
where
    P1: CParser<'a, R1>,
    P2: CParser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

fn one_or_more<'a, P, A>(parser: P) -> impl CParser<'a, Vec<A>>
where
    P: CParser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        if let Ok((next_input, first_item)) = parser.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err("scheiße gelaufen, keinen gematched");
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

fn zero_or_more<'a, P, A>(parser: P) -> impl CParser<'a, Vec<A>>
where
    P: CParser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pair_combinator() {}
}
