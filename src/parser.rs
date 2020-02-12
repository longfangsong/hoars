use super::HoaLexer;
use super::HoaConsumer;

pub struct HoaParser<C: HoaConsumer> {
    consumer: C,
}

impl<C: HoaConsumer> HoaParser<C> {
    fn new(consumer: C) -> HoaParser<C> {
        HoaParser {
            consumer,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trait_test() {

    }
}