pub mod utils;

pub use utils::*;

mod lexer;
pub mod parser;
pub mod consumer;

use lexer::{TokenType, Token, HoaLexer};

use std::string::String;

pub use consumer::HoaConsumer;
pub use parser::HoaParser;

#[cfg(test)]
mod tests {
    use super::*;
}
