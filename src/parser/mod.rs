use crate::tokens::{Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    current: u32,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        return Parser {
            tokens,
            current: 0,
        };
    }

    pub fn expression(self) {

    }

}