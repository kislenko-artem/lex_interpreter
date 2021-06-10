use std::collections::HashMap;

use crate::tokens::{Token, TokenType};

enum ExpressionType {
    // NUMBER | STRING | "true" | "false" | "nil" ;
    Literal,
    // "(" expression ")"
    Grouping,
    // ( "-" | "!" ) expression
    Unary,
    // expression operator expression
    Binary,
    // "==" | "!=" | "<" | "<=" | ">" | ">=" | "+"  | "-"  | "*" | "/"
    Operator,
}

struct Expression {
    type_expr: ExpressionType,
    left: Option<*const Expression>,
    right: Option<*const Expression>,
    operator: Token,
}

impl Expression {
    fn default() -> Self {
        return Expression {
            type_expr: ExpressionType::Binary,
            left: None,
            right: None,
            operator: Token::new(TokenType::START, "".to_owned()),
        };
    }

    fn binary(left: Option<*const Expression>, right: Option<*const Expression>, operator: Token) -> Self {
        return Expression {
            type_expr: ExpressionType::Binary,
            left,
            right,
            operator,
        };
    }
}

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
        let mut counter: usize = 0;
        let mut expression: Expression;
        loop {
            let tkn = &self.tokens[counter];
            match tkn.token_type {
                TokenType::EqualEqual => {
                    // TODO: check counter > 0
                    //let prev_expr = self.tokens[counter - 1];
                    //Expression::binary(Option(prev_expr), )
                }
                TokenType::BangEqual => {}
                TokenType::GREATER => {}
                TokenType::GreaterEqual => {}
                TokenType::LESS => {}
                TokenType::LessEqual => {}
                TokenType::MINUS => {}
                TokenType::PLUS => {}
                TokenType::SLASH => {}
                TokenType::STAR => {}
                _ => {}
            }
        }
    }
}