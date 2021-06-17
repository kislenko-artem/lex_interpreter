use std::io::{self, BufRead};
use crate::lexer::tokens::Token;
use crate::lexer::ast::{Parser, Expression};
use crate::lexer::parser::{Scanner};

mod lexer;

pub struct Lox {}

impl Lox {
    fn run_prompt() {
        let stdin = io::stdin();
        for line in stdin.lock().lines() {
            let r_line = line.unwrap();
            if r_line == "" {
                break;
            }
            Lox::run(r_line);
        }
    }

    fn run(line: String) {
        let sc = Scanner::new(line.to_owned());
        let tokens: Vec<Token> = sc.scan_tokens();
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        println!("Result: {:?}", Expression::execute(tree));
    }
}


fn main() {
    Lox::run_prompt();
}
