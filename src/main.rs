use std::io::{self, BufRead};
use crate::tokens::Token;
use crate::parser::{Parser, Expression};

mod scaner;
mod tokens;
mod parser;

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
        let sc = scaner::Scanner::new(line.to_owned());
        let tokens: Vec<Token> = sc.scan_tokens();
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        println!("Result: {:?}", Expression::execute(tree));
    }
}


fn main() {
    Lox::run_prompt();
}
