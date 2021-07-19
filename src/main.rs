use std::io::{self, BufRead};
use crate::lexer::tokens::Token;
use crate::lexer::ast::{Parser, Statement, Environment};
use crate::lexer::parser::{Scanner};

mod lexer;

pub struct Lox {}

impl Lox {
    fn run_prompt() {
        let mut env: Environment = Environment::new();
        let stdin = io::stdin();
        for line in stdin.lock().lines() {
            let r_line = line.unwrap();
            if r_line == "" {
                break;
            }
            Lox::run(r_line, &mut env);
        }
    }

    fn run(line: String, mut env: &mut Environment) {
        let sc = Scanner::new(line.to_owned());
        let tokens: Vec<Token> = sc.scan_tokens();
        let mut prsr: Parser = Parser::new(tokens);
        //let tree = prsr.expression();
        //println!("Result: {:?}", Expression::execute(tree.clone()));
        let stmts = prsr.statement(&mut env);
        Statement::execute(stmts, &mut env);
    }
}


fn main() {
    Lox::run_prompt();
}
