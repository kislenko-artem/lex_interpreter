use std::io::{self, BufRead};

mod scan;

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
        let sc = scan::Scanner::new(line.to_owned());
        sc.scan_tokens();
        println!("Tokens: {}", line);
    }
}


fn main() {
    Lox::run_prompt();
}
