use std::borrow::Borrow;
use std::collections::HashMap;

use crate::lexer::tokens::{Token, TokenType};

pub struct Scanner {
    source: String,

    //start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        return Scanner {
            source,
            //start: 0,
            current: 0,
            line: 1,
        };
    }

    pub fn scan_tokens(mut self: Scanner) -> Vec<Token> {
        let mut keywords: HashMap<String, TokenType> = HashMap::new();
        let mut token: Vec<Token> = vec![];
        keywords.insert("and".to_owned(), TokenType::AND);
        keywords.insert("class".to_owned(), TokenType::CLASS);
        keywords.insert("else".to_owned(), TokenType::ELSE);
        keywords.insert("false".to_owned(), TokenType::FALSE);
        keywords.insert("for".to_owned(), TokenType::FOR);
        keywords.insert("fun".to_owned(), TokenType::FUN);
        keywords.insert("if".to_owned(), TokenType::IF);
        keywords.insert("nil".to_owned(), TokenType::NIL);
        keywords.insert("or".to_owned(), TokenType::OR);
        keywords.insert("return".to_owned(), TokenType::RETURN);
        keywords.insert("super".to_owned(), TokenType::SUPER);
        keywords.insert("this".to_owned(), TokenType::THIS);
        keywords.insert("true".to_owned(), TokenType::TRUE);
        keywords.insert("var".to_owned(), TokenType::VAR);
        keywords.insert("while".to_owned(), TokenType::WHILE);

        loop {
            let c_string_opt = self.advance();
            if c_string_opt.is_none() {
                panic!("worng symbol general")
            }
            let c_string: String = c_string_opt.unwrap();
            // println!("Symbol: {}", c_string);
            let c: &str = c_string.as_str();


            match c {
                "(" => {
                    token.push(Token::new(TokenType::LeftParen, "".to_owned()))
                }
                ")" => {
                    token.push(Token::new(TokenType::RightParen, "".to_owned()))
                }
                "{" => {
                    token.push(Token::new(TokenType::LeftBrace, "".to_owned()))
                }
                "}" => {
                    token.push(Token::new(TokenType::RightBrace, "".to_owned()))
                }
                "," => {
                    token.push(Token::new(TokenType::COMMA, "".to_owned()))
                }
                "." => {
                    token.push(Token::new(TokenType::DOT, "".to_owned()))
                }
                "-" => {
                    token.push(Token::new(TokenType::MINUS, "".to_owned()))
                }
                "+" => {
                    token.push(Token::new(TokenType::PLUS, "".to_owned()))
                }
                ";" => {
                    token.push(Token::new(TokenType::SEMICOLON, "".to_owned()))
                }
                "*" => {
                    token.push(Token::new(TokenType::STAR, "".to_owned()))
                }
                "!" => {
                    let c_string_opt = self.next();
                    if c_string_opt.is_none() {
                        panic!("wrong symbol")
                    }
                    let c_string: String = c_string_opt.unwrap();
                    let c: &str = c_string.as_str();
                    match c {
                        "=" => {
                            self.advance();
                            token.push(Token::new(TokenType::BangEqual, "".to_owned()))

                        },
                        _ => token.push(Token::new(TokenType::BANG, "".to_owned()))
                    }
                }
                "=" => {
                    let c_string_opt = self.next();
                    if c_string_opt.is_none() {
                        panic!("worng symbol =")
                    }
                    let c_string: String = c_string_opt.unwrap();
                    let c: &str = c_string.as_str();
                    match c {
                        "=" => {
                            self.advance();
                            token.push(Token::new(TokenType::EqualEqual, "".to_owned()))
                        },
                        _ => token.push(Token::new(TokenType::EQUAL, "".to_owned()))
                    }
                }
                "<" => {
                    let c_string_opt = self.next();
                    if c_string_opt.is_none() {
                        panic!("worng symbol <")
                    }
                    let c_string: String = c_string_opt.unwrap();
                    let c: &str = c_string.as_str();
                    match c {
                        "=" => {
                            self.advance();
                            token.push(Token::new(TokenType::LessEqual, "".to_owned()))
                        },
                        _ => token.push(Token::new(TokenType::LESS, "".to_owned()))
                    }
                }
                ">" => {
                    let c_string_opt = self.next();
                    if c_string_opt.is_none() {
                        panic!("worng symbol >")
                    }
                    let c_string: String = c_string_opt.unwrap();
                    let c: &str = c_string.as_str();
                    match c {
                        "=" => {
                            self.advance();
                            token.push(Token::new(TokenType::GreaterEqual, "".to_owned()))
                        },
                        _ => token.push(Token::new(TokenType::GREATER, "".to_owned()))
                    }
                }
                "/" => {
                    let c_string_opt = self.advance();
                    if c_string_opt.is_none() {
                        panic!("worng symbol /")
                    }
                    let c_string: String = c_string_opt.unwrap();
                    let c: &str = c_string.as_str();
                    match c {
                        "/" => {
                            loop {
                                let c_string_opt = self.advance();
                                if c_string_opt.is_none() {
                                    panic!("worng symbol /[2]")
                                }
                                let c_string: String = c_string_opt.unwrap();
                                if self.is_at_end() {
                                    break;
                                }
                                if c_string == "\n" {
                                    break;
                                }
                            }
                        }
                        _ => token.push(Token::new(TokenType::SLASH, "".to_owned()))
                    }
                }
                " " => {}
                "\r" => {}
                "\t" => {}
                "\n" => {
                    self.line += 1
                }
                "\"" => {
                    let mut string_v: String = String::new();
                    loop {
                        let c_string_opt = self.advance();
                        if c_string_opt.is_none() {
                            panic!("worng symbol \"")
                        }
                        let c_string: String = c_string_opt.unwrap();
                        if c_string == "\"" {
                            break;
                        }
                        string_v.push_str(&c_string);
                    }
                    token.push(Token::new(TokenType::STRING, string_v.clone()))
                }
                _ => {
                    if !Scanner::is_digit(&c_string) && !Scanner::is_alpha(&c_string) {
                        panic!("Unexpected character.")
                    }
                    let is_digit = Scanner::is_digit(&c_string);

                    let mut string_v: String = String::new();
                    string_v.push_str(&c_string);

                    match is_digit {
                        true =>
                            {
                                loop {
                                    let c_string_opt = self.advance();
                                    if c_string_opt.is_none() {
                                        break;
                                    }
                                    let c_string: String = c_string_opt.unwrap();
                                    if !Scanner::is_digit(&c_string) {
                                        self.current -= 1;
                                        break;
                                    }
                                    string_v.push_str(&c_string);
                                    break;
                                }
                                token.push(Token::new(TokenType::NUMBER, string_v.to_owned()));
                            }
                        false =>
                            {
                                loop {
                                    let c_string_opt = self.advance();
                                    if c_string_opt.is_none() {
                                        break;
                                    }
                                    let c_string: String = c_string_opt.unwrap();
                                    if c_string == " " {
                                        break;
                                    }
                                    if c_string == "=" || c_string == ">" || c_string == "<" || c_string == "+" || c_string == "-" || c_string == "*" || c_string == "/" || c_string == ")" || c_string == "(" {
                                        self.current -= 1;
                                        break;
                                    }
                                    string_v.push_str(&c_string);
                                }
                                match keywords.get(&string_v) {
                                    Some(token_type) => {
                                        let t_type = token_type.borrow().clone();
                                        token.push(Token::new(t_type, string_v.to_owned()));
                                    }
                                    None => {
                                        token.push(Token::new(TokenType::IDENTIFIER, string_v.to_owned()));
                                    }
                                }
                            }
                    }
                }
            }

            if self.is_at_end() {
                break;
            }
        }
        return token;
    }

    fn is_digit(ch: &str) -> bool {
        return ch >= "0" && ch <= "9";
    }

    fn is_alpha(ch: &str) -> bool {
        return ch >= "a" && ch <= "z" || ch >= "A" && ch <= "Z" || ch == "_";
    }


    fn advance(&mut self) -> Option<String> {
        let ch: Option<char> = self.source.chars().nth(self.current);
        match ch {
            Some(ch) => {
                self.current += 1;
                return Some(ch.to_string());
            }
            None => {
                return None;
            }
        }
    }

    fn next(&mut self) -> Option<String> {
        let ch: Option<char> = self.source.chars().nth(self.current);
        match ch {
            Some(ch) => {
                return Some(ch.to_string());
            }
            None => {
                return None;
            }
        }
    }

    fn is_at_end(&self) -> bool {
       return self.current >= self.source.len();
    }
}


#[cfg(test)]
mod tests {
    use crate::lexer::tokens::{Token, TokenType};
    use crate::lexer::parser::{Scanner};

    #[test]
    fn num_parce_simple() {
        let sc = Scanner::new("1+2".to_owned());
        let tokens: Vec<Token> = sc.scan_tokens();
        assert!(tokens[0].token_type == TokenType::NUMBER);
        assert!(tokens[1].token_type == TokenType::PLUS);
        assert!(tokens[2].token_type == TokenType::NUMBER);
        assert!(tokens[0].lexeme == "1");
        assert!(tokens[2].lexeme == "2");
    }

    #[test]
    fn num_parce() {
        let sc = Scanner::new("-11+12".to_owned());
        let tokens: Vec<Token> = sc.scan_tokens();
        assert!(tokens[0].token_type == TokenType::MINUS);
        assert!(tokens[1].token_type == TokenType::NUMBER);
        assert!(tokens[2].token_type == TokenType::PLUS);
        assert!(tokens[3].token_type == TokenType::NUMBER);
        assert!(tokens[1].lexeme == "11");
        assert!(tokens[3].lexeme == "12");
    }

    #[test]
    fn string_parce() {
        let sc = Scanner::new("\"11\" - 12".to_owned());
        let tokens: Vec<Token> = sc.scan_tokens();
        assert!(tokens[0].token_type == TokenType::STRING);
        assert!(tokens[1].token_type == TokenType::MINUS);
        assert!(tokens[2].token_type == TokenType::NUMBER);
        assert!(tokens[0].lexeme == "11");
        assert!(tokens[2].lexeme == "12");
    }

    #[test]
    fn ident_parce() {
        let sc = Scanner::new("class gg 11 or if".to_owned());
        let tokens: Vec<Token> = sc.scan_tokens();
        assert!(tokens[0].token_type == TokenType::CLASS);
        assert!(tokens[1].token_type == TokenType::IDENTIFIER);
        assert!(tokens[1].lexeme == "gg");
        assert!(tokens[2].token_type == TokenType::NUMBER);
        assert!(tokens[2].lexeme == "11");
        assert!(tokens[3].token_type == TokenType::OR);
        assert!(tokens[4].token_type == TokenType::IF);
    }

    #[test]
    fn unary_bool() {
        let sc = Scanner::new("!true==false".to_owned());
        let tokens: Vec<Token> = sc.scan_tokens();
        assert!(tokens[0].token_type == TokenType::BANG);
        assert!(tokens[1].token_type == TokenType::TRUE);
        assert!(tokens[2].token_type == TokenType::EqualEqual);
        assert!(tokens[3].token_type == TokenType::FALSE);
    }

    #[test]
    fn gram_parce() {
        let sc = Scanner::new("1 - (2 * 3) < 4 == false".to_owned());
        let tokens: Vec<Token> = sc.scan_tokens();
        assert!(tokens[0].token_type == TokenType::NUMBER);
        assert!(tokens[1].token_type == TokenType::MINUS);
        assert!(tokens[2].token_type == TokenType::LeftParen);
        assert!(tokens[3].token_type == TokenType::NUMBER);
        assert!(tokens[4].token_type == TokenType::STAR);
        assert!(tokens[5].token_type == TokenType::NUMBER);
        assert!(tokens[6].token_type == TokenType::RightParen);
        assert!(tokens[7].token_type == TokenType::LESS);
        assert!(tokens[8].token_type == TokenType::NUMBER);
        assert!(tokens[9].token_type == TokenType::EqualEqual);
        assert!(tokens[10].token_type == TokenType::FALSE);
    }

    #[test]
    fn gram_parce_no_spaces() {
        let sc = Scanner::new("1-(2*3)<4==false".to_owned());
        let tokens: Vec<Token> = sc.scan_tokens();
        assert!(tokens[0].token_type == TokenType::NUMBER);
        assert!(tokens[1].token_type == TokenType::MINUS);
        assert!(tokens[2].token_type == TokenType::LeftParen);
        assert!(tokens[3].token_type == TokenType::NUMBER);
        assert!(tokens[4].token_type == TokenType::STAR);
        assert!(tokens[5].token_type == TokenType::NUMBER);
        assert!(tokens[6].token_type == TokenType::RightParen);
        assert!(tokens[7].token_type == TokenType::LESS);
        assert!(tokens[8].token_type == TokenType::NUMBER);
        assert!(tokens[9].token_type == TokenType::EqualEqual);
        assert!(tokens[10].token_type == TokenType::FALSE);
    }

    #[test]
    fn less() {
        let sc = Scanner::new("1<1".to_owned());
        let tokens: Vec<Token> = sc.scan_tokens();
        assert!(tokens[0].token_type == TokenType::NUMBER);
        assert!(tokens[1].token_type == TokenType::LESS);
        assert!(tokens[2].token_type == TokenType::NUMBER);
    }

    #[test]
    fn less_eq() {
        let sc = Scanner::new("1<=1".to_owned());
        let tokens: Vec<Token> = sc.scan_tokens();
        assert!(tokens[0].token_type == TokenType::NUMBER);
        assert!(tokens[1].token_type == TokenType::LessEqual);
        assert!(tokens[2].token_type == TokenType::NUMBER);
    }

    #[test]
    fn greater_eq() {
        let sc = Scanner::new("1>=1".to_owned());
        let tokens: Vec<Token> = sc.scan_tokens();
        assert!(tokens[0].token_type == TokenType::NUMBER);
        assert!(tokens[1].token_type == TokenType::GreaterEqual);
        assert!(tokens[2].token_type == TokenType::NUMBER);
    }
}
