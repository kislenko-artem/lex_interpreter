use crate::tokens::{Token, TokenType};

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Literal {
    // The raw values available
    Float(f64),
    //Int(i64),
    Str(String),
    True(bool),
    False(bool),
    Nil,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    // The possible operators for the binary and unary expression
    BangEqual,
    EqualEqual,
    LessThan,
    GreaterThan,
    Plus,
    Minus,
    Comma,
    Star,
    Slash,
    Bang,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Binary {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
    Unary {
        operator: Operator,
        expr: Box<Expression>,
    },
    Literal(Literal),
}


#[derive(Debug, PartialEq, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

fn get_operator(token: TokenType) -> Operator {
    match token {
        TokenType::BangEqual => Operator::BangEqual,
        TokenType::EqualEqual => Operator::EqualEqual,
        TokenType::LessEqual => Operator::LessThan,
        //TokenType::LESSTHANEQUAL => Operator::LessThanEqual,
        TokenType::GreaterEqual => Operator::GreaterThan,
        //TokenType::GREATERTHANEQUAL => Operator::GreaterThanEqual,
        TokenType::PLUS => Operator::Plus,
        TokenType::MINUS => Operator::Minus,
        TokenType::STAR => Operator::Star,
        TokenType::SLASH => Operator::Slash,
        //TokenType::MODULO => Operator::Modulo,
        //TokenType::EXPONENTIAL => Operator::Exponential,
        TokenType::COMMA => Operator::Comma,
        TokenType::BANG => Operator::Bang,
        _ => unreachable!(),
    }
}


impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        return Parser {
            tokens,
            current: 0,
        };
    }

    pub fn expression(&mut self) -> Expression {
        return self.equality();
    }

    fn math(&mut self, tokens: Vec<TokenType>) -> bool {
        if self.current >= self.tokens.len() - 1 {
            return false;
        }
        for (i, source_token) in self.tokens.iter().enumerate() {
            if i < self.current {
                continue;
            }
            for cond_token in tokens.iter() {
                if *cond_token == source_token.token_type {
                    self.current += 1;
                    return true;
                }
            }
        }
        // self.current += 1;
        return false;
    }

    fn equality(&mut self) -> Expression {
        let mut expr = self.comparison();
        if self.current >= self.tokens.len() - 1 {
            return expr;
        }
        let operator = get_operator(self.tokens[self.current].token_type);
        while self.math(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
            let right_expr = self.comparison();
            expr = Expression::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right_expr),
            }
        }

        return expr;
    }

    fn comparison(&mut self) -> Expression {
        let mut expr = self.term();
        if self.current >= self.tokens.len() - 1 {
            return expr;
        }

        let operator = get_operator(self.tokens[self.current].token_type);
        while self.math(vec![TokenType::GREATER, TokenType::GreaterEqual, TokenType::LESS, TokenType::LessEqual]) {
            let right_expr = self.term();
            expr = Expression::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right_expr),
            }
        }

        return expr;
    }

    fn term(&mut self) -> Expression {
        let mut expr = self.factor();
        if self.current >= self.tokens.len() - 1 {
            return expr;
        }

        let operator = get_operator(self.tokens[self.current].token_type);
        while self.math(vec![TokenType::MINUS, TokenType::PLUS]) {
            let right_expr = self.factor();
            expr = Expression::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right_expr),
            }
        }

        return expr;
    }

    fn factor(&mut self) -> Expression {
        let mut expr = self.unary();
        if self.current >= self.tokens.len() - 1 {
            return expr;
        }

        let operator = get_operator(self.tokens[self.current].token_type);
        while self.math(vec![TokenType::SLASH, TokenType::STAR]) {
            let right_expr = self.unary();
            expr = Expression::Binary {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right_expr),
            }
        }

        return expr;
    }

    fn unary(&mut self) -> Expression {
        if self.math(vec![TokenType::BANG]) {
            let operator = get_operator(self.tokens[self.current].token_type);
            let expr = self.unary();
            return Expression::Unary {
                expr: Box::new(expr),
                operator: operator,
            };
        }
        return self.primary();
    }

    fn primary(&mut self) -> Expression {
        let tkn = &self.tokens[self.current];
        self.current += 1;
        match tkn.token_type {
            TokenType::FALSE => {
                return Expression::Literal(Literal::False(false));
            }
            TokenType::TRUE => {
                return Expression::Literal(Literal::True(true));
            }
            TokenType::NIL => {
                return Expression::Literal(Literal::Nil);
            }
            // TokenType::INT(ref i) => {
            //     return Expression::Literal(Literal::Int(*i))
            // }
            TokenType::NUMBER => {
                let f = tkn.lexeme.parse::<f64>();
                match f {
                    Ok(val) => {
                        return Expression::Literal(
                            Literal::Float(val)
                        );
                    }
                    Err(_) => {
                        panic!("wrong number: {}", tkn.lexeme)
                    }
                }
            }
            TokenType::STRING => {
                return Expression::Literal(Literal::Str(tkn.lexeme.to_owned()));
            }
            // TokenType::LPAREN => {
            //     // return Expression::Literal(Literal::False(false))
            // }
            _ => {
                panic!("wrong token {:?}", tkn.token_type)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{Parser};
    use crate::tokens::{Token, TokenType};

    #[test]
    fn equal_equal_parce() {
        let tokens = vec![
            Token::new(TokenType::NUMBER, "2".to_owned()),
            Token::new(TokenType::EqualEqual, "==".to_owned()),
            Token::new(TokenType::NUMBER, "3".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        println!("{:?}", tree);
    }

    #[test]
    fn plus_parce() {
        let tokens = vec![
            Token::new(TokenType::NUMBER, "2".to_owned()),
            Token::new(TokenType::PLUS, "+".to_owned()),
            Token::new(TokenType::NUMBER, "3".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        println!("{:?}", tree);
    }
}