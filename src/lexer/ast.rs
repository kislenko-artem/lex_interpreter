use crate::lexer::tokens::{Token, TokenType};
use std::collections::HashMap;

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Literal {
    // The raw values available
    Float(f64),
    IDENTIFIER(String),
    //Int(i64),
    Str(String),
    Bool(bool),
    Nil,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    // The possible operators for the binary and unary expression
    BangEqual,
    EqualEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Plus,
    Minus,
    Comma,
    Star,
    Slash,
    Bang,
    UnaryMinus,
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
    Grouping { expr: Box<Expression> },
    Assigment{
        name: String,
        value: Box<Expression>,
    },
}

impl Expression {
    pub fn execute(expr: Expression, env: &mut Environment) -> Literal {

        match expr {
            Expression::Binary {left, operator, right} => {
                let exr_one = Expression::execute(*left, env);
                let exr_two = Expression::execute(*right, env);
                match operator {
                    Operator::Plus => {
                        match exr_one {
                            Literal::Float(v1) => {
                                match exr_two {
                                    Literal::Float(v2) => {
                                        return Literal::Float(v1 + v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Float)")
                                    }
                                }
                            }
                            Literal::Str(v1) => {
                                match exr_two {
                                    Literal::Str(v2) => {
                                        let mut new_v = v1;
                                        new_v.push_str(v2.as_str());
                                        return Literal::Str(new_v);
                                    }
                                    _ => {
                                        panic!("wrong format (Str)")
                                    }
                                }

                            }
                            _ => {
                                panic!("wrong operation")
                            }
                        }
                    }
                    Operator::Minus => {
                        match exr_one {
                            Literal::Float(v1) => {
                                match exr_two {
                                    Literal::Float(v2) => {
                                        return Literal::Float(v1 - v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Float)")
                                    }
                                }
                            }
                            _ => {
                                panic!("wrong operation")
                            }
                        }
                    }
                    Operator::Star => {
                        match exr_one {
                            Literal::Float(v1) => {
                                match exr_two {
                                    Literal::Float(v2) => {
                                        return Literal::Float(v1 * v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Float)")
                                    }
                                }
                            }
                            _ => {
                                panic!("wrong operation")
                            }
                        }
                    }
                    Operator::Slash => {
                        match exr_one {
                            Literal::Float(v1) => {
                                match exr_two {
                                    Literal::Float(v2) => {
                                        return Literal::Float(v1 / v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Float)")
                                    }
                                }
                            }
                            _ => {
                                panic!("wrong operation")
                            }
                        }
                    }
                    Operator::EqualEqual => {
                        match exr_one {
                            Literal::Bool(v1) => {
                                match exr_two {
                                    Literal::Bool(v2) => {
                                        return Literal::Bool(v1 == v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Bool)")
                                    }
                                }
                            }
                            Literal::Str(v1) => {
                                match exr_two {
                                    Literal::Str(v2) => {
                                        return Literal::Bool(v1 == v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Str)")
                                    }
                                }
                            }
                            Literal::Float(v1) => {
                                match exr_two {
                                    Literal::Float(v2) => {
                                        return Literal::Bool(v1 == v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Float)")
                                    }
                                }
                            }
                            _ => {
                                panic!("wrong operation")
                            }
                        }

                    }
                    Operator::BangEqual => {
                        match exr_one {
                            Literal::Bool(v1) => {
                                match exr_two {
                                    Literal::Bool(v2) => {
                                        return Literal::Bool(v1 != v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Bool)")
                                    }
                                }
                            }
                            Literal::Str(v1) => {
                                match exr_two {
                                    Literal::Str(v2) => {
                                        return Literal::Bool(v1 != v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Str)")
                                    }
                                }
                            }
                            Literal::Float(v1) => {
                                match exr_two {
                                    Literal::Float(v2) => {
                                        return Literal::Bool(v1 != v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Float)")
                                    }
                                }
                            }
                            _ => {
                                panic!("wrong operation")
                            }
                        }

                    }
                    Operator::Greater => {
                        match exr_one {
                            Literal::Str(v1) => {
                                match exr_two {
                                    Literal::Str(v2) => {
                                        return Literal::Bool(v1 > v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Str)")
                                    }
                                }
                            }
                            Literal::Float(v1) => {
                                match exr_two {
                                    Literal::Float(v2) => {
                                        return Literal::Bool(v1 > v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Str)")
                                    }
                                }
                            }
                            _ => {
                                panic!("wrong operation")
                            }

                        }
                    }
                    Operator::Less => {
                        match exr_one {
                            Literal::Str(v1) => {
                                match exr_two {
                                    Literal::Str(v2) => {
                                        return Literal::Bool(v1 < v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Str)")
                                    }
                                }
                            }
                            Literal::Float(v1) => {
                                match exr_two {
                                    Literal::Float(v2) => {
                                        return Literal::Bool(v1 < v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Str)")
                                    }
                                }
                            }
                            _ => {
                                panic!("wrong operation")
                            }

                        }

                    }
                    Operator::GreaterEqual => {
                        match exr_one {
                            Literal::Str(v1) => {
                                match exr_two {
                                    Literal::Str(v2) => {
                                        return Literal::Bool(v1 >= v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Str)")
                                    }
                                }
                            }
                            Literal::Float(v1) => {
                                match exr_two {
                                    Literal::Float(v2) => {
                                        return Literal::Bool(v1 >= v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Str)")
                                    }
                                }
                            }
                            _ => {
                                panic!("wrong operation")
                            }

                        }

                    }
                    Operator::LessEqual => {
                        match exr_one {
                            Literal::Str(v1) => {
                                match exr_two {
                                    Literal::Str(v2) => {
                                        return Literal::Bool(v1 <= v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Str)")
                                    }
                                }
                            }
                            Literal::Float(v1) => {
                                match exr_two {
                                    Literal::Float(v2) => {
                                        return Literal::Bool(v1 <= v2);
                                    }
                                    _ => {
                                        panic!("wrong format (Str)")
                                    }
                                }
                            }
                            _ => {
                                panic!("wrong operation")
                            }

                        }

                    }
                    Operator::Comma => {}
                    Operator::Bang => {}
                    Operator::UnaryMinus => {}
                }
            }
            Expression::Unary { expr, operator } => {
                match operator {
                    Operator::Bang => {
                        match *expr {
                            Expression::Literal(v) => {
                                match v {
                                    Literal::Bool(v) => {
                                        match v {
                                            true => {
                                                return Literal::Bool(false);
                                            }
                                            false => {
                                                return Literal::Bool(true);
                                            }
                                        }
                                    },
                                    _ => {
                                        panic!("wrong unary")
                                    }
                                }

                            }
                            _ => {}
                        }
                    }
                    Operator::UnaryMinus => {

                        match *expr {
                            Expression::Literal(v) => {
                                match v {

                                    Literal::Float(v) => {
                                        return Literal::Float(v * -1.0);
                                    }
                                    _ => {
                                        panic!("wrong unary")
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
            Expression::Literal(v) => {
                match v {
                    Literal::IDENTIFIER(val) => {
                        let d = env.global_vars.get(&val);
                        match d {
                            None => {
                                panic!("wrong variable");
                            }
                            Some(r_val) => {
                                return r_val.clone()
                            }
                        }
                    }
                    _ => {
                        return v.to_owned()
                    }
                }

            }
            Expression::Grouping { expr } => {
                return Expression::execute(*expr, env);
            }
            Expression::Assigment{name, value} => {
                let v = Expression::execute(*value.clone(), env);
                env.global_vars.insert(name, v);
            }
        }
        panic!("wrong token")
    }
}

pub struct Environment {
    pub global_vars: HashMap<String, Literal>
}

impl Environment {
    pub fn new() -> Self {
        return Environment{global_vars: HashMap::new()}
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Print(Expression),
    VarDeclaration(String, Expression),
    Box(Vec<Statement>),
    Empty(Expression),
}

impl Statement {
    pub fn execute(stmts: Vec<Statement>, env: &mut Environment) {
        for st in stmts {
            match st {
                Statement::Print(v) => {
                    println!("{:?}", Expression::execute(v, env));
                }
                Statement::Empty(v) => {
                    Expression::execute(v, env);
                }
                Statement::VarDeclaration(name, v) => {
                    let v = Expression::execute(v, env);
                    env.global_vars.insert(name, v);
                }
                Statement::Box(stmts) => {
                    let mut new_env: Environment = Environment::new();
                    for (key, val) in env.global_vars.clone() {
                        new_env.global_vars.insert(key, val.clone());
                    }
                    Statement::execute(stmts, &mut new_env);
                }
            }
        }
    }
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
        TokenType::LESS => Operator::Less,
        TokenType::GREATER => Operator::Greater,
        TokenType::LessEqual => Operator::LessEqual,
        TokenType::GreaterEqual => Operator::GreaterEqual,
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

fn get_unary_operator(token: TokenType) -> Operator {
    match token {
        TokenType::MINUS => Operator::UnaryMinus,
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
        return self.assignment();
    }

    pub fn statement_checker(&mut self, statmets: &mut Vec<Statement>) {
        let tkn = &self.tokens[self.current];
        match tkn.token_type {
            TokenType::PRINT => {
                statmets.push(self.print_st());
            }
            TokenType::VAR => {
                statmets.push(self.declaration());
            }
            TokenType::RightBrace => {
                return;
            },
            TokenType::LeftBrace => {
                self.current += 1;
                let mut new_statmets = vec![];
                loop {
                    if self.current >= self.tokens.len() - 1 {
                        break;
                    }
                    let tkn = &self.tokens[self.current];
                    if tkn.token_type == TokenType::RightBrace {
                        break;
                    }
                    self.statement_checker(&mut new_statmets);
                    self.current += 1;
                }
                statmets.push(Statement::Box(new_statmets));
            }
            _ => {
                statmets.push(Statement::Empty(self.expression()));
            }
        }
    }

    pub fn statement(&mut self, env: &mut Environment) -> Vec<Statement> {
        let mut statmets = vec![];
        loop {
            if self.current >= self.tokens.len() - 1 {
                return statmets;
            }
            self.statement_checker(&mut statmets);
            self.current += 1;
        }

    }


    fn consume(&mut self, tokens: Vec<TokenType>, msg: String) {
        let mut found = false;
        for (i, source_token) in self.tokens.iter().enumerate() {
            if i < self.current {
                continue;
            }
            for cond_token in tokens.iter() {
                if *cond_token == source_token.token_type {
                    found = true;
                    break
                }
            }
        }
        if !found {
            panic!("{}", msg)
        }
    }

    fn math(&mut self, tokens: Vec<TokenType>) -> bool {
        if self.current >= self.tokens.len() - 1 {
            return false;
        }
        if self.tokens[self.current].token_type == TokenType::RightParen {
            self.current += 1;
        }
        for cond_token in tokens.iter() {
            if *cond_token == self.tokens[self.current].token_type {
                self.current += 1;
                return true;
            }
        }
        return false;
    }

    // statement

    fn print_st(&mut self) -> Statement {
        self.current += 1;
        let expr = self.expression();
        self.consume(
            vec![TokenType::SEMICOLON],
            "Expect \';\' after expression".to_owned(),
        );
        return Statement::Print(expr);
    }

    fn declaration(&mut self) -> Statement {
        let tkn = &self.tokens[self.current];
        if tkn.token_type != TokenType::VAR {
            return Statement::Empty(Expression::Literal(Literal::Nil))
        }
        self.current += 1;
        self.consume(
            vec![TokenType::IDENTIFIER],
            "Expect \'IDENTIFIER\' after expression".to_owned(),
        );
        let tkn = &self.tokens[self.current];
        let var_name = tkn.lexeme.clone();
        self.current += 1;
        self.consume(
            vec![TokenType::EQUAL],
            "Expect \'EQUAL\' after expression".to_owned(),
        );
        self.current += 1;
        let expr = self.expression();
        self.consume(
            vec![TokenType::SEMICOLON],
            "Expect \';\' after expression".to_owned(),
        );
        return Statement::VarDeclaration(var_name, expr);

    }

    // not statement

    fn assignment(&mut self) -> Expression {
        let mut expr = self.equality();
        if self.current >= self.tokens.len() - 1 {
            return expr;
        }
        while self.math(vec![TokenType::EQUAL]) {
            let variable = self.tokens[self.current - 2].lexeme.clone();
            // if env.global_vars.get(&variable).is_none() {
            //     panic!("not initial variable")
            // }
            let val = self.assignment();
            expr = Expression::Assigment {
                name: variable,
                value: Box::new(val),
            }
        }

        return expr;
    }

    fn equality(&mut self) -> Expression {
        let mut expr = self.comparison();
        if self.current >= self.tokens.len() - 1 {
            return expr;
        }
        while self.math(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = get_operator(self.tokens[self.current - 1].token_type);
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

        while self.math(vec![TokenType::GREATER, TokenType::GreaterEqual, TokenType::LESS, TokenType::LessEqual]) {
            let operator = get_operator(self.tokens[self.current - 1].token_type);
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

        while self.math(vec![TokenType::MINUS, TokenType::PLUS]) {
            let operator = get_operator(self.tokens[self.current - 1].token_type);
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

        while self.math(vec![TokenType::SLASH, TokenType::STAR]) {
            let operator = get_operator(self.tokens[self.current - 1].token_type);
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
        if self.math(vec![TokenType::BANG, TokenType::MINUS]) {
            let operator = get_unary_operator(self.tokens[self.current-1].token_type);
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
            TokenType::IDENTIFIER => {
                return Expression::Literal(Literal::IDENTIFIER(tkn.lexeme.clone()));
            }
            TokenType::FALSE => {
                return Expression::Literal(Literal::Bool(false));
            }
            TokenType::TRUE => {
                return Expression::Literal(Literal::Bool(true));
            }
            TokenType::NIL => {
                return Expression::Literal(Literal::Nil);
            }
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
            TokenType::LeftParen => {
                self.consume(
                    vec![TokenType::RightParen],
                    "Expect \')\' after expression".to_owned(),
                );
                let expr = Box::new(self.expression());
                return Expression::Grouping { expr };
            }
            _ => {
                panic!("wrong token {:?}", tkn.token_type)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::ast::{Parser, Expression, Literal, Environment, Statement};
    use crate::lexer::tokens::{Token, TokenType};

    #[test]
    fn less() {
        let mut env: Environment = Environment::new();
        let tokens = vec![
            Token::new(TokenType::NUMBER, "1".to_owned()),
            Token::new(TokenType::LESS, "".to_owned()),
            Token::new(TokenType::NUMBER, "2".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Bool(true));

        let tokens = vec![
            Token::new(TokenType::NUMBER, "2".to_owned()),
            Token::new(TokenType::LESS, "".to_owned()),
            Token::new(TokenType::NUMBER, "1".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Bool(false));
    }

    #[test]
    fn less_eq() {
        let mut env: Environment = Environment::new();
        let tokens = vec![
            Token::new(TokenType::NUMBER, "2".to_owned()),
            Token::new(TokenType::LessEqual, "".to_owned()),
            Token::new(TokenType::NUMBER, "2".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Bool(true));

        let tokens = vec![
            Token::new(TokenType::NUMBER, "2".to_owned()),
            Token::new(TokenType::LessEqual, "".to_owned()),
            Token::new(TokenType::NUMBER, "1".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Bool(false));
    }

    #[test]
    fn bigger() {
        let mut env: Environment = Environment::new();
        let tokens = vec![
            Token::new(TokenType::NUMBER, "2".to_owned()),
            Token::new(TokenType::GREATER, "".to_owned()),
            Token::new(TokenType::NUMBER, "1".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Bool(true));

        let tokens = vec![
            Token::new(TokenType::NUMBER, "1".to_owned()),
            Token::new(TokenType::GREATER, "".to_owned()),
            Token::new(TokenType::NUMBER, "2".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Bool(false));
    }

    #[test]
    fn equal_equal_parce() {
        let mut env: Environment = Environment::new();
        let tokens = vec![
            Token::new(TokenType::NUMBER, "2".to_owned()),
            Token::new(TokenType::EqualEqual, "==".to_owned()),
            Token::new(TokenType::NUMBER, "2".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Bool(true));
    }

    #[test]
    fn bang_equal_parce() {
        let mut env: Environment = Environment::new();
        let tokens = vec![
            Token::new(TokenType::NUMBER, "2".to_owned()),
            Token::new(TokenType::BangEqual, "!=".to_owned()),
            Token::new(TokenType::NUMBER, "2".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Bool(false));
    }

    #[test]
    fn unary_equal_parce() {
        let mut env: Environment = Environment::new();
        let tokens = vec![
            Token::new(TokenType::BANG, "".to_owned()),
            Token::new(TokenType::TRUE, "".to_owned()),
            Token::new(TokenType::EqualEqual, "==".to_owned()),
            Token::new(TokenType::TRUE, "".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Bool(false));

        let tokens = vec![
            Token::new(TokenType::BANG, "".to_owned()),
            Token::new(TokenType::FALSE, "".to_owned()),
            Token::new(TokenType::EqualEqual, "==".to_owned()),
            Token::new(TokenType::TRUE, "".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Bool(true));
    }

    #[test]
    fn multiply_unary() {
        let mut env: Environment = Environment::new();
        let tokens = vec![
            Token::new(TokenType::MINUS, "-".to_owned()),
            Token::new(TokenType::NUMBER, "1".to_owned()),
            Token::new(TokenType::STAR, "".to_owned()),
            Token::new(TokenType::NUMBER, "3".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Float(-3.0));

    }

    #[test]
    fn plus_num() {
        let mut env: Environment = Environment::new();
        let tokens = vec![
            Token::new(TokenType::NUMBER, "4".to_owned()),
            Token::new(TokenType::PLUS, "+".to_owned()),
            Token::new(TokenType::NUMBER, "3".to_owned()),
            Token::new(TokenType::MINUS, "-".to_owned()),
            Token::new(TokenType::NUMBER, "2".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Float(5.0));
    }

    #[test]
    fn plus_str() {
        let mut env: Environment = Environment::new();
        let tokens = vec![
            Token::new(TokenType::STRING, "2".to_owned()),
            Token::new(TokenType::PLUS, "+".to_owned()),
            Token::new(TokenType::STRING, "3".to_owned()),
            Token::new(TokenType::PLUS, "+".to_owned()),
            Token::new(TokenType::STRING, "5".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Str("235".to_owned()));
    }

    #[test]
    fn plus_parce_paren() {
        let mut env: Environment = Environment::new();
        let tokens = vec![
            Token::new(TokenType::NUMBER, "8".to_owned()),
            Token::new(TokenType::MINUS, "-".to_owned()),
            Token::new(TokenType::LeftParen, "(".to_owned()),
            Token::new(TokenType::NUMBER, "5".to_owned()),
            Token::new(TokenType::PLUS, "+".to_owned()),
            Token::new(TokenType::NUMBER, "2".to_owned()),
            Token::new(TokenType::RightParen, ")".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Float(1.0));
    }

    #[test]
    fn factor() {
        let mut env: Environment = Environment::new();
        let tokens = vec![
            Token::new(TokenType::NUMBER, "2".to_owned()),
            Token::new(TokenType::PLUS, "+".to_owned()),
            Token::new(TokenType::NUMBER, "3".to_owned()),
            Token::new(TokenType::STAR, "*".to_owned()),
            Token::new(TokenType::NUMBER, "5".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Float(17.0));
    }

    #[test]
    fn factor_paren() {
        let mut env: Environment = Environment::new();
        let tokens = vec![
            Token::new(TokenType::LeftParen, "".to_owned()),
            Token::new(TokenType::NUMBER, "3".to_owned()),
            Token::new(TokenType::PLUS, "+".to_owned()),
            Token::new(TokenType::NUMBER, "3".to_owned()),
            Token::new(TokenType::RightParen, "".to_owned()),
            Token::new(TokenType::STAR, "*".to_owned()),
            Token::new(TokenType::NUMBER, "5".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.expression();
        assert_eq!(Expression::execute(tree, &mut env), Literal::Float(18.0));
    }

    #[test]
    fn print_st() {
        let mut env: Environment = Environment::new();
        let tokens = vec![
            Token::new(TokenType::PRINT, "".to_owned()),
            Token::new(TokenType::STRING, "3".to_owned()),
            Token::new(TokenType::SEMICOLON, "+".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.statement(&mut env);
        // println!("{:?}", tree)
    }

    #[test]
    fn assigment() {
        let mut env: Environment = Environment::new();
        let tokens = vec![
            Token::new(TokenType::VAR, "".to_owned()),
            Token::new(TokenType::IDENTIFIER, "a".to_owned()),
            Token::new(TokenType::EQUAL, "=".to_owned()),
            Token::new(TokenType::NUMBER, "1".to_owned()),
            Token::new(TokenType::SEMICOLON, ";".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.statement(&mut env);
        Statement::execute(tree, &mut env);

        let tokens = vec![
            Token::new(TokenType::IDENTIFIER, "a".to_owned()),
            Token::new(TokenType::EQUAL, "=".to_owned()),
            Token::new(TokenType::NUMBER, "2".to_owned()),
            Token::new(TokenType::SEMICOLON, ";".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let tree = prsr.statement(&mut env);
        Statement::execute(tree, &mut env);
        assert_eq!(Some(Literal::Float(2.0)), env.global_vars.get("a").cloned());
    }

    #[test]
    fn block_assigment() {
        let mut env: Environment = Environment::new();
        let tokens = vec![
            Token::new(TokenType::VAR, "".to_owned()),
            Token::new(TokenType::IDENTIFIER, "a".to_owned()),
            Token::new(TokenType::EQUAL, "=".to_owned()),
            Token::new(TokenType::NUMBER, "1".to_owned()),
            Token::new(TokenType::SEMICOLON, ";".to_owned()),

            Token::new(TokenType::LeftBrace, "{".to_owned()),
            Token::new(TokenType::VAR, "".to_owned()),
            Token::new(TokenType::IDENTIFIER, "a".to_owned()),
            Token::new(TokenType::EQUAL, "=".to_owned()),
            Token::new(TokenType::NUMBER, "2".to_owned()),
            Token::new(TokenType::SEMICOLON, ";".to_owned()),
            Token::new(TokenType::PRINT, "".to_owned()),
            Token::new(TokenType::IDENTIFIER, "a".to_owned()),
            Token::new(TokenType::SEMICOLON, ";".to_owned()),
            Token::new(TokenType::RightBrace, "}".to_owned()),

            Token::new(TokenType::PRINT, "".to_owned()),
            Token::new(TokenType::IDENTIFIER, "a".to_owned()),
            Token::new(TokenType::SEMICOLON, ";".to_owned()),
        ];
        let mut prsr: Parser = Parser::new(tokens);
        let stmts = prsr.statement(&mut env);
        Statement::execute(stmts, &mut env);
        assert_eq!(Some(Literal::Float(1.0)), env.global_vars.get("a").cloned());
    }
}