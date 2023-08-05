use std::collections::HashMap;
use std::fmt;

use crate::error::scanner_error::Error;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    ParameterType,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Caret,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Integer,
    Float,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Function,
    For,
    If,
    Nil,
    Or,
    Println,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    Mut,

    Extends,
    New,

    Use,

    Public,

    While,
    Lambda,

    Eof,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Identifier(String),
    Path(String),
    Str(String),
    Integer(i64),
    Float(f64),
    ParameterType(String),
}

#[derive(Clone)]
pub struct Token {
    pub ty: TokenType,
    pub lexeme: Vec<u8>,
    pub literal: Option<Literal>,
    pub line: usize,
    pub col: i64,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Token {{ ty: {:?}, lexeme: \"{}\", literal: {:?}, line: {:?}, col: {:?}}}",
            self.ty,
            String::from_utf8(self.lexeme.clone()).unwrap(),
            self.literal,
            self.line,
            self.col
        )
    }
}

pub fn scan_tokens(input: String) -> Result<Vec<Token>, Error> {
    let mut scanner: Scanner = Default::default();

    scanner.scan_tokens(input);

    match scanner.err {
        Some(err) => Err(err),
        None => Ok(scanner.tokens),
    }
}

struct Scanner {
    source: Vec<u8>,
    tokens: Vec<Token>,
    err: Option<Error>,
    start: usize,
    current: usize,
    line: usize,
    col: i64,
    keywords: HashMap<String, TokenType>,
}

impl Default for Scanner {
    fn default() -> Scanner {
        Scanner {
            source: Vec::new(),
            tokens: Vec::new(),
            err: None,
            start: 0,
            current: 0,
            line: 1,
            col: -1,
            keywords: vec![
                ("and", TokenType::And),
                ("class", TokenType::Class),
                ("else", TokenType::Else),
                ("false", TokenType::False),
                ("for", TokenType::For),
                ("fn", TokenType::Function),
                ("if", TokenType::If),
                ("nil", TokenType::Nil),
                ("or", TokenType::Or),
                ("println", TokenType::Println),
                ("print", TokenType::Print),
                ("return", TokenType::Return),
                ("super", TokenType::Super),
                ("this", TokenType::This),
                ("true", TokenType::True),
                ("let", TokenType::Var),
                ("mut", TokenType::Mut),
                ("while", TokenType::While),
                ("lambda", TokenType::Lambda),
                ("extends", TokenType::Extends),
                ("new", TokenType::New),
                ("use", TokenType::Use),
                ("pub", TokenType::Public),
            ]
            .into_iter()
            .map(|(k, v)| (String::from(k), v))
            .collect(),
        }
    }
}

impl Scanner {
    fn scan_tokens(&mut self, input: String) {
        self.source = input.into_bytes();

        while !self.done() {
            self.start = self.current;
            self.scan_token();
        }

        match self.err {
            Some(_) => {}
            None => self.tokens.push(Token {
                ty: TokenType::Eof,
                lexeme: Vec::new(),
                literal: None,
                line: self.line,
                col: self.col,
            }),
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.col += 1;

        char::from(self.source[self.current - 1])
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            '[' => self.add_token(TokenType::LeftBracket),
            ']' => self.add_token(TokenType::RightBracket),
            ',' => self.add_token(TokenType::Comma),
            ':' => self.parameter_type(),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '^' => self.add_token(TokenType::Caret),
            '!' => {
                let matches_eq = self.matches('=');
                self.add_token(if matches_eq {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                })
            }
            '=' => {
                let matches_eq = self.matches('=');
                self.add_token(if matches_eq {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                })
            }
            '<' => {
                let matches_eq = self.matches('=');
                self.add_token(if matches_eq {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                })
            }
            '>' => {
                let matches_eq = self.matches('=');
                self.add_token(if matches_eq {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                })
            }
            '/' => {
                if self.matches('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash)
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => {
                self.line += 1;
                self.col = 0
            }
            '"' => self.string(),
            _ => {
                if Scanner::is_decimal_digit(c) {
                    self.number()
                } else if Scanner::is_alpha(c) {
                    while Scanner::is_alphanumeric(self.peek()) {
                        self.advance();
                    }

                    let literal_val =
                    String::from_utf8(self.source[self.start..self.current].to_vec()).unwrap();
        
                    let token_type = match self.keywords.get(&literal_val) {
                        Some(kw_token_type) => *kw_token_type,
                        None => TokenType::Identifier,
                    };

                    if token_type == TokenType::Use {
                        self.path(token_type)
                    } else {
                        self.identifier(literal_val, token_type)
                    }
                } else {
                    self.err = Some(Error {
                        what: format!("scanner can't handle {}", c),
                        line: self.line,
                        col: self.col,
                    })
                }
            }
        }
    }

    fn is_alpha(c: char) -> bool {
        c.is_alphabetic()
    }

    fn is_decimal_digit(c: char) -> bool {
        c.is_digit(10)
    }

    fn is_alphanumeric(c: char) -> bool {
        Scanner::is_alpha(c) || Scanner::is_decimal_digit(c)
    }

    fn identifier(&mut self,  literal_val: String, token_type:TokenType) {
        match token_type {
            TokenType::Identifier => self.add_token_literal(
                TokenType::Identifier,
                Some(Literal::Identifier(literal_val)),
            ), // book doesn't do this. why not?}
            _ => self.add_token(token_type),
        }
    }

    fn path(&mut self, token_type:TokenType) {
        match token_type {
            TokenType::Use => {
                self.add_token_literal_path()
            }, 
            _ => self.add_token(token_type),
        }
    }

    fn number(&mut self) {
        let mut is_decimal = false;
        while Scanner::is_decimal_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && Scanner::is_decimal_digit(self.peek_next()) {
            is_decimal = true;
            self.advance();
        }

        while Scanner::is_decimal_digit(self.peek()) {
            self.advance();
        }

        if is_decimal {
            let val: f64 = String::from_utf8(self.source[self.start..self.current].to_vec())
                .unwrap()
                .parse()
                .unwrap();
            self.add_token_literal(TokenType::Float, Some(Literal::Float(val)))
        } else {
            let val: i64 = String::from_utf8(self.source[self.start..self.current].to_vec())
                .unwrap()
                .parse()
                .unwrap();
            self.add_token_literal(TokenType::Integer, Some(Literal::Integer(val)))
        }

    }

    fn parameter_type(&mut self) {
        while (self.peek() == ' ' ||  self.peek() == '\r' ||  self.peek() == '\t' || self.peek() == '\n') && !self.is_at_end(){
            if self.peek() == '\n' {
                self.line += 1
            }
            self.advance();
        }
        let start = self.current;
        while self.peek_next() != ',' && self.peek_next() != '{' && self.peek_next() != ')' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1
            }
            if self.peek_next() == ' ' ||  self.peek_next() == '\r' ||  self.peek_next() == '\t' || self.peek_next() == '\n'{
                break;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.err = Some(Error {
                what: "Unterminated string".to_string(),
                line: self.line,
                col: self.col,
            })
        }

        self.advance();

        self.add_token_literal(
            TokenType::ParameterType,
            Some(Literal::ParameterType(
                String::from_utf8(self.source[start..self.current].to_vec()).unwrap(),
            )),
        )
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1
            }
            self.advance();
        }

        if self.is_at_end() {
            self.err = Some(Error {
                what: "Unterminated string".to_string(),
                line: self.line,
                col: self.col,
            })
        }

        if self.peek() != '"' {
            return;
        }

        self.advance();

        self.add_token_literal(
            TokenType::String,
            Some(Literal::Str(
                String::from_utf8(self.source[self.start + 1..self.current - 1].to_vec()).unwrap(),
            )),
        )
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            char::from(self.source[self.current + 1])
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            char::from(self.source[self.current])
        }
    }

    fn matches(&mut self, c: char) -> bool {
        if self.is_at_end() {
            return true;
        }

        if char::from(self.source[self.current]) != c {
            return false;
        }

        self.current += 1;
        self.col += 1;
        true
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_literal(token_type, None)
    }

    fn add_token_literal(&mut self, token_type: TokenType, literal: Option<Literal>) {
        let text = self.source[self.start..self.current].to_vec();

        self.tokens.push(Token {
            ty: token_type,
            lexeme: text,
            literal,
            line: self.line,
            col: self.col,
        })
    }

    fn add_token_literal_path(&mut self) {
        let mut pathtext = vec![];
        let mut literal_string = String::new(); 
        let mut current;
        loop {
            current = char::from(self.source[self.current]);
            if current == '\n' {
                self.line += 1;
                self.col = 0;
                continue;
            }
            if current == ' ' || current == '\r' || current == '\t' {
                self.advance();
            } else {
                break;
            }
        }

        loop {
            current = char::from(self.source[self.current]);
            if current == ';' || current == '\n' || self.is_at_end() {
                break;
            }
            pathtext.push(self.source[self.current]);
            literal_string.push(current);
            self.advance();
        }

        self.tokens.push(Token {
            ty: TokenType::Use,
            lexeme: pathtext,
            literal: Some(Literal::Path(literal_string)),
            line: self.line,
            col: self.col,
        });
    }

    fn done(&self) -> bool {
        self.err.is_some() || self.is_at_end()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}
