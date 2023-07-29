use crate::error::parser_error::Error;
use crate::scanner;
use crate::value::expr::BinaryOp;
use crate::value::expr::BinaryOpTy;
use crate::value::expr::UnaryOp;
use crate::value::expr::{
    Expr,
    Stmt, Symbol, ClassDecl, FunDecl, SourceLocation,
    Literal, LogicalOp, LambdaDecl, UnaryOpTy
};
use crate::value::functions::Type;
use crate::value::functions::from_string_to_type;
use std::fmt;

#[derive(Default)]
struct Parser {
    tokens: Vec<super::scanner::Token>,
    current: usize,
    in_fundec: bool, // in rust, booleans default to false: https://doc.rust-lang.org/std/primitive.bool.html#impl-Default
}

#[derive(Debug)]
pub enum FunctionKind {
    Function,
    Method,
    Lambda,
}

pub fn parse(
    tokens: Vec<super::scanner::Token>,
) -> Result<Vec<Stmt>, Error> {
    let mut p = Parser {
        tokens,
        ..Default::default()
    };
    let stmts_or_err = p.parse()?;

    if !p.is_at_end() {
        let tok = &p.tokens[p.current];
        Err(Error::UnexpectedToken(tok.clone()))
    } else {
        Ok(stmts_or_err)
    }
}

/*
Recursive descent using the following grammar

program     → declaration* EOF ;

declaration → classDecl
            | funDecl
            | varDecl
            | statement ;

classDecl → "class" IDENTIFIER ( "<" IDENTIFIER )?
            "{" function* "}" ;

funDecl  → "fun" function ;
function → IDENTIFIER "(" parameters? ")" block ;
parameters  → IDENTIFIER ( "," IDENTIFIER )* ;

statement → exprStmt
          | forStmt
          | ifStmt
          | printStmt
          | returnStmt
          | whileStmt
          | block ;

returnStmt → "return" expression? ";" ;

forStmt   → "for" "(" ( varDecl | exprStmt | ";" )
                      expression? ";"
                      expression? ")" statement ;

whileStmt → "while" "(" expression ")" statement ;

ifStmt    → "if" "(" expression ")" statement ( "else" statement )? ;

block     → "{" declaration* "}" ;

varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;

exprStmt  → expression ";" ;
printStmt → "print" expression ";" ;

expression → assignment ;
assignment → ( call "." )? IDENTIFIER "=" assignment
           | logic_or;
logic_or   → logic_and ( "or" logic_and )* ;
logic_and  → equality ( "and" equality )* ;

equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
multiplication → unary ( ( "/" | "*" ) unary )* ;
unary → ( "!" | "-" ) unary | call ;
call → primary ( "(" arguments? ")" | "." IDENTIFIER | "[" expression "]" )* ;
arguments → expression ( "," expression )* ;

primary → "true" | "false" | "nil" | "this"
        | NUMBER | STRING | IDENTIFIER | "(" expression ")"
        | "super" "." IDENTIFIER
        | "[" arguments? "]" ;

*/
impl Parser {
    pub fn parse(&mut self) -> Result<Vec<Stmt>, Error> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            let stmt = self.declaration()?;
            statements.push(stmt);
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt, Error> {
        if self.matches(super::scanner::TokenType::Var) {
            return self.var_decl();
        }

        if self.matches(super::scanner::TokenType::Function) {
            return Ok(Stmt::FunDecl(self.fun_decl(FunctionKind::Function)?));
        }

        if self.matches(super::scanner::TokenType::Class) {
            return self.class_decl();
        }

        self.statement()
    }

    fn class_decl(&mut self) -> Result<Stmt, Error> {
        let name_tok = self
            .consume(super::scanner::TokenType::Identifier, "Expected class name")?
            .clone();

        let class_symbol = Symbol {
            name: String::from_utf8(name_tok.lexeme).unwrap(),
            val_type: Type::Unspecified,
            is_mutable: true,
            line: name_tok.line,
            col: name_tok.col,
        };

        let superclass_maybe = if self.matches(super::scanner::TokenType::Less) {
            let superclass_tok =
                self.consume(super::scanner::TokenType::Identifier, "Expected class name.")?;
            Some(Symbol {
                name: String::from_utf8(superclass_tok.lexeme.clone()).unwrap(),
                val_type: Type::Unspecified,
                is_mutable: true,
                line: superclass_tok.line,
                col: superclass_tok.col,
            })
        } else {
            None
        };

        self.consume(super::scanner::TokenType::LeftBrace, "Expected { after class name")?;

        let mut methods = Vec::new();
        while !self.check(super::scanner::TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.fun_decl(FunctionKind::Method)?);
        }
        let methods = methods;

        self.consume(
            super::scanner::TokenType::RightBrace,
            "Expected } after class body",
        )?;

        Ok(Stmt::ClassDecl(ClassDecl {
            name: class_symbol,
            superclass: superclass_maybe,
            methods,
        }))
    }

    fn fun_decl(&mut self, kind: FunctionKind) -> Result<FunDecl, Error> {
        let name_tok = self
            .consume(
                super::scanner::TokenType::Identifier,
                format!("Expected {:?} name", kind).as_ref(),
            )?
            .clone();

        let fun_symbol = Symbol {
            name: String::from_utf8(name_tok.lexeme).unwrap(),
            val_type: Type::Unspecified,
            is_mutable: true,
            line: name_tok.line,
            col: name_tok.col,
        };

        let (parameters, body, function_type) = self.params_and_body(kind)?;

        Ok(FunDecl {
            name: fun_symbol,
            params: parameters,
            body,
            function_type,
        })
    }

    fn params_and_body(
        &mut self,
        kind: FunctionKind,
    ) -> Result<(Vec<Symbol>, Vec<Stmt>, Type), Error> {
        self.consume(
            super::scanner::TokenType::LeftParen,
            format!("Expected ( after {:?} name", kind).as_ref(),
        )?;

        let mut parameters = Vec::new();

        if !self.check(super::scanner::TokenType::RightParen) {
            loop {
                if parameters.len() >= 255 {
                    let peek_tok = self.peek();
                    return Err(Error::MaxParamsExceeded {
                        kind,
                        line: peek_tok.line,
                        col: peek_tok.col,
                    });
                }
                let is_mutable = if self.check(scanner::TokenType::Mut) {
                    self.advance();
                    true
                } else {
                    false
                };

                let tok = self
                    .consume(super::scanner::TokenType::Identifier, "Expected parameter name")?
                    .clone();

                if !self.check(scanner::TokenType::ParameterType) {
                    let peek_tok = self.peek();
                    return Err(Error::ParseError {
                        message: "Expected a parameter type".to_owned(),
                        line: peek_tok.line,
                        col: peek_tok.col,
                    });
                }

                let parameter_type = if let scanner::Literal::ParameterType(type_name) = &self.peek().literal.as_ref().unwrap() {
                    Ok(from_string_to_type(type_name))
                } else {
                    let peek_tok = self.peek();
                    return Err(Error::ParseError {
                        message: "Expected a parameter type".to_owned(),
                        line: peek_tok.line,
                        col: peek_tok.col,
                    });
                }?;
                self.advance();

                parameters.push(Symbol {
                    name: String::from_utf8(tok.lexeme).unwrap(),
                    val_type: parameter_type,
                    is_mutable: is_mutable,
                    line: tok.line,
                    col: tok.col,
                });

                if !self.matches(super::scanner::TokenType::Comma) {
                    break;
                }
            }
        }
        let parameters = parameters;

        self.consume(
            super::scanner::TokenType::RightParen,
            "Expected ) after parameter list",
        )?;

        let function_type = if let Some(scanner_type_name) =  &self.peek().literal.as_ref() {
            if let scanner::Literal::ParameterType(type_name) = scanner_type_name {
                Ok(from_string_to_type(type_name))
            } else {
                let peek_tok = self.peek();
                Err(Error::ParseError {
                    message: "Expected a parameter type of function".to_owned(),
                    line: peek_tok.line,
                    col: peek_tok.col,
                })
            }
        } else {
            let peek_tok = self.peek();
            Err(Error::ParseError {
                message: "Expected a parameter type of function".to_owned(),
                line: peek_tok.line,
                col: peek_tok.col,
            })
        }?;
        self.advance();

        self.consume(
            super::scanner::TokenType::LeftBrace,
            "Expected { before function body",
        )?;
        let saved_is_in_fundec = self.in_fundec;
        self.in_fundec = true;
        let body = self.block()?;
        self.in_fundec = saved_is_in_fundec;

        Ok((parameters, body, function_type))
    }

    fn var_decl(&mut self) -> Result<Stmt, Error> {
        let is_mutable = if self.check(scanner::TokenType::Mut) {
            self.advance();
            true
        } else {
            false
        };

        let name_token = self
            .consume(super::scanner::TokenType::Identifier, "Expected variable name")?
            .clone();

        let maybe_initializer = if self.matches(super::scanner::TokenType::Equal) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            super::scanner::TokenType::Semicolon,
            "Expected ; after variable declaration",
        )?;

        Ok(Stmt::VarDecl(
            Symbol {
                name: String::from_utf8(name_token.lexeme).unwrap(),
                val_type: Type::Unspecified,
                is_mutable: is_mutable,
                line: name_token.line,
                col: name_token.col,
            },
            maybe_initializer,
        ))
    }

    fn statement(&mut self) -> Result<Stmt, Error> {
        if self.matches(super::scanner::TokenType::While) {
            return self.while_statement();
        }

        if self.matches(super::scanner::TokenType::LeftBrace) {
            return Ok(Stmt::Block(self.block()?));
        }

        if self.matches(super::scanner::TokenType::For) {
            return self.for_statement();
        }

        if self.matches(super::scanner::TokenType::If) {
            return self.if_statement();
        }

        if self.matches(super::scanner::TokenType::Return) {
            return self.return_statement();
        }

        if self.matches(super::scanner::TokenType::Println) {
            return self.print_statement();
        }

        self.expression_statement()
    }


    fn print_statement(&mut self) -> Result<Stmt, Error> {
        let expr = self.expression()?;
        self.consume(scanner::TokenType::Semicolon, "Expected ; after value")?;
        Ok(Stmt::Print(expr))
    }

    fn return_statement(&mut self) -> Result<Stmt, Error> {
        let prev_tok = self.previous().clone();

        if !self.in_fundec {
            return Err(Error::ReturnNotInFun {
                line: prev_tok.line,
                col: prev_tok.col,
            });
        }

        let maybe_retval = if !self.matches(super::scanner::TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        if maybe_retval.is_some() {
            self.consume(
                super::scanner::TokenType::Semicolon,
                "Expected ; after return value",
            )?;
        }

        Ok(Stmt::Return(
            SourceLocation {
                line: prev_tok.line,
                col: prev_tok.col,
            },
            maybe_retval,
        ))
    }

    fn for_statement(&mut self) -> Result<Stmt, Error> {
        self.consume(super::scanner::TokenType::LeftParen, "Expected ( after for.")?;

        let mut maybe_initializer: Option<Stmt> = None;
        if self.matches(super::scanner::TokenType::Semicolon) {
        } else if self.matches(super::scanner::TokenType::Var) {
            maybe_initializer = Some(self.var_decl()?)
        } else {
            maybe_initializer = Some(self.expression_statement()?)
        }
        let maybe_initializer = maybe_initializer;

        let mut maybe_condition: Option<Expr> = None;
        if !self.check(super::scanner::TokenType::Semicolon) {
            maybe_condition = Some(self.expression()?)
        }
        let maybe_condition = maybe_condition;

        self.consume(
            super::scanner::TokenType::Semicolon,
            "Expected ; after loop condition",
        )?;

        let maybe_increment = if !self.check(super::scanner::TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            super::scanner::TokenType::RightParen,
            "Expected ) after for clauses",
        )?;

        let mut body = self.statement()?;

        if let Some(increment) = maybe_increment {
            body = Stmt::Block(vec![body, Stmt::Expr(increment)])
        }

        let condition = match maybe_condition {
            Some(cond) => cond,
            None => Expr::Literal(Literal::True),
        };
        body = Stmt::While(condition, Box::new(body));

        if let Some(initializer) = maybe_initializer {
            body = Stmt::Block(vec![initializer, body])
        }
        let body = body;

        Ok(body)
    }

    fn while_statement(&mut self) -> Result<Stmt, Error> {
        self.consume(super::scanner::TokenType::LeftParen, "Expected ( after while")?;
        let cond = self.expression()?;
        self.consume(
            super::scanner::TokenType::RightParen,
            "Expected ) after while condition",
        )?;
        let body = Box::new(self.statement()?);
        Ok(Stmt::While(cond, body))
    }

    fn if_statement(&mut self) -> Result<Stmt, Error> {
        self.consume(super::scanner::TokenType::LeftParen, "Expected ( after if.")?;
        let cond = self.expression()?;
        self.consume(
            super::scanner::TokenType::RightParen,
            "Expected ) after if condition.",
        )?;
        let then_branch = Box::new(self.statement()?);
        let maybe_else_branch = if self.matches(super::scanner::TokenType::Else) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If(cond, then_branch, maybe_else_branch))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, Error> {
        let mut stmts = Vec::new();

        while !self.check(super::scanner::TokenType::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?)
        }

        self.consume(super::scanner::TokenType::RightBrace, "Expected } after block.")?;

        Ok(stmts)
    }

    fn expression_statement(&mut self) -> Result<Stmt, Error> {
        let expr = self.expression()?;
        self.consume(super::scanner::TokenType::Semicolon, "Expected ; after value")?;
        Ok(Stmt::Expr(expr))
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, Error> {
        let expr = self.or()?;

        if self.matches(super::scanner::TokenType::Equal) {
            let equals = self.previous().clone();
            let new_value = self.assignment()?;

            if let Expr::Variable(sym) = &expr {
                return Ok(Expr::Assign(sym.clone(), Box::new(new_value)));
            } else if let Expr::Get(e, attr) = expr {
                return Ok(Expr::Set(e, attr, Box::new(new_value)));
            }
            if let Expr::Subscript {
                value,
                slice,
                source_location,
            } = expr
            {
                return Ok(Expr::SetItem {
                    lhs: value,
                    slice,
                    rhs: Box::new(new_value),
                    source_location,
                });
            } else {
                return Err(Error::InvalidAssignment {
                    line: equals.line,
                    col: equals.col,
                });
            }
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, Error> {
        let mut expr = self.and()?;

        while self.matches(super::scanner::TokenType::Or) {
            let right = self.and()?;
            expr = Expr::Logical(Box::new(expr), LogicalOp::Or, Box::new(right));
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, Error> {
        let mut expr = self.equality()?;

        while self.matches(super::scanner::TokenType::And) {
            let right = self.equality()?;
            expr = Expr::Logical(Box::new(expr), LogicalOp::And, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, Error> {
        let mut expr = self.addition()?;

        while self.match_one_of(vec![
            super::scanner::TokenType::Greater,
            super::scanner::TokenType::GreaterEqual,
            super::scanner::TokenType::Less,
            super::scanner::TokenType::LessEqual,
        ]) {
            let operator_token = self.previous().clone();
            let right = Box::new(self.addition()?);
            let binop_maybe = Parser::op_token_to_binop(&operator_token);

            match binop_maybe {
                Ok(binop) => {
                    let left = Box::new(expr);
                    expr = Expr::Binary(left, binop, right);
                }
                Err(err) => return Err(err),
            }
        }
        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr, Error> {
        let mut expr = self.multiplication()?;

        while self.match_one_of(vec![super::scanner::TokenType::Minus, super::scanner::TokenType::Plus]) {
            let operator_token = self.previous().clone();
            let right = Box::new(self.multiplication()?);
            let binop_maybe = Parser::op_token_to_binop(&operator_token);

            match binop_maybe {
                Ok(binop) => {
                    let left = Box::new(expr);
                    expr = Expr::Binary(left, binop, right);
                }
                Err(err) => return Err(err),
            }
        }
        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, Error> {
        let mut expr = self.unary()?;

        while self.match_one_of(vec![super::scanner::TokenType::Slash, super::scanner::TokenType::Star]) {
            let operator_token = self.previous().clone();
            let right = Box::new(self.unary()?);
            let binop_maybe = Parser::op_token_to_binop(&operator_token);

            match binop_maybe {
                Ok(binop) => {
                    let left = Box::new(expr);
                    expr = Expr::Binary(left, binop, right);
                }
                Err(err) => return Err(err),
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, Error> {
        if self.match_one_of(vec![super::scanner::TokenType::Bang, super::scanner::TokenType::Minus]) {
            let operator_token = self.previous().clone();
            let right = Box::new(self.unary()?);
            let unary_op_maybe = Parser::op_token_to_unary_op(&operator_token);

            return match unary_op_maybe {
                Ok(unary_op) => Ok(Expr::Unary(unary_op, right)),
                Err(err) => Err(err),
            };
        }
        self.call()
    }

    fn call(&mut self) -> Result<Expr, Error> {
        let mut expr = self.primary()?;

        loop {
            if self.matches(super::scanner::TokenType::LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.matches(super::scanner::TokenType::Dot) {
                let name_tok = self
                    .consume(
                        super::scanner::TokenType::Identifier,
                        "Expected property name after '.'.",
                    )?
                    .clone();
                expr = Expr::Get(
                    Box::new(expr),
                    Symbol {
                        name: String::from_utf8(name_tok.lexeme).unwrap(),
                        val_type: Type::Unspecified,
                        is_mutable: true,
                        line: name_tok.line,
                        col: name_tok.col,
                    },
                );
            } else if self.matches(super::scanner::TokenType::LeftBracket) {
                let slice_expr = self.expression()?;
                let token = self.consume(
                    super::scanner::TokenType::RightBracket,
                    "Expected ] after subscript",
                )?;
                expr = Expr::Subscript {
                    value: Box::new(expr),
                    slice: Box::new(slice_expr),
                    source_location: SourceLocation {
                        line: token.line,
                        col: token.col,
                    },
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, Error> {
        let mut arguments = Vec::new();

        if !self.check(super::scanner::TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    let peek_tok = self.peek();
                    return Err(Error::TooManyArguments {
                        line: peek_tok.line,
                        col: peek_tok.col,
                    });
                }
                arguments.push(self.expression()?);
                if !self.matches(super::scanner::TokenType::Comma) {
                    break;
                }
            }
        }

        let token = self.consume(
            super::scanner::TokenType::RightParen,
            "Expected ) after arguments.",
        )?;

        Ok(Expr::Call(
            Box::new(callee),
            SourceLocation {
                line: token.line,
                col: token.col,
            },
            arguments,
        ))
    }

    fn primary(&mut self) -> Result<Expr, Error> {
        if self.matches(super::scanner::TokenType::False) {
            return Ok(Expr::Literal(Literal::False));
        }
        if self.matches(super::scanner::TokenType::True) {
            return Ok(Expr::Literal(Literal::True));
        }
        if self.matches(super::scanner::TokenType::Nil) {
            return Ok(Expr::Literal(Literal::Nil));
        }
        if self.matches(super::scanner::TokenType::Super) {
            let super_tok = self.previous().clone();
            self.consume(super::scanner::TokenType::Dot, "Expected '.' after 'super'.")?;
            let method_tok = self.consume(
                super::scanner::TokenType::Identifier,
                "Expected superclass method name.",
            )?;
            return Ok(Expr::Super(
                SourceLocation {
                    line: super_tok.line,
                    col: super_tok.col,
                },
                Symbol {
                    name: String::from_utf8(method_tok.lexeme.clone()).unwrap(),
                    val_type: Type::Unspecified,
                    is_mutable: true,
                    line: method_tok.line,
                    col: method_tok.col,
                },
            ));
        }
        if self.matches(super::scanner::TokenType::Number) {
            match &self.previous().literal {
                Some(super::scanner::Literal::Number(n)) => {
                    return Ok(Expr::Literal(Literal::Number(*n)))
                }
                Some(l) => panic!(
                    "internal error in parser: when parsing number, found literal {:?}",
                    l
                ),
                None => panic!("internal error in parser: when parsing number, found no literal"),
            }
        }
        if self.matches(super::scanner::TokenType::String) {
            match &self.previous().literal {
                Some(super::scanner::Literal::Str(s)) => {
                    return Ok(Expr::Literal(Literal::String(s.clone())))
                }
                Some(l) => panic!(
                    "internal error in parser: when parsing string, found literal {:?}",
                    l
                ),
                None => panic!("internal error in parser: when parsing string, found no literal"),
            }
        }
        if self.matches(super::scanner::TokenType::This) {
            let prev = self.previous();
            return Ok(Expr::This(SourceLocation {
                line: prev.line,
                col: prev.col,
            }));
        }
        if self.matches(super::scanner::TokenType::Identifier) {
            match &self.previous().literal {
                Some(super::scanner::Literal::Identifier(s)) => {
                    return Ok(Expr::Variable(Symbol {
                        name: s.clone(),
                        val_type: Type::Unspecified,
                        is_mutable: true,
                        line: self.previous().line,
                        col: self.previous().col,
                    }))
                }
                Some(l) => panic!(
                    "internal error in parser: when parsing identifier, found literal {:?}",
                    l
                ),
                None => {
                    panic!("internal error in parser: when parsing identifier, found no literal")
                }
            }
        }
        if self.matches(super::scanner::TokenType::LeftParen) {
            let expr = Box::new(self.expression()?);
            self.consume(
                super::scanner::TokenType::RightParen,
                "Expected ')' after expression.",
            )?;
            return Ok(Expr::Grouping(expr));
        }
        if self.matches(super::scanner::TokenType::LeftBracket) {
            let mut list_elements = Vec::new();

            if !self.check(super::scanner::TokenType::RightBracket) {
                loop {
                    list_elements.push(self.expression()?);
                    if !self.matches(super::scanner::TokenType::Comma) {
                        break;
                    }
                }
            }

            self.consume(super::scanner::TokenType::RightBracket, "Expected ].")?;

            return Ok(Expr::List(list_elements));
        }
        if self.matches(super::scanner::TokenType::Lambda) {
            let (params, body, function_type) = self.params_and_body(FunctionKind::Lambda)?;
            return Ok(Expr::Lambda(LambdaDecl { params, body }));
        }

        Err(Error::ExpectedExpression {
            token_type: self.peek().ty,
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    fn consume(
        &mut self,
        tok: super::scanner::TokenType,
        on_err_str: &str,
    ) -> Result<&super::scanner::Token, Error> {
        if self.check(tok) {
            return Ok(self.advance());
        }
        Err(Error::TokenMismatch {
            expected: tok,
            found: self.peek().clone(),
            maybe_on_err_string: Some(on_err_str.into()),
        })
    }

    fn op_token_to_unary_op(tok: &super::scanner::Token) -> Result<UnaryOp, Error> {
        match tok.ty {
            super::scanner::TokenType::Minus => Ok(UnaryOp {
                ty: UnaryOpTy::Minus,
                line: tok.line,
                col: tok.col,
            }),
            super::scanner::TokenType::Bang => Ok(UnaryOp {
                ty: UnaryOpTy::Bang,
                line: tok.line,
                col: tok.col,
            }),
            _ => Err(Error::InvalidTokenInUnaryOp {
                token_type: tok.ty,
                line: tok.line,
                col: tok.col,
            }),
        }
    }

    fn equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.comparison()?;

        while self.match_one_of(vec![
            super::scanner::TokenType::BangEqual,
            super::scanner::TokenType::EqualEqual,
        ]) {
            let operator_token = self.previous().clone();
            let right = Box::new(self.comparison()?);

            let binop_maybe = Parser::op_token_to_binop(&operator_token);

            match binop_maybe {
                Ok(binop) => {
                    let left = Box::new(expr);
                    expr = Expr::Binary(left, binop, right);
                }
                Err(err) => return Err(err),
            }
        }
        Ok(expr)
    }

    fn op_token_to_binop(tok: &super::scanner::Token) -> Result<BinaryOp, Error> {
        match tok.ty {
            super::scanner::TokenType::EqualEqual => Ok(BinaryOp {
                ty: BinaryOpTy::EqualEqual,
                line: tok.line,
                col: tok.col,
            }),
            super::scanner::TokenType::BangEqual => Ok(BinaryOp {
                ty: BinaryOpTy::NotEqual,
                line: tok.line,
                col: tok.col,
            }),
            super::scanner::TokenType::Less => Ok(BinaryOp {
                ty: BinaryOpTy::Less,
                line: tok.line,
                col: tok.col,
            }),
            super::scanner::TokenType::LessEqual => Ok(BinaryOp {
                ty: BinaryOpTy::LessEqual,
                line: tok.line,
                col: tok.col,
            }),
            super::scanner::TokenType::Greater => Ok(BinaryOp {
                ty: BinaryOpTy::Greater,
                line: tok.line,
                col: tok.col,
            }),
            super::scanner::TokenType::GreaterEqual => Ok(BinaryOp {
                ty: BinaryOpTy::GreaterEqual,
                line: tok.line,
                col: tok.col,
            }),
            super::scanner::TokenType::Plus => Ok(BinaryOp {
                ty: BinaryOpTy::Plus,
                line: tok.line,
                col: tok.col,
            }),
            super::scanner::TokenType::Minus => Ok(BinaryOp {
                ty: BinaryOpTy::Minus,
                line: tok.line,
                col: tok.col,
            }),
            super::scanner::TokenType::Star => Ok(BinaryOp {
                ty: BinaryOpTy::Star,
                line: tok.line,
                col: tok.col,
            }),
            super::scanner::TokenType::Slash => Ok(BinaryOp {
                ty: BinaryOpTy::Slash,
                line: tok.line,
                col: tok.col,
            }),
            _ => Err(Error::InvalidTokenInBinaryOp {
                token_type: tok.ty,
                line: tok.line,
                col: tok.col,
            }),
        }
    }

    fn match_one_of(&mut self, types: Vec<super::scanner::TokenType>) -> bool {
        for ty in types.iter() {
            if self.matches(*ty) {
                return true;
            }
        }
        false
    }

    fn matches(&mut self, ty: super::scanner::TokenType) -> bool {
        if self.check(ty) {
            self.advance();
            return true;
        }
        false
    }

    fn check(&self, ty: super::scanner::TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().ty == ty
    }

    fn advance(&mut self) -> &super::scanner::Token {
        if !self.is_at_end() {
            self.current += 1
        }

        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().ty == super::scanner::TokenType::Eof
    }

    fn peek(&self) -> &super::scanner::Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &super::scanner::Token {
        &self.tokens[self.current - 1]
    }
}
