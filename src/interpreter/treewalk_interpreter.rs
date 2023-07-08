use std::collections::HashMap;
use std::fs;

use crate::value::environment::Environment;
use crate::value::expr::{
    Expr,
    Stmt, Symbol, ClassDecl, FunDecl, SourceLocation,
    Literal, LogicalOp, LambdaDecl, UnaryOpTy
};
use crate::value::functions::{Function, Class, Instance};
use crate::value::values::Value;

pub struct Interpreter {
    pub counter: u64,
    pub backtrace: Vec<(u64, String)>,
    pub retval: Option<Value>,
    pub env: Environment,
    pub functions: HashMap<u64, Function>,
    pub instances: HashMap<u64, Instance>,
    pub classes: HashMap<u64, Class>,
}

impl Default for Interpreter {
    fn default() -> Interpreter {
        Interpreter {
            counter: 0,
            backtrace: vec![(0, "script".to_string())],
            retval: None,
            functions: Default::default(),
            instances: Default::default(),
            classes: Default::default(),
            env: Default::default(),
        }
    }
}

impl Interpreter {
    pub fn interpret(&mut self, file_name: String, stmts: &[Stmt]) -> Result<(), String> {
        for stmt in stmts {
            self.execute(stmt)?
        }
        //Self::write_rust_code(file_name);
        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), String> {
        if self.retval.is_some() {
            return Ok(());
        }

        match stmt {
            Stmt::Expr(e) => match self.interpret_expr(e) {
                Ok(_) => Ok(()),
                Err(err) => Err(err),
            },
            Stmt::ClassDecl(ClassDecl {
                name: sym,
                superclass: maybe_superclass,
                methods: stmt_methods,
            }) => {

                Ok(())
            }
            Stmt::FunDecl(FunDecl {
                name,
                params: parameters,
                body,
            }) => {
                let func_id = self.alloc_id();
                self.env.define(
                    name.clone(),
                    Some(Value::Function(name.clone(), func_id, None)),
                );

                let function = Function {
                    id: func_id,
                    name: name.clone(),
                    parameters: parameters.clone(),
                    body: body.clone(),
                    closure: self.env.clone(),
                    this_binding: None,
                    superclass: None,
                    is_initializer: false,
                };

                self.functions.insert(func_id, function);

                Ok(())
            }
            Stmt::If(cond, if_true, maybe_if_false) => {

                Ok(())
            }
            Stmt::VarDecl(sym, maybe_expr) => {
                let maybe_val = match maybe_expr {
                    Some(expr) => Some(self.interpret_expr(expr)?),
                    None => None,
                };
                self.env.define(sym.clone(), maybe_val);
                Ok(())
            }
            Stmt::Block(stmts) => {
                Ok(())
            }
            Stmt::While(cond, body) => {

                Ok(())
            }
            Stmt::Return(_, maybe_res) => {
                
                Ok(())
            }
        }
    }

    pub fn get_function(&self, id: u64) -> &Function {
        match self.functions.get(&id) {
            Some(func) => func,
            None => panic!(
                "Internal interpreter error! couldn't find an function with id {}.",
                id
            ),
        }
    }

    pub fn get_class(&self, id: u64) -> &Class {
        match self.classes.get(&id) {
            Some(func) => func,
            None => panic!(
                "Internal interpreter error! couldn't find class with id {}.",
                id
            ),
        }
    }

    pub fn get_instance(&self, id: u64) -> &Instance {
        match self.instances.get(&id) {
            Some(inst) => inst,
            None => panic!(
                "Internal interpreter error: could not find an instance with id {}.",
                id
            ),
        }
    }

    pub fn create_instance(&mut self, class_name: &Symbol, class_id: u64) -> Value {
        let inst_id = self.alloc_id();
        let inst = Instance {
            class_name: class_name.clone(),
            class_id,
            id: inst_id,
            fields: HashMap::new(),
        };
        self.instances.insert(inst_id, inst);
        Value::Instance(class_name.clone(), inst_id)
    }

    fn alloc_id(&mut self) -> u64 {
        let res = self.counter;
        self.counter += 1;
        res
    }

    fn lookup(&self, sym: &Symbol) -> Result<&Value, String> {
        match self.env.get(sym) {
            Ok(val) => Ok(val),
            Err(_) => Err("This value not defined".to_owned()),
        }
    }

    fn interpret_expr(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::This(source_location) => Ok(Value::Nil),
            Expr::Literal(lit) => Ok(Value::Nil),
            Expr::Unary(op, e) => Ok(Value::Nil),
            Expr::Binary(lhs, op, rhs) => Ok(Value::Nil),
            Expr::Call(callee, loc, args) => Ok(Value::Nil),
            Expr::Get(lhs, attr) => Ok(Value::Nil),
            Expr::Set(lhs, attr, rhs) => Ok(Value::Nil),
            Expr::Grouping(e) => self.interpret_expr(e),
            Expr::Variable(sym) => match self.lookup(sym) {
                Ok(val) => Ok(val.clone()),
                Err(err) => Err(err),
            },
            Expr::Assign(sym, val_expr) => {
                let val = self.interpret_expr(val_expr)?;

                self.env.assign(sym.clone(), &val)?;

                Ok(val)
            }
            Expr::Logical(left_expr, LogicalOp::Or, right_expr) => {
                let left = self.interpret_expr(left_expr)?;
                Ok(self.interpret_expr(right_expr)?)
            }
            Expr::Logical(left_expr, LogicalOp::And, right_expr) => {
                let left = self.interpret_expr(left_expr)?;
                Ok(self.interpret_expr(right_expr)?)
            }
            Expr::Super(source_location, sym) => Ok(Value::Nil),
            Expr::List(elements) => Ok(Value::Nil),
            Expr::Subscript {
                value,
                slice,
                source_location,
            } => Ok(Value::Nil),
            Expr::SetItem {
                lhs,
                slice,
                rhs,
                source_location,
            } => Ok(Value::Nil),
            Expr::Lambda(lambda_decl) => Ok(Value::Nil),
        }
    }

    fn write_rust_code (file_name: String) -> std::io::Result<()> {
        let data = "Some data to write to the file";
        let path = "rustcode/".to_owned() + &file_name + ".rs";
        fs::write(path, data)?;
    
        Ok(())
    }

    pub fn format_backtrace(&self) -> String {
        let lines: Vec<_> = self
            .backtrace
            .iter()
            .map(|(_, funname)| format!("[line ??] in {}", funname))
            .collect();
        format!("Backtrace (most recent call last):\n\n{}", lines.join("\n"))
    }

}
