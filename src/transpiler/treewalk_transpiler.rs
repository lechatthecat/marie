use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;

use crate::PROJECT_PATH;
use crate::error::source_map::SourceMap;
use crate::value::environment::Environment;
use crate::value::expr::{
    Expr,
    Stmt, Symbol, ClassDecl, FunDecl, 
    Literal, LogicalOp,
};
use crate::value::functions::{Function, Class, Instance, MainFunction};
use crate::value::values::Value;

pub struct Transpiler {
    pub opration_counter: usize,
    pub counter: u64,
    pub backtrace: Vec<(u64, String)>,
    pub retval: Option<Value>,
    pub env: Environment,
    pub functions: HashMap<u64, Function>,
    pub instances: HashMap<u64, Instance>,
    pub classes: HashMap<u64, Class>,
    pub source_map: SourceMap,
}

impl Default for Transpiler {
    fn default() -> Transpiler {
        Transpiler {
            opration_counter: 0,
            counter: 0,
            backtrace: vec![(0, "script".to_string())],
            retval: None,
            functions: Default::default(),
            instances: Default::default(),
            classes: Default::default(),
            env: Default::default(),
            source_map: Default::default(),
        }
    }
}

impl Transpiler {
    pub fn has_main_function (&mut self) -> bool {
        self.env.has_main_function
    }

    pub fn interpret(&mut self, file_name: String, stmts: &[Stmt]) -> Result<String, String> {
        // Set the project root
        let content = format!("const PROJECT_PATH: &'static str = \"{}/output\";\n", PROJECT_PATH);
        match Transpiler::write_rust_code(
            file_name.clone(), 
            &content
        ) {
            Ok(_) => Ok(()),
            Err(err) => Err(format!("Failed to write the rust code to the output directory. Error: {}", err)),
        }?;

        // Execute the stmts
        loop {
            let stmts_size = stmts.len();
            let stmt = &stmts[self.opration_counter];
            self.execute(stmt, file_name.clone())?;
            if stmts_size == self.opration_counter + 1 {
                break;
            }
            self.opration_counter += 1;
        }

        Ok("".to_owned())
    }

    pub fn interpret_stme_to_string(&mut self, file_name: String, stmt: &Stmt) -> Result<String, String> {
        let string = self.execute(stmt, file_name)?;
        //Self::write_rust_code(file_name);
        Ok(string.to_string())
    }

    fn execute(&mut self, stmt: &Stmt, file_name: String) -> Result<String, String> {
        if self.retval.is_some() {
            return Ok("".to_owned());
        }

        match stmt {
            Stmt::Expr(e) => match self.interpret_expr(e) {
                Ok(_) => Ok("".to_owned()),
                Err(err) => Err(err),
            },
            Stmt::ClassDecl(ClassDecl {
                name: sym,
                superclass: maybe_superclass,
                methods: stmt_methods,
            }) => {

                Ok("".to_owned())
            }
            Stmt::FunDecl(FunDecl {
                name,
                params: parameters,
                body,
                function_type,
            }) => {
                let func_id = self.alloc_id();
                self.env.define(
                    name.clone(),
                    Some(Value::Function(name.clone(), func_id, None, *function_type)),
                );

                let function = Function {
                    id: func_id,
                    function_type: *function_type,
                    name: name.clone(),
                    parameters: parameters.clone(),
                    body: body.clone(),
                    closure: self.env.clone(),
                    this_binding: None,
                    superclass: None,
                    is_initializer: false,
                };


                if name.name == "main" {
                    let path = Path::new(&file_name);
                    let file_stem = path.file_stem().unwrap().to_str().unwrap();
                    if file_stem != "main" {
                        return Err(format!("main function should be written in main.mr!"));
                    }
                    self.env.has_main_function = true;
                    let main_function = MainFunction {
                        body: body.clone(),
                        this_binding: None,
                    };
                    self.env.main_function = Some(main_function.clone());
                    let content = main_function.to_string(self, file_name.clone())?;
                    match Transpiler::write_rust_code(file_name, &content) {
                        Ok(_) => Ok(()),
                        Err(err) => Err(format!("Failed to write the rust code to the output directory. Cannot empty the output directory. Please check if the output directory exists in the project root. If it does, maybe Permission error or CARGO_MANIFEST_DIR env variable is not set to the project root? Error: {}", err)),
                    }?;
                } else {
                    let content = function.to_string(self, file_name.clone())?;
                    match Transpiler::write_rust_code(file_name, &content) {
                        Ok(_) => Ok(()),
                        Err(err) => Err(format!("Failed to write the rust code to the output directory. Cannot empty the output directory. Please check if the output directory exists in the project root. If it does, maybe Permission error or  Maybe Permission error or CARGO_MANIFEST_DIR env variable is not set to the project root? Error: {}", err)),
                    }?;
                }

                self.functions.insert(func_id, function.clone());

                Ok("".to_owned())
            }
            Stmt::If(cond, if_true, maybe_if_false) => {

                Ok("".to_owned())
            }
            Stmt::VarDecl(sym, maybe_expr) => {
                let maybe_val = match maybe_expr {
                    Some(expr) => Some(self.interpret_expr(expr)?),
                    None => None,
                };
                self.env.define(sym.clone(), maybe_val);
                Ok("".to_owned())
            }
            Stmt::Block(stmts) => {
                Ok("".to_owned())
            }
            Stmt::While(cond, body) => {

                Ok("".to_owned())
            }
            Stmt::Return(_, maybe_res) => {
                let return_string = "return".to_owned();
                let retval = Some(if let Some(res) = maybe_res {
                    self.interpret_expr(res)?
                } else {
                    Value::Nil
                });
                Ok(format!("{} {};", return_string, retval.unwrap()))
            }
        }
    }

    fn write_rust_code (file_name: String, content: &str) -> std::io::Result<()> {
        let path = Path::new(&file_name);
        let file_stem = path.file_stem().unwrap().to_str().unwrap();
        let path = format!("{}/output/src/{}.rs", PROJECT_PATH, file_stem);
        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .append(true)
            .open(path)?;

        writeln!(file, "{}", content)?;
    
        Ok(())
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
            Err(_) => Err("This is value not defined".to_owned()),
        }
    }

    fn interpret_expr(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::This(source_location) => Ok(Value::Nil),
            Expr::Literal(lit) => Ok(Transpiler::interpret_literal(lit)),
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

    fn interpret_literal(lit: &Literal) -> Value {
        match lit {
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
            Literal::True => Value::Bool(true),
            Literal::False => Value::Bool(false),
            Literal::Nil => Value::Nil,
        }
    }

    pub fn format_backtrace(&self) -> String {
        let lines: Vec<_> = self
            .backtrace
            .iter()
            .map(|(line, funname)| format!("[line {}] in {}", line, funname))
            .collect();
        format!("Backtrace (most recent call last):\n\n{}", lines.join("\n"))
    }

}
