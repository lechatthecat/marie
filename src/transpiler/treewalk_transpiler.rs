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
    Literal, LogicalOp, SourceLocation, BinaryOp, BinaryOpTy,
};
use crate::value::functions::{Function, Class, Instance, MainFunction, as_callable, self, Type};
use crate::value::values::Value;

pub struct Transpiler {
    pub opration_counter: usize,
    pub counter: u64,
    pub backtrace: Vec<(u64, String)>,
    pub retval: Option<Value>,
    pub env: Environment,
    pub functions: HashMap<u64, Function>,
    pub funtion_body: String,
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
            funtion_body: "".to_owned(),
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

    pub fn define_functions(&mut self, file_name: String, stmts: &[Stmt]) -> Result<(), String> {
        // Execute the stmts
        for stmt in stmts {
            match stmt {
                Stmt::ClassDecl(ClassDecl {
                    name: sym,
                    superclass: maybe_superclass,
                    methods: stmt_methods,
                }) => {
    

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
                    }
    
                    self.functions.insert(func_id, function.clone());

                }
                _ => {}
            }
        }

        Ok(())
    }

    pub fn interpret(&mut self, file_name: &str, stmts: &[Stmt]) -> Result<String, String> {
        // Set the project root
        let content = format!("#[allow(dead_code)]\nconst PROJECT_PATH: &'static str = \"{}/output\";\n", PROJECT_PATH);
        match Transpiler::write_rust_code(
            file_name, 
            &content
        ) {
            Ok(_) => Ok(()),
            Err(err) => Err(format!("Failed to write the rust code to the output directory. Error: {}", err)),
        }?;

        // Execute the stmts
        loop {
            let stmts_size = stmts.len();
            let stmt = &stmts[self.opration_counter];
            self.execute(stmt, file_name)?;
            if stmts_size == self.opration_counter + 1 {
                break;
            }
            self.opration_counter += 1;
        }

        Ok("".to_owned())
    }

    pub fn interpret_stme_to_string(&mut self, file_name: &str, stmt: &Stmt) -> Result<String, String> {
        let string = self.execute(stmt, file_name)?;
        //Self::write_rust_code(file_name);
        Ok(string.to_string())
    }

    fn execute(&mut self, stmt: &Stmt, file_name: &str) -> Result<String, String> {
        match stmt {
            Stmt::Expr(e) => match self.interpret_expr(e, file_name) {
                Ok((str_value, value)) => Ok(format!("{};", str_value)),
                Err(err) => Err(err),
            },
            Stmt::Print(e) => match self.interpret_expr(e, file_name)  {
                Ok(val) => {
                    Ok(format!("print!(\"{{}}\", {});", val.0))
                }
                Err(err) => Err(err),
            },
            Stmt::Println(e) => match self.interpret_expr(e, file_name)  {
                Ok(val) => {
                    let value = val.1;
                    match value {
                        Value::Integer | Value::Float | Value::Bool | Value::Variable(_, _) => {
                            Ok(format!("println!(\"{{}}\", {});", val.0))
                        },
                        Value::String => {
                            Ok(format!("println!(\"{{}}\", \"{}\");", val.0))
                        },
                        _ => {
                            Ok(format!("println!(\"{{}}\", \"{}\");", val.0))
                        }
                        // Nil,
                        // NativeFunction(NativeFunction),
                        // Function(
                        //     expr::Symbol,
                        //     /*id*/ u64,
                        //     /*this binding*/ Option<Box<Value>>,
                        //     Type,
                        // ),
                        // Class(expr::Symbol, /*id*/ u64),
                        // Instance(expr::Symbol, /*id*/ u64),
                        // List(/*id*/ u64),
                    }

                }
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
                let func = self.lookup(name).unwrap(); 
                let func_id = if let Value::Function(_, func_id, _, _) = func {
                    Ok(func_id)
                } else {
                    Err(format!("This function is not defined: {}", name.name))
                }.unwrap();
                
                let function = Function {
                    id: *func_id,
                    function_type: *function_type,
                    name: name.clone(),
                    parameters: parameters.clone(),
                    closure: self.env.clone(),
                    body: body.clone(),
                    this_binding: None,
                    superclass: None,
                    is_initializer: false,
                };

                if name.name == "main" {
                    let main_function = self.env.main_function.as_mut().unwrap().clone();
                    let content = main_function.to_string(self, file_name)?;
                    match Transpiler::write_rust_code(file_name, &content) {
                        Ok(_) => Ok(()),
                        Err(err) => Err(format!("Failed to write the rust code to the output directory. Cannot empty the output directory. Please check if the output directory exists in the project root. If it does, maybe Permission error or CARGO_MANIFEST_DIR env variable is not set to the project root? Error: {}", err)),
                    }?;
                } else {
                    let content = function.to_string(self, file_name)?;
                    match Transpiler::write_rust_code(file_name, &content) {
                        Ok(_) => Ok(()),
                        Err(err) => Err(format!("Failed to write the rust code to the output directory. Cannot empty the output directory. Please check if the output directory exists in the project root. If it does, maybe Permission error or  Maybe Permission error or CARGO_MANIFEST_DIR env variable is not set to the project root? Error: {}", err)),
                    }?;
                }

                Ok("".to_owned())
            }
            Stmt::If(cond, if_true, maybe_if_false) => {
                let cond = self.interpret_expr(cond, file_name)?;
                let mut str_code = format!("if {} {{ \n", cond.0);
                let executed_str = self.execute(if_true, file_name)?;
                str_code = format!("{} {} \n}} ", str_code, executed_str);
                if let Some(if_false) = maybe_if_false {
                    let execute_when_false = self.execute(if_false, file_name)?;
                    let if_false: &Stmt = if_false;
                    match if_false {
                        Stmt::If(_,  _, _) => {
                            str_code = format!("{} else {} ", str_code, execute_when_false);
                            
                        }
                        Stmt::Block(_) => {
                            str_code = format!("{} else {{\n {} \n}}", str_code, execute_when_false);
                        }
                        _ => panic!("This is not expected for if-selse sentence.")
                    }
                }
                Ok(str_code)
            }
            Stmt::VarDecl(sym, maybe_expr) => {
                let maybe_val_str = match maybe_expr {
                    Some(expr) => Some(self.interpret_expr(expr, file_name)),
                    None => None,
                };
                let maybe_val = match maybe_expr {
                    Some(expr) => Some(self.interpret_expr(expr, file_name)?.1),
                    None => None,
                };
                let mut_string = if sym.is_mutable {
                    "mut"
                } else {
                    ""
                };
                let value_string = if maybe_val_str.is_none() {
                    Ok("None".to_owned())
                } else {
                    let value = maybe_val_str.clone().unwrap()?;
                    match value.1 {
                        Value::Integer => Ok(format!("{}", value.0)),
                        Value::Float => Ok(format!("{}", value.0)),
                        Value::String => Ok(format!("\"{}\"", value.0)),
                        Value::Bool => Ok(format!("{}", value.0)),
                        Value::Nil => Ok(format!("None")),
                        Value::Instance(_, _) => todo!(),
                        Value::Variable(_, _) => todo!(),
                        Value::List(_) => todo!(),
                        _ => Err(format!("You cannot assign this value to a variable: \"{}\"", value.0))
                    }
                }?;
                self.env.define(sym.clone(), maybe_val);
                Ok(format!("let {} {} = {};", mut_string, sym.name, value_string))
            }
            Stmt::Block(stmts) => {
                let mut executed_str = "".to_string();
                for stmt in stmts.iter() {
                    executed_str = format!("{}{}", executed_str, self.execute(stmt, file_name)?);
                }

                Ok(executed_str)
            }
            Stmt::While(cond, body) => {

                Ok("".to_owned())
            }
            Stmt::Return(_, maybe_res) => {
                let return_string = "return".to_owned();
                let retval = Some(if let Some(res) = maybe_res {
                    self.interpret_expr(res, file_name)?
                } else {
                    ("nil".to_owned(), Value::Nil)
                });
                Ok(format!("{} {};", return_string, retval.unwrap().0))
            }
        }
    }

    pub fn write_rust_code (file_name: &str, content: &str) -> std::io::Result<()> {
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

    pub fn lookup(&self, sym: &Symbol) -> Result<&Value, String> {
        match self.env.get(sym) {
            Ok(val) => Ok(val),
            Err(_) => Err(format!("This value is not defined: {}", sym.name)),
        }
    }

    fn interpret_expr(&mut self, expr: &Expr, file_name: &str) -> Result<(String, Value), String> {
        match expr {
            Expr::This(source_location) => Ok(("nil".to_string(), Value::Nil)),
            Expr::Literal(lit) => {
                let interpreted_value = Transpiler::interpret_literal(lit);
                let mut lit_string = lit.to_string();
                if functions::type_of(&interpreted_value) == Type::Float && !lit_string.contains(".") {
                    lit_string = format!("{}.0", lit_string);
                }
                Ok((lit_string, interpreted_value))
            },
            Expr::Unary(op, e) => Ok(("nil".to_string(), Value::Nil)),
            Expr::Binary(lhs, op, rhs) => self.interpret_binary(file_name, lhs, *op, rhs),
            Expr::Call(callee, loc, args) => {
                let returned_value = self.call(callee, loc, args, file_name);
                returned_value
            },
            Expr::Get(lhs, attr) => Ok(("nil".to_string(), Value::Nil)),
            Expr::Set(lhs, attr, rhs) => Ok(("nil".to_string(), Value::Nil)),
            Expr::Grouping(e) => self.interpret_expr(e, file_name),
            Expr::Variable(sym) => {
                match self.lookup(sym) {
                    Ok(val) => Ok((sym.name.to_string(), val.clone())),
                    Err(err) => Err(err),
                }
            },
            Expr::Assign(sym, val_expr) => {
                let val = self.interpret_expr(val_expr, file_name)?;

                self.env.assign(sym.clone(), &val.1)?;

                Ok(val)
            }
            Expr::Logical(left_expr, LogicalOp::Or, right_expr) => {
                let left = self.interpret_expr(left_expr, file_name)?;
                Ok(self.interpret_expr(right_expr, file_name)?)
            }
            Expr::Logical(left_expr, LogicalOp::And, right_expr) => {
                let left = self.interpret_expr(left_expr, file_name)?;
                Ok(self.interpret_expr(right_expr, file_name)?)
            }
            Expr::Super(source_location, sym) => Ok(("nil".to_string(), Value::Nil)),
            Expr::List(elements) => Ok(("nil".to_string(), Value::Nil)),
            Expr::Subscript {
                value,
                slice,
                source_location,
            } => Ok(("nil".to_string(), Value::Nil)),
            Expr::SetItem {
                lhs,
                slice,
                rhs,
                source_location,
            } => Ok(("nil".to_string(), Value::Nil)),
            Expr::Lambda(lambda_decl) => Ok(("nil".to_string(), Value::Nil)),
        }
    }

    fn interpret_binary(
        &mut self,
        file_name: &str,
        lhs_expr: &Expr,
        op: BinaryOp,
        rhs_expr: &Expr,
    ) -> Result<(std::string::String, Value), String> {
        let lhs = self.interpret_expr(lhs_expr, file_name)?;
        let rhs = self.interpret_expr(rhs_expr, file_name)?;

        match (&lhs.1, op.ty, &rhs.1) {
            // Integer
            (Value::Integer, BinaryOpTy::Less, Value::Integer) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Bool))
            }
            (Value::Integer, BinaryOpTy::LessEqual, Value::Integer) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Bool))
            }
            (Value::Integer, BinaryOpTy::Greater, Value::Integer) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Bool))
            }
            (Value::Integer, BinaryOpTy::GreaterEqual, Value::Integer) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Bool))
            }
            (Value::Integer, BinaryOpTy::Plus, Value::Integer) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Integer))
            }
            (Value::Integer, BinaryOpTy::Minus, Value::Integer) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Integer))
            }
            (Value::Integer, BinaryOpTy::Star, Value::Integer) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Integer))
            }
            (Value::Integer, BinaryOpTy::Slash, Value::Integer) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Integer))
            }
            // Float
            (Value::Float, BinaryOpTy::Less, Value::Float) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Bool))
            }
            (Value::Float, BinaryOpTy::LessEqual, Value::Float) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Bool))
            }
            (Value::Float, BinaryOpTy::Greater, Value::Float) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Bool))
            }
            (Value::Float, BinaryOpTy::GreaterEqual, Value::Float) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Bool))
            }
            (Value::Float, BinaryOpTy::Plus, Value::Float) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Float))
            }
            (Value::Float, BinaryOpTy::Minus, Value::Float) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Float))
            }
            (Value::Float, BinaryOpTy::Star, Value::Float) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Float))
            }
            (Value::Float, BinaryOpTy::Slash, Value::Float) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Float))
            }
            // Comparison
            (_, BinaryOpTy::EqualEqual, _) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Bool))
            }
            (_, BinaryOpTy::NotEqual, _) => {
                Ok((format!("{} {} {}", &lhs.0, op, &rhs.0).to_owned(), Value::Bool))
            },
            _ => Err(format!(
                "invalid operands in binary operator {:?} of type {:?} and {:?} at line={},col={}",
                op.ty,
                functions::type_of(&lhs.1),
                functions::type_of(&rhs.1),
                op.line,
                op.col
            )),
        }
    }

    fn call(
        &mut self,
        callee_expr: &Expr,
        loc: &SourceLocation,
        arg_exprs: &[Expr],
        file_name: &str
    ) -> Result<(String, Value), String> {
        let callee = self.interpret_expr(callee_expr, file_name)?;
        match as_callable(self, &callee.1) {
            Some(callable) => {
                let maybe_args: Result<Vec<(String, Value)>, String> = arg_exprs
                    .iter()
                    .map(|arg| {
                        let res = self.interpret_expr(arg, file_name);
                        match res {
                            Ok((var_name, val)) => Ok((var_name, val)),
                            Err(e) => Err(e),
                        }
                    })
                .collect::<Result<Vec<_>, _>>();
                match maybe_args {
                    Ok(args) => {
                        if args.len() != callable.arguments(self) as usize {
                            Err(format!(
                                "Invalid call at line={},col={}: callee has {} arguments, but \
                                         was called with {} arguments",
                                loc.line,
                                loc.col,
                                callable.arguments(self),
                                args.len()
                            ))
                        } else {
                            callable.call(self, &args, file_name)
                        }
                    }
                    Err(err) => Err(err),
                }
            }
            None => Err(format!(
                "value {:?} is not callable at line={},col={}",
                callee.0, loc.line, loc.col
            )),
        }
    }

    fn interpret_literal(lit: &Literal) -> Value {
        match lit {
            Literal::Integer(n) => Value::Integer,
            Literal::Float(n) => Value::Float,
            Literal::String(s) => Value::String,
            Literal::True => Value::Bool,
            Literal::False => Value::Bool,
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
