use crate::expr;

pub struct Interpreter {
    pub counter: u64,
    pub backtrace: Vec<(u64, String)>,
}

impl Default for Interpreter {
    fn default() -> Interpreter {
        Interpreter {
            counter: 0,
            backtrace: vec![(0, "script".to_string())],
        }
    }
}

impl Interpreter {
    pub fn interpret(&mut self, stmts: &[expr::Stmt]) -> Result<(), String> {

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
