use rustyline::{Editor, Helper, Result};
use rustyline::history::FileHistory;
use rustyline::error::ReadlineError;

pub struct LineReader {
    rl: Editor<(), FileHistory>,
    history_file: String,
    prompt: String,
}

impl Drop for LineReader {
    fn drop(&mut self) {
        self.rl.save_history(&self.history_file).ok();
    }
}

pub enum LineReadStatus {
    Line(String),
    Done,
}

impl LineReader {
    pub fn new(history_file: &str, prompt: &str) -> LineReader {
        let mut rl = Editor::<(), FileHistory>::new().expect("Failed to create Editor");
        rl.load_history(history_file).ok();
        LineReader {
            rl,
            history_file: history_file.into(),
            prompt: prompt.into(),
        }
    }

    pub fn readline(&mut self) -> LineReadStatus {
        let res = self.rl.readline(&self.prompt);

        match res {
            Ok(line) => {
                self.rl.add_history_entry(line.as_str()).ok();
                LineReadStatus::Line(line)
            }
            Err(ReadlineError::Eof | ReadlineError::Interrupted) => LineReadStatus::Done,
            Err(_) => LineReadStatus::Done,
        }
    }
}