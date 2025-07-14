pub mod bytecode;
pub mod bytecode_interpreter;
mod frames;
mod functions;
mod gc;
mod stack;
mod eval;
mod jit;
pub mod values;

pub enum StepResult<T, E> {
    Ok(T),
    OkReturn(T),
    Err(E),
}

