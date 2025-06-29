pub mod bytecode;
pub mod bytecode_interpreter;
mod frames;
mod functions;
mod gc;
mod stack;
mod eval;
pub mod values;

pub enum StepResult<T, E> {
    Ok(T),
    OkReturn(T),
    Err(E),
}
