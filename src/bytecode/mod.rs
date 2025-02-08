pub mod bytecode_interpreter;
pub mod bytecode;
pub mod builtins;

pub enum StepResult<T, E> {
    Ok(T),
    OkReturn(T),
    Err(E),
}
