use crate::bytecode::bytecode_interpreter::Interpreter;

pub trait CallFuncPointer {
    fn call_func_pointer(
        &mut self,
        fn_code: *const u8,
        arguments: &[i64],
        arguments_meta: &[i64],
    ) -> Result<(i64, i64), String>;
}

impl CallFuncPointer for Interpreter {
    fn call_func_pointer(
        &mut self,
        fn_code: *const u8,
        arguments: &[i64],
        arguments_meta: &[i64],
    ) -> Result<(i64, i64), String> {
        match arguments.len() {
            0 => {
                let func: unsafe extern "C" fn(*mut Interpreter) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            1 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            2 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[1], arguments_meta[1], arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            3 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64, i64, i64, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[2], arguments_meta[2], arguments[1], arguments_meta[1], arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            4 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64, i64, i64, i64, i64, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[3], arguments_meta[3], arguments[2], arguments_meta[2], arguments[1], arguments_meta[1], arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            5 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[4], arguments_meta[4], arguments[3], arguments_meta[3], arguments[2], arguments_meta[2], arguments[1], arguments_meta[1], arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            6 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[5], arguments_meta[5], arguments[4], arguments_meta[4], arguments[3], arguments_meta[3], arguments[2], arguments_meta[2], arguments[1], arguments_meta[1], arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            7 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[6], arguments_meta[6], arguments[5], arguments_meta[5], arguments[4], arguments_meta[4], arguments[3], arguments_meta[3], arguments[2], arguments_meta[2], arguments[1], arguments_meta[1], arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            8 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[7], arguments_meta[7], arguments[6], arguments_meta[6], arguments[5], arguments_meta[5], arguments[4], arguments_meta[4], arguments[3], arguments_meta[3], arguments[2], arguments_meta[2], arguments[1], arguments_meta[1], arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            9 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[8], arguments_meta[8], arguments[7], arguments_meta[7], arguments[6], arguments_meta[6], arguments[5], arguments_meta[5], arguments[4], arguments_meta[4], arguments[3], arguments_meta[3], arguments[2], arguments_meta[2], arguments[1], arguments_meta[1], arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            10 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[9], arguments_meta[9], arguments[8], arguments_meta[8], arguments[7], arguments_meta[7], arguments[6], arguments_meta[6], arguments[5], arguments_meta[5], arguments[4], arguments_meta[4], arguments[3], arguments_meta[3], arguments[2], arguments_meta[2], arguments[1], arguments_meta[1], arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            11 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[10], arguments_meta[10], arguments[9], arguments_meta[9], arguments[8], arguments_meta[8], arguments[7], arguments_meta[7], arguments[6], arguments_meta[6], arguments[5], arguments_meta[5], arguments[4], arguments_meta[4], arguments[3], arguments_meta[3], arguments[2], arguments_meta[2], arguments[1], arguments_meta[1], arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            12 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[11], arguments_meta[11], arguments[10], arguments_meta[10], arguments[9], arguments_meta[9], arguments[8], arguments_meta[8], arguments[7], arguments_meta[7], arguments[6], arguments_meta[6], arguments[5], arguments_meta[5], arguments[4], arguments_meta[4], arguments[3], arguments_meta[3], arguments[2], arguments_meta[2], arguments[1], arguments_meta[1], arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            13 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[12], arguments_meta[12], arguments[11], arguments_meta[11], arguments[10], arguments_meta[10], arguments[9], arguments_meta[9], arguments[8], arguments_meta[8], arguments[7], arguments_meta[7], arguments[6], arguments_meta[6], arguments[5], arguments_meta[5], arguments[4], arguments_meta[4], arguments[3], arguments_meta[3], arguments[2], arguments_meta[2], arguments[1], arguments_meta[1], arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            14 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[13], arguments_meta[13], arguments[12], arguments_meta[12], arguments[11], arguments_meta[11], arguments[10], arguments_meta[10], arguments[9], arguments_meta[9], arguments[8], arguments_meta[8], arguments[7], arguments_meta[7], arguments[6], arguments_meta[6], arguments[5], arguments_meta[5], arguments[4], arguments_meta[4], arguments[3], arguments_meta[3], arguments[2], arguments_meta[2], arguments[1], arguments_meta[1], arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            15 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[14], arguments_meta[14], arguments[13], arguments_meta[13], arguments[12], arguments_meta[12], arguments[11], arguments_meta[11], arguments[10], arguments_meta[10], arguments[9], arguments_meta[9], arguments[8], arguments_meta[8], arguments[7], arguments_meta[7], arguments[6], arguments_meta[6], arguments[5], arguments_meta[5], arguments[4], arguments_meta[4], arguments[3], arguments_meta[3], arguments[2], arguments_meta[2], arguments[1], arguments_meta[1], arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            16 => {
                let func: unsafe extern "C" fn(*mut Interpreter, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> u128 =
                    unsafe { std::mem::transmute(fn_code) };
                let ret = unsafe { func(self as *mut _, arguments[15], arguments_meta[15], arguments[14], arguments_meta[14], arguments[13], arguments_meta[13], arguments[12], arguments_meta[12], arguments[11], arguments_meta[11], arguments[10], arguments_meta[10], arguments[9], arguments_meta[9], arguments[8], arguments_meta[8], arguments[7], arguments_meta[7], arguments[6], arguments_meta[6], arguments[5], arguments_meta[5], arguments[4], arguments_meta[4], arguments[3], arguments_meta[3], arguments[2], arguments_meta[2], arguments[1], arguments_meta[1], arguments[0], arguments_meta[0]) };
                let val = ret as i64;
                let meta = (ret >> 64) as i64;
                Ok((val, meta))
            },
            _ => Err(format!("unsupported argument count {}", arguments.len())),
        }
    }
}
