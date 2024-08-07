use crate::{
    sq_error::Span,
    vm::{
        bytecode::Reg,
        error::{ExecError, ExecResult},
        value::{TypeName, Value},
    },
};
use hashbrown::HashMap;
use std::cell::RefCell;
use std::{io::Write, rc::Rc};

use super::VMState;

// pub type NativeFn = fn(*mut VMState, Vec<Value>) -> Result<Value, ExecError>;

macro_rules! make_delegate {
    ($($field:ident),* $(,)?) => {
        pub fn delegate(field: &str) -> Option<Value> {
            match field {
                $(stringify!($field) => Some(Value::NativeFn($field)),)*
                _ => None,
            }
        }
    };
}

macro_rules! forward_functions {
    ($this:ty, $inner:ty; $($name:ident),* $(,)?) => {
        $(fn $name(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
            let this = get_this::<$this>(this)?;
            <$inner>::$name(context, this, args)
        })*
    };
}

fn adjust_index(start: i64, end: Option<i64>, len: usize) -> Result<(usize, usize), ExecError> {
    // let ilen = len as i64;
    // if start < -ilen || start >= ilen {
    //     return Err(ExecError::index_out_of_bounds(start, len, span));
    // }
    // let start = if start < 0 { start + ilen } else { start } as usize;

    // let end = if let Some(end) = end {
    //     if end < -ilen || end > ilen {
    //         return Err(ExecError::index_out_of_bounds(end, len, span));
    //     }
    //     (if end < 0 { end + ilen } else { end } as usize)
    // } else {
    //     len
    // };

    // if start > end {
    //     return Err(ExecError::index_out_of_bounds(end as i64, len, span));
    // }

    // Ok((start, end))
    todo!()
}

fn get_this<T: TypeName>(this: Value) -> Result<T, ExecError> {
    todo!()
}

fn tofloat(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
    // argparse::parse0(args, call_info)?;
    // match this {
    //     Value::Float(f) => Ok(f),
    //     Value::Integer(i) => Ok(i as f64),
    //     Value::String(s) => str::parse::<f64>(&s)
    //         .map_err(|e| ExecError::number_parse_error(call_info.func_span, e.to_string())),
    //     other => Err(ExecError::wrong_arg_type(
    //         call_info.clone(),
    //         0,
    //         "Float, Int, String".to_string(),
    //         other.type_str().to_string(),
    //     )),
    // }
    // .map(Value::Float)
    todo!()
}

fn tointeger(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
    // TODO: Handle 2nd arg as the base
    // argparse::parse0(args, call_info)?;
    // match this {
    //     Value::Float(f) => Ok(f as i64),
    //     Value::Integer(i) => Ok(i),
    //     Value::String(s) => str::parse::<i64>(&s)
    //         .map_err(|e| ExecError::number_parse_error(call_info.func_span, e.to_string())),
    //     other => Err(ExecError::wrong_arg_type(
    //         call_info.clone(),
    //         0,
    //         "Float, Int, String".to_string(),
    //         other.type_str().to_string(),
    //     )),
    // }
    // .map(Value::Integer)
    todo!()
}

pub fn tostring(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
    // argparse::parse0(args, call_info)?;
    // let meta = match &this {
    //     Value::Object(o) => o.borrow().get_field_str("_tostring"),
    //     Value::Instance(inst) => inst.borrow().get_field_str("_tostring"),
    //     _ => None,
    // };
    // if let Some(meta) = meta {
    //     let result = run_rawcall(
    //         unsafe { &mut *context },
    //         meta,
    //         this,
    //         Vec::new(),
    //         call_info.func_span,
    //         call_info.call_span,
    //     )?;
    //     match result {
    //         Value::String(s) => Ok(Value::String(s)),
    //         other => Err(ExecError::wrong_metamethod_return_type(
    //             call_info.func_span,
    //             call_info.call_span,
    //             "_tostring".to_string(),
    //             "String".to_string(),
    //             other.type_str().to_string(),
    //         )),
    //     }
    // } else {
    //     Ok(Value::String(Rc::from(this.to_string())))
    // }
    todo!()
}

fn weakref(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
    todo!()
}

fn len(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
    // argparse::parse0(args, call_info)?;
    // let l = match this {
    //     Value::String(s) => s.len(),
    //     Value::Object(o) => o.borrow().len(),
    //     Value::Array(a) => a.borrow().len(),
    //     _ => {
    //         return Err(ExecError::wrong_this_type(
    //             call_info.clone(),
    //             "String, Object, Array".to_string(),
    //             this.type_str().to_string(),
    //         ))
    //     }
    // };
    // Ok(Value::Integer(l as i64))
    todo!()
}

fn slice(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
    // argparse::validate_num_args(1..2, args.len(), call_info)?;
    // let mut arg_iter = args.into_iter();
    // let mut start = argparse::convert_arg::<i64>(arg_iter.next().unwrap(), 0, call_info)?;
    // let mut end = arg_iter
    //     .next()
    //     .map(|v| argparse::convert_arg::<i64>(v, 0, call_info))
    //     .transpose()?;

    // let span = call_info.call_span;
    // match this {
    //     Value::String(s) => {
    //         let len = s.len() as i64;
    //         let (start, end) = adjust_index(start, end, len as usize, span)?;
    //         Ok(Value::string(&s[start as usize..end as usize]))
    //     }
    //     Value::Array(a) => {
    //         let a = a.borrow();
    //         let len = a.len() as i64;
    //         let (start, end) = adjust_index(start, end, len as usize, span)?;
    //         Ok(Value::array(
    //             a[start as usize..end as usize].iter().cloned().collect(),
    //         ))
    //     }
    //     _ => {
    //         return Err(ExecError::wrong_this_type(
    //             call_info.clone(),
    //             "String, Array".to_string(),
    //             this.type_str().to_string(),
    //         ))
    //     }
    // }
    todo!()
}

pub mod integer {
    use super::*;

    fn tochar(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // let this = get_this::<i64>(this, call_info)?;
        // argparse::parse0(args, call_info)?;
        // Ok(Value::String(Rc::from(format!("{}", this as u8 as char))))
        todo!()
    }

    make_delegate!(tofloat, tostring, tointeger, tochar, weakref);
}

pub mod string {
    use std::ops::Deref;

    use super::*;

    fn find(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // let this = get_this::<Rc<str>>(this)?;
        // argparse::validate_num_args(1..2, args.len(), call_info)?;
        // let mut arg_iter = args.into_iter();
        // let needle = argparse::convert_arg::<Rc<str>>(arg_iter.next().unwrap(), 0, call_info)?;
        // let start = match arg_iter.next() {
        //     Some(this) => argparse::convert_arg::<i64>(this, 0, call_info)?,
        //     None => 0,
        // };

        // // TODO: Check if negative index is allowed here
        // let start = if start < 0 {
        //     this.len() as i64 + start
        // } else {
        //     start
        // };

        // let found = this[start as usize..].find(needle.deref());
        // Ok(found
        //     .map(|i| Value::Integer(i as i64))
        //     .unwrap_or(Value::Null))
        todo!()
    }

    fn toupper(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // let this = get_this::<Rc<str>>(this, call_info)?;
        // argparse::parse0(args, call_info)?;
        // Ok(Value::String(Rc::from(this.to_uppercase())))
        todo!()
    }

    fn tolower(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // let this = get_this::<Rc<str>>(this, call_info)?;
        // argparse::parse0(args, call_info)?;
        // Ok(Value::String(Rc::from(this.to_lowercase())))
        todo!()
    }

    make_delegate!(len, tointeger, tofloat, tostring, slice, find, toupper, tolower, weakref);
}

pub mod object {
    use crate::vm::value::Table;

    use super::*;

    // TODO
    // forward_functions!(Rc<RefCell<Table>>, Table; rawget, rawin, getdelegate, filter, keys, values, rawset, rawdelete, clear, setdelegate);

    make_delegate!(
        len, // rawget,
        // rawset,
        // rawdelete,
        // rawin,
        weakref,
        tostring,
        // clear,
        // setdelegate,
        // getdelegate,
        // filter,
        // keys,
        // values
    );
}

pub mod array {
    use std::iter;

    use super::*;
    // TODO: Most of these panic on out of bounds access, fix that

    fn push(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // let this = get_this::<Rc<RefCell<Vec<Value>>>>(this, call_info)?;
        // let arg = argparse::parse1::<Value>(args, call_info)?;
        // this.borrow_mut().push(arg);
        // Ok(Value::Array(this))
        todo!()
    }

    fn append(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        push(context, reg, n_args)
    }

    fn extend(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!("how to do iteration?")
    }

    fn pop(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // let this = get_this::<Rc<RefCell<Vec<Value>>>>(this)?;
        // argparse::parse0(args, call_info)?;
        // let ret = this.borrow_mut().pop();
        // // TODO: is this null or an error
        // Ok(ret.unwrap_or(Value::Null))
        todo!()
    }

    fn top(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!("what does this function do LOL")
    }

    fn insert(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // let this = get_this::<Rc<RefCell<Vec<Value>>>>(this, call_info)?;
        // let (idx, val) = argparse::parse2::<i64, Value>(args, call_info)?;
        // // TODO: support negative indexes here?
        // this.borrow_mut().insert(idx as usize, val);
        // Ok(Value::Array(this))
        todo!()
    }

    fn remove(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // let this = get_this::<Rc<RefCell<Vec<Value>>>>(this, call_info)?;
        // let idx = argparse::parse1::<i64>(args, call_info)?;
        // // TODO: support negative indexes here?
        // let ret = this.borrow_mut().remove(idx as usize);
        // Ok(Value::Array(this))
        todo!()
    }

    fn resize(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // let this = get_this::<Rc<RefCell<Vec<Value>>>>(this, call_info)?;
        // argparse::validate_num_args(1..2, args.len(), call_info)?;
        // let mut arg_iter = args.into_iter();
        // let size = argparse::convert_arg::<i64>(arg_iter.next().unwrap(), 0, call_info)?;
        // let fill = arg_iter.next().unwrap_or(Value::Null);
        // let current_size = this.borrow().len();
        // this.borrow_mut()
        //     .resize_with(size as usize, || fill.clone());
        // Ok(Value::Array(this))
        todo!()
    }

    fn sort(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!("Sorting metamethods and calling back into squirrel")
    }

    fn reverse(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // let this = get_this::<Rc<RefCell<Vec<Value>>>>(this, call_info)?;
        // argparse::parse0(args, call_info)?;
        // this.borrow_mut().reverse();
        // Ok(Value::Array(this))
        todo!()
    }

    fn clear(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // let this = get_this::<Rc<RefCell<Vec<Value>>>>(this, call_info)?;
        // argparse::parse0(args, call_info)?;
        // this.borrow_mut().clear();
        // // TODO: Return is not documented
        // Ok(Value::Array(this))
        todo!()
    }

    fn map(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!("How to do iteration? and call back into squirrel")
    }

    fn apply(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!("How to do iteration? and call back into squirrel")
    }

    fn reduce(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!("How to do iteration? and call back into squirrel")
    }

    fn filter(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!("How to do iteration? and call back into squirrel")
    }

    fn find(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // let this = get_this::<Rc<RefCell<Vec<Value>>>>(this, call_info)?;
        // let needle = argparse::parse1::<Value>(args, call_info)?;
        // let found = this.borrow().iter().position(|v| v == &needle);
        // Ok(found
        //     .map(|i| Value::Integer(i as i64))
        //     .unwrap_or(Value::Null))
        todo!()
    }

    make_delegate!(
        len, append, push, extend, pop, top, insert, remove, resize, sort, reverse, slice, weakref,
        tostring, clear, map, apply, filter, find
    );
}

pub mod global {
    use std::mem::{self, size_of};

    use crate::vm::value::{HashValue, Table};

    use super::*;

    fn array(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // argparse::validate_num_args(1..2, args.len(), call_info)?;
        // let mut arg_iter = args.into_iter();
        // let size = argparse::convert_arg::<i64>(arg_iter.next().unwrap(), 0, call_info)?;
        // let fill = arg_iter.next().unwrap_or(Value::Null);
        // Ok(Value::array(vec![fill; size as usize]))
        todo!()
    }

    fn seterrorhandler(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!()
    }

    fn callee(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // argparse::parse0(args, call_info)?;
        // Ok(Value::Closure(unsafe { (*context).infunc.closure.clone() }))
        todo!()
    }

    fn setdebughook(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!()
    }

    fn enabledebuginfo(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!()
    }

    fn getroottable(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // argparse::parse0(args, call_info)?;
        // Ok(Value::Object(unsafe {
        //     (*context).vm_state.root_table.clone()
        // }))
        todo!()
    }

    fn setroottable(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // let new_root = argparse::parse1::<Value>(args, call_info)?;
        // let mut new_root = match new_root {
        //     Value::Object(obj) => obj,
        //     other => {
        //         return Err(ExecError::wrong_arg_type(
        //             call_info.clone(),
        //             0,
        //             "Object|null".to_string(),
        //             other.type_str().to_string(),
        //         ))
        //     }
        // };
        // mem::swap(&mut new_root, unsafe {
        //     &mut (*context).vm_state.root_table
        // });
        // Ok(Value::Object(new_root))
        todo!()
    }

    fn getconsttable(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!()
    }

    fn setconsttable(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!()
    }

    fn assert(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // argparse::validate_num_args(1..2, args.len(), call_info)?;
        // let mut arg_iter = args.into_iter();
        // let exp = arg_iter.next().unwrap();
        // let message = arg_iter.next();
        // if !exp.truthy() {
        //     return Err(ExecError::assertion_failed(
        //         call_info.func_span | call_info.call_span,
        //         message.map(|m| m.to_string()),
        //     ));
        // }
        // Ok(Value::Null)
        todo!()
    }

    fn print(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        let mut stdout = &mut unsafe { &mut *context }.stdout;
        // let arg = argparse::parse1::<Value>(args, call_info)?;
        // write!(stdout, "{}", arg).unwrap();
        // Ok(Value::Null)
        if n_args != 1 {
            panic!("Wrong number of args");
        }

        // reg.nth(0) is "this"
        let arg = unsafe { &mut *context }.frame().get_reg(reg.nth(1));
        write!(stdout, "{}", arg).unwrap();

        Ok(())
    }

    fn error(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // let mut stderr = &mut unsafe { &mut *context }.vm_state.stderr;
        // let arg = argparse::parse1::<Value>(args, call_info)?;
        // write!(stderr, "{}", arg).unwrap();
        // Ok(Value::Null)
        todo!()
    }

    fn compilestring(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!()
    }

    fn collectgarbage(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!()
    }

    fn resurrectunreachable(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!()
    }

    fn _type(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        // let this = get_this::<Value>(this, call_info)?;
        // argparse::parse0(args, call_info)?;
        // Ok(Value::String(Rc::from(this.type_str())))
        todo!()
    }

    fn getstackinfos(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!()
    }

    fn newthread(context: *mut VMState, reg: Reg, n_args: u8) -> ExecResult {
        todo!()
    }

    const _VERSION_NUMBER: i64 = 100;
    const _VERSION: &str = "1.0.0";
    const _CHAR_SIZE: i64 = size_of::<u8>() as i64;
    const _INT_SIZE: i64 = size_of::<i64>() as i64;
    const _FLOAT_SIZE: i64 = size_of::<f64>() as i64;

    pub fn make_root_table() -> Table {
        let slots = vec![
            ("array", Value::NativeFn(array)),
            ("seterrorhandler", Value::NativeFn(seterrorhandler)),
            ("callee", Value::NativeFn(callee)),
            ("setdebughook", Value::NativeFn(setdebughook)),
            ("enabledebuginfo", Value::NativeFn(enabledebuginfo)),
            ("getroottable", Value::NativeFn(getroottable)),
            ("setroottable", Value::NativeFn(setroottable)),
            ("getconsttable", Value::NativeFn(getconsttable)),
            ("setconsttable", Value::NativeFn(setconsttable)),
            ("assert", Value::NativeFn(assert)),
            ("print", Value::NativeFn(print)),
            ("error", Value::NativeFn(error)),
            ("compilestring", Value::NativeFn(compilestring)),
            ("collectgarbage", Value::NativeFn(collectgarbage)),
            (
                "resurrectunreachable",
                Value::NativeFn(resurrectunreachable),
            ),
            ("type", Value::NativeFn(_type)),
            ("getstackinfos", Value::NativeFn(getstackinfos)),
            ("newthread", Value::NativeFn(newthread)),
            ("_versionnumber_", Value::Integer(_VERSION_NUMBER)),
            ("_version_", Value::String(Rc::from(_VERSION))),
            ("_charsize_", Value::Integer(_CHAR_SIZE)),
            ("_intsize_", Value::Integer(_INT_SIZE)),
            ("_floatsize_", Value::Integer(_FLOAT_SIZE)),
        ]
        .into_iter()
        .map(|(k, v)| (HashValue::string(k), v))
        .collect::<HashMap<_, _>>();

        Table::new(None, slots)
    }
}
