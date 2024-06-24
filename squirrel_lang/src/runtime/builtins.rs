use crate::runtime::{value::{Value, TypeName, Object}, Context, ExecError};
use crate::runtime::{argparse, CallInfo, ExprResult};
use crate::context::Span;
use std::{io::Write, rc::Rc};
use std::cell::RefCell;

// pub type NativeFn = fn(*mut Context, Vec<Value>) -> Result<Value, ExecError>;

macro_rules! make_delegate {
    ($del_name:ident, $($field:ident),*) => {
        fn integer_delegate(field: &str, span: Span) -> ExprResult {
            match field {
                $(stringify!($field) => Ok(Value::NativeFn($field)),)*
                _ => Err(ExecError::undefined_field(span, Value::string(field)))
            }
        }
    };
}

macro_rules! forward_functions {
    ($this:ty, $inner:ty; $($name:ident),*) => {
        $(fn $name(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
            let this = get_this::<$this>(context, call_info)?;
            <$inner>::$name(context, this, args, call_info)
        })*
    };
}


fn get_this<T: TypeName>(context: *mut Context, call_info: &CallInfo) -> Result<T, ExecError> {
    let this = unsafe { (*context).infunc.env.clone() };
    Ok(T::typed_from(this).map_err(|wrong| ExecError::wrong_arg_type(call_info.clone(), 0, T::type_name().to_string(), wrong.type_str().to_string()))?)
}

fn tofloat(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
    argparse::parse0(args, call_info)?;
    let this = unsafe { (*context).infunc.env.clone() };
    match this {
        Value::Float(f) => Ok(f),
        Value::Integer(i) => Ok(i as f64),
        Value::String(s) => str::parse::<f64>(&s).map_err(
            |e| ExecError::number_parse_error(call_info.func_span, e.to_string())
        ),
        other => Err(ExecError::wrong_arg_type(call_info.clone(), 0, "Float, Int, String".to_string(), other.type_str().to_string()))
    }.map(Value::Float)
}

fn tointeger(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
    // TODO: Handle 2nd arg as the base
    argparse::parse0(args, call_info)?;
    let this = unsafe { (*context).infunc.env.clone() };
    match this {
        Value::Float(f) => Ok(f as i64),
        Value::Integer(i) => Ok(i),
        Value::String(s) => str::parse::<i64>(&s).map_err(
            |e| ExecError::number_parse_error(call_info.func_span, e.to_string())
        ),
        other => Err(ExecError::wrong_arg_type(call_info.clone(), 0, "Float, Int, String".to_string(), other.type_str().to_string()))
    }.map(Value::Integer)
}

fn tostring(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
    argparse::parse0(args, call_info)?;
    let this = unsafe { (*context).infunc.env.clone() };
    Ok(Value::String(Rc::from(this.to_string())))
}

fn weakref(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
    todo!()
}

pub mod integer {
    use super::*;

    fn tochar(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<i64>(context, call_info)?;
        argparse::parse0(args, call_info)?;
        Ok(Value::String(Rc::from(format!("{}", this as u8 as char))))
    }

    make_delegate!(integer_delegate, tofloat, tostring, tointeger, tochar, weakref);
}

pub mod string {
    use std::ops::Deref;

    use super::*;

    fn len(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<str>>(context, call_info)?;
        argparse::parse0(args, call_info)?;
        Ok(Value::Integer(this.len() as i64))
    }

    fn slice(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<str>>(context, call_info)?;
        argparse::validate_num_args(1..2, args.len(), call_info)?;
        let mut arg_iter = args.into_iter();
        let mut start = argparse::convert_arg::<i64>(arg_iter.next().unwrap(), 0, call_info)?;
        let mut end = arg_iter.next().map(|v| argparse::convert_arg::<i64>(v, 0, call_info)).unwrap_or(Ok(this.len() as i64))?;

        if start < -(this.len() as i64) || start >= this.len() as i64 {
            return Err(ExecError::index_out_of_bounds(start, this.len(), call_info.call_span));
        }
        if start < 0 {
            start += this.len() as i64;
        }

        if end < -(this.len() as i64) || end >= this.len() as i64 {
            return Err(ExecError::index_out_of_bounds(end, this.len(), call_info.call_span));
        }
        if end < 0 {
            end += this.len() as i64;
        }

        Ok(Value::String(Rc::from(&this[start as usize..end as usize])))
    }

    fn find(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<str>>(context, call_info)?;
        argparse::validate_num_args(1..2, args.len(), call_info)?;
        let mut arg_iter = args.into_iter();
        let needle = argparse::convert_arg::<Rc<str>>(arg_iter.next().unwrap(), 0, call_info)?;
        let start = match arg_iter.next() {
            Some(this) => argparse::convert_arg::<i64>(this, 0, call_info)? as usize,
            None => 0,
        };

        // TODO: Check if negative index is allowed here
        let start = if start < 0 {
            this.len() + start
        } else {
            start
        };

        let found = this[start..].find(needle.deref());
        Ok(found.map(|i| Value::Integer(i as i64)).unwrap_or(Value::Null))
    }

    fn toupper(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<str>>(context, call_info)?;
        argparse::parse0(args, call_info)?;
        Ok(Value::String(Rc::from(this.to_uppercase())))
    }

    fn tolower(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<str>>(context, call_info)?;
        argparse::parse0(args, call_info)?;
        Ok(Value::String(Rc::from(this.to_lowercase())))
    }

    make_delegate!(string_delegate, len, tointeger, tofloat, tostring, slice, find, toupper, tolower, weakref);
}

pub mod object {
    use super::*;

    forward_functions!(Rc<RefCell<Object>>, Object; len, rawget, rawin, getdelegate, filter, keys, values, rawset, rawdelete, clear, setdelegate);

    make_delegate!(object_delegate, len, rawget, rawset, rawdelete, rawin, weakref, tostring, clear, setdelegate, getdelegate, filter, keys, values);
}

pub mod global {
    use std::{collections::HashMap, hash::Hash, mem::{self, size_of}};

    use crate::runtime::value::HashValue;

    use super::*;

    fn array(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        argparse::validate_num_args(1..2, args.len(), call_info)?;
        let mut arg_iter = args.into_iter();
        let size = argparse::convert_arg::<i64>(arg_iter.next().unwrap(), 0, call_info)?;
        let fill = arg_iter.next().unwrap_or(Value::Null);
        Ok(Value::array(vec![fill; size as usize]))
    }

    fn seterrorhandler(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn callee(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        argparse::parse0(args, call_info)?;
        Ok(Value::Closure(unsafe { (*context).infunc.closure.clone() }))
    }

    fn setdebughook(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn enabledebuginfo(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn getroottable(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        argparse::parse0(args, call_info)?;
        Ok(Value::Object(unsafe { (*context).vm_state.root_table.clone() }))
    }

    fn setroottable(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let new_root = argparse::parse1::<Value>(args, call_info)?;
        let mut new_root = match new_root {
            Value::Object(obj) => obj,
            other => return Err(ExecError::wrong_arg_type(call_info.clone(), 0, "Object|null".to_string(), other.type_str().to_string()))
        };
        mem::swap(&mut new_root, unsafe { &mut (*context).vm_state.root_table });
        Ok(Value::Object(new_root))
    }

    fn getconsttable(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn setconsttable(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn assert(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        argparse::validate_num_args(1..2, args.len(), call_info)?;
        let mut arg_iter = args.into_iter();
        let exp = arg_iter.next().unwrap();
        let message = arg_iter.next();
        if !exp.truthy() {
            return Err(ExecError::assertion_failed(call_info.func_span | call_info.call_span, message.map(|m| m.to_string())));
        }
        Ok(Value::Null)
    }

    fn print(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let mut stdout = &mut unsafe { &mut *context }.vm_state.stdout;
        let arg = argparse::parse1::<Value>(args, call_info)?;
        write!(stdout, "{}", arg).unwrap();
        Ok(Value::Null)
    }

    fn error(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let mut stderr = &mut unsafe { &mut *context }.vm_state.stderr;
        let arg = argparse::parse1::<Value>(args, call_info)?;
        write!(stderr, "{}", arg).unwrap();
        Ok(Value::Null)
    }

    fn compilestring(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn collectgarbage(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn resurrectunreachable(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn _type(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Value>(context, call_info)?;
        argparse::parse0(args, call_info)?;
        Ok(Value::String(Rc::from(this.type_str())))
    }

    fn getstackinfos(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn newthread(context: *mut Context, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    const _versionnumber_: i64 = 100;
    const _version_: &str = "1.0.0";
    const _charsize_: i64 = size_of::<u8>() as i64;
    const _intsize_: i64 = size_of::<i64>() as i64;
    const _floatsize_: i64 = size_of::<f64>() as i64;

    pub fn make_root_table() -> Object {
        let mut slots = vec![
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
            ("resurrectunreachable", Value::NativeFn(resurrectunreachable)),
            ("type", Value::NativeFn(_type)),
            ("getstackinfos", Value::NativeFn(getstackinfos)),
            ("newthread", Value::NativeFn(newthread)),
            ("_versionnumber_", Value::Integer(_versionnumber_)),
            ("_version_", Value::String(Rc::from(_version_))),
            ("_charsize_", Value::Integer(_charsize_)),
            ("_intsize_", Value::Integer(_intsize_)),
            ("_floatsize_", Value::Integer(_floatsize_)),
        ].into_iter().map(|(k,v)| (HashValue::string(k), v)).collect::<HashMap<_, _>>();

        Object::new(None, slots, false)
    }
}