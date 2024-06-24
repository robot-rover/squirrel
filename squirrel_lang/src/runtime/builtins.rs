use crate::runtime::{value::{Value, TypeName, Object}, Context, ExecError};
use crate::runtime::{argparse, CallInfo, ExprResult};
use crate::context::Span;
use std::{io::Write, rc::Rc};
use std::cell::RefCell;
use hashbrown::HashMap;

// pub type NativeFn = fn(*mut Context, Vec<Value>) -> Result<Value, ExecError>;

macro_rules! make_delegate {
    ($($field:ident),*) => {
        pub fn delegate(field: &str) -> Option<Value> {
            match field {
                $(stringify!($field) => Some(Value::NativeFn($field)),)*
                _ => None,
            }
        }
    };
}

macro_rules! forward_functions {
    ($this:ty, $inner:ty; $($name:ident),*) => {
        $(fn $name(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
            let this = get_this::<$this>(this, call_info)?;
            <$inner>::$name(context, this, args, call_info)
        })*
    };
}


fn get_this<T: TypeName>(this: Value, call_info: &CallInfo) -> Result<T, ExecError> {
    Ok(T::typed_from(this).map_err(|wrong| ExecError::wrong_this_type(call_info.clone(), T::type_name().to_string(), wrong.type_str().to_string()))?)
}

fn tofloat(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
    argparse::parse0(args, call_info)?;
    match this {
        Value::Float(f) => Ok(f),
        Value::Integer(i) => Ok(i as f64),
        Value::String(s) => str::parse::<f64>(&s).map_err(
            |e| ExecError::number_parse_error(call_info.func_span, e.to_string())
        ),
        other => Err(ExecError::wrong_arg_type(call_info.clone(), 0, "Float, Int, String".to_string(), other.type_str().to_string()))
    }.map(Value::Float)
}

fn tointeger(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
    // TODO: Handle 2nd arg as the base
    argparse::parse0(args, call_info)?;
    match this {
        Value::Float(f) => Ok(f as i64),
        Value::Integer(i) => Ok(i),
        Value::String(s) => str::parse::<i64>(&s).map_err(
            |e| ExecError::number_parse_error(call_info.func_span, e.to_string())
        ),
        other => Err(ExecError::wrong_arg_type(call_info.clone(), 0, "Float, Int, String".to_string(), other.type_str().to_string()))
    }.map(Value::Integer)
}

fn tostring(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
    argparse::parse0(args, call_info)?;
    Ok(Value::String(Rc::from(this.to_string())))
}

fn weakref(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
    todo!()
}

fn len(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
    argparse::parse0(args, call_info)?;
    let l = match this {
        Value::String(s) => s.len(),
        Value::Object(o) => o.borrow().len(),
        Value::Array(a) => a.borrow().len(),
        _ => return Err(ExecError::wrong_this_type(call_info.clone(), "String, Object, Array".to_string(), this.type_str().to_string()))
    };
    Ok(Value::Integer(l as i64))
}

pub mod integer {
    use super::*;

    fn tochar(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<i64>(this, call_info)?;
        argparse::parse0(args, call_info)?;
        Ok(Value::String(Rc::from(format!("{}", this as u8 as char))))
    }

    make_delegate!(tofloat, tostring, tointeger, tochar, weakref);
}

pub mod string {
    use std::ops::Deref;

    use super::*;

    fn slice(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<str>>(this, call_info)?;
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

    fn find(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<str>>(this, call_info)?;
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

    fn toupper(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<str>>(this, call_info)?;
        argparse::parse0(args, call_info)?;
        Ok(Value::String(Rc::from(this.to_uppercase())))
    }

    fn tolower(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<str>>(this, call_info)?;
        argparse::parse0(args, call_info)?;
        Ok(Value::String(Rc::from(this.to_lowercase())))
    }

    make_delegate!(len, tointeger, tofloat, tostring, slice, find, toupper, tolower, weakref);
}

pub mod object {
    use super::*;

    forward_functions!(Rc<RefCell<Object>>, Object; rawget, rawin, getdelegate, filter, keys, values, rawset, rawdelete, clear, setdelegate);

    make_delegate!(len, rawget, rawset, rawdelete, rawin, weakref, tostring, clear, setdelegate, getdelegate, filter, keys, values);
}

pub mod array {
    use std::iter;

    use super::*;
    // TODO: Most of these panic on out of bounds access, fix that

    fn push(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<RefCell<Vec<Value>>>>(this, call_info)?;
        let arg = argparse::parse1::<Value>(args, call_info)?;
        this.borrow_mut().push(arg);
        Ok(Value::Array(this))
    }

    fn append(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        push(context, this, args, call_info)
    }

    fn extend(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!("how to do iteration?")
    }

    fn pop(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<RefCell<Vec<Value>>>>(this, call_info)?;
        argparse::parse0(args, call_info)?;
        let ret = this.borrow_mut().pop();
        // TODO: is this null or an error
        Ok(ret.unwrap_or(Value::Null))
    }

    fn top(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!("what does this function do LOL")
    }

    fn insert(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<RefCell<Vec<Value>>>>(this, call_info)?;
        let (idx, val) = argparse::parse2::<i64, Value>(args, call_info)?;
        // TODO: support negative indexes here?
        this.borrow_mut().insert(idx as usize, val);
        Ok(Value::Array(this))
    }

    fn remove(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<RefCell<Vec<Value>>>>(this, call_info)?;
        let idx = argparse::parse1::<i64>(args, call_info)?;
        // TODO: support negative indexes here?
        let ret = this.borrow_mut().remove(idx as usize);
        Ok(Value::Array(this))
    }

    fn resize(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<RefCell<Vec<Value>>>>(this, call_info)?;
        argparse::validate_num_args(1..2, args.len(), call_info)?;
        let mut arg_iter = args.into_iter();
        let size = argparse::convert_arg::<i64>(arg_iter.next().unwrap(), 0, call_info)?;
        let fill = arg_iter.next().unwrap_or(Value::Null);
        let current_size = this.borrow().len();
        {
            let mut vec = this.borrow_mut();
            match (size as usize).cmp(&current_size) {
                std::cmp::Ordering::Less => {
                    vec.truncate(size as usize)
                },
                std::cmp::Ordering::Equal => {},
                std::cmp::Ordering::Greater => {
                    vec.extend(iter::repeat(fill).take(size as usize - current_size));
                },
            }
        }
        Ok(Value::Array(this))
    }

    fn sort(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!("Sorting metamethods and calling back into squirrel")
    }

    fn reverse(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<RefCell<Vec<Value>>>>(this, call_info)?;
        argparse::parse0(args, call_info)?;
        this.borrow_mut().reverse();
        Ok(Value::Array(this))
    }

    fn slice(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!("Clean up string slice method then make it a top level func in builtins")
    }

    fn clear(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<RefCell<Vec<Value>>>>(this, call_info)?;
        argparse::parse0(args, call_info)?;
        this.borrow_mut().clear();
        // TODO: Return is not documented
        Ok(Value::Array(this))
    }

    fn map(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!("How to do iteration? and call back into squirrel")
    }

    fn apply(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!("How to do iteration? and call back into squirrel")
    }

    fn reduce(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!("How to do iteration? and call back into squirrel")
    }

    fn filter(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!("How to do iteration? and call back into squirrel")
    }

    fn find(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Rc<RefCell<Vec<Value>>>>(this, call_info)?;
        let needle = argparse::parse1::<Value>(args, call_info)?;
        let found = this.borrow().iter().position(|v| v == &needle);
        Ok(found.map(|i| Value::Integer(i as i64)).unwrap_or(Value::Null))
    }

    make_delegate!(len, append, push, extend, pop, top, insert, remove, resize, sort, reverse, slice, weakref, tostring, clear, map, apply, filter, find);
}

pub mod global {
    use std::mem::{self, size_of};

    use crate::runtime::value::HashValue;

    use super::*;

    fn array(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        argparse::validate_num_args(1..2, args.len(), call_info)?;
        let mut arg_iter = args.into_iter();
        let size = argparse::convert_arg::<i64>(arg_iter.next().unwrap(), 0, call_info)?;
        let fill = arg_iter.next().unwrap_or(Value::Null);
        Ok(Value::array(vec![fill; size as usize]))
    }

    fn seterrorhandler(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn callee(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        argparse::parse0(args, call_info)?;
        Ok(Value::Closure(unsafe { (*context).infunc.closure.clone() }))
    }

    fn setdebughook(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn enabledebuginfo(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn getroottable(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        argparse::parse0(args, call_info)?;
        Ok(Value::Object(unsafe { (*context).vm_state.root_table.clone() }))
    }

    fn setroottable(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let new_root = argparse::parse1::<Value>(args, call_info)?;
        let mut new_root = match new_root {
            Value::Object(obj) => obj,
            other => return Err(ExecError::wrong_arg_type(call_info.clone(), 0, "Object|null".to_string(), other.type_str().to_string()))
        };
        mem::swap(&mut new_root, unsafe { &mut (*context).vm_state.root_table });
        Ok(Value::Object(new_root))
    }

    fn getconsttable(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn setconsttable(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn assert(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        argparse::validate_num_args(1..2, args.len(), call_info)?;
        let mut arg_iter = args.into_iter();
        let exp = arg_iter.next().unwrap();
        let message = arg_iter.next();
        if !exp.truthy() {
            return Err(ExecError::assertion_failed(call_info.func_span | call_info.call_span, message.map(|m| m.to_string())));
        }
        Ok(Value::Null)
    }

    fn print(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let mut stdout = &mut unsafe { &mut *context }.vm_state.stdout;
        let arg = argparse::parse1::<Value>(args, call_info)?;
        write!(stdout, "{}", arg).unwrap();
        Ok(Value::Null)
    }

    fn error(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let mut stderr = &mut unsafe { &mut *context }.vm_state.stderr;
        let arg = argparse::parse1::<Value>(args, call_info)?;
        write!(stderr, "{}", arg).unwrap();
        Ok(Value::Null)
    }

    fn compilestring(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn collectgarbage(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn resurrectunreachable(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn _type(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let this = get_this::<Value>(this, call_info)?;
        argparse::parse0(args, call_info)?;
        Ok(Value::String(Rc::from(this.type_str())))
    }

    fn getstackinfos(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    fn newthread(context: *mut Context, this: Value, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!()
    }

    const _versionnumber_: i64 = 100;
    const _version_: &str = "1.0.0";
    const _charsize_: i64 = size_of::<u8>() as i64;
    const _intsize_: i64 = size_of::<i64>() as i64;
    const _floatsize_: i64 = size_of::<f64>() as i64;

    pub fn make_root_table() -> Object {
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