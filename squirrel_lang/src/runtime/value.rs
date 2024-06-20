use core::fmt;
use std::{alloc::{self, handle_alloc_error, Layout}, any, cell::RefCell, collections::HashMap, mem::MaybeUninit, ptr::{self, addr_of, addr_of_mut, from_raw_parts, from_raw_parts_mut, NonNull}, rc::{Rc, Weak}};
use std::hash::Hash;

use crate::{context::Span, parser::ast::{self, ExprData}};

use super::{sqrc::{ArrayStrg, ClosureStrg, ObjectStrg, SqRc, SqRcEnum, SqWk, StringStrg}, Context, ExecError};

pub type NativeFn = fn(*mut Context, Vec<Value>) -> Result<Value, ExecError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Float(f64),
    Integer(i64),
    Null,
    NativeFn(NativeFn),
    Rc(SqRc),
}

macro_rules! value_common_impl {
    () => {
        pub fn boolean(val: bool) -> Self {
            Self::Integer(if val { 1 } else { 0 })
        }

        pub fn string(val: &str) -> Self {
            Self::Rc(SqRcEnum::String(StringStrg::new(val)).stash())
        }

        pub fn array(arr: Vec<Value>) -> Self {
            Self::Rc(SqRcEnum::array(arr).stash())
        }

        pub fn closure(closure: Closure) -> Self {
            Self::Rc(SqRcEnum::closure(closure).stash())
        }

        pub fn rc_enum<T: Into<SqRcEnum>>(val: T) -> Self {
            Self::Rc(val.into().stash())
        }

        pub fn object(object: Object) -> Self {
            Self::Rc(SqRcEnum::object(object).stash())
        }

    }
}

impl Value {
    value_common_impl!();

    pub fn float(val: f64) -> Self {
        Self::Float(val)
    }

    pub fn truthy(&self) -> bool {
        match self {
            Self::Integer(val) if *val == 0 => false,
            Self::Float(b) if *b == 0.0 => false,
            Self::Null => false,
            _ => true,
        }
    }

    pub fn get_field(&self, key: &Value, span: Span) -> Result<Value, ExecError> {
        todo!()
    }

    pub fn get_field_str(&self, key: &str, span: Span) -> Result<Value, ExecError> {
        todo!()
    }

}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashValue {
    Integer(i64),
    Null,
    NativeFn(NativeFn),
    Rc(SqRc),
}

impl HashValue {
    value_common_impl!();

    pub fn truthy(&self) -> bool {
        match self {
            Self::Integer(val) if *val == 0 => false,
            Self::Null => false,
            _ => true,
        }
    }

}

impl From<HashValue> for Value {
    fn from(value: HashValue) -> Self {
        match value {
            HashValue::Integer(val) => Value::Integer(val),
            HashValue::Null => Value::Null,
            HashValue::NativeFn(val) => Value::NativeFn(val),
            HashValue::Rc(val) => Value::Rc(val),
        }
    }
}

impl TryFrom<Value> for HashValue {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Integer(val) => Ok(HashValue::Integer(val)),
            Value::Null => Ok(HashValue::Null),
            Value::NativeFn(val) => Ok(HashValue::NativeFn(val)),
            Value::Rc(val) => Ok(HashValue::Rc(val)),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Object {
    delegate: Option<Rc<ObjectStrg>>,
    slots: HashMap<HashValue, Value>,
    is_class_inst: bool,
}

impl Object {
    pub fn new(delegate: Option<Rc<ObjectStrg>>, slots: HashMap<HashValue, Value>, is_class_inst: bool) -> Object {
        Object {
            delegate,
            slots,
            is_class_inst,
        }
    }

    pub fn get_is_class_inst(&self) -> bool {
        self.is_class_inst
    }

    pub fn get_delegate(&self) -> Option<&Rc<ObjectStrg>> {
        self.delegate.as_ref()
    }

    fn get_field(&self, key: &HashValue) -> Option<Value> {
        // Todo: Have exec error here
        // TODO: This shouldn't be recursive
        if let Some(value) = self.slots.get(key) {
            return Some(value.clone().into());
        }
        match &self.delegate {
            Some(delegate) => {
                let parent = (delegate.get_data()).borrow();
                parent.get_field(key)
            }
            None => return None,
        }
    }
}

#[derive(Debug)]
pub struct Closure {
    pub ast_fn: NonNull<ast::Function>,
    pub default_vals: Vec<Value>,
    pub root: SqWk,
    pub env: Option<Value>,
}

impl Closure {
    pub fn new(ast_fn: &ast::Function, default_vals: Vec<Value>, root: SqWk) -> Closure {
        Closure {
            ast_fn: NonNull::from(ast_fn),
            default_vals,
            root,
            env: None,
        }
    }
}
