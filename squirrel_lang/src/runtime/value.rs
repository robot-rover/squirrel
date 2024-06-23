use std::{cell::RefCell, hash::Hash, mem};
use std::{collections::HashMap, ptr::NonNull, rc::Rc};

use crate::parser::ast::{self};

use super::{
    sqrc::{ObjectStrg, SqRc, SqRcEnum, SqRef, SqRefAnc, SqWk, StringStrg},
    Context, ExecError,
};

pub type NativeFn = fn(*mut Context, Vec<Value>) -> Result<Value, ExecError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Float(f64),
    Integer(i64),
    Null,
    NativeFn(NativeFn),
    Rc(SqRc),
}

pub enum ValueRef<'a> {
    Float(f64),
    Integer(i64),
    Null,
    NativeFn(NativeFn),
    Rc(SqRefAnc<'a>),
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
    };
}

macro_rules! value_ref_redirect {
    ($name:ident($($arg:ident : $argty:ty),*) -> $ret:ty) => {
        pub fn $name(&self, $($arg : $argty),*) -> $ret {
            self.borrow().$name($($arg),*)
        }
    }
}

impl Value {
    value_common_impl!();

    value_ref_redirect!(truthy() -> bool);
    value_ref_redirect!(get_field(key: &HashValue) -> Option<Value>);
    value_ref_redirect!(get_field_str(key: &str) -> Option<Value>);
    value_ref_redirect!(type_str() -> &'static str);

    pub fn float(val: f64) -> Self {
        Self::Float(val)
    }

    pub fn borrow(&self) -> ValueRef {
        match self {
            Value::Float(f) => ValueRef::Float(*f),
            Value::Integer(i) => ValueRef::Integer(*i),
            Value::Null => ValueRef::Null,
            Value::NativeFn(n) => ValueRef::NativeFn(*n),
            Value::Rc(rc) => ValueRef::Rc(rc.borrow()),
        }
    }
}

impl<'a> ValueRef<'a> {
    pub fn truthy(&self) -> bool {
        match self {
            Self::Integer(val) if *val == 0 => false,
            Self::Float(b) if *b == 0.0 => false,
            Self::Null => false,
            _ => true,
        }
    }

    pub fn get_field(&self, key: &HashValue) -> Option<Value> {
        match self {
            ValueRef::Float(f) => todo!(),
            ValueRef::Integer(i) => todo!(),
            ValueRef::Null => todo!(),
            ValueRef::NativeFn(n) => todo!(),
            ValueRef::Rc(rc) => rc.as_ref().get_field(key),
        }
    }

    pub fn get_field_str(&self, key: &str) -> Option<Value> {
        // TODO: Inefficient
        self.get_field(&HashValue::string(key))
    }

    pub fn type_str(&self) -> &'static str {
        match self {
            ValueRef::Null => "null",
            ValueRef::Integer(_) => "integer",
            ValueRef::Float(_) => "float",
            ValueRef::NativeFn(_) => "function",
            ValueRef::Rc(rc) => match rc.as_ref() {
                SqRef::String(_) => "string",
                SqRef::Array(_) => "array",
                SqRef::Closure(_) => "function",
                SqRef::Object(_) => "object",
            },
        }
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
    type Error = Value;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Integer(val) => Ok(HashValue::Integer(val)),
            Value::Null => Ok(HashValue::Null),
            Value::NativeFn(val) => Ok(HashValue::NativeFn(val)),
            Value::Rc(val) => Ok(HashValue::Rc(val)),
            other => Err(other),
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
    pub fn new(
        delegate: Option<Rc<ObjectStrg>>,
        slots: HashMap<HashValue, Value>,
        is_class_inst: bool,
    ) -> Object {
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

    pub fn set_field(&mut self, key: HashValue, value: Value, is_newslot: bool) -> bool {
        if is_newslot || self.slots.contains_key(&key) {
            self.slots.insert(key, value);
            true
        } else {
            match &mut self.delegate {
                Some(delegate) => {
                    let mut parent = delegate.get_data().borrow_mut();
                    parent.set_field(key, value, is_newslot)
                }
                None => false,
            }
        }
    }

    pub fn get_field(&self, key: &HashValue) -> Option<Value> {
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

    pub fn get_field_str(&self, key: &str) -> Option<Value> {
        // TODO: Inefficient
        let key = HashValue::string(key);
        self.get_field(&key)
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
