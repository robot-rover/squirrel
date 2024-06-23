use std::{cell::RefCell, fmt, hash::Hash, mem, ops::Deref, ptr};
use std::{collections::HashMap, ptr::NonNull, rc::Rc};

use crate::parser::ast::{self};

use super::{
    Context, ExecError,
};

pub type NativeFn = fn(*mut Context, Vec<Value>) -> Result<Value, ExecError>;

#[derive(Debug, Clone)]
pub enum Value {
    Float(f64),
    Integer(i64),
    Null,
    NativeFn(NativeFn),
    String(Rc<str>),
    Object(Rc<RefCell<Object>>),
    Array(Rc<RefCell<Vec<Value>>>),
    Closure(Rc<RefCell<Closure>>),
}

macro_rules! value_common_impl {
    () => {
        pub fn boolean(val: bool) -> Self {
            Self::Integer(if val { 1 } else { 0 })
        }

        pub fn string(val: &str) -> Self {
            Self::String(Rc::from(val))
        }

        pub fn array(arr: Vec<Value>) -> Self {
            Self::Array(Rc::new(RefCell::new(arr)))
        }

        pub fn closure(closure: Closure) -> Self {
            Self::Closure(Rc::new(RefCell::new(closure)))
        }

        pub fn object(object: Object) -> Self {
            Self::Object(Rc::new(RefCell::new(object)))
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

    pub fn get_field(&self, key: &HashValue) -> Option<Value> {
        match self {
            Value::Float(f) => todo!(),
            Value::Integer(i) => todo!(),
            Value::Null => todo!(),
            Value::NativeFn(n) => todo!(),
            _ => todo!(),
        }
    }

    pub fn get_field_str(&self, key: &str) -> Option<Value> {
        // TODO: Inefficient
        self.get_field(&HashValue::string(key))
    }

    pub fn type_str(&self) -> &'static str {
        match self {
            Value::Null => "null",
            Value::Integer(_) => "integer",
            Value::Float(_) => "float",
            Value::NativeFn(_) => "function",
            Value::String(_) => "string",
            Value::Array(_) => "array",
            Value::Closure(_) => "function",
            Value::Object(_) => "object",
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Float(l0), Self::Float(r0)) => *l0 == *r0,
            (Self::Integer(l0), Self::Integer(r0)) => *l0 == *r0,
            (Self::NativeFn(l0), Self::NativeFn(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Object(l0), Self::Object(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            (Self::Array(l0), Self::Array(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            (Self::Closure(l0), Self::Closure(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            (Self::Null, Self::Null) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum HashValue {
    Integer(i64),
    Null,
    NativeFn(NativeFn),
    String(Rc<str>),
    Object(Rc<RefCell<Object>>),
    Array(Rc<RefCell<Vec<Value>>>),
    Closure(Rc<RefCell<Closure>>),
}

impl PartialEq for HashValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => *l0 == *r0,
            (Self::NativeFn(l0), Self::NativeFn(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0.deref() == r0.deref(),
            (Self::Object(l0), Self::Object(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            (Self::Array(l0), Self::Array(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            (Self::Closure(l0), Self::Closure(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            (Self::Null, Self::Null) => true,
            _ => false,
        }
    }
}
impl Eq for HashValue {}
impl Hash for HashValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            HashValue::Integer(i) => i.hash(state),
            HashValue::Null => {},
            HashValue::NativeFn(nf) => nf.hash(state),
            HashValue::String(s) => s.deref().hash(state),
            HashValue::Object(o) => o.as_ptr().hash(state),
            HashValue::Array(a) => a.as_ptr().hash(state),
            HashValue::Closure(c) => c.as_ptr().hash(state),
        }
    }
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
            HashValue::String(s) => Value::String(s),
            HashValue::Object(o) => Value::Object(o),
            HashValue::Array(a) => Value::Array(a),
            HashValue::Closure(c) => Value::Closure(c),
        }
    }
}

impl TryFrom<Value> for HashValue {
    type Error = Value;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let val = match value {
            Value::Integer(val) => HashValue::Integer(val),
            Value::Null => HashValue::Null,
            Value::NativeFn(val) => HashValue::NativeFn(val),
            Value::String(s) => HashValue::String(s),
            Value::Object(o) => HashValue::Object(o),
            Value::Array(a) => HashValue::Array(a),
            Value::Closure(c) => HashValue::Closure(c),
            other => return Err(other),
        };
        Ok(val)
    }
}

macro_rules! value_variant_tryfrom {
    ($base:ident::$variant:ident($data:ty)) => {
        impl TryFrom<$base> for $data {
            type Error = $base;

            fn try_from(value: $base) -> Result<Self, Self::Error> {
                match value {
                    $base::$variant(val) => Ok(val),
                    other => Err(other),
                }
            }
        }
    };
}

value_variant_tryfrom!(Value::Integer(i64));
value_variant_tryfrom!(Value::Float(f64));
value_variant_tryfrom!(Value::NativeFn(NativeFn));
value_variant_tryfrom!(Value::String(Rc<str>));
value_variant_tryfrom!(Value::Object(Rc<RefCell<Object>>));
value_variant_tryfrom!(Value::Closure(Rc<RefCell<Closure>>));
value_variant_tryfrom!(Value::Array(Rc<RefCell<Vec<Value>>>));

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Float(n) => write!(f, "{}", n),
            Value::Integer(i) => write!(f, "{}", i),
            Value::Null => write!(f, "null"),
            Value::String(s) => write!(f, "{}", s),
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Object {
    delegate: Option<Rc<RefCell<Object>>>,
    slots: HashMap<HashValue, Value>,
    is_class_inst: bool,
}

impl Object {
    pub fn new(
        delegate: Option<Rc<RefCell<Object>>>,
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

    pub fn get_delegate(&self) -> Option<&Rc<RefCell<Object>>> {
        self.delegate.as_ref()
    }

    pub fn set_field(&mut self, key: HashValue, value: Value, is_newslot: bool) -> bool {
        if is_newslot || self.slots.contains_key(&key) {
            self.slots.insert(key, value);
            true
        } else {
            match &mut self.delegate {
                Some(delegate) => {
                    let mut parent = delegate.borrow_mut();
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
                let parent = delegate.borrow();
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
    // TODO: This should be weak
    pub root: Value,
    pub env: Option<Value>,
}

impl Closure {
    pub fn new(ast_fn: &ast::Function, default_vals: Vec<Value>, root: Value) -> Closure {
        Closure {
            ast_fn: NonNull::from(ast_fn),
            default_vals,
            root,
            env: None,
        }
    }
}
