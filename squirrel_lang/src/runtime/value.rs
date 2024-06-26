use std::{cell::RefCell, fmt, hash::Hash, mem, ops::Deref, ptr};
use std::{ptr::NonNull, rc::Rc};
use hashbrown::{Equivalent, HashMap};

use crate::parser::ast::{self, Expr};

use super::{
    argparse, builtins, CallInfo, Context, ExecError, ExprResult, FuncRuntime
};

pub type NativeFn = fn(*mut Context, Value, Vec<Value>, &CallInfo) -> Result<Value, ExecError>;

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
        if let HashValue::String(key) = key {
            self.get_field_str(key)
        } else {
            match (self, key) {
                (Value::Object(obj), any) => obj.borrow().get_field(any),
                (Value::Array(arr), HashValue::Integer(i)) => arr.borrow().get(*i as usize).cloned(),
                _ => None,
            }
        }
    }

    pub fn get_field_str(&self, key: &str) -> Option<Value> {
        match self {
            Value::Float(_) => todo!(),
            Value::Integer(_) => builtins::integer::delegate(key),
            Value::Null => todo!(),
            Value::NativeFn(_) => todo!(),
            Value::String(_) => builtins::string::delegate(key),
            Value::Object(obj) => obj.borrow().get_field_str(key),
            Value::Array(_) => builtins::array::delegate(key),
            Value::Closure(_) => todo!(),
        }
    }

    pub fn type_str(&self) -> &'static str {
        match self {
            Value::Null => "null",
            Value::Integer(_) => <i64 as TypeName>::type_name(),
            Value::Float(_) => <f64 as TypeName>::type_name(),
            Value::NativeFn(_) => <NativeFn as TypeName>::type_name(),
            Value::String(_) => <Rc<str> as TypeName>::type_name(),
            Value::Array(_) => <Rc<RefCell<Vec<Value>>> as TypeName>::type_name(),
            Value::Closure(_) => <Rc<RefCell<Closure>> as TypeName>::type_name(),
            Value::Object(_) => <Rc<RefCell<Object>> as TypeName>::type_name(),
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
        // Todo: This breaks impl Equivalent<HashValue> for str
        // core::mem::discriminant(self).hash(state);
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

impl Equivalent<HashValue> for str {
    fn equivalent(&self, key: &HashValue) -> bool {
        if let HashValue::String(s) = key {
            self == Rc::deref(s)
        } else {
            false
        }
    }
}

pub trait TypeName: Sized {
    fn type_name() -> &'static str;

    fn typed_from(value: Value) -> Result<Self, Value>;
}

impl TypeName for Value {
    fn type_name() -> &'static str {
        "any"
    }

    fn typed_from(value: Value) -> Result<Self, Value> {
        Ok(value)
    }
}

macro_rules! value_variant_tryfrom {
    ($base:ident::$variant:ident($data:ty) $name:literal) => {
        impl TypeName for $data {
            fn type_name() -> &'static str {
                $name
            }

            fn typed_from(value: $base) -> Result<Self, $base> {
                match value {
                    $base::$variant(val) => Ok(val),
                    other => Err(other),
                }
            }
        }
    };
}

// TODO: Are these names right?
value_variant_tryfrom!(Value::Integer(i64) "integer");
value_variant_tryfrom!(Value::Float(f64) "float");
value_variant_tryfrom!(Value::NativeFn(NativeFn) "function");
value_variant_tryfrom!(Value::String(Rc<str>) "string");
value_variant_tryfrom!(Value::Object(Rc<RefCell<Object>>) "table");
value_variant_tryfrom!(Value::Closure(Rc<RefCell<Closure>>) "function");
value_variant_tryfrom!(Value::Array(Rc<RefCell<Vec<Value>>>) "array");

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

#[derive(Clone)]
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

    pub fn set_field(&mut self, key: HashValue, value: Value, is_newslot: bool) -> Result<(), HashValue> {
        if is_newslot || self.slots.contains_key(&key) {
            self.slots.insert(key, value);
            Ok(())
        } else {
            match self.delegate.as_ref() {
                Some(delegate) => {
                    let mut parent = delegate.borrow_mut();
                    parent.set_field(key, value, is_newslot)
                }
                None => Err(key),
            }
        }
    }

    pub fn get_field(&self, key: &HashValue) -> Option<Value> {
        // Todo: Have exec error here
        // TODO: This shouldn't be recursive
        if let Some(value) = self.slots.get(key) {
            return Some(value.clone().into());
        }
        self.delegate.as_ref().and_then(|del| del.borrow().get_field(key)).or_else(|| match key {
            HashValue::String(s) => builtins::object::delegate(s),
            _ => None,
        })
    }

    pub fn get_field_str(&self, key: &str) -> Option<Value> {
        if let Some(value) = self.slots.get(key) {
            return Some(value.clone().into());
        }
        self.delegate.as_ref().and_then(|del| del.borrow().get_field_str(key)).or_else(|| builtins::object::delegate(key))
    }

    pub fn len(&self) -> usize {
        self.slots.len()
    }

    // Squirrel Default Delegate Functions
    pub fn rawget(context: *mut Context, this: Rc<RefCell<Object>>, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let key = argparse::parse1::<Value>(args, call_info)?;
        let hash_key: HashValue = key.try_into().map_err(|unhash: Value| ExecError::unhashable_type(unhash.type_str().to_string(), call_info.call_span))?;
        this.borrow().slots.get(&hash_key).map(|v| v.clone()).ok_or_else(|| ExecError::undefined_field(call_info.call_span, hash_key.clone().into()))
    }

    pub fn rawset(context: *mut Context, this: Rc<RefCell<Object>>, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let (key, val) = argparse::parse2::<Value, Value>(args, call_info)?;
        let hash_key = key.try_into().map_err(|unhash: Value| ExecError::unhashable_type(unhash.type_str().to_string(), call_info.call_span))?;
        this.borrow_mut().slots.insert(hash_key, val);
        Ok(Value::Object(this.clone()))
    }

    pub fn rawdelete(context: *mut Context, this: Rc<RefCell<Object>>, args: Vec<Value>, call_info: &CallInfo) -> ExprResult { let key = argparse::parse1::<Value>(args, call_info)?;
        let hash_key: HashValue = key.try_into().map_err(|unhash: Value| ExecError::unhashable_type(unhash.type_str().to_string(), call_info.call_span))?;
        Ok(this.borrow_mut().slots.remove(&hash_key).unwrap_or(Value::Null))
    }

    pub fn rawin(context: *mut Context, this: Rc<RefCell<Object>>, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let key = argparse::parse1::<Value>(args, call_info)?;
        let hash_key: HashValue = key.try_into().map_err(|unhash: Value| ExecError::unhashable_type(unhash.type_str().to_string(), call_info.call_span))?;
        Ok(Value::boolean(this.borrow().slots.contains_key(&hash_key)))
    }

    pub fn clear(context: *mut Context, this: Rc<RefCell<Object>>, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        this.borrow_mut().slots.clear();
        Ok(Value::Object(this.clone()))
    }

    pub fn setdelegate(context: *mut Context, this: Rc<RefCell<Object>>, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        let delegate = argparse::parse1::<Value>(args, call_info)?;
        let delegate = match delegate {
            Value::Null => None,
            Value::Object(obj) => Some(obj),
            _ => return Err(ExecError::wrong_arg_type(call_info.clone(), 0, String::from("object|null"), delegate.type_str().to_string())),
        };
        this.borrow_mut().delegate = delegate;
        Ok(Value::Object(this.clone()))
    }

    pub fn getdelegate(context: *mut Context, this: Rc<RefCell<Object>>, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        argparse::parse0(args, call_info)?;
        Ok(this.borrow().delegate.clone().map(|d| Value::Object(d)).unwrap_or(Value::Null))
    }

    pub fn filter(context: *mut Context, this: Rc<RefCell<Object>>, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        todo!();
        let filter_fn = argparse::parse1::<Value>(args, call_info)?;
        let filter_fn = match filter_fn {
            Value::Closure(closure) => closure,
            _ => return Err(ExecError::wrong_arg_type(call_info.clone(), 0, String::from("function"), filter_fn.type_str().to_string())),
        };
        let mut new_obj = Object::new(None, HashMap::new(), false);
        // for (key, val) in this.borrow().slots.iter() {
        //     let res = context.call_closure(&filter_fn, vec![val.clone(), key.clone().into()], call_info)?;
        //     if res.truthy() {
        //         new_obj.set_field(key.clone(), val.clone(), false);
        //     }
        // }
        Ok(Value::Object(Rc::new(RefCell::new(new_obj))))
    }

    pub fn keys(context: *mut Context, this: Rc<RefCell<Object>>, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        argparse::parse0(args, call_info)?;
        let keys = this.borrow().slots.keys().cloned().map(|k| k.into()).collect::<Vec<_>>();
        Ok(Value::array(keys))
    }

    pub fn values(context: *mut Context, this: Rc<RefCell<Object>>, args: Vec<Value>, call_info: &CallInfo) -> ExprResult {
        argparse::parse0(args, call_info)?;
        let values = this.borrow().slots.values().cloned().collect::<Vec<_>>();
        Ok(Value::array(values))
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Object")
            .field("ptr", &(self as *const Object))
            .field("delegate", &self.delegate.as_ref().map(|del| del.deref().borrow().deref() as *const Object))
            .field("is_class_inst", &self.is_class_inst)
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
pub struct Closure {
    pub ast_fn: NonNull<ast::Function>,
    pub default_vals: Vec<Value>,
    pub upvalues: Vec<Rc<RefCell<Value>>>,
    // TODO: This should be weak
    pub root: Value,
    pub env: Option<Value>,
}

impl Closure {
    pub fn new(ast_fn: &ast::Function, default_vals: Vec<Value>, parent_rt: &FuncRuntime, root: Value) -> Self {
        let upvalues = ast_fn.upvalues.iter().cloned().map(|(parent_idx, _this_idx)| parent_rt.locals[parent_idx as usize].clone()).collect();
        Closure {
            ast_fn: NonNull::from(ast_fn),
            default_vals,
            root,
            env: None,
            upvalues,
        }
    }

    pub fn root(ast_fn: &ast::Function, root: Value) -> Self {
        Closure {
            ast_fn: NonNull::from(ast_fn),
            default_vals: Vec::new(),
            root,
            env: None,
            upvalues: Vec::new(),
        }
    }
}
