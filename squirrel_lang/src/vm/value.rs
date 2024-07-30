use hashbrown::{Equivalent, HashMap};
use std::{cell::RefCell, fmt, hash::Hash, mem, ops::Deref, ptr};
use std::{ptr::NonNull, rc::Rc};

use crate::{
    context::Span,
    parser::ast::{self, Expr},
    vm::error::ExecError,
};

use super::{bytecode::Reg, runtime::{RtFunction, VMState}};

pub type NativeFn = fn(*mut VMState, Reg, u8) -> Result<(), ExecError>;

#[derive(Debug, Clone)]
pub enum Value {
    Boolean(bool),
    Float(f64),
    Integer(i64),
    Null,
    NativeFn(NativeFn),
    String(Rc<str>),
    Table(Rc<RefCell<Table>>),
    Array(Rc<RefCell<Vec<Value>>>),
    Closure(Rc<RefCell<Closure>>),
    Class(Rc<RefCell<Class>>),
    Instance(Rc<RefCell<Instance>>),
}

macro_rules! value_common_impl {
    () => {
        pub fn boolean(val: bool) -> Self {
            Self::Boolean(val)
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

        pub fn table(table: Table) -> Self {
            Self::Table(Rc::new(RefCell::new(table)))
        }

        pub fn class(class: Class) -> Self {
            Self::Class(Rc::new(RefCell::new(class)))
        }

        pub fn instance(instance: Instance) -> Self {
            Self::Instance(Rc::new(RefCell::new(instance)))
        }
    };
}

impl Value {
    value_common_impl!();

    pub fn truthy(&self) -> bool {
        match self {
            Self::Integer(val) if *val == 0 => false,
            Self::Float(b) if *b == 0.0 => false,
            Self::Null => false,
            Self::Boolean(b) => *b,
            _ => true,
        }
    }

    pub fn get_field(&self, key: &HashValue) -> Option<Value> {
        match (self, key) {
            (Value::Table(obj), any) => obj.borrow().get_field(any),
            (Value::Array(arr), HashValue::Integer(i)) => arr.borrow().get(*i as usize).cloned(),
            (Value::String(s), HashValue::Integer(idx)) => s
                .chars()
                .nth(*idx as usize)
                .map(|c| Value::Integer(c as i64)),
            _ => None,
        }
    }

    pub fn type_str(&self) -> &'static str {
        match self {
            Value::Null => "null",
            Value::Boolean(_) => <bool as TypeName>::type_name(),
            Value::Integer(_) => <i64 as TypeName>::type_name(),
            Value::Float(_) => <f64 as TypeName>::type_name(),
            Value::NativeFn(_) => <NativeFn as TypeName>::type_name(),
            Value::String(_) => <Rc<str> as TypeName>::type_name(),
            Value::Array(_) => <Rc<RefCell<Vec<Value>>> as TypeName>::type_name(),
            Value::Closure(_) => <Rc<RefCell<Closure>> as TypeName>::type_name(),
            Value::Table(_) => <Rc<RefCell<Table>> as TypeName>::type_name(),
            Value::Class(_) => <Rc<RefCell<Class>> as TypeName>::type_name(),
            Value::Instance(_) => <Rc<RefCell<Instance>> as TypeName>::type_name(),
        }
    }
}

impl From<ast::Literal> for Value {
    fn from(value: ast::Literal) -> Self {
        match value {
            ast::Literal::Integer(i) => Value::Integer(i),
            ast::Literal::Number(f) => Value::Float(f),
            ast::Literal::String(s) => Value::string(&s),
            ast::Literal::Boolean(b) => Value::Boolean(b),
            ast::Literal::Null => Value::Null,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Numerics
            (Self::Float(l0), Self::Float(r0)) => *l0 == *r0,
            (Self::Float(l0), Self::Integer(r0)) => *l0 == *r0 as f64,
            (Self::Integer(l0), Self::Float(r0)) => *l0 as f64 == *r0,
            (Self::Integer(l0), Self::Integer(r0)) => *l0 == *r0,
            // Unique Values
            (Self::Boolean(l0), Self::Boolean(r0)) => *l0 == *r0,
            (Self::Null, Self::Null) => true,
            // Pointer Identity
            (Self::NativeFn(l0), Self::NativeFn(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Table(l0), Self::Table(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            (Self::Array(l0), Self::Array(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            (Self::Closure(l0), Self::Closure(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            (Self::Class(l0), Self::Class(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            (Self::Instance(l0), Self::Instance(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum HashValue {
    Boolean(bool),
    Integer(i64),
    Null,
    NativeFn(NativeFn),
    String(Rc<str>),
    Table(Rc<RefCell<Table>>),
    Array(Rc<RefCell<Vec<Value>>>),
    Closure(Rc<RefCell<Closure>>),
    Class(Rc<RefCell<Class>>),
    Instance(Rc<RefCell<Instance>>),
}

impl PartialEq for HashValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Boolean(l0), Self::Boolean(r0)) => *l0 == *r0,
            (Self::Integer(l0), Self::Integer(r0)) => *l0 == *r0,
            (Self::Null, Self::Null) => true,
            (Self::NativeFn(l0), Self::NativeFn(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0.deref() == r0.deref(),
            (Self::Table(l0), Self::Table(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            (Self::Array(l0), Self::Array(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            (Self::Closure(l0), Self::Closure(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            (Self::Class(l0), Self::Class(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            (Self::Instance(l0), Self::Instance(r0)) => ptr::addr_eq(l0.as_ptr(), r0.as_ptr()),
            _ => false,
        }
    }
}
impl Eq for HashValue {}
impl Hash for HashValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // This breaks impl Equivalent<HashValue> for str
        if !matches!(self, &HashValue::String(_)) {
            core::mem::discriminant(self).hash(state);
        }
        match self {
            HashValue::Boolean(b) => b.hash(state),
            HashValue::Integer(i) => i.hash(state),
            HashValue::Null => {}
            HashValue::NativeFn(nf) => nf.hash(state),
            HashValue::String(s) => s.deref().hash(state),
            HashValue::Table(o) => o.as_ptr().hash(state),
            HashValue::Array(a) => a.as_ptr().hash(state),
            HashValue::Closure(c) => c.as_ptr().hash(state),
            HashValue::Class(c) => c.as_ptr().hash(state),
            HashValue::Instance(i) => i.as_ptr().hash(state),
        }
    }
}

impl HashValue {
    value_common_impl!();

    pub fn truthy(&self) -> bool {
        match self {
            Self::Integer(val) if *val == 0 => false,
            Self::Null => false,
            Self::Boolean(b) => *b,
            _ => true,
        }
    }
}

impl From<HashValue> for Value {
    fn from(value: HashValue) -> Self {
        match value {
            HashValue::Boolean(val) => Value::Boolean(val),
            HashValue::Integer(val) => Value::Integer(val),
            HashValue::Null => Value::Null,
            HashValue::NativeFn(val) => Value::NativeFn(val),
            HashValue::String(s) => Value::String(s),
            HashValue::Table(o) => Value::Table(o),
            HashValue::Array(a) => Value::Array(a),
            HashValue::Closure(c) => Value::Closure(c),
            HashValue::Class(c) => Value::Class(c),
            HashValue::Instance(i) => Value::Instance(i),
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
            Value::Table(o) => HashValue::Table(o),
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
    fn typed_from_ref(value: &Value) -> Result<&Self, &Value>;
    fn typed_from_mut(value: &mut Value) -> Result<&mut Self, &mut Value>;
}

impl TypeName for Value {
    fn type_name() -> &'static str {
        "any"
    }

    fn typed_from(value: Value) -> Result<Self, Value> {
        Ok(value)
    }

    fn typed_from_ref(value: &Value) -> Result<&Self, &Value> {
        Ok(value)
    }

    fn typed_from_mut(value: &mut Value) -> Result<&mut Self, &mut Value> {
        Ok(value)
    }
}

macro_rules! value_variant {
    ($base:ident::$variant:ident($data:ty) $name:literal) => {
        impl From<$data> for $base {
            fn from(value: $data) -> Self {
                $base::$variant(value)
            }
        }

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

            fn typed_from_ref(value: &$base) -> Result<&Self, &$base> {
                match value {
                    $base::$variant(val) => Ok(val),
                    other => Err(other),
                }
            }

            fn typed_from_mut(value: &mut $base) -> Result<&mut Self, &mut $base> {
                match value {
                    $base::$variant(val) => Ok(val),
                    other => Err(other),
                }
            }
        }
    };
}

// TODO: Are these names right?
value_variant!(Value::Boolean(bool) "bool");
value_variant!(Value::Integer(i64) "integer");
value_variant!(Value::Float(f64) "float");
value_variant!(Value::NativeFn(NativeFn) "function");
value_variant!(Value::String(Rc<str>) "string");
value_variant!(Value::Table(Rc<RefCell<Table>>) "table");
value_variant!(Value::Closure(Rc<RefCell<Closure>>) "function");
value_variant!(Value::Array(Rc<RefCell<Vec<Value>>>) "array");
value_variant!(Value::Class(Rc<RefCell<Class>>) "class");
value_variant!(Value::Instance(Rc<RefCell<Instance>>) "instance");

fn print_addr<T>(f: &mut fmt::Formatter<'_>, ptr: &Rc<T>) -> fmt::Result
where
    Rc<T>: TypeName,
{
    write!(
        f,
        "({} : 0x{:012x})",
        <Rc<T> as TypeName>::type_name(),
        Rc::as_ptr(ptr) as usize
    )
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Boolean(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Value::Float(n) => write!(f, "{}", n),
            Value::Integer(i) => write!(f, "{}", i),
            Value::Null => write!(f, "null"),
            Value::String(s) => write!(f, "{}", s),
            Value::NativeFn(n) => write!(
                f,
                "({} : 0x{:012x})",
                <NativeFn as TypeName>::type_name(),
                *n as usize
            ),
            Value::Table(o) => print_addr(f, o),
            Value::Array(a) => print_addr(f, a),
            Value::Closure(c) => print_addr(f, c),
            Value::Class(c) => print_addr(f, c),
            Value::Instance(i) => print_addr(f, i),
        }
    }
}

#[derive(Clone)]
pub struct Table {
    delegate: Option<Rc<RefCell<Table>>>,
    slots: HashMap<HashValue, Value>,
}

impl Table {
    pub fn new(delegate: Option<Rc<RefCell<Table>>>, slots: HashMap<HashValue, Value>) -> Table {
        Table { delegate, slots }
    }

    pub fn get_delegate(&self) -> Option<&Rc<RefCell<Table>>> {
        self.delegate.as_ref()
    }

    pub fn set_field(
        &mut self,
        key: HashValue,
        value: Value,
        is_newslot: bool,
    ) -> Result<(), ExecError> {
        if is_newslot || self.slots.contains_key(&key) {
            self.slots.insert(key, value);
            Ok(())
        } else {
            match self.delegate.as_ref() {
                Some(delegate) => {
                    let mut parent = delegate.borrow_mut();
                    parent.set_field(key, value, is_newslot)
                }
                None => todo!("Undefined field"),
            }
        }
    }

    pub fn get_field(&self, key: &HashValue) -> Option<Value> {
        // TODO: This shouldn't be recursive
        if let Some(value) = self.slots.get(key) {
            return Some(value.clone().into());
        }
        self.delegate
            .as_ref()
            .and_then(|del| del.borrow().get_field(key))
            .or_else(|| match key {
                // todo: Object default delegate
                // HashValue::String(s) => builtins::object::delegate(s),
                _ => None,
            })
    }

    pub fn len(&self) -> usize {
        self.slots.len()
    }

    pub fn slot_iter(&self) -> impl Iterator<Item = (&HashValue, &Value)> {
        self.slots.iter()
    }
}

impl fmt::Debug for Table {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Table")
            .field("ptr", &(self as *const Table))
            .field(
                "delegate",
                &self
                    .delegate
                    .as_ref()
                    .map(|del| del.deref().borrow().deref() as *const Table),
            )
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
pub struct Closure {
    pub rt_fn: Rc<RtFunction>,
    pub default_vals: Vec<Value>,
    pub upvalues: Vec<Rc<RefCell<Value>>>,
    // TODO: This should be weak
    pub root: Value,
    pub env: Option<Value>,
}

impl Closure {
    pub fn new(
        ast_fn: Rc<RtFunction>,
        default_vals: Vec<Value>,
        // parent_rt: &FuncRuntime,
        root: Value,
    ) -> Self {
        todo!()
        // let upvalues = ast_fn
        //     .upvalues
        //     .iter()
        //     .cloned()
        //     .map(|(parent_idx, _this_idx)| parent_rt.locals[parent_idx as usize].clone())
        //     .collect();
        // Closure {
        //     ast_fn: NonNull::from(ast_fn),
        //     default_vals,
        //     root,
        //     env: None,
        //     upvalues,
        // }
    }

    pub fn root(root_fn: RtFunction, root: Value) -> Self {
        Closure {
            rt_fn: Rc::new(root_fn),
            default_vals: Vec::new(),
            root,
            env: None,
            upvalues: Vec::new(),
        }
    }
}

#[derive(Debug)]
enum ClassFields {
    NoOffsets(HashMap<HashValue, Value>),
    Offsets(HashMap<HashValue, (u32, Value)>),
}

impl Default for ClassFields {
    fn default() -> Self {
        ClassFields::NoOffsets(HashMap::new())
    }
}

pub struct Class {
    parent: Option<Rc<RefCell<Class>>>,
    fields: ClassFields,
}

impl Class {
    pub fn new(parent: Option<Rc<RefCell<Class>>>, fields: HashMap<HashValue, Value>) -> Self {
        Class {
            parent,
            fields: ClassFields::NoOffsets(fields),
        }
    }

    fn get_next_valid_offset(&mut self) -> u32 {
        self.parent
            .as_ref()
            .map(|p| p.borrow_mut().get_next_valid_offset())
            .unwrap_or(0)
            + self.get_or_make_offsets().len() as u32
    }

    fn get_offsets(&self) -> &HashMap<HashValue, (u32, Value)> {
        match &self.fields {
            ClassFields::NoOffsets(_) => panic!("Cannot get offsets before they are initialized"),
            ClassFields::Offsets(offsets) => offsets,
        }
    }

    fn get_or_make_offsets(&mut self) -> &HashMap<HashValue, (u32, Value)> {
        match &mut self.fields {
            ClassFields::Offsets(offsets) => {}
            ClassFields::NoOffsets(fields) => {
                let fields = mem::take(fields);
                let first_offset = self
                    .parent
                    .as_ref()
                    .map(|p| p.borrow_mut().get_next_valid_offset() as u32)
                    .unwrap_or(0);
                let mut offsets = HashMap::new();
                for (field_idx, (key, value)) in fields.into_iter().enumerate() {
                    offsets.insert(key.clone(), (first_offset + field_idx as u32, value));
                }

                self.fields = ClassFields::Offsets(offsets);
            }
        };
        let ClassFields::Offsets(offsets) = &self.fields else {
            unreachable!()
        };
        offsets
    }

    pub fn set_field(
        &mut self,
        key: HashValue,
        value: Value,
        is_newslot: bool,
    ) -> Result<(), ExecError> {
        match &mut self.fields {
            ClassFields::NoOffsets(fields) => {
                if is_newslot || fields.contains_key(&key) {
                    fields.insert(key, value);
                    Ok(())
                } else {
                    todo!("Undefined field")
                }
            }
            ClassFields::Offsets(_) => todo!("Mutating instantiated class"),
        }
    }

    // TODO: Do this with other get_field(_str)? functions
    fn get_field_generic<K: Equivalent<HashValue> + Hash + ?Sized>(
        &self,
        key: &K,
    ) -> Option<Value> {
        match &self.fields {
            ClassFields::NoOffsets(fields) => fields.get(key).map(|v| v.clone()),
            ClassFields::Offsets(fields) => fields.get(key).map(|v| v.1.clone()),
        }
    }

    pub fn get_field(&self, key: &HashValue) -> Option<Value> {
        self.get_field_generic(key)
    }

    pub fn get_field_str(&self, key: &str) -> Option<Value> {
        self.get_field_generic(key)
    }

    fn initialize(&mut self, instance: &mut Instance) -> Result<(), ExecError> {
        if let Some(parent) = self.parent.as_ref() {
            parent.borrow_mut().initialize(instance)?;
        }

        let offsets = self.get_or_make_offsets();
        for (key, (offset, value)) in offsets.iter() {
            instance.fields[*offset as usize] = value.clone();
        }
        Ok(())
    }
}

impl fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Class")
            .field("ptr", &(self as *const Class))
            .field(
                "parent",
                &self
                    .parent
                    .as_ref()
                    .map(|del| del.deref().borrow().deref() as *const Class),
            )
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
pub struct Instance {
    class: Rc<RefCell<Class>>,
    fields: Vec<Value>,
}

impl Instance {
    pub fn get_field_idx<T>(&self, class: &Class, key: &T) -> Option<u32>
    where
        T: Equivalent<HashValue> + Hash + ?Sized,
    {
        if let Some((field_idx, field_default_val)) = class.get_offsets().get(key) {
            Some(*field_idx)
        } else {
            class
                .parent
                .as_ref()
                .and_then(|p| self.get_field_idx(&p.borrow(), key))
        }
    }

    pub fn set_field(
        &mut self,
        key: HashValue,
        value: Value,
        is_newslot: bool,
    ) -> Result<(), ExecError> {
        if is_newslot {
            return todo!("Mutating instantiated class");
        }
        if let Some(offset) = self.get_field_idx(&self.class.borrow(), &key) {
            self.fields[offset as usize] = value;
            Ok(())
        } else {
            todo!("Undefined field")
        }
    }

    pub fn get_field(&self, key: &HashValue) -> Option<Value> {
        self.get_field_idx(&self.class.borrow(), key)
            .map(|offset| self.fields[offset as usize].clone())
    }

    pub fn get_field_str(&self, key: &str) -> Option<Value> {
        self.get_field_idx(&self.class.borrow(), key)
            .map(|offset| self.fields[offset as usize].clone())
    }

    pub fn construct(class: Rc<RefCell<Class>>) -> Result<Self, ExecError> {
        let fields = vec![Value::Null; class.borrow_mut().get_next_valid_offset() as usize];
        let mut instance = Instance {
            class: class.clone(),
            fields,
        };
        class.borrow_mut().initialize(&mut instance)?;
        Ok(instance)
    }
}
