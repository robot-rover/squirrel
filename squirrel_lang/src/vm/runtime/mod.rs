mod compare;

use std::{cell::RefCell, io::{self, stdout}, rc::Rc};

use hashbrown::HashMap;

use crate::{context::SquirrelError, parser::ast, util::WriteOption};

use super::{bytecode::{decode, DcCompare, DcTag, Inst, Tag}, compiler::Function, inst_impl::run_compare, value::{Object, Value}};

pub struct RtFunction {
    pub code: Vec<Inst>,
    pub block_offsets: Vec<u32>,
    pub constants: Vec<Value>,
    pub is_varargs: bool,
    pub num_regs: u32,
    pub num_params: u32,
    pub locals: Vec<(String, u32)>,
    pub sub_functions: Vec<Function>,
}

impl From<Function> for RtFunction {
    fn from(value: Function) -> Self {
        let Function {
            code,
            constants,
            is_varargs,
            num_params,
            locals,
            sub_functions,
            block_offsets,
            num_locals,
            num_regs,
        } = value;
        let constants = constants.into_iter().map(Value::from).collect::<Vec<_>>();
        todo!()
    }
}

struct VMState<'a, 'f> {
    root_table: Rc<RefCell<Object>>,
    stdout: WriteOption<'a>,
    call_stack: Vec<StackFrame<'f>>,
    acc: Value,
}

impl<'a, 'f> VMState<'a, 'f> {
    fn frame(&self) -> &StackFrame {
        self.call_stack.last().unwrap()
    }

    fn take_acc(&mut self) -> Value {
        std::mem::replace(&mut self.acc, Value::Null)
    }
}

struct StackFrame<'f> {
    ip: usize,
    func: &'f RtFunction,
    env: Value,
    // TODO: The locals that are not upvalues can be stored w/o extra indirection
    locals: Vec<Rc<RefCell<Value>>>,
    registers: Vec<Value>,

}

fn init_root_table() -> Rc<RefCell<Object>> {
    let root_table = Object::new(None, HashMap::new());
    let root_table = Rc::new(RefCell::new(root_table));
    root_table
}

pub fn run<'a, 'f>(
    root_fn: &'f RtFunction,
    file_name: &str,
    stdout: Option<&mut dyn io::Write>,
    args: impl IntoIterator<Item = &'a str>,
) {
    assert_eq!(root_fn.num_params, 0, "root function must have 0 parameters");
    assert!(root_fn.is_varargs, "root function must be varargs");
    assert_eq!(root_fn.locals.get(0), Some(&(String::from("vargv"), 0)));

    let stdout = stdout.map(|stream| WriteOption::Dyn(stream)).unwrap_or_else(|| WriteOption::Stdout(io::stdout()));
    let root_table = init_root_table();
    let mut locals = vec![Rc::new(RefCell::new(Value::Null)); root_fn.locals.len()];
    *locals[0].borrow_mut() = Value::array(args.into_iter().map(Value::string).collect::<Vec<_>>());

    let call_stack = vec![StackFrame {
        ip: 0,
        func: root_fn,
        env: Value::Object(root_table.clone()),
        registers: Vec::new(),
        locals,
    }];

    let mut state = VMState {
        root_table,
        stdout,
        call_stack,
        acc: Value::Null,
    };

    run_vm(&mut state);
}

fn run_vm(state: &mut VMState) {
    loop {
        let inst = {
            let frame = state.call_stack.last_mut().unwrap();
            let ip = frame.ip;
            frame.ip += 1;
            frame.func.code[ip]
        };

        match decode(inst.tag) {
            DcTag::Compare(c) => run_compare(state, c),
            other => todo!("{:?}", other),
        }
    }
}
