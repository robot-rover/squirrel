mod builtins;
// mod argparse;

use std::{
    cell::RefCell,
    io::{self, stdout},
    rc::Rc,
};

use hashbrown::HashMap;

use crate::{context::SquirrelError, parser::ast, util::WriteOption};

use super::{
    bytecode::{
        run_arith, run_bitwise, run_call, run_compare, run_get, run_jump, run_load, run_misc,
        run_ret, run_set, run_store, run_unary, Block, Const, Data, FunIdx, Inst, Local, Reg, Tag,
    },
    compiler::Function,
    value::{Table, Value},
};

#[derive(Debug)]
pub struct RtFunction {
    code: Vec<Inst>,
    block_offsets: Vec<u32>,
    constants: Vec<Value>,
    num_regs: u32,
    num_params: u32,
    num_req_params: u32,
    is_varargs: bool,
    num_locals: u32,
    locals: Vec<(String, u32)>,
    sub_functions: Vec<Rc<RtFunction>>,
}

impl RtFunction {
    pub fn get_constant(&self, constant: Const) -> &Value {
        &self.constants[constant.as_idx()]
    }

    pub fn get_sub_function(&self, idx: FunIdx) -> &RtFunction {
        &self.sub_functions[idx.as_idx()]
    }
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
        let sub_functions = sub_functions
            .into_iter()
            .map(RtFunction::from)
            .map(Rc::new)
            .collect::<Vec<_>>();
        RtFunction {
            code,
            block_offsets,
            constants,
            num_regs,
            num_params,
            num_req_params: num_params,
            is_varargs,
            num_locals,
            locals,
            sub_functions,
        }
    }
}

pub struct VMState<'a> {
    root_table: Rc<RefCell<Table>>,
    stdout: WriteOption<'a>,
    pub call_stack: Vec<StackFrame>,
    acc: Value,
}

impl<'a> VMState<'a> {
    pub fn frame(&self) -> &StackFrame {
        self.call_stack.last().unwrap()
    }

    pub fn frame_mut(&mut self) -> &mut StackFrame {
        self.call_stack.last_mut().unwrap()
    }

    pub fn take_acc(&mut self) -> Value {
        std::mem::replace(&mut self.acc, Value::Null)
    }

    pub fn set_acc(&mut self, value: Value) {
        self.acc = value;
    }

    pub fn call_fn(&mut self, func: Rc<RtFunction>, env: Value, args: Vec<Value>) {
        let registers = vec![Value::Null; func.num_regs as usize];
        // TODO: Handle varargs and default args here
        let mut locals = args
            .into_iter()
            .map(|v| Rc::new(RefCell::new(v)))
            .collect::<Vec<_>>();
        locals.resize_with(func.num_locals as usize, || {
            Rc::new(RefCell::new(Value::Null))
        });

        self.call_stack.push(StackFrame {
            ip: 0,
            func,
            env,
            locals,
            registers,
        });
    }

    pub fn get_root(&self) -> &Rc<RefCell<Table>> {
        &self.root_table
    }
}

pub struct StackFrame {
    ip: usize,
    func: Rc<RtFunction>,
    env: Value,
    // TODO: The locals that are not upvalues can be stored w/o extra indirection
    locals: Vec<Rc<RefCell<Value>>>,
    registers: Vec<Value>,
}

impl StackFrame {
    pub fn get_reg(&self, reg: Reg) -> &Value {
        &self.registers[reg.as_idx()]
    }

    pub fn get_reg_mut(&mut self, reg: Reg) -> &mut Value {
        &mut self.registers[reg.as_idx()]
    }

    pub fn get_env(&self) -> &Value {
        &self.env
    }

    pub fn get_env_mut(&mut self) -> &mut Value {
        &mut self.env
    }

    pub fn get_func(&self) -> &RtFunction {
        &self.func
    }

    pub fn get_sub_func(&self, idx: FunIdx) -> &RtFunction {
        &self.func.sub_functions[idx.as_idx()]
    }

    pub fn get_local(&self, local: Local) -> &Rc<RefCell<Value>> {
        &self.locals[local.as_idx()]
    }

    pub fn jump_to_block(&mut self, block: Block) {
        self.ip = self.func.block_offsets[block.as_idx()] as usize;
    }
}

fn init_root_table() -> Rc<RefCell<Table>> {
    let root_table = builtins::global::make_root_table();
    let root_table = Rc::new(RefCell::new(root_table));
    root_table
}

pub fn run<'a>(
    root_fn: Rc<RtFunction>,
    file_name: &str,
    stdout: Option<&mut dyn io::Write>,
    args: impl IntoIterator<Item = &'a str>,
) {
    assert_eq!(
        root_fn.num_params, 0,
        "root function must have 0 parameters"
    );
    assert!(root_fn.is_varargs, "root function must be varargs");
    assert_eq!(root_fn.locals.get(0), Some(&(String::from("vargv"), 0)));

    let stdout = stdout
        .map(|stream| WriteOption::Dyn(stream))
        .unwrap_or_else(|| WriteOption::Stdout(io::stdout()));
    let root_table = init_root_table();
    let mut locals = vec![Rc::new(RefCell::new(Value::Null)); root_fn.locals.len()];
    *locals[0].borrow_mut() = Value::array(args.into_iter().map(Value::string).collect::<Vec<_>>());
    let num_regs = root_fn.num_regs;

    let call_stack = vec![StackFrame {
        ip: 0,
        func: root_fn,
        env: Value::Table(root_table.clone()),
        registers: vec![Value::Null; num_regs as usize],
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
            let frame = if let Some(frame) = state.call_stack.last_mut() {
                frame
            } else {
                break;
            };
            let ip = frame.ip;
            frame.ip += 1;
            // TODO: REMOVE THIS CLONE
            frame.func.code[ip].clone()
        };

        let res = match inst {
            Inst::Compare(c) => run_compare(state, &c),
            Inst::Arith(a) => run_arith(state, &a),
            Inst::Bitwise(b) => run_bitwise(state, &b),
            Inst::Call(c) => run_call(state, &c),
            Inst::Get(g) => run_get(state, &g),
            Inst::Set(s) => run_set(state, &s),
            Inst::Jump(j) => run_jump(state, &j),
            Inst::Ret(r) => run_ret(state, &r),
            Inst::Load(l) => run_load(state, &l),
            Inst::Store(s) => run_store(state, &s),
            Inst::Misc(m) => run_misc(state, &m),
            Inst::Unary(u) => run_unary(state, &u),
        };
        res.unwrap()
    }
}

#[cfg(test)]
mod tests {
    use std::iter;

    use super::*;
    use crate::{
        context::IntoSquirrelErrorContext, parser::parse, test_foreach, test_util::exchange_str,
        vm::compiler::compile_function,
    };

    test_foreach!(sample_test);

    fn sample_test(file_name: &str, file_contents: &str) {
        let test_name = format!("vm-{}", file_name.replace("/", "-"));
        let test_desc = format!("VM test for {}", file_name);

        let actual_ast = match parse(file_contents, file_name.to_string()) {
            Ok(ast) => ast,
            Err(err) => panic!("{}", err),
        };

        let actual_code = compile_function(&actual_ast);
        println!("{:#?}", actual_code);
        let rt_code = RtFunction::from(actual_code);

        let mut output = Vec::new();
        run(rt_code.into(), file_name, Some(&mut output), iter::empty());

        let actual_str = String::from_utf8(output).expect("Invalid UTF-8 in test output");
        #[cfg(not(miri))]
        {
            insta::with_settings!({filters => vec![
                (r"\((?<kind>[a-z]+) : 0x(?:0x)?[0-9a-f]+\)", "($kind : 0x<memory>)")
            ]}, {
                insta::assert_snapshot!(test_name, actual_str, &test_desc);
            });
        }
    }
}
