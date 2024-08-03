mod builtins;
// mod argparse;

use std::{
    cell::RefCell,
    io::{self, stdout},
    rc::Rc,
};

use ariadne::{Cache, Source};
use hashbrown::HashMap;

use crate::{
    context::{SquirrelError, SquirrelErrorRendered},
    parser::ast,
    util::WriteOption,
};

use super::{
    bytecode::{
        run_arith, run_bitwise, run_call, run_compare, run_get, run_jump, run_load, run_misc,
        run_ret, run_set, run_store, run_unary, Block, Const, FunIdx, Inst, InstCtx, Local, Reg,
        SubInstGetContext, Tag,
    },
    compiler::{self, Function},
    value::{Table, Value},
};

#[derive(Debug)]
pub struct RtFunction {
    code: Vec<InstCtx>,
    block_offsets: Vec<u32>,
    constants: Vec<Value>,
    num_regs: u32,
    num_params: u32,
    num_req_params: u32,
    is_varargs: bool,
    num_locals: u32,
    locals: Vec<(String, u32)>,
    source_file_id: usize,
    sub_functions: Vec<Rc<RtFunction>>,
}

impl RtFunction {
    pub fn get_constant(&self, constant: Const) -> &Value {
        &self.constants[constant.as_idx()]
    }

    pub fn get_sub_function(&self, idx: FunIdx) -> &RtFunction {
        &self.sub_functions[idx.as_idx()]
    }

    fn load(func: Function, source_file_id: usize) -> Self {
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
        } = func;
        let constants = constants.into_iter().map(Value::from).collect::<Vec<_>>();
        let sub_functions = sub_functions
            .into_iter()
            .map(|f| RtFunction::load(f, source_file_id))
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
            source_file_id,
            sub_functions,
        }
    }
}

struct File {
    file_name: String,
    // TODO: Load source from String lazily?
    contents: Source<String>,
}

impl File {
    fn fetch(&self) -> &Source<String> {
        &self.contents
    }

    fn display(&self) -> String {
        self.file_name.clone()
    }
}

pub struct VMState<'a> {
    root_table: Rc<RefCell<Table>>,
    stdout: WriteOption<'a>,
    pub call_stack: Vec<StackFrame>,
    acc: Value,
    files: Vec<File>,
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

    pub fn get_context<I: SubInstGetContext>(&self, inst: I) -> &I::Context {
        let frame = self.frame();
        I::get_ctx(&frame.func.code[frame.ip - 1]).expect("Instruction and Context type mismatch")
    }

    pub fn load_file(&mut self, file: compiler::File) -> Rc<RtFunction> {
        let source_id = self.files.len();
        let compiler::File {
            file_name,
            source,
            code,
        } = file;
        self.files.push(File {
            file_name,
            contents: Source::from(source),
        });

        Rc::new(RtFunction::load(code, source_id))
    }
}

impl Cache<usize> for VMState<'_> {
    type Storage = String;

    fn fetch(
        &mut self,
        id: &usize,
    ) -> Result<&Source<Self::Storage>, Box<dyn std::fmt::Debug + '_>> {
        Ok(self.files.get(*id).expect("Illegal file ID").fetch())
    }

    fn display<'a>(&self, id: &'a usize) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(
            self.files.get(*id).expect("Illegal file ID").display(),
        ))
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
    root_file: compiler::File,
    file_name: &str,
    stdout: Option<&mut dyn io::Write>,
    args: impl IntoIterator<Item = &'a str>,
) -> Result<(), SquirrelErrorRendered> {
    assert_eq!(
        root_file.code.num_params, 0,
        "root function must have 0 parameters"
    );
    assert!(root_file.code.is_varargs, "root function must be varargs");
    assert_eq!(
        root_file.code.locals.get(0),
        Some(&(String::from("vargv"), 0))
    );

    let stdout = stdout
        .map(|stream| WriteOption::Dyn(stream))
        .unwrap_or_else(|| WriteOption::Stdout(io::stdout()));
    let root_table = init_root_table();
    let mut locals = vec![Rc::new(RefCell::new(Value::Null)); root_file.code.locals.len()];
    *locals[0].borrow_mut() = Value::array(args.into_iter().map(Value::string).collect::<Vec<_>>());
    let num_regs = root_file.code.num_regs;

    let mut state = VMState {
        root_table: root_table.clone(),
        stdout,
        call_stack: Vec::new(),
        acc: Value::Null,
        files: Vec::new(),
    };

    let root_fn = state.load_file(root_file);

    state.call_stack.push(StackFrame {
        ip: 0,
        func: root_fn,
        env: Value::Table(root_table),
        registers: vec![Value::Null; num_regs as usize],
        locals,
    });

    run_vm(&mut state).map_err(|err| err.render(&mut state))
}

fn run_vm(state: &mut VMState) -> Result<(), SquirrelError> {
    loop {
        let inst = {
            let frame = if let Some(frame) = state.call_stack.last_mut() {
                frame
            } else {
                break Ok(());
            };
            let ip = frame.ip;
            frame.ip += 1;
            frame.func.code[ip].strip()
        };

        let res = match inst {
            Inst::Compare(c) => run_compare(state, c),
            Inst::Arith(a) => run_arith(state, a),
            Inst::Bitwise(b) => run_bitwise(state, b),
            Inst::Call(c) => run_call(state, c),
            Inst::Get(g) => run_get(state, g),
            Inst::Set(s) => run_set(state, s),
            Inst::Jump(j) => run_jump(state, j),
            Inst::Ret(r) => run_ret(state, r),
            Inst::Load(l) => run_load(state, l),
            Inst::Store(s) => run_store(state, s),
            Inst::Misc(m) => run_misc(state, m),
            Inst::Unary(u) => run_unary(state, u),
        };
        if let Err(err) = res {
            return Err(err.with_context(state.frame().func.source_file_id, &mut *state));
        }
    }
}

#[cfg(test)]
mod tests {
    use std::iter;

    use compiler::FunctionDebug;

    use super::*;
    use crate::{parser::parse, test_foreach, test_util::exchange_str, vm::compiler::compile};

    test_foreach!(sample_test);

    fn sample_test(file_name: &str, file_contents: &str) {
        let test_name = format!("vm-{}", file_name.replace("/", "-"));
        let test_desc = format!("VM test for {}", file_name);

        let actual_ast = match parse(file_contents, file_name.to_string()) {
            Ok(ast) => ast,
            Err(err) => panic!("{}", todo!()),
        };

        let actual_code = compile(
            &actual_ast,
            file_name.to_string(),
            file_contents.to_string(),
        );
        println!("{:#?}", actual_code.code.wrap(&actual_code));

        let mut output = Vec::new();
        let run_res = run(actual_code, file_name, Some(&mut output), iter::empty());

        if let Err(err) = run_res {
            panic!("{}", err);
        }

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
