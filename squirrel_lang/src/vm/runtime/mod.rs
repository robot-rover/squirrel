mod builtins;
// mod argparse;

use std::{
    borrow::Borrow,
    cell::RefCell,
    cmp::Ordering,
    fmt,
    io::{self, stdout},
    iter,
    rc::Rc,
};

use ariadne::{Cache, Source};
use hashbrown::HashMap;

use crate::{
    parser::ast,
    sq_error::{SquirrelError, SquirrelErrorRendered},
    util::WriteOption,
    vm::compiler::LocalMapping,
};

use super::{
    bytecode::{
        run_arith, run_bitwise, run_call, run_class, run_compare, run_del, run_get, run_jump,
        run_load, run_misc, run_ret, run_set, run_store, run_unary, Block, Const, FunIdx, Inst,
        InstCtx, Local, Reg, SubInstGetContext, Tag,
    },
    compiler::{self, FormatInst, Function},
    value::{Table, Value},
};

#[derive(Debug)]
pub struct RtFunction {
    function_idx: usize,
    constants: Vec<Value>,
    f: Function,
    source_file_id: usize,
}

impl RtFunction {
    pub fn get_constant(&self, constant: Const) -> &Value {
        &self.constants[constant.as_idx()]
    }

    pub fn get_sub_fn_idx(&self, fun_idx: FunIdx) -> usize {
        fun_idx.as_idx() + self.function_idx
    }

    fn load(func: Function, source_file_id: usize, global_fn_id: usize) -> Self {
        let constants = func.constants.iter().map(Value::from).collect::<Vec<_>>();
        RtFunction {
            f: func,
            function_idx: global_fn_id,
            constants,
            source_file_id,
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
    rt_funcs: Vec<Rc<RtFunction>>,
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

    pub fn get_acc(&self) -> &Value {
        &self.acc
    }

    pub fn set_acc(&mut self, value: Value) {
        self.acc = value;
    }

    pub fn call_fn(&mut self, func: Rc<RtFunction>, env: Value, args: Vec<Value>) {
        let registers = vec![Value::Null; func.f.num_regs as usize];
        // TODO: Handle varargs and default args here
        let mut locals = args
            .into_iter()
            .map(|v| Rc::new(RefCell::new(v)))
            .collect::<Vec<_>>();
        locals.resize_with(func.f.num_locals as usize, || {
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
        I::get_ctx(&frame.func.f.code[frame.ip - 1]).expect("Instruction and Context type mismatch")
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

        let first_idx = self.rt_funcs.len();
        self.rt_funcs.extend(
            (first_idx..)
                .zip(code)
                .map(|(i, f)| Rc::new(RtFunction::load(f, source_id, i))),
        );

        self.rt_funcs[first_idx].clone()
    }

    pub fn get_rt_func(&self, fun_offset: usize) -> Rc<RtFunction> {
        self.rt_funcs[fun_offset].clone()
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

    pub fn get_local(&self, local: Local) -> &Rc<RefCell<Value>> {
        &self.locals[local.as_idx()]
    }

    pub fn jump_to_block(&mut self, block: Block) {
        self.ip = self.func.f.block_offsets[block.as_idx()] as usize;
    }
}

impl fmt::Display for StackFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "StackFrame")?;
        writeln!(f, "ip: {} {:?}", self.ip, self.func.f.code[self.ip])?;
        writeln!(f, "env: {}", self.env)?;
        write!(f, "locals:")?;
        let mut local_name_iter = self.func.f.locals.iter().peekable();
        // TODO: Show named registers and locals
        // for (i, local) in self.locals.iter().enumerate() {
        //     write!(f, "\n  {}: {:?}", i, RefCell::borrow(local))?;
        //     match local_name_iter
        //         .peek()
        //         .map(|(name, idx)| (name, idx.cmp(&(i as u32))))
        //     {
        //         Some((_, Ordering::Less)) => unreachable!("Local names are out of order"),
        //         Some((name, Ordering::Equal)) => {
        //             write!(f, " ({})", name)?;
        //             local_name_iter.next();
        //         }
        //         None | Some((_, Ordering::Greater)) => {}
        //     }
        // }
        write!(f, "\nregisters ({})", self.registers.len())?;
        for (i, reg) in self.registers.iter().enumerate() {
            if !matches!(reg, Value::Null) {
                write!(f, "\n  {}: {:?}", i, reg)?;
            }
        }

        Ok(())
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
    let root_fn = &root_file.code[0];
    assert_eq!(
        root_fn.num_params, 0,
        "root function must have 0 parameters"
    );
    assert!(root_fn.is_varargs, "root function must be varargs");
    assert!(
        matches!(
            root_fn.locals.get(0).and_then(|lm| lm.get_name()),
            Some("vargv")
        ),
        "root function must have named registers vargv and vargc"
    );

    let stdout = stdout
        .map(|stream| WriteOption::Dyn(stream))
        .unwrap_or_else(|| WriteOption::Stdout(io::stdout()));
    let root_table = init_root_table();
    let mut locals: Vec<_> = iter::repeat_with(|| Rc::new(RefCell::new(Value::Null)))
        .take(root_fn.locals.len())
        .collect();
    *locals[0].borrow_mut() = Value::array(args.into_iter().map(Value::string).collect::<Vec<_>>());
    let num_regs = root_fn.num_regs;

    let mut state = VMState {
        root_table: root_table.clone(),
        stdout,
        call_stack: Vec::new(),
        acc: Value::Null,
        files: Vec::new(),
        rt_funcs: Vec::new(),
    };

    let root_rt_fn = state.load_file(root_file);

    state.call_stack.push(StackFrame {
        ip: 0,
        func: root_rt_fn,
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
            frame.func.f.code[ip].strip()
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
            Inst::Class(c) => run_class(state, c),
            Inst::Del(d) => run_del(state, d),
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
        // TODO: Only prints 1 function
        println!("{:#?}", actual_code.code[0].wrap(&actual_code));

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
