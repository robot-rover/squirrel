use crate::{
    context::Span,
    impl_sub_inst,
    vm::{compiler::{self, FormatInst}, error::ExecResult, runtime::VMState, value::Value},
};

use super::{context::FnCallContext, Const, FunIdx, Inst, InstCtx, Local, Reg, Tag};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct InstCall {
    reg: Reg,
    n_args: u8,
}

impl_sub_inst!(type Inst::Call(InstCallCtx<InstCall, FnCallContext>));

impl InstCtx {
    pub fn call(reg: Reg, n_args: usize, ctx: FnCallContext) -> Self {
        // TODO: Don't unwrap here
        Self::Call(InstCallCtx {
            data: InstCall {
                reg,
                n_args: n_args.try_into().unwrap(),
            },
            ctx,
        })
    }
}

impl FormatInst for InstCall {
    fn fmt_inst(&self, f: &mut std::fmt::Formatter<'_>, fun: &compiler::Function) -> std::fmt::Result {
        write!(f, "{:5} {} {}", "call", self.reg, self.n_args)
    }
}

pub fn run_call(state: &mut VMState, inst: InstCall) -> ExecResult {
    let frame = state.frame();
    let fun_idx = match state.take_acc() {
        Value::NativeFn(f) => f(state as *mut VMState, inst.reg, inst.n_args),
        Value::Closure(c) => todo!(),
        other => return Err(state.get_context(inst).uncallable_type(other)),
    };

    Ok(())
}
