use crate::{
    context::Span,
    vm::{error::ExecResult, runtime::VMState, value::Value},
};

use super::{context::FnCallContext, Const, FunIdx, Inst, Local, Reg, Tag};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstCall {
    reg: Reg,
    n_args: u8,
    ctx: FnCallContext,
}

impl Inst {
    pub fn call(reg: Reg, n_args: usize, ctx: FnCallContext) -> Self {
        // TODO: Don't unwrap here
        Inst::Call(InstCall {
            reg,
            n_args: n_args.try_into().unwrap(),
            ctx,
        })
    }
}

pub fn run_call(state: &mut VMState, inst: &InstCall) -> ExecResult {
    let frame = state.frame();
    let fun_idx = match state.take_acc() {
        Value::NativeFn(f) => f(state as *mut VMState, inst.reg, inst.n_args),
        Value::Closure(c) => todo!(),
        other => todo!("error handling"),
    };

    Ok(())
}
