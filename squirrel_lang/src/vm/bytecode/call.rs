use crate::context::Span;

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
        Inst::Call(InstCall { reg, n_args: n_args.try_into().unwrap(), ctx })
    }
}