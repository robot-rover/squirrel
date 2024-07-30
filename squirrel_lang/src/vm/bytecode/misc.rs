use crate::{context::Span, vm::{error::ExecResult, runtime::VMState}};

use super::{Const, FunIdx, Inst, Local, Reg, Tag};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
enum MiscOp {
    This,
    Root,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstMisc {
    op: MiscOp,
    ctx: Span,
}

impl Inst {
    pub fn this(ctx: Span) -> Self {
        Inst::Misc(InstMisc { op: MiscOp::This, ctx })
    }

    pub fn root(ctx: Span) -> Self {
        Inst::Misc(InstMisc { op: MiscOp::Root, ctx })
    }
}

pub fn run_misc(state: &mut VMState, inst: &InstMisc) -> ExecResult {
    match inst.op {
        MiscOp::This => {
            state.set_acc(state.frame().get_env().clone());
        }
        MiscOp::Root => {
            state.set_acc(state.get_root().clone().into());
        }
    }

    Ok(())
}