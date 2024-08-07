use crate::{
    impl_sub_inst,
    sq_error::Span,
    vm::{
        compiler::{self, FormatInst},
        error::ExecResult,
        runtime::VMState,
        value::{Class, Value},
    },
};

use super::{Const, FunIdx, Inst, InstCtx, Local, Reg, Tag};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Serialize, Deserialize, strum_macros::Display)]
pub enum InstMisc {
    #[strum(to_string = "this")]
    This,
    #[strum(to_string = "root")]
    Root,
    #[strum(to_string = "base")]
    Base,
    #[strum(to_string = "array")]
    Array(u8),
}
impl FormatInst for InstMisc {
    fn fmt_inst(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        fun: &compiler::Function,
    ) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl_sub_inst!(type Inst::Misc(InstMiscCtx<InstMisc, Span>));

impl InstCtx {
    pub fn this(ctx: Span) -> Self {
        Self::Misc(InstMiscCtx {
            data: InstMisc::This,
            ctx,
        })
    }

    pub fn root(ctx: Span) -> Self {
        Self::Misc(InstMiscCtx {
            data: InstMisc::Root,
            ctx,
        })
    }

    pub fn base(ctx: Span) -> Self {
        Self::Misc(InstMiscCtx {
            data: InstMisc::Base,
            ctx,
        })
    }

    pub fn array(ctx: Span, init_cap: u8) -> Self {
        Self::Misc(InstMiscCtx {
            data: InstMisc::Array(init_cap),
            ctx,
        })
    }
}

pub fn run_misc(state: &mut VMState, inst: InstMisc) -> ExecResult {
    match inst {
        InstMisc::This => {
            state.set_acc(state.frame().get_env().clone());
        }
        InstMisc::Root => {
            state.set_acc(state.get_root().clone().into());
        }
        InstMisc::Base => {
            let env = state.frame().get_env();
            state.set_acc(match env {
                Value::Class(c) => c.borrow().get_base(),
                _ => Value::Null,
            })
        }
        InstMisc::Array(cap) => state.set_acc(Value::array(Vec::with_capacity(cap as usize))),
    }

    Ok(())
}
