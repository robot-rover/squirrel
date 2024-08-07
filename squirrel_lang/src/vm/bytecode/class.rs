use crate::{
    context::NewClassContext,
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

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct InstClass {
    inherits: bool,
}

impl FormatInst for InstClass {
    fn fmt_inst(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        fun: &compiler::Function,
    ) -> std::fmt::Result {
        write!(
            f,
            "{:5} {}",
            "class",
            if self.inherits { "inherits" } else { "" }
        )
    }
}

impl_sub_inst!(type Inst::Class(InstClassCtx<InstClass, NewClassContext>));

impl InstCtx {
    pub fn class(inherits: bool, ctx: NewClassContext) -> Self {
        Self::Class(InstClassCtx {
            data: InstClass { inherits },
            ctx,
        })
    }
}

pub fn run_class(state: &mut VMState, inst: InstClass) -> ExecResult {
    let parent = if inst.inherits {
        match state.get_acc().clone() {
            Value::Class(c) => Some(c),
            other => return Err(state.get_context(inst).extending_non_class(other)),
        }
    } else {
        None
    };
    state.set_acc(Value::class(Class::new(parent)));

    Ok(())
}
