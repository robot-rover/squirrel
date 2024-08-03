use std::ops::Not;

use serde::{Deserialize, Serialize};

use crate::{
    context::Span,
    impl_sub_inst,
    vm::{
        compiler::{self, FormatInst},
        error::ExecResult,
        runtime::VMState,
        value::Value,
    },
};

use super::{context::UnaryOpContext, Const, FunIdx, Inst, InstCtx, InstJump, Local, Reg};

#[derive(Clone, Copy, Debug, Serialize, Deserialize, strum_macros::Display)]
pub enum InstUnary {
    #[strum(to_string = "lnot")]
    LNOT,
    #[strum(to_string = "lbot")]
    BNOT,
    #[strum(to_string = "neg")]
    NEG,
}

impl InstUnary {
    pub fn name(&self) -> &'static str {
        match self {
            Self::LNOT => "!",
            Self::BNOT => "~",
            Self::NEG => "-",
        }
    }
}

impl FormatInst for InstUnary {
    fn fmt_inst(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        fun: &compiler::Function,
    ) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl_sub_inst!(type Inst::Unary(InstUnaryCtx<InstUnary, UnaryOpContext>));

impl InstCtx {
    pub fn lnot(ctx: UnaryOpContext) -> Self {
        Self::Unary(InstUnaryCtx {
            data: InstUnary::LNOT,
            ctx,
        })
    }

    pub fn bnot(ctx: UnaryOpContext) -> Self {
        Self::Unary(InstUnaryCtx {
            data: InstUnary::BNOT,
            ctx,
        })
    }

    pub fn neg(ctx: UnaryOpContext) -> Self {
        Self::Unary(InstUnaryCtx {
            data: InstUnary::NEG,
            ctx,
        })
    }
}

pub fn run_unary(state: &mut VMState, inst: InstUnary) -> ExecResult {
    let acc = state.get_acc();
    let result = match (inst, acc) {
        (InstUnary::LNOT, any) => (!any.truthy()).into(),
        (InstUnary::BNOT, Value::Integer(i)) => Value::Integer(!i),
        (InstUnary::NEG, Value::Integer(i)) => Value::Integer(-i),
        (InstUnary::NEG, Value::Float(f)) => Value::Float(-f),
        (InstUnary::NEG, Value::Instance(inst)) => todo!("Metamethods"),
        (InstUnary::NEG, Value::Table(table)) => todo!("Metamethods"),
        (InstUnary::BNOT | InstUnary::NEG, other) => {
            return Err(state
                .get_context(inst)
                .illegal_operation(inst.name(), other.clone()))
        }
    };

    state.set_acc(result);
    Ok(())
}
