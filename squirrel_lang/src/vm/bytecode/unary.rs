use std::ops::Not;

use serde::{Deserialize, Serialize};

use crate::{
    context::Span,
    impl_sub_inst,
    vm::{error::ExecResult, runtime::VMState, value::Value},
};

use super::{context::UnaryOpContext, Const, FunIdx, Inst, InstCtx, Local, Reg};

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum InstUnary {
    LNOT,
    BNOT,
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
    let acc = state.take_acc();
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
                .illegal_operation(inst.name(), other))
        }
    };

    state.set_acc(result);
    Ok(())
}
