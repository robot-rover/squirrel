use std::ops::Not;

use serde::{Deserialize, Serialize};

use crate::{
    context::Span,
    vm::{error::ExecResult, runtime::VMState, value::Value},
};

use super::{context::UnaryOpContext, Const, FunIdx, Inst, Local, Reg};

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
enum UnaryOp {
    LNOT,
    BNOT,
    NEG,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstUnary {
    op: UnaryOp,
    ctx: UnaryOpContext,
}

impl Inst {
    pub fn lnot(ctx: UnaryOpContext) -> Self {
        Inst::Unary(InstUnary {
            op: UnaryOp::LNOT,
            ctx,
        })
    }

    pub fn bnot(ctx: UnaryOpContext) -> Self {
        Inst::Unary(InstUnary {
            op: UnaryOp::BNOT,
            ctx,
        })
    }

    pub fn neg(ctx: UnaryOpContext) -> Self {
        Inst::Unary(InstUnary {
            op: UnaryOp::NEG,
            ctx,
        })
    }
}

pub fn run_unary(state: &mut VMState, inst: &InstUnary) -> ExecResult {
    let acc = state.take_acc();
    let result = match (inst.op, acc) {
        (UnaryOp::LNOT, any) => (!any.truthy()).into(),
        (UnaryOp::BNOT, Value::Integer(i)) => Value::Integer(!i),
        (UnaryOp::BNOT, other) => panic!("Expected integer, got {:?}", other),
        (UnaryOp::NEG, Value::Integer(i)) => Value::Integer(-i),
        (UnaryOp::NEG, Value::Float(f)) => Value::Float(-f),
        (UnaryOp::NEG, Value::Instance(inst)) => todo!("Metamethods"),
        (UnaryOp::NEG, Value::Table(table)) => todo!("Metamethods"),
        (UnaryOp::NEG, other) => panic!("Expected number, got {:?}", other),
    };

    state.set_acc(result);
    Ok(())
}
