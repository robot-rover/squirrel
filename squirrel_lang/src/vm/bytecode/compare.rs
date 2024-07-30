use num_traits::ConstZero;
use serde::{Deserialize, Serialize};

use crate::vm::{bytecode::context::BinaryOpContext, runtime::VMState};
use crate::vm::{bytecode::Data, error::ExecResult, value::Value};

use super::{Inst, Reg};

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
enum CompareKind {
    ISLT,
    ISLE,
    ISGT,
    ISGE,
    ISEQ,
    ISNE,
    CMP,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstCompare {
    reg: Reg,
    kind: CompareKind,
    ctx: BinaryOpContext,
}

macro_rules! compare_constructor {
    ($name:ident, $kind:ident) => {
        pub fn $name(reg: Reg, ctx: BinaryOpContext) -> Self {
            Inst::Compare(InstCompare {
                reg,
                kind: CompareKind::$kind,
                ctx,
            })
        }
    };
}

impl Inst {
    compare_constructor!(islt, ISLT);
    compare_constructor!(isle, ISLE);
    compare_constructor!(isgt, ISGT);
    compare_constructor!(isge, ISGE);
    compare_constructor!(iseq, ISEQ);
    compare_constructor!(isne, ISNE);
    compare_constructor!(cmp, CMP);
}

pub fn run_compare(state: &mut VMState, inst: &InstCompare) -> ExecResult {
    let frame = state.frame();
    let lhs = frame.get_reg(inst.reg).clone();
    let rhs = state.take_acc();
    state.set_acc(match (lhs, rhs) {
        (Value::Integer(lhs), Value::Integer(rhs)) => compare_zero(inst.kind, lhs - rhs),
        (Value::Float(lhs), Value::Integer(rhs)) => compare_zero(inst.kind, lhs - rhs as f64),
        (Value::Integer(lhs), Value::Float(rhs)) => compare_zero(inst.kind, lhs as f64 - rhs),
        (Value::Float(lhs), Value::Float(rhs)) => compare_zero(inst.kind, lhs - rhs),
        (Value::Instance(inst), other) => todo!("Metamethods"),
        (Value::Table(inst), other) => todo!("Metamethods"),
        other => todo!("Error handling"),
    });

    Ok(())
}

fn compare_zero<T: ConstZero + PartialOrd + Into<Value>>(cmp: CompareKind, diff: T) -> Value {
    let zero = T::zero();
    Value::Boolean(match cmp {
        CompareKind::ISLT => diff < zero,
        CompareKind::ISLE => diff <= zero,
        CompareKind::ISGT => diff > zero,
        CompareKind::ISGE => diff >= zero,
        CompareKind::ISEQ => diff == zero,
        CompareKind::ISNE => diff != zero,
        CompareKind::CMP => return diff.into(),
    })
}
