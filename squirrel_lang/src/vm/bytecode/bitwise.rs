use serde::{Deserialize, Serialize};

use super::{Inst, Reg};
use crate::vm::{bytecode::context::BinaryOpContext, error::ExecResult, runtime::VMState, value::Value};

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
enum BitwiseOp {
    BAND,
    BOR,
    BXOR,
    BRSH,
    BASH,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstBitwise {
    op: BitwiseOp,
    reg: Reg,
    ctx: BinaryOpContext,
}

macro_rules! bitwise_constructor {
    ($name:ident, $op:ident) => {
        pub fn $name(reg: Reg, ctx: BinaryOpContext) -> Self {
            Inst::Bitwise(InstBitwise { op: BitwiseOp::$op, reg, ctx })
        }
    };
}

impl Inst {
    bitwise_constructor!(band, BAND);
    bitwise_constructor!(bor, BOR);
    bitwise_constructor!(bxor, BXOR);
    bitwise_constructor!(brsh, BRSH);
    bitwise_constructor!(bash, BASH);
}

pub fn run_bitwise(state: &mut VMState, inst: &InstBitwise) -> ExecResult {
    let frame = state.frame();
    let lhs = frame.get_reg(inst.reg).clone();
    let rhs = state.take_acc();
    match (lhs, rhs) {
        (Value::Integer(lhs), Value::Integer(rhs)) => {
            state.set_acc(run_bitwise_int(lhs, rhs, inst.op));
        },
        (lhs, rhs) => {
            todo!("Bitwise operation on non-integer values: {:?} {:?} {:?}", lhs, inst.op, rhs);
        },
    }
    Ok(())
}

fn run_bitwise_int(lhs: i64, rhs: i64, op: BitwiseOp) -> Value {
    match op {
        BitwiseOp::BAND => Value::Integer(lhs & rhs),
        BitwiseOp::BOR => Value::Integer(lhs | rhs),
        BitwiseOp::BXOR => Value::Integer(lhs ^ rhs),
        BitwiseOp::BRSH => Value::Integer(lhs >> rhs),
        BitwiseOp::BASH => Value::Integer(lhs << rhs),
    }
}