use std::fmt;

use serde::{Deserialize, Serialize};

use super::{Inst, InstCtx, Reg};
use crate::{
    impl_sub_inst,
    vm::{bytecode::context::BinaryOpContext, compiler::{self, FormatInst}, error::ExecResult, runtime::VMState, value::Value},
};

#[derive(Clone, Copy, Debug, Serialize, Deserialize, strum_macros::Display)]
enum BitwiseOp {
    #[strum(to_string="band")]
    Band,
    #[strum(to_string="bor")]
    Bor,
    #[strum(to_string="bxor")]
    Bxor,
    #[strum(to_string="brsh")]
    Brsh,
    #[strum(to_string="bash")]
    Bash,
}

impl BitwiseOp {
    // TODO: Shows the wrong operator direction in some cases
    fn name(&self) -> &'static str {
        match self {
            Self::Band => "&",
            Self::Bor => "|",
            Self::Bxor => "^",
            Self::Brsh => ">>",
            Self::Bash => "<<",
        }
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct InstBitwise {
    op: BitwiseOp,
    reg: Reg,
}

impl FormatInst for InstBitwise {
    fn fmt_inst(&self, f: &mut std::fmt::Formatter<'_>, fun: &compiler::Function) -> fmt::Result {
        write!(f, "{:5} {}", self.op, self.reg)
    }
}

impl_sub_inst!(type Inst::Bitwise(InstBitwiseCtx<InstBitwise, BinaryOpContext>));

macro_rules! bitwise_constructor {
    ($name:ident, $op:ident) => {
        pub fn $name(reg: Reg, ctx: BinaryOpContext) -> Self {
            Self::Bitwise(InstBitwiseCtx {
                data: InstBitwise {
                    op: BitwiseOp::$op,
                    reg,
                },
                ctx,
            })
        }
    };
}

impl InstCtx {
    bitwise_constructor!(band, Band);
    bitwise_constructor!(bor, Bor);
    bitwise_constructor!(bxor, Bxor);
    bitwise_constructor!(brsh, Brsh);
    bitwise_constructor!(bash, Bash);
}

pub fn run_bitwise(state: &mut VMState, inst: InstBitwise) -> ExecResult {
    let frame = state.frame();
    let lhs = frame.get_reg(inst.reg).clone();
    let rhs = state.take_acc();
    match (lhs, rhs) {
        (Value::Integer(lhs), Value::Integer(rhs)) => {
            state.set_acc(run_bitwise_int(lhs, rhs, inst.op));
        }
        other => {
            return Err(state
                .get_context(inst)
                .illegal_operation(inst.op.name(), other.0, other.1))
        }
    }
    Ok(())
}

fn run_bitwise_int(lhs: i64, rhs: i64, op: BitwiseOp) -> Value {
    match op {
        BitwiseOp::Band => Value::Integer(lhs & rhs),
        BitwiseOp::Bor => Value::Integer(lhs | rhs),
        BitwiseOp::Bxor => Value::Integer(lhs ^ rhs),
        BitwiseOp::Brsh => Value::Integer(lhs >> rhs),
        BitwiseOp::Bash => Value::Integer(lhs << rhs),
    }
}
