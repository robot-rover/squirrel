use serde::{Deserialize, Serialize};

use super::{Inst, Reg};
use crate::vm::bytecode::context::BinaryOpContext;

#[derive(Clone, Debug, Serialize, Deserialize)]
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