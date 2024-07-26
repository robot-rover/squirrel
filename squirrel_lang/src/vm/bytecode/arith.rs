use super::{Inst, Reg};
use crate::vm::bytecode::context::BinaryOpContext;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
enum ArithOp {
    ADD,
    SUB,
    MUL,
    DIV,
    MODU,
    POW,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstArith {
    op: ArithOp,
    reg: Reg,
    ctx: BinaryOpContext,
}

macro_rules! arith_constructor {
    ($name:ident, $op:ident) => {
        pub fn $name(reg: Reg, ctx: BinaryOpContext) -> Self {
            Inst::Arith(InstArith { op: ArithOp::$op, reg, ctx })
        }
    };
}

impl Inst {
    arith_constructor!(add, ADD);
    arith_constructor!(sub, SUB);
    arith_constructor!(mul, MUL);
    arith_constructor!(div, DIV);
    arith_constructor!(modu, MODU);
    arith_constructor!(pow, POW);
}
