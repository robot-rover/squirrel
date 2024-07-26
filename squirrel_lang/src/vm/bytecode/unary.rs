use serde::{Deserialize, Serialize};

use crate::context::Span;

use super::{context::UnaryOpContext, Const, FunIdx, Inst, Local, Reg};

#[derive(Clone, Debug, Serialize, Deserialize)]
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
        Inst::Unary(InstUnary { op: UnaryOp::LNOT, ctx })
    }

    pub fn bnot(ctx: UnaryOpContext) -> Self {
        Inst::Unary(InstUnary { op: UnaryOp::BNOT, ctx })
    }

    pub fn neg(ctx: UnaryOpContext) -> Self {
        Inst::Unary(InstUnary { op: UnaryOp::NEG, ctx })
    }
}