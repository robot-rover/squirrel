use crate::context::Span;

use super::{Const, FunIdx, Inst, Local, Reg, Tag};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
enum MiscOp {
    This,
    Root,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstMisc {
    op: MiscOp,
    ctx: Span,
}

impl Inst {
    pub fn this(ctx: Span) -> Self {
        Inst::Misc(InstMisc { op: MiscOp::This, ctx })
    }

    pub fn root(ctx: Span) -> Self {
        Inst::Misc(InstMisc { op: MiscOp::Root, ctx })
    }
}