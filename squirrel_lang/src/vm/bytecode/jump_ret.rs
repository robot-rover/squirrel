use serde::{Deserialize, Serialize};

use super::{Block, Const, FunIdx, Inst, Local, Reg};
use crate::context::Span;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum JumpKind {
    Always,
    IfTrue,
    IfFalse,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstJump {
    pub kind: JumpKind,
    pub block: Block,
    ctx: Span,
}

impl Inst {
    pub fn jmp(block: Block, ctx: Span) -> Inst {
        Inst::Jump(InstJump { kind: JumpKind::Always, block, ctx })
    }

    pub fn jt(block: Block, ctx: Span) -> Inst {
        Inst::Jump(InstJump { kind: JumpKind::IfTrue, block, ctx })
    }

    pub fn jf(block: Block, ctx: Span) -> Inst {
        Inst::Jump(InstJump { kind: JumpKind::IfFalse, block, ctx })
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
enum RetKind {
    Value,
    Void,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstRet {
    kind: RetKind,
    ctx: Span,
}

impl Inst {
    pub fn retn(ctx: Span) -> Inst {
        Inst::Ret(InstRet { kind: RetKind::Void, ctx })
    }

    pub fn ret(ctx: Span) -> Inst {
        Inst::Ret(InstRet { kind: RetKind::Value, ctx })
    }
}