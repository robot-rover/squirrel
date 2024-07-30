use serde::{Deserialize, Serialize};

use super::{Block, Const, FunIdx, Inst, Local, Reg};
use crate::{
    context::Span,
    vm::{error::ExecResult, runtime::VMState, value::Value},
};

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
        Inst::Jump(InstJump {
            kind: JumpKind::Always,
            block,
            ctx,
        })
    }

    pub fn jt(block: Block, ctx: Span) -> Inst {
        Inst::Jump(InstJump {
            kind: JumpKind::IfTrue,
            block,
            ctx,
        })
    }

    pub fn jf(block: Block, ctx: Span) -> Inst {
        Inst::Jump(InstJump {
            kind: JumpKind::IfFalse,
            block,
            ctx,
        })
    }
}

pub fn run_jump(state: &mut VMState, inst: &InstJump) -> ExecResult {
    match inst.kind {
        JumpKind::Always => {}
        JumpKind::IfTrue | JumpKind::IfFalse => {
            let state_to_jump = matches!(inst.kind, JumpKind::IfTrue);
            if state.take_acc().truthy() != state_to_jump {
                return Ok(());
            }
        }
    }

    state.frame_mut().jump_to_block(inst.block);

    Ok(())
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
        Inst::Ret(InstRet {
            kind: RetKind::Void,
            ctx,
        })
    }

    pub fn ret(ctx: Span) -> Inst {
        Inst::Ret(InstRet {
            kind: RetKind::Value,
            ctx,
        })
    }
}

pub fn run_ret(state: &mut VMState, inst: &InstRet) -> ExecResult {
    if matches!(inst.kind, RetKind::Void) {
        state.set_acc(Value::Null);
    }

    state.call_stack.pop().unwrap();

    Ok(())
}
