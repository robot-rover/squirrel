use serde::{Deserialize, Serialize};

use super::{Block, Const, FunIdx, Inst, InstCtx, Local, Reg};
use crate::{
    context::Span,
    impl_sub_inst,
    vm::{
        compiler::{self, FormatInst},
        error::ExecResult,
        runtime::VMState,
        value::Value,
    },
};

#[derive(Clone, Copy, Debug, Serialize, Deserialize, strum_macros::Display)]
pub enum JumpKind {
    #[strum(to_string = "jmp")]
    Always,
    #[strum(to_string = "jt")]
    IfTrue,
    #[strum(to_string = "jf")]
    IfFalse,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct InstJump {
    pub kind: JumpKind,
    pub block: Block,
}

impl FormatInst for InstJump {
    fn fmt_inst(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        fun: &compiler::Function,
    ) -> std::fmt::Result {
        write!(f, "{:5} {}", self.kind, self.block)
    }
}

impl_sub_inst!(type Inst::Jump(InstJumpCtx<InstJump, Span>));

impl InstCtx {
    pub fn jmp(block: Block, ctx: Span) -> Self {
        Self::Jump(InstJumpCtx {
            data: InstJump {
                kind: JumpKind::Always,
                block,
            },
            ctx,
        })
    }

    pub fn jt(block: Block, ctx: Span) -> Self {
        Self::Jump(InstJumpCtx {
            data: InstJump {
                kind: JumpKind::IfTrue,
                block,
            },
            ctx,
        })
    }

    pub fn jf(block: Block, ctx: Span) -> Self {
        Self::Jump(InstJumpCtx {
            data: InstJump {
                kind: JumpKind::IfFalse,
                block,
            },
            ctx,
        })
    }
}

pub fn run_jump(state: &mut VMState, inst: InstJump) -> ExecResult {
    match inst.kind {
        JumpKind::Always => {}
        JumpKind::IfTrue | JumpKind::IfFalse => {
            let state_to_jump = matches!(inst.kind, JumpKind::IfTrue);
            if state.get_acc().truthy() != state_to_jump {
                return Ok(());
            }
        }
    }

    state.frame_mut().jump_to_block(inst.block);

    Ok(())
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, strum_macros::Display)]
pub enum InstRet {
    #[strum(to_string = "ret")]
    Value,
    #[strum(to_string = "retn")]
    Void,
}
impl FormatInst for InstRet {
    fn fmt_inst(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        fun: &compiler::Function,
    ) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl_sub_inst!(type Inst::Ret(InstRetCtx<InstRet, Span>));

impl InstCtx {
    pub fn retn(ctx: Span) -> Self {
        Self::Ret(InstRetCtx {
            data: InstRet::Void,
            ctx,
        })
    }

    pub fn ret(ctx: Span) -> Self {
        Self::Ret(InstRetCtx {
            data: InstRet::Value,
            ctx,
        })
    }
}

pub fn run_ret(state: &mut VMState, inst: InstRet) -> ExecResult {
    if matches!(inst, InstRet::Void) {
        state.set_acc(Value::Null);
    }

    state.call_stack.pop().unwrap();

    Ok(())
}
