use serde::{Deserialize, Serialize};

use crate::context::Span;

use super::{context::{BinaryOpContext, GetFieldContext, GetIdentContext, SetFieldContext, SetIdentContext}, Const, FunIdx, Inst, Local, Reg, Tag};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum InstGet {
    GET(GetIdentContext),
    GETC(Const, GetIdentContext),
    GETF(Reg, GetFieldContext),
    GETFC(Const, GetFieldContext),
    ISIN(Reg, BinaryOpContext),
}

impl Inst {
    pub fn get(ctx: GetIdentContext) -> Inst {
        Inst::Get(InstGet::GET(ctx))
    }

    pub fn getc(constant: Const, ctx: GetIdentContext) -> Inst {
        Inst::Get(InstGet::GETC(constant, ctx))
    }

    pub fn getf(reg_idx: Reg, ctx: GetFieldContext) -> Inst {
        Inst::Get(InstGet::GETF(reg_idx, ctx))
    }

    pub fn getfc(constant: Const, ctx: GetFieldContext) -> Inst {
        Inst::Get(InstGet::GETFC(constant, ctx))
    }

    pub fn isin(reg_idx: Reg, ctx: BinaryOpContext) -> Inst {
        Inst::Get(InstGet::ISIN(reg_idx, ctx))
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum InstSet {
    SET(Reg, SetIdentContext),
    SETS(Reg, SetIdentContext),
    SETC(Const, SetIdentContext),
    SETCS(Const, SetIdentContext),
    SETF(Reg, SetFieldContext),
    SETFS(Reg, SetFieldContext),
}

impl Inst {
    pub fn setf(reg_idx: Reg, ctx: SetFieldContext, is_ns: bool) -> Inst {
        Inst::Set(if is_ns {
            InstSet::SETFS(reg_idx, ctx)
        } else {
            InstSet::SETF(reg_idx, ctx)
        })
    }

    pub fn set(reg_idx: Reg, ctx: SetIdentContext, is_ns: bool) -> Inst {
        Inst::Set(if is_ns {
            InstSet::SETS(reg_idx, ctx)
        } else {
            InstSet::SET(reg_idx, ctx)
        })
    }

    pub fn setc(constant: Const, ctx: SetIdentContext, is_ns: bool) -> Inst {
        Inst::Set(if is_ns {
            InstSet::SETCS(constant, ctx)
        } else {
            InstSet::SETC(constant, ctx)
        })
    }
}