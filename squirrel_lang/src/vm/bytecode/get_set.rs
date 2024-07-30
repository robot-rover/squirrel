use serde::{Deserialize, Serialize};

use crate::{context::Span, vm::{error::ExecResult, runtime::VMState, value::Value}};

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

pub fn run_get(state: &mut VMState, inst: &InstGet) -> ExecResult {
    let parent = match inst {
        InstGet::GET(_) | InstGet::GETC(_, _) => state.frame().get_env().clone(),
        InstGet::GETF(reg, _) | InstGet::ISIN(reg, _) => state.frame().get_reg(*reg).clone(),
        InstGet::GETFC(_, _) => state.take_acc(),
    };

    let field = match inst {
        InstGet::GETC(constant, _) | InstGet::GETFC(constant, _) => state.frame().get_func().get_constant(*constant).clone(),
        InstGet::GET(_) | InstGet::GETF(_, _) | InstGet::ISIN(_, _) => state.take_acc(),
    };

    state.set_acc(parent.get_field(&field.try_into().expect("Error Handling")).expect("Error Handling").clone());

    Ok(())
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

pub fn run_set(state: &mut VMState, inst: &InstSet) -> ExecResult {
    let parent = match inst {
        InstSet::SET(_, _) | InstSet::SETS(_, _) | InstSet::SETC(_, _) | InstSet::SETCS(_, _) => state.frame().get_env().clone(),
        InstSet::SETF(reg, _) | InstSet::SETFS(reg, _) => state.frame().get_reg(*reg).clone(),
    };

    let field = match inst {
        InstSet::SET(reg, _) | InstSet::SETS(reg, _) => state.frame().get_reg(*reg).clone(),
        InstSet::SETC(constant, _) | InstSet::SETCS(constant, _) => state.frame().get_func().get_constant(*constant).clone(),
        InstSet::SETF(reg, _) | InstSet::SETFS(reg, _) => state.frame().get_reg(reg.nth(1)).clone(),
    };

    let is_newslot = match inst {
        InstSet::SETS(_, _) | InstSet::SETCS(_, _) | InstSet::SETFS(_, _) => true,
        InstSet::SET(_, _) | InstSet::SETC(_, _) | InstSet::SETF(_, _) => false,
    };

    let value = state.take_acc();

    match (parent, field) {
        // TODO: Bounds checking
        (Value::Array(array), Value::Integer(index)) => *array.borrow_mut().get_mut(index as usize).unwrap() = value,
        (Value::Array(array), other) => todo!("Error handling"),
        (Value::Table(table), field) => table.borrow_mut().set_field(field.try_into().unwrap(), value, is_newslot).unwrap(),
        (Value::Class(class), field) => class.borrow_mut().set_field(field.try_into().unwrap(), value, is_newslot).unwrap(),
        (Value::Instance(inst), field) => inst.borrow_mut().set_field(field.try_into().unwrap(), value, is_newslot).unwrap(),
        (other, _) => todo!("Error handling"),
    };

    Ok(())
}