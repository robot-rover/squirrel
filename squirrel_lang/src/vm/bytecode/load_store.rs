use serde::{Deserialize, Serialize};

use super::{Const, FunIdx, Inst, InstCtx, Local, Reg};
use crate::{
    context::Span,
    impl_sub_inst,
    vm::{
        compiler::{self, FormatInst}, error::ExecResult, runtime::VMState, value::{Closure, Value}
    },
};

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum PrimType {
    Null,
    False,
    True,
}

impl From<PrimType> for Value {
    fn from(prim_type: PrimType) -> Self {
        match prim_type {
            PrimType::Null => Value::Null,
            PrimType::False => Value::Boolean(false),
            PrimType::True => Value::Boolean(true),
        }
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum InstLoad {
    Reg(Reg),
    Local(Local),
    Const(Const),
    FunIdx(FunIdx),
    PrimType(PrimType),
    U8(u8),
    I8(i8),
}

impl FormatInst for InstLoad {
    fn fmt_inst(&self, f: &mut std::fmt::Formatter<'_>, fun: &compiler::Function) -> std::fmt::Result {
        match self {
            InstLoad::Reg(reg) => write!(f, "{:5} {}", "load", reg),
            InstLoad::Local(local) => write!(f, "{:5} {}", "load", local.fmt_inst(fun)),
            InstLoad::Const(constant) => write!(f, "{:5} {}", "load", constant.fmt_inst(fun)),
            InstLoad::FunIdx(fun) => write!(f, "{:5} {}", "load", fun),
            InstLoad::PrimType(prim) => write!(f, "{:5} {:?}", "load", prim),
            InstLoad::U8(imm) => write!(f, "{:5} u {}", "load", imm),
            InstLoad::I8(imm) => write!(f, "{:5} i {}", "load", imm),
        }
    }
}

impl_sub_inst!(type Inst::Load(InstLoadCtx<InstLoad, Span>));

macro_rules! load_constructor {
    ($name:ident($data_name:ident: $variant:ident)) => {
        load_constructor!($name($data_name: $variant($variant)));
    };
    ($name:ident($data_name:ident: $variant:ident($data_ty:ident))) => {
        pub fn $name($data_name: $data_ty, ctx: Span) -> Self {
            Self::Load(InstLoadCtx {
                data: InstLoad::$variant($data_name),
                ctx,
            })
        }
    };
}

impl InstCtx {
    load_constructor!(loadr(reg: Reg));
    load_constructor!(loadl(local: Local));
    load_constructor!(loadc(constant: Const));
    load_constructor!(loadf(fun_idx: FunIdx));
    load_constructor!(loadp(prim_type: PrimType));
    load_constructor!(loadi(value: U8(u8)));
    load_constructor!(loads(value: I8(i8)));
}

pub fn run_load(state: &mut VMState, inst: InstLoad) -> ExecResult {
    let value = match inst {
        InstLoad::Reg(reg) => state.frame().get_reg(reg).clone(),
        InstLoad::Local(local) => state.frame().get_local(local).borrow().clone(),
        InstLoad::Const(constant) => state.frame().get_func().get_constant(constant).clone(),
        InstLoad::FunIdx(fun_idx) => todo!(),
        InstLoad::PrimType(prim_type) => prim_type.into(),
        InstLoad::U8(value) => Value::Integer(value as u64 as i64),
        InstLoad::I8(value) => Value::Integer(value as i64),
    };

    state.set_acc(value);

    Ok(())
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum InstStore {
    Reg(Reg),
    Local(Local),
}

impl FormatInst for InstStore {
    fn fmt_inst(&self, f: &mut std::fmt::Formatter<'_>, fun: &compiler::Function) -> std::fmt::Result {
        match self {
            InstStore::Reg(reg) => write!(f, "{:5} {}", "store", reg),
            InstStore::Local(local) => write!(f, "{:5} {}", "store", local.fmt_inst(fun)),
        }
    }
}


impl_sub_inst!(type Inst::Store(InstStoreCtx<InstStore, Span>));

impl InstCtx {
    pub fn storr(reg: Reg, ctx: Span) -> Self {
        Self::Store(InstStoreCtx {
            data: InstStore::Reg(reg),
            ctx,
        })
    }

    pub fn storl(local: Local, ctx: Span) -> Self {
        Self::Store(InstStoreCtx {
            data: InstStore::Local(local),
            ctx,
        })
    }
}

pub fn run_store(state: &mut VMState, inst: InstStore) -> ExecResult {
    let value = state.take_acc();
    match inst {
        InstStore::Reg(reg) => *state.frame_mut().get_reg_mut(reg) = value,
        InstStore::Local(local) => *state.frame().get_local(local).borrow_mut() = value,
    }
    Ok(())
}
