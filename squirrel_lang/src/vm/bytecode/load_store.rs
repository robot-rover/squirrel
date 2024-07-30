use serde::{Deserialize, Serialize};

use super::{Const, FunIdx, Inst, Local, Reg};
use crate::{context::Span, vm::{error::ExecResult, runtime::VMState, value::{Closure, Value}}};

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
enum LoadSrc {
    Reg(Reg),
    Local(Local),
    Const(Const),
    FunIdx(FunIdx),
    PrimType(PrimType),
    U8(u8),
    I8(i8),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstLoad {
    data: LoadSrc,
    ctx: Span,
}

macro_rules! load_constructor {
    ($name:ident($data_name:ident: $variant:ident)) => {
        pub fn $name($data_name: $variant, ctx: Span) -> Inst {
            Inst::Load(InstLoad { data: LoadSrc::$variant($data_name), ctx })
        }
    };
    ($name:ident($data_name:ident: $variant:ident($data_ty:ident))) => {
        pub fn $name($data_name: $data_ty, ctx: Span) -> Inst {
            Inst::Load(InstLoad { data: LoadSrc::$variant($data_name), ctx })
        }
    };
}

impl Inst {
    load_constructor!(loadr(reg: Reg));
    load_constructor!(loadl(local: Local));
    load_constructor!(loadc(constant: Const));
    load_constructor!(loadf(fun_idx: FunIdx));
    load_constructor!(loadp(prim_type: PrimType));
    load_constructor!(loadi(value: U8(u8)));
    load_constructor!(loads(value: I8(i8)));
}

pub fn run_load(state: &mut VMState, inst: &InstLoad) -> ExecResult {
    let value = match inst.data {
        LoadSrc::Reg(reg) => state.frame().get_reg(reg).clone(),
        LoadSrc::Local(local) => state.frame().get_local(local).borrow().clone(),
        LoadSrc::Const(constant) => state.frame().get_func().get_constant(constant).clone(),
        LoadSrc::FunIdx(fun_idx) => todo!(),
        LoadSrc::PrimType(prim_type) => prim_type.into(),
        LoadSrc::U8(value) => Value::Integer(value as u64 as i64),
        LoadSrc::I8(value) => Value::Integer(value as i64),
    };

    state.set_acc(value);

    Ok(())
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
enum StoreTarget {
    Reg(Reg),
    Local(Local),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstStore {
    target: StoreTarget,
    ctx: Span,
}

impl Inst {
    pub fn storr(reg: Reg, ctx: Span) -> Inst {
        Inst::Store(InstStore { target: StoreTarget::Reg(reg), ctx })
    }

    pub fn storl(local: Local, ctx: Span) -> Inst {
        Inst::Store(InstStore { target: StoreTarget::Local(local), ctx })
    }
}

pub fn run_store(state: &mut VMState, inst: &InstStore) -> ExecResult {
    let value = state.take_acc();
    match inst.target {
        StoreTarget::Reg(reg) => *state.frame_mut().get_reg_mut(reg) = value,
        StoreTarget::Local(local) => *state.frame().get_local(local).borrow_mut() = value,
    }
    Ok(())
}