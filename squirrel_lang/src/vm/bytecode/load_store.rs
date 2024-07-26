use serde::{Deserialize, Serialize};

use super::{Const, FunIdx, Inst, Local, Reg};
use crate::context::Span;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum PrimType {
    Null,
    False,
    True,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
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

#[derive(Clone, Debug, Serialize, Deserialize)]
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