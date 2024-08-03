mod arith;
mod bitwise;
mod call;
mod compare;
pub mod context;
mod get_set;
mod jump_ret;
mod load_store;
mod misc;
mod unary;

pub use arith::{run_arith, InstArith, InstArithCtx};
pub use bitwise::{run_bitwise, InstBitwise, InstBitwiseCtx};
pub use call::{run_call, InstCall, InstCallCtx};
pub use compare::{run_compare, InstCompare, InstCompareCtx};
pub use get_set::{run_get, run_set, InstGet, InstGetCtx, InstSet, InstSetCtx};
pub use jump_ret::{run_jump, run_ret, InstJump, InstJumpCtx, InstRet, InstRetCtx, JumpKind};
pub use load_store::{run_load, run_store, InstLoad, InstLoadCtx, InstStore, InstStoreCtx};
use misc::InstMiscCtx;
pub use misc::{run_misc, InstMisc};
use unary::InstUnaryCtx;
pub use unary::{run_unary, InstUnary};

use serde::{Deserialize, Serialize};
use strum_macros::EnumDiscriminants;

use super::compiler::FormatInst;

macro_rules! newtype {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, Serialize, Deserialize)]
        pub struct $name(u32);

        impl From<u32> for $name {
            fn from(value: u32) -> $name {
                $name(value)
            }
        }

        impl From<$name> for u32 {
            fn from(wrapper: $name) -> u32 {
                wrapper.0
            }
        }

        impl $name {
            pub fn as_idx(&self) -> usize {
                self.0 as usize
            }
        }
    };
}

newtype!(Const);
newtype!(Reg);
newtype!(FunIdx);
newtype!(Block);
newtype!(Local);

impl Reg {
    pub fn nth(&self, n: u32) -> Self {
        Self(self.0 + n)
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
#[repr(u8)]
pub enum Tag {
    // Comparison Instructions
    // data is register index
    // acc = reg[data] (op) acc
    ISLT, // less
    ISLE, // less or equal
    ISGT, // greater
    ISGE, // greater or equal
    ISEQ, // equal
    ISNE, // not equal
    CMP,  // compare

    // Move Instructions
    LOADR, // acc = reg[data]
    STORR, // reg[data] = acc
    // SWAPR, // acc <=> reg[data]
    LOADL, // acc = locals[data]
    STORL, // locals[data] = acc
    // SWAPL, // acc <=> locals[data]
    LOADC, // acc = const[data]
    LOADF, // acc = functions[data]
    LOADP, // acc = match data { 0 => false, 1 => true, 2 => null }
    LOADI, // acc = data (as an unsigned integer)
    LOADS, // acc = data (as an sign extended integer)

    // Arithmetic Instructions
    NEG,  // acc = -acc
    ADD,  // acc = reg[data] + acc
    SUB,  // acc = reg[data] - acc
    MUL,  // acc = reg[data] * acc
    DIV,  // acc = reg[data] / acc
    MODU, // acc = reg[data] % acc

    // Logical Instructions
    LNOT, // acc = !acc
    // LAND, // acc = !acc ? acc : reg[data]
    // LOR, // acc = acc ? acc : reg[data]

    // Bitwise Instructions
    BNOT, // acc = ~acc
    BAND, // acc = reg[data] & acc
    BOR,  // acc = reg[data] | acc
    BXOR, // acc = reg[data] ^ acc
    BRSH, // acc = reg[data] >> acc
    BASH, // acc = reg[data] >>> acc

    // Control Flow
    JMP,  // goes to blocks[data]
    JT,   // goes to blocks[data] if acc
    JF,   // goes to blocks[data] if !acc
    RET,  // returns from function (returns acc)
    RETN, // returns from function (returns null)

    // Function Calls
    // CAL<X> = calls function with X arguments
    // function is in acc, this is reg[data], arguments are in reg[data+1..data+X+1]
    CALL0,
    CALL1,
    CALL2,
    CALL3,
    CALL4,
    CALL5,
    CALL6,
    // CALN = calls function with N arguments
    // Function is special, it must appear twice in a row
    // data of the first instruction is N
    // data of the second is idx
    // function is in acc
    // arguments are in reg[idx..idx+N]
    CALLN,

    // Field Instructions
    GETF,  // acc = reg[data][acc]
    GETFC, // acc = acc[const[data]]
    GET,   // acc = env[acc]
    GETC,  // acc = env[const[data]]

    SETF,  // reg[data][reg[data+1]] = acc
    SETFS, // reg[data][reg[data+1]] = acc (newslot)
    SET,   // env[reg[data]] = acc
    SETC,  // env[const[data]] = acc
    SETS,  // env[reg[data]] = acc (newslot)
    SETCS, // env[const[data]] = acc (newslot)

    ISIN, // acc = reg[data] in acc

    // Runtime State
    THIS, // acc = this
    ROOT, // acc = root
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SubInst<D, C> {
    pub data: D,
    pub ctx: C,
}

impl<D: Copy, C> SubInst<D, C> {
    pub fn strip(&self) -> D {
        self.data
    }
}

pub trait SubInstGetContext {
    type Context;
    fn get_ctx(inst: &InstCtx) -> Option<&Self::Context>;
}

#[macro_export]
macro_rules! impl_sub_inst {
    (type Inst::$var:ident($name:ident<$data:ty, $context:ty>)) => {
        pub type $name = crate::vm::bytecode::SubInst<$data, $context>;

        impl crate::vm::bytecode::SubInstGetContext for $data {
            type Context = $context;
            fn get_ctx(inst: &crate::vm::bytecode::InstCtx) -> Option<&Self::Context> {
                match inst {
                    crate::vm::bytecode::InstCtx::$var(sub) => Some(&sub.ctx),
                    _ => None,
                }
            }
        }
    };
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[repr(C)]
pub enum InstCtx {
    Arith(InstArithCtx),
    Bitwise(InstBitwiseCtx),
    Call(InstCallCtx),
    Compare(InstCompareCtx),
    Get(InstGetCtx),
    Set(InstSetCtx),
    Jump(InstJumpCtx),
    Ret(InstRetCtx),
    Load(InstLoadCtx),
    Store(InstStoreCtx),
    Misc(InstMiscCtx),
    Unary(InstUnaryCtx),
}

pub enum Inst {
    Arith(InstArith),
    Bitwise(InstBitwise),
    Call(InstCall),
    Compare(InstCompare),
    Get(InstGet),
    Set(InstSet),
    Jump(InstJump),
    Ret(InstRet),
    Load(InstLoad),
    Store(InstStore),
    Misc(InstMisc),
    Unary(InstUnary),
}

impl FormatInst for Inst {
    fn fmt_inst(&self, f: &mut std::fmt::Formatter<'_>, fun: &super::compiler::Function) -> std::fmt::Result {
        match self {
            Inst::Arith(arith) => arith.fmt_inst(f, fun),
            Inst::Bitwise(bitwise) => bitwise.fmt_inst(f, fun),
            Inst::Call(call) => call.fmt_inst(f, fun),
            Inst::Compare(compare) => compare.fmt_inst(f, fun),
            Inst::Get(get) => get.fmt_inst(f, fun),
            Inst::Set(set) => set.fmt_inst(f, fun),
            Inst::Jump(jump) => jump.fmt_inst(f, fun),
            Inst::Ret(ret) => ret.fmt_inst(f, fun),
            Inst::Load(load) => load.fmt_inst(f, fun),
            Inst::Store(store) => store.fmt_inst(f, fun),
            Inst::Misc(misc) => misc.fmt_inst(f, fun),
            Inst::Unary(unary) => unary.fmt_inst(f, fun),
        }
    }
}

impl InstCtx {
    pub fn strip(&self) -> Inst {
        match self {
            InstCtx::Arith(arith) => Inst::Arith(arith.strip()),
            InstCtx::Bitwise(bitwise) => Inst::Bitwise(bitwise.strip()),
            InstCtx::Call(call) => Inst::Call(call.strip()),
            InstCtx::Compare(compare) => Inst::Compare(compare.strip()),
            InstCtx::Get(get) => Inst::Get(get.strip()),
            InstCtx::Set(set) => Inst::Set(set.strip()),
            InstCtx::Jump(jump) => Inst::Jump(jump.strip()),
            InstCtx::Ret(ret) => Inst::Ret(ret.strip()),
            InstCtx::Load(load) => Inst::Load(load.strip()),
            InstCtx::Store(store) => Inst::Store(store.strip()),
            InstCtx::Misc(misc) => Inst::Misc(misc.strip()),
            InstCtx::Unary(unary) => Inst::Unary(unary.strip()),
        }
    }
}
