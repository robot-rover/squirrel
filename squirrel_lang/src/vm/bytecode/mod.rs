pub mod context;
mod compare;
mod load_store;
mod arith;
mod bitwise;
mod jump_ret;
mod unary;
mod call;
mod get_set;
mod misc;

pub use arith::InstArith;
pub use bitwise::InstBitwise;
pub use call::InstCall;
pub use compare::{InstCompare, run_compare};
pub use get_set::{InstGet, InstSet};
pub use jump_ret::{InstJump, InstRet, JumpKind};
pub use load_store::{InstLoad, InstStore};
pub use misc::InstMisc;
pub use unary::InstUnary;

use serde::{Deserialize, Serialize};
use strum_macros::EnumDiscriminants;

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
    POW,  // acc = reg[data] ^ acc

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
    SETF,  // reg[data][reg[data+1]] = acc
    SETFS, // reg[data][reg[data+1]] = acc (newslot)

    GET,   // acc = env[acc]
    GETC,  // acc = env[const[data]]
    SET,   // env[reg[data]] = acc
    SETC,  // env[const[data]] = acc
    SETS,  // env[reg[data]] = acc (newslot)
    SETCS, // env[const[data]] = acc (newslot)

    ISIN, // acc = reg[data] in acc

    // Runtime State
    THIS, // acc = this
    ROOT, // acc = root
}

pub type Data = u8;

#[derive(Clone, Debug, Serialize, Deserialize)]
#[repr(C)]
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