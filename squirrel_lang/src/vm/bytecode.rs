use serde::{Deserialize, Serialize};

use super::compiler::{Block, Const, FunIdx, Reg};

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

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
#[repr(C)]
pub struct Inst {
    pub tag: Tag,
    pub data: Data,
}

macro_rules! inst_constructor {
    ($name:ident, $variant:ident, $arg:ident: $wrapper:ty) => {
        pub fn $name($arg: $wrapper) -> Inst {
            let data = u8::try_from(u32::from($arg)).unwrap();
            Inst { tag: Tag::$variant, data }
        }
    };
    ($name:ident, $variant:ident) => {
        inst_constructor!($name, $variant, reg_idx: Reg);
    };
    ($name:ident, $variant:ident, !) => {
        pub fn $name() -> Inst {
            Inst { tag: Tag::$variant, data: 0 }
        }
    };
}

impl Inst {
    inst_constructor!(islt, ISLT);
    inst_constructor!(isle, ISLE);
    inst_constructor!(isgt, ISGT);
    inst_constructor!(isge, ISGE);
    inst_constructor!(iseq, ISEQ);
    inst_constructor!(isne, ISNE);
    inst_constructor!(cmp, CMP);

    inst_constructor!(loadr, LOADR);
    inst_constructor!(storr, STORR);
    inst_constructor!(loadl, LOADL, local_idx: u32);
    inst_constructor!(storl, STORL, local_idx: u32);

    inst_constructor!(loadc, LOADC, const_idx: Const);
    inst_constructor!(loadf, LOADF, fun_idx: FunIdx);
    pub fn loadp(prim_ty: u32) -> Inst {
        let data = if prim_ty < 3 {
            prim_ty as u8
        } else {
            panic!("Invalid primitive type: {}", prim_ty);
        };
        Inst {
            tag: Tag::LOADP,
            data,
        }
    }
    pub fn loadi(imm: u32) -> Inst {
        let data = if imm <= u8::MAX as u32 {
            imm as u8
        } else {
            panic!("Immediate value too large: {}", imm);
        };
        Inst {
            tag: Tag::LOADI,
            data,
        }
    }
    pub fn loads(imm: i32) -> Inst {
        let data = if imm <= i8::MAX as i32 && imm >= i8::MIN as i32 {
            imm as u8
        } else {
            panic!("Immediate value too large: {}", imm);
        };
        Inst {
            tag: Tag::LOADS,
            data,
        }
    }

    inst_constructor!(neg, NEG, !);
    inst_constructor!(add, ADD);
    inst_constructor!(sub, SUB);
    inst_constructor!(mul, MUL);
    inst_constructor!(div, DIV);
    inst_constructor!(modu, MODU);
    inst_constructor!(pow, POW);

    inst_constructor!(lnot, LNOT, !);

    inst_constructor!(bnot, BNOT, !);
    inst_constructor!(band, BAND);
    inst_constructor!(bor, BOR);
    inst_constructor!(bxor, BXOR);
    inst_constructor!(brsh, BRSH);
    inst_constructor!(bash, BASH);

    inst_constructor!(jmp, JMP, block_idx: &Block);
    inst_constructor!(jt, JT, block_idx: &Block);
    inst_constructor!(jf, JF, block_idx: &Block);
    inst_constructor!(ret, RET, !);
    inst_constructor!(retn, RETN, !);

    pub fn call(reg_idx: Reg, n_args: u32) -> (Inst, Option<Inst>) {
        let data1 = u8::try_from(u32::from(reg_idx)).unwrap();
        let data2 = u8::try_from(n_args).unwrap();
        match n_args {
            0 => (
                Inst {
                    tag: Tag::CALL0,
                    data: data1,
                },
                None,
            ),
            1 => (
                Inst {
                    tag: Tag::CALL1,
                    data: data1,
                },
                None,
            ),
            2 => (
                Inst {
                    tag: Tag::CALL2,
                    data: data1,
                },
                None,
            ),
            3 => (
                Inst {
                    tag: Tag::CALL3,
                    data: data1,
                },
                None,
            ),
            4 => (
                Inst {
                    tag: Tag::CALL4,
                    data: data1,
                },
                None,
            ),
            5 => (
                Inst {
                    tag: Tag::CALL5,
                    data: data1,
                },
                None,
            ),
            6 => (
                Inst {
                    tag: Tag::CALL6,
                    data: data1,
                },
                None,
            ),
            other => (
                Inst {
                    tag: Tag::CALLN,
                    data: data2,
                },
                Some(Inst {
                    tag: Tag::CALLN,
                    data: data1,
                }),
            ),
        }
    }

    inst_constructor!(getf, GETF);
    inst_constructor!(getfc, GETFC, const_idx: Const);
    pub fn setf(reg_idx: Reg, is_ns: bool) -> Inst {
        let data = u8::try_from(u32::from(reg_idx)).unwrap();
        Inst {
            tag: if is_ns { Tag::SETFS } else { Tag::SETF },
            data,
        }
    }

    inst_constructor!(get, GET, !);
    inst_constructor!(getc, GETC, const_idx: Const);
    pub fn set(reg_idx: Reg, is_ns: bool) -> Inst {
        let data = u8::try_from(u32::from(reg_idx)).unwrap();
        Inst {
            tag: if is_ns { Tag::SETS } else { Tag::SET },
            data,
        }
    }
    pub fn setc(const_idx: Const, is_ns: bool) -> Inst {
        let data = u8::try_from(u32::from(const_idx)).unwrap();
        Inst {
            tag: if is_ns { Tag::SETCS } else { Tag::SETC },
            data,
        }
    }
    inst_constructor!(isin, ISIN);

    inst_constructor!(this, THIS, !);
    inst_constructor!(root, ROOT, !);
}

#[derive(Clone, Copy, Debug)]
pub enum DcCompare {
    ISLT,
    ISLE,
    ISGT,
    ISGE,
    ISEQ,
    ISNE,
    CMP,
}

#[derive(Clone, Copy, Debug)]
pub enum DcLoad {
    LOADR,
    LOADL,
    LOADC,
    LOADF,
    LOADP,
    LOADI,
    LOADS,
}

#[derive(Clone, Copy, Debug)]
pub enum DcStore {
    STORR,
    STORL,
}

#[derive(Clone, Copy, Debug)]
pub enum DcUnary {
    NEG,
    LNOT,
    BNOT,
}

#[derive(Clone, Copy, Debug)]
pub enum DcArith {
    ADD,
    SUB,
    MUL,
    DIV,
    MODU,
    POW,
}

#[derive(Clone, Copy, Debug)]
pub enum DcBitwise {
    BAND,
    BOR,
    BXOR,
    BRSH,
    BASH,
}

#[derive(Clone, Copy, Debug)]
pub enum DcJump {
    JMP,
    JT,
    JF,
}

#[derive(Clone, Copy, Debug)]
pub struct DcReturn {
    pub ret_null: bool,
}

#[derive(Clone, Copy, Debug)]
pub struct DcCall {
    pub arg_count: Option<u8>,
}

#[derive(Clone, Copy, Debug)]
pub enum DcField {
    GETF,
    GETFC,
    SETF,
    SETFS,

    GET,
    GETC,
    SET,
    SETC,
    SETS,
    SETCS,

    ISIN,
}

#[derive(Clone, Copy, Debug)]
pub enum DcRuntime {
    THIS,
    ROOT,
}

#[derive(Clone, Copy, Debug)]
pub enum DcTag {
    Compare(DcCompare),
    Load(DcLoad),
    Store(DcStore),
    Unary(DcUnary),
    Arith(DcArith),
    Bitwise(DcBitwise),
    Jump(DcJump),
    Return(DcReturn),
    Call(DcCall),
    Field(DcField),
    Runtime(DcRuntime),
}

pub fn decode(tag: Tag) -> DcTag {
    match tag {
        Tag::ISLT => DcTag::Compare(DcCompare::ISLT),
        Tag::ISLE => DcTag::Compare(DcCompare::ISLE),
        Tag::ISGT => DcTag::Compare(DcCompare::ISGT),
        Tag::ISGE => DcTag::Compare(DcCompare::ISGE),
        Tag::ISEQ => DcTag::Compare(DcCompare::ISEQ),
        Tag::ISNE => DcTag::Compare(DcCompare::ISNE),
        Tag::CMP => DcTag::Compare(DcCompare::CMP),
        Tag::LOADR => DcTag::Load(DcLoad::LOADR),
        Tag::STORR => DcTag::Store(DcStore::STORR),
        Tag::LOADL => DcTag::Load(DcLoad::LOADL),
        Tag::STORL => DcTag::Store(DcStore::STORL),
        Tag::LOADC => DcTag::Load(DcLoad::LOADC),
        Tag::LOADF => DcTag::Load(DcLoad::LOADF),
        Tag::LOADP => DcTag::Load(DcLoad::LOADP),
        Tag::LOADI => DcTag::Load(DcLoad::LOADI),
        Tag::LOADS => DcTag::Load(DcLoad::LOADS),
        Tag::NEG => DcTag::Unary(DcUnary::NEG),
        Tag::ADD => DcTag::Arith(DcArith::ADD),
        Tag::SUB => DcTag::Arith(DcArith::SUB),
        Tag::MUL => DcTag::Arith(DcArith::MUL),
        Tag::DIV => DcTag::Arith(DcArith::DIV),
        Tag::MODU => DcTag::Arith(DcArith::MODU),
        Tag::POW => DcTag::Arith(DcArith::POW),
        Tag::LNOT => DcTag::Unary(DcUnary::LNOT),
        Tag::BNOT => DcTag::Unary(DcUnary::BNOT),
        Tag::BAND => DcTag::Bitwise(DcBitwise::BAND),
        Tag::BOR => DcTag::Bitwise(DcBitwise::BOR),
        Tag::BXOR => DcTag::Bitwise(DcBitwise::BXOR),
        Tag::BRSH => DcTag::Bitwise(DcBitwise::BRSH),
        Tag::BASH => DcTag::Bitwise(DcBitwise::BASH),
        Tag::JMP => DcTag::Jump(DcJump::JMP),
        Tag::JT => DcTag::Jump(DcJump::JT),
        Tag::JF => DcTag::Jump(DcJump::JF),
        Tag::RET => DcTag::Return(DcReturn { ret_null: false }),
        Tag::RETN => DcTag::Return(DcReturn { ret_null: true }),
        Tag::CALL0 => DcTag::Call(DcCall { arg_count: Some(0) }),
        Tag::CALL1 => DcTag::Call(DcCall { arg_count: Some(1) }),
        Tag::CALL2 => DcTag::Call(DcCall { arg_count: Some(2) }),
        Tag::CALL3 => DcTag::Call(DcCall { arg_count: Some(3) }),
        Tag::CALL4 => DcTag::Call(DcCall { arg_count: Some(4) }),
        Tag::CALL5 => DcTag::Call(DcCall { arg_count: Some(5) }),
        Tag::CALL6 => DcTag::Call(DcCall { arg_count: Some(6) }),
        Tag::CALLN => DcTag::Call(DcCall { arg_count: None }),
        Tag::GETF => DcTag::Field(DcField::GETF),
        Tag::GETFC => DcTag::Field(DcField::GETFC),
        Tag::SETF => DcTag::Field(DcField::SETF),
        Tag::SETFS => DcTag::Field(DcField::SETFS),
        Tag::GET => DcTag::Field(DcField::GET),
        Tag::GETC => DcTag::Field(DcField::GETC),
        Tag::SET => DcTag::Field(DcField::SET),
        Tag::SETC => DcTag::Field(DcField::SETC),
        Tag::SETS => DcTag::Field(DcField::SETS),
        Tag::SETCS => DcTag::Field(DcField::SETCS),
        Tag::ISIN => DcTag::Field(DcField::ISIN),
        Tag::THIS => DcTag::Runtime(DcRuntime::THIS),
        Tag::ROOT => DcTag::Runtime(DcRuntime::ROOT),
    }
}
