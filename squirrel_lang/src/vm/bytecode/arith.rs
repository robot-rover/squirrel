use std::{
    fmt,
    ops::{Add, Div, Mul, Rem, Sub},
};

use super::{Inst, InstCtx, Reg};
use crate::{
    impl_sub_inst,
    vm::{
        bytecode::context::BinaryOpContext,
        compiler::{self, FormatInst},
        error::ExecResult,
        runtime::VMState,
        value::Value,
    },
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Serialize, Deserialize, strum_macros::Display)]
enum ArithOp {
    #[strum(to_string = "add")]
    Add,
    #[strum(to_string = "sub")]
    Sub,
    #[strum(to_string = "mul")]
    Mul,
    #[strum(to_string = "div")]
    Div,
    #[strum(to_string = "modu")]
    Modu,
}

impl ArithOp {
    fn name(&self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Modu => "%",
        }
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct InstArith {
    op: ArithOp,
    reg: Reg,
}

impl FormatInst for InstArith {
    fn fmt_inst(&self, f: &mut fmt::Formatter<'_>, fun: &compiler::Function) -> fmt::Result {
        write!(f, "{:5} {}", self.op, self.reg)
    }
}

impl_sub_inst!(type Inst::Arith(InstArithCtx<InstArith, BinaryOpContext>));

macro_rules! arith_constructor {
    ($name:ident, $op:ident) => {
        pub fn $name(reg: Reg, ctx: BinaryOpContext) -> Self {
            Self::Arith(InstArithCtx {
                data: InstArith {
                    op: ArithOp::$op,
                    reg,
                },
                ctx,
            })
        }
    };
}

impl InstCtx {
    arith_constructor!(add, Add);
    arith_constructor!(sub, Sub);
    arith_constructor!(mul, Mul);
    arith_constructor!(div, Div);
    arith_constructor!(modu, Modu);
}

pub fn run_arith(state: &mut VMState, inst: InstArith) -> ExecResult {
    let frame = state.frame();
    let lhs = frame.get_reg(inst.reg);
    let rhs = state.get_acc();
    match (lhs, rhs) {
        (lhs @ Value::String(_), rhs) | (lhs, rhs @ Value::String(_)) => {
            state.set_acc(run_concat(lhs, rhs));
        }
        (Value::Integer(lhs), Value::Integer(rhs)) => {
            state.set_acc(run_arith_numeric(*lhs, *rhs, inst.op));
        }
        (Value::Float(lhs), Value::Integer(rhs)) => {
            state.set_acc(run_arith_numeric(*lhs, *rhs as f64, inst.op));
        }
        (Value::Integer(lhs), Value::Float(rhs)) => {
            state.set_acc(run_arith_numeric(*lhs as f64, *rhs, inst.op));
        }
        (Value::Float(lhs), Value::Float(rhs)) => {
            state.set_acc(run_arith_numeric(*lhs, *rhs, inst.op));
        }
        (Value::Instance(inst), other) => todo!("Metamethods"),
        (Value::Table(inst), other) => todo!("Metamethods"),
        other => {
            return Err(state.get_context(inst).illegal_operation(
                inst.op.name(),
                other.0.clone(),
                other.1.clone(),
            ))
        }
    }

    Ok(())
}

fn run_concat<L, R>(lhs: L, rhs: R) -> Value
where
    L: fmt::Display,
    R: fmt::Display,
{
    Value::string(&format!("{}{}", lhs, rhs))
}

fn run_arith_numeric<T>(lhs: T, rhs: T, op: ArithOp) -> Value
where
    T: Add<Output = T>
        + Sub<Output = T>
        + Mul<Output = T>
        + Div<Output = T>
        + Rem<Output = T>
        + Into<Value>,
{
    match op {
        ArithOp::Add => (lhs + rhs).into(),
        ArithOp::Sub => (lhs - rhs).into(),
        ArithOp::Mul => (lhs * rhs).into(),
        ArithOp::Div => (lhs / rhs).into(),
        ArithOp::Modu => (lhs % rhs).into(),
    }
}
