use std::{fmt, ops::{Add, Div, Mul, Rem, Sub}};

use super::{Inst, Reg};
use crate::vm::{bytecode::context::BinaryOpContext, error::ExecResult, runtime::VMState, value::Value};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
enum ArithOp {
    ADD,
    SUB,
    MUL,
    DIV,
    MODU,
    POW,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstArith {
    op: ArithOp,
    reg: Reg,
    ctx: BinaryOpContext,
}

macro_rules! arith_constructor {
    ($name:ident, $op:ident) => {
        pub fn $name(reg: Reg, ctx: BinaryOpContext) -> Self {
            Inst::Arith(InstArith { op: ArithOp::$op, reg, ctx })
        }
    };
}

impl Inst {
    arith_constructor!(add, ADD);
    arith_constructor!(sub, SUB);
    arith_constructor!(mul, MUL);
    arith_constructor!(div, DIV);
    arith_constructor!(modu, MODU);
    arith_constructor!(pow, POW);
}

pub fn run_arith(state: &mut VMState, inst: &InstArith) -> ExecResult {
    let frame = state.frame();
    let lhs = frame.get_reg(inst.reg).clone();
    let rhs = state.take_acc();
    match (lhs, rhs) {
        (lhs @ Value::String(_), rhs) | (lhs, rhs @ Value::String(_)) => {
            state.set_acc(run_concat(lhs, rhs));
        },
        (Value::Integer(lhs), Value::Integer(rhs)) => {
            state.set_acc(run_arith_numeric(lhs, rhs, inst.op));
        },
        (Value::Float(lhs), Value::Integer(rhs)) => {
            state.set_acc(run_arith_numeric(lhs, rhs as f64, inst.op));
        },
        (Value::Integer(lhs), Value::Float(rhs)) => {
            state.set_acc(run_arith_numeric(lhs as f64, rhs, inst.op));
        },
        (Value::Float(lhs), Value::Float(rhs)) => {
            state.set_acc(run_arith_numeric(lhs, rhs, inst.op));
        },
        (Value::Instance(inst), other) => todo!("Metamethods"),
        (Value::Table(inst), other) => todo!("Metamethods"),
        other => todo!("Error handling"),
    }

    Ok(())
}

trait ArithPow {
    fn pow(self, rhs: Self) -> Self;
}

impl ArithPow for i64 {
    fn pow(self, rhs: Self) -> Self {
        self.pow(rhs.try_into().unwrap())
    }
}

impl ArithPow for f64 {
    fn pow(self, rhs: Self) -> Self {
        self.powf(rhs)
    }
}

fn run_concat<L, R>(lhs: L, rhs: R) -> Value
where
    L: fmt::Display,
    R: fmt::Display {
    Value::string(&format!("{}{}", lhs, rhs))
}

fn run_arith_numeric<T>(lhs: T, rhs: T, op: ArithOp) -> Value
where
    T: Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Div<Output = T> + Rem<Output = T> + ArithPow + Into<Value> {
    match op {
        ArithOp::ADD => (lhs + rhs).into(),
        ArithOp::SUB => (lhs - rhs).into(),
        ArithOp::MUL => (lhs * rhs).into(),
        ArithOp::DIV => (lhs / rhs).into(),
        ArithOp::MODU =>(lhs % rhs).into(),
        ArithOp::POW => lhs.pow(rhs).into(),
    }
}