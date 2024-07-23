use num_traits::ConstZero;

use crate::vm::{bytecode::{Data, DcCompare}, error::ExecResult, value::Value};
use crate::vm::runtime::VMState;

pub fn run_compare(state: &mut VMState, c: DcCompare, data: Data) -> ExecResult {
    let frame = state.frame();
    let lhs = frame.registers[data as usize].clone();
    let rhs = state.take_acc();
    state.acc = match (lhs, rhs) {
        (Value::Integer(lhs), Value::Integer(rhs)) => compare_zero(c, lhs - rhs),
        (Value::Float(lhs), Value::Integer(rhs)) => compare_zero(c, lhs - rhs as f64),
        (Value::Integer(lhs), Value::Float(rhs)) => compare_zero(c, lhs as f64 - rhs),
        (Value::Float(lhs), Value::Float(rhs)) => compare_zero(c, lhs - rhs),
        (Value::Instance(inst), other) => todo!("Metamethods"),
        (Value::Table(inst), other) => todo!("Metamethods"),
        other => todo!("Error handling"),
    };

    Ok(())
}

fn compare_zero<T: ConstZero + PartialOrd + Into<Value>>(cmp: DcCompare, diff: T) -> Value {
    let zero = T::zero();
    Value::Boolean(match cmp {
        DcCompare::ISLT => diff < zero,
        DcCompare::ISLE => diff <= zero,
        DcCompare::ISGT => diff > zero,
        DcCompare::ISGE => diff >= zero,
        DcCompare::ISEQ => diff == zero,
        DcCompare::ISNE => diff != zero,
        DcCompare::CMP => return diff.into(),
    })
}