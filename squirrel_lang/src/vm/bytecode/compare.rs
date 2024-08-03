use num_traits::ConstZero;
use serde::{Deserialize, Serialize};

use crate::vm::{
    compiler::{self, FormatInst},
    error::ExecResult,
    value::Value,
};
use crate::{
    impl_sub_inst,
    vm::{bytecode::context::BinaryOpContext, runtime::VMState},
};

use super::{Inst, InstCtx, Reg};

#[derive(Clone, Copy, Debug, Serialize, Deserialize, strum_macros::Display)]
enum CompareKind {
    #[strum(to_string = "islt")]
    IsLt,
    #[strum(to_string = "isle")]
    IsLe,
    #[strum(to_string = "isgt")]
    IsGt,
    #[strum(to_string = "isge")]
    IsGe,
    #[strum(to_string = "iseq")]
    IsEq,
    #[strum(to_string = "isne")]
    IsNe,
    #[strum(to_string = "cmp")]
    Cmp,
}

impl CompareKind {
    fn op_name(&self) -> &'static str {
        match self {
            Self::IsLt => "<",
            Self::IsLe => "<=",
            Self::IsGt => ">",
            Self::IsGe => ">=",
            Self::IsEq => "==",
            Self::IsNe => "!=",
            Self::Cmp => "<=>",
        }
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct InstCompare {
    reg: Reg,
    kind: CompareKind,
}

impl FormatInst for InstCompare {
    fn fmt_inst(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        fun: &compiler::Function,
    ) -> std::fmt::Result {
        write!(f, "{:5} {}", self.kind, self.reg)
    }
}

impl_sub_inst!(type Inst::Compare(InstCompareCtx<InstCompare, BinaryOpContext>));

macro_rules! compare_constructor {
    ($name:ident, $kind:ident) => {
        pub fn $name(reg: Reg, ctx: BinaryOpContext) -> Self {
            Self::Compare(InstCompareCtx {
                data: InstCompare {
                    reg,
                    kind: CompareKind::$kind,
                },
                ctx,
            })
        }
    };
}

impl InstCtx {
    compare_constructor!(islt, IsLt);
    compare_constructor!(isle, IsLe);
    compare_constructor!(isgt, IsGt);
    compare_constructor!(isge, IsGe);
    compare_constructor!(iseq, IsEq);
    compare_constructor!(isne, IsNe);
    compare_constructor!(cmp, Cmp);
}

pub fn run_compare(state: &mut VMState, inst: InstCompare) -> ExecResult {
    let frame = state.frame();
    let lhs = frame.get_reg(inst.reg).clone();
    let rhs = state.take_acc();
    state.set_acc(match (lhs, rhs) {
        (Value::Integer(lhs), Value::Integer(rhs)) => compare_zero(inst.kind, lhs - rhs),
        (Value::Float(lhs), Value::Integer(rhs)) => compare_zero(inst.kind, lhs - rhs as f64),
        (Value::Integer(lhs), Value::Float(rhs)) => compare_zero(inst.kind, lhs as f64 - rhs),
        (Value::Float(lhs), Value::Float(rhs)) => compare_zero(inst.kind, lhs - rhs),
        (Value::Instance(inst), other) => todo!("Metamethods"),
        (Value::Table(inst), other) => todo!("Metamethods"),
        other => {
            return Err(state.get_context(inst).illegal_operation(
                inst.kind.op_name(),
                other.0,
                other.1,
            ))
        }
    });

    Ok(())
}

fn compare_zero<T: ConstZero + PartialOrd + Into<Value>>(cmp: CompareKind, diff: T) -> Value {
    let zero = T::zero();
    Value::Boolean(match cmp {
        CompareKind::IsLt => diff < zero,
        CompareKind::IsLe => diff <= zero,
        CompareKind::IsGt => diff > zero,
        CompareKind::IsGe => diff >= zero,
        CompareKind::IsEq => diff == zero,
        CompareKind::IsNe => diff != zero,
        CompareKind::Cmp => return diff.into(),
    })
}
