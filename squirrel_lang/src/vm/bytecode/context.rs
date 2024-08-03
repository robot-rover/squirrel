use std::{fmt::Binary, ops::Range};

use serde::{Deserialize, Serialize};

use crate::{
    context::Span,
    vm::{
        error::{CallInfo, ExecError},
        value::Value,
    },
};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GetIdentContext {
    pub ident_span: Span,
}

impl GetIdentContext {
    pub fn undefined_variable(&self) -> ExecError {
        ExecError::UndefinedVariable(self.ident_span)
    }

    pub fn get_span(&self) -> Span {
        self.ident_span
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SetIdentContext {
    pub ident_span: Span,
    pub assignment_span: Span,
    pub value_span: Span,
}

impl SetIdentContext {
    pub fn undefined_variable(&self) -> ExecError {
        ExecError::UndefinedVariable(self.ident_span)
    }

    pub fn cannot_modify_type(&self, this: Value) -> ExecError {
        ExecError::CannotModifyType {
            span: self.ident_span,
            this,
        }
    }

    pub fn get_span(&self) -> Span {
        self.ident_span | self.value_span
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GetFieldContext {
    pub parent_span: Span,
    pub field_span: Span,
}

impl GetFieldContext {
    pub fn undefined_field(&self, field: Value) -> ExecError {
        ExecError::UndefinedField {
            parent_span: self.parent_span,
            field_span: self.field_span,
            field,
        }
    }

    pub fn unhashable_type(&self, field: Value) -> ExecError {
        ExecError::UnhashableType {
            value: field,
            span: self.field_span,
        }
    }

    pub fn index_out_of_bounds(&self, index: i64, len: usize) -> ExecError {
        ExecError::IndexOutOfBounds {
            index,
            len,
            span: self.field_span,
        }
    }

    pub fn get_span(&self) -> Span {
        self.parent_span | self.field_span
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SetFieldContext {
    pub parent_span: Span,
    pub field_span: Span,
    pub assignment_span: Span,
    pub value_span: Span,
}

impl SetFieldContext {
    pub fn undefined_field(&self, field: Value) -> ExecError {
        ExecError::UndefinedField {
            parent_span: self.parent_span,
            field_span: self.field_span,
            field,
        }
    }

    pub fn cannot_modify_type(&self, this: Value) -> ExecError {
        ExecError::CannotModifyType {
            span: self.parent_span,
            this,
        }
    }

    pub fn unhashable_type(&self, field: Value) -> ExecError {
        ExecError::UnhashableType {
            value: field,
            span: self.field_span,
        }
    }

    pub fn index_out_of_bounds(&self, index: i64, len: usize) -> ExecError {
        ExecError::IndexOutOfBounds {
            index,
            len,
            span: self.field_span,
        }
    }

    pub fn wrong_index_type(&self, expected: &'static str, got: Value) -> ExecError {
        ExecError::WrongIndexType {
            span: self.field_span,
            expected,
            got,
        }
    }

    pub fn mutating_instantiated_class(&self) -> ExecError {
        ExecError::MutatingInstantiatedClass {
            class_span: self.parent_span,
            assign_span: self.assignment_span,
        }
    }

    pub fn get_span(&self) -> Span {
        self.parent_span | self.value_span
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FnCallContext {
    pub fn_span: Span,
    pub call_span: Span,
    pub args: Vec<Span>,
}

impl FnCallContext {
    fn make_call_info(&self) -> CallInfo {
        CallInfo {
            func_span: self.fn_span.clone(),
            call_span: self.call_span.clone(),
            arg_spans: self.args.clone(),
        }
    }

    pub fn uncallable_type(&self, not_fn: Value) -> ExecError {
        ExecError::UncallableType {
            call_info: self.make_call_info(),
            not_fn,
        }
    }

    pub fn wrong_arg_count(&self, expected: Range<usize>, def_span: Option<Span>) -> ExecError {
        ExecError::WrongArgCount {
            call_info: self.make_call_info(),
            expected,
            def_span,
        }
    }

    pub fn wrong_arg_type(
        &self,
        arg_index: usize,
        expected: &'static str,
        got: Value,
    ) -> ExecError {
        ExecError::WrongArgType {
            call_info: self.make_call_info(),
            arg_index,
            expected,
            got,
        }
    }

    pub fn get_span(&self) -> Span {
        self.fn_span | self.call_span
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NewClassContext {
    pub class_kw_span: Span,
    pub class_name_span: Span,
    pub class_name: String,
    pub class_body_span: Span,
    pub extends_kw_ty_span: Option<[Span; 2]>,
}

impl NewClassContext {
    pub fn extending_non_class(&self, non_class: Value) -> ExecError {
        ExecError::ExtendingNonClass {
            span: self
                .extends_kw_ty_span
                .expect("Class doesn't extend anything")[1],
            non_class,
        }
    }

    pub fn get_span(&self) -> Span {
        self.class_kw_span | self.class_body_span
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BinaryOpContext {
    pub lhs_span: Span,
    pub op_span: Span,
    pub rhs_span: Span,
}

impl BinaryOpContext {
    pub fn illegal_operation(
        &self,
        op_name: &'static str,
        lhs_val: Value,
        rhs_val: Value,
    ) -> ExecError {
        ExecError::IllegalOperation {
            op_name,
            op_span: self.op_span,
            lhs: (lhs_val, self.lhs_span),
            rhs: Some((rhs_val, self.rhs_span)),
        }
    }

    pub fn missing_metamethod(&self, op_name: &'static str, mm_name: &'static str) -> ExecError {
        ExecError::MissingMetamethod {
            obj_span: self.lhs_span,
            op_span: self.op_span,
            op_name,
            mm_name,
        }
    }

    pub fn wrong_metamethod_return_type(
        &self,
        mm_name: &'static str,
        expected: &'static str,
        got: Value,
    ) -> ExecError {
        ExecError::WrongMetamethodReturnType {
            obj_span: self.lhs_span,
            op_span: self.op_span,
            mm_name,
            expected,
            got,
        }
    }

    pub fn get_span(&self) -> Span {
        self.lhs_span | self.rhs_span
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct UnaryOpContext {
    pub op_span: Span,
    pub val_span: Span,
}

impl UnaryOpContext {
    pub fn illegal_operation(&self, op_name: &'static str, val: Value) -> ExecError {
        ExecError::IllegalOperation {
            op_name,
            op_span: self.op_span,
            lhs: (val, self.val_span),
            rhs: None,
        }
    }

    pub fn get_span(&self) -> Span {
        self.op_span | self.val_span
    }
}
