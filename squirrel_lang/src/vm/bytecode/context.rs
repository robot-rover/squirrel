use serde::{Deserialize, Serialize};

use crate::context::Span;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum InstContext {
    General(Span),
    GetIdent(GetIdentContext),
    GetField(GetFieldContext),
    FnCall(FnCallContext),
    NewClass(NewClassContext),
    BinaryOp(BinaryOpContext),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GetIdentContext {
    pub ident_span: Span,
    pub ident_name: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SetIdentContext {
    pub ident_span: Span,
    pub ident_name: String,
    pub assignment_span: Span,
    pub value_span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GetFieldContext {
    pub parent_span: Span,
    pub field_span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SetFieldContext {
    pub parent_span: Span,
    pub field_span: Span,
    pub assignment_span: Span,
    pub value_span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FnCallContext {
    pub fn_span: Span,
    pub call_span: Span,
    pub args: Vec<Span>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NewClassContext {
    pub class_kw_span: Span,
    pub class_name_span: Span,
    pub class_name: String,
    pub class_body_span: Span,
    pub extends_kw_ty_span: Option<[Span; 2]>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BinaryOpContext {
    pub lhs_span: Span,
    pub op_span: Span,
    pub op_name: String,
    pub rhs_span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct UnaryOpContext {
    pub op_span: Span,
    pub op_name: String,
    pub rhs_span: Span,
}
