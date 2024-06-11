use std::convert::TryFrom;

use serde::{Deserialize, Serialize};

use crate::context::Span;

use super::error::ParseError;

pub type Ident = (String, Span);

#[derive(Debug, Serialize, Deserialize)]
pub enum Literal {
    Integer(i64),
    Number(f64),
    String(String),
    Null,
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Literal::Integer(lhs), Literal::Integer(rhs)) => lhs == rhs,
            (Literal::Number(lhs), Literal::Number(rhs)) => {
                (lhs.is_nan() && rhs.is_nan()) || (lhs == rhs)
            }
            (Literal::String(lhs), Literal::String(rhs)) => lhs == rhs,
            (Literal::Null, Literal::Null) => true,
            _ => false,
        }
    }
}
impl Eq for Literal {}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Comparison
    Eq,
    Greater,
    Less,
    Compare,
    // TODO: Need GTE, LTE, and NEQ for floating point
    // Logical
    And,
    Or,
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    AShr,
    // Utility
    Comma,
    Dot,
    In,
    InstanceOf,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
    TypeOf,
    Clone,
    Resume,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryRefOp {
    PreIncr,
    PreDecr,
    PostIncr,
    PostDecr,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Function {
    pub keyword_span: Span,
    pub args: Vec<Ident>,
    pub default_expr: Vec<Expr>,
    pub is_varargs: bool,
    pub body: StateRef,
}

pub type StateRef = Box<Statement>;

#[derive(strum_macros::Display, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum StatementData {
    Block(Vec<Statement>),
    Expr(Expr),
    IfElse(Expr, StateRef, StateRef),
    While(Expr, StateRef),
    DoWhile(Expr, StateRef),
    Switch(Expr, Vec<(Expr, Statement)>, Option<StateRef>),
    For {
        init: StateRef,
        cond: Expr,
        incr: StateRef,
        body: StateRef,
    },
    Foreach {
        index_id: Option<Ident>,
        value_id: Ident,
        iterable: Expr,
        body: StateRef,
    },
    Break,
    Continue,
    Return(Expr),
    Yield(Expr),
    LocalDec(Ident, Option<Expr>),
    TryCatch(StateRef, Ident, StateRef),
    Throw(Expr),
    Const(Ident, Literal),
    // TODO Enum
    Empty,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Statement {
    pub data: StatementData,
    pub span: Span,
}

impl Statement {
    pub fn block(statements: Vec<Statement>, start_span: Span, end_span: Span) -> Self {
        let span = start_span
            | end_span
            | statements
                .iter()
                .fold(Span::empty(), |acc, stmt| acc | stmt.span);
        StatementData::Block(statements).spanning(span)
    }

    pub fn expr(expr: Expr) -> Self {
        let span = expr.span;
        StatementData::Expr(expr).spanning(span)
    }

    pub fn if_else(cond: Expr, if_body: Statement, else_body: Statement, if_span: Span) -> Self {
        let span = if_span | if_body.span | else_body.span;
        StatementData::IfElse(cond, Box::new(if_body), Box::new(else_body)).spanning(span)
    }

    pub fn while_loop(cond: Expr, body: Statement, while_span: Span) -> Self {
        let span = while_span | body.span;
        StatementData::While(cond, Box::new(body)).spanning(span)
    }

    pub fn do_while_loop(cond: Expr, body: Statement, do_span: Span, while_span: Span) -> Self {
        let span = do_span | while_span | body.span;
        StatementData::DoWhile(cond, Box::new(body)).spanning(span)
    }

    pub fn switch(
        expr: Expr,
        cases: Vec<(Expr, Statement)>,
        default: Option<Statement>,
        switch_span: Span,
        end_span: Span,
    ) -> Self {
        let span = switch_span | end_span;
        StatementData::Switch(expr, cases, default.map(|val| Box::new(val))).spanning(span)
    }

    pub fn for_loop(
        init: Statement,
        cond: Expr,
        incr: Statement,
        body: Statement,
        for_span: Span,
    ) -> Self {
        let span = for_span | body.span;
        StatementData::For {
            init: Box::new(init),
            cond,
            incr: Box::new(incr),
            body: Box::new(body),
        }
        .spanning(span)
    }

    pub fn foreach(
        index_id: Option<Ident>,
        value_id: Ident,
        iterable: Expr,
        body: Statement,
        for_span: Span,
    ) -> Self {
        let span = for_span | body.span;
        StatementData::Foreach {
            index_id,
            value_id,
            iterable,
            body: Box::new(body),
        }
        .spanning(span)
    }

    pub fn break_stmt(break_span: Span) -> Self {
        StatementData::Break.spanning(break_span)
    }

    pub fn continue_stmt(continue_span: Span) -> Self {
        StatementData::Continue.spanning(continue_span)
    }

    pub fn return_stmt(expr: Expr, return_span: Span) -> Self {
        let span = expr.span | return_span;
        StatementData::Return(expr).spanning(span)
    }

    pub fn yield_stmt(expr: Expr, yield_span: Span) -> Self {
        let span = expr.span | yield_span;
        StatementData::Yield(expr).spanning(span)
    }

    pub fn local_dec(ident: Ident, val: Option<Expr>, local_span: Span) -> Self {
        let mut span = ident.1 | local_span;
        if let Some(val) = &val {
            span = span | val.span;
        }
        StatementData::LocalDec(ident, val).spanning(span)
    }

    pub fn try_catch(
        try_body: Statement,
        catch_id: Ident,
        catch_body: Statement,
        try_span: Span,
    ) -> Self {
        let span = try_span | try_body.span | catch_body.span;
        StatementData::TryCatch(Box::new(try_body), catch_id, Box::new(catch_body)).spanning(span)
    }

    pub fn throw(expr: Expr, throw_span: Span) -> Self {
        let span = expr.span | throw_span;
        StatementData::Throw(expr).spanning(span)
    }

    pub fn const_decl(ident: Ident, value: Literal, val_span: Span) -> Self {
        let span = ident.1 | val_span;
        StatementData::Const(ident, value).spanning(span)
    }

    pub fn empty_stmt() -> Self {
        StatementData::Empty.spanning(Span::empty())
    }
}

impl StatementData {
    pub fn spanning(self, span: Span) -> Statement {
        Statement { data: self, span }
    }
}

impl From<Statement> for StatementData {
    fn from(value: Statement) -> Self {
        value.data
    }
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum AssignTarget {
    Ident(Ident),
    ArrayAccess {
        array: ExprRef,
        index: ExprRef,
        span: Span,
    },
    FieldAccess(ExprRef, Ident),
}

impl AssignTarget {
    pub fn span(&self) -> Span {
        match self {
            AssignTarget::Ident(ident) => ident.1,
            AssignTarget::ArrayAccess { span, .. } => *span,
            AssignTarget::FieldAccess(path, ident) => path.span | ident.1,
        }
    }

    pub fn ident(ident: Ident) -> Self {
        AssignTarget::Ident(ident)
    }

    pub fn array_access(array: ExprRef, index: ExprRef, span: Span) -> Self {
        AssignTarget::ArrayAccess { array, index, span }
    }

    pub fn field_access(path: ExprRef, ident: Ident) -> Self {
        AssignTarget::FieldAccess(path, ident)
    }
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum AssignKind {
    Normal,
    NewSlot,
    Mult,
    Div,
    Add,
    Sub,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Assign {
    pub target: AssignTarget,
    pub value: ExprRef,
    pub kind: AssignKind,
}

impl TryFrom<Expr> for AssignTarget {
    type Error = ParseError;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        let Expr { data, span } = value;
        let target = match data {
            ExprData::Ident(ident) => AssignTarget::ident((ident, span)),
            ExprData::ArrayAccess { array, index } => {
                AssignTarget::array_access(array, index, span)
            }
            ExprData::FieldAccess(path, ident) => AssignTarget::field_access(path, ident),
            _ => return Err(ParseError::invalid_assignment(span, data.to_string())),
        };
        Ok(target)
    }
}

#[derive(strum_macros::Display, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExprData {
    Literal(Literal),
    TableDecl(Vec<(Expr, Expr)>),
    ArrayDecl(Vec<Expr>),
    FunctionDef(Function),
    ClassDef {
        parent: Option<Ident>,
        members: Vec<(Expr, Expr)>,
    },
    Assign(Assign),
    Ternary {
        cond: ExprRef,
        true_expr: ExprRef,
        false_expr: ExprRef,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: ExprRef,
        rhs: ExprRef,
    },
    UnaryOp(UnaryOp, ExprRef),
    UnaryRefOp(UnaryRefOp, AssignTarget),
    FunctionCall {
        func: AssignTarget,
        args: Vec<Expr>,
    },
    ArrayAccess {
        array: ExprRef,
        index: ExprRef,
    },
    This,
    FieldAccess(ExprRef, Ident),
    Globals,
    Ident(String),
    Base,
}

impl ExprData {
    pub fn spanning(self, span: Span) -> Expr {
        Expr { data: self, span }
    }
}

pub type ExprRef = Box<Expr>;
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Expr {
    pub data: ExprData,
    pub span: Span,
}

impl From<Expr> for ExprData {
    fn from(value: Expr) -> Self {
        value.data
    }
}

impl Expr {
    pub fn literal(literal: Literal, span: Span) -> Self {
        ExprData::Literal(literal).spanning(span)
    }

    pub fn table_decl(table: Vec<(Expr, Expr)>, start_span: Span, end_span: Span) -> Self {
        ExprData::TableDecl(table).spanning(start_span | end_span)
    }

    pub fn array_decl(array: Vec<Expr>, span: Span) -> Self {
        ExprData::ArrayDecl(array).spanning(span)
    }

    pub fn function_def(func: Function, fn_keyword_span: Span) -> Self {
        let span = func.body.span | fn_keyword_span;
        ExprData::FunctionDef(func).spanning(span)
    }

    pub fn class_def(
        parent: Option<Ident>,
        members: Vec<(Expr, Expr)>,
        class_keyword_span: Span,
        end_span: Span,
    ) -> Self {
        let span = end_span | class_keyword_span;
        ExprData::ClassDef { parent, members }.spanning(span)
    }

    pub fn assign(target: AssignTarget, value: Expr, kind: AssignKind) -> Self {
        let span = target.span() | value.span;
        let assign = Assign {
            target,
            value: Box::new(value),
            kind,
        };
        ExprData::Assign(assign).spanning(span)
    }

    pub fn ternary(cond: Expr, true_expr: Expr, false_expr: Expr) -> Self {
        let span = cond.span | true_expr.span | false_expr.span;
        ExprData::Ternary {
            cond: Box::new(cond),
            true_expr: Box::new(true_expr),
            false_expr: Box::new(false_expr),
        }
        .spanning(span)
    }

    pub fn binary_op(op: BinaryOp, lhs: Expr, rhs: Expr) -> Self {
        let span = lhs.span | rhs.span;
        ExprData::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
        .spanning(span)
    }

    pub fn unary_op(op: UnaryOp, expr: Expr, op_span: Span) -> Self {
        let span = op_span | expr.span;
        ExprData::UnaryOp(op, Box::new(expr)).spanning(span)
    }

    pub fn unary_ref_op(op: UnaryRefOp, expr: AssignTarget, op_span: Span) -> Self {
        let span = op_span | expr.span();
        ExprData::UnaryRefOp(op, expr).spanning(span)
    }

    pub fn function_call(func: AssignTarget, args: Vec<Expr>, call_span: Span) -> Self {
        ExprData::FunctionCall { func, args }.spanning(call_span)
    }

    pub fn array_access(array: Expr, index: Expr, start_span: Span, end_span: Span) -> Self {
        let span = start_span | end_span;
        ExprData::ArrayAccess {
            array: Box::new(array),
            index: Box::new(index),
        }
        .spanning(span)
    }

    pub fn field_access(path: Expr, ident: Ident) -> Self {
        let span = path.span | ident.1;
        ExprData::FieldAccess(Box::new(path), ident).spanning(span)
    }
}

impl From<Ident> for Expr {
    fn from(value: Ident) -> Self {
        ExprData::Ident(value.0).spanning(value.1)
    }
}

impl From<Expr> for Statement {
    fn from(expr: Expr) -> Self {
        Statement::expr(expr)
    }
}
