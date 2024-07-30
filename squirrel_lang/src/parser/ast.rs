use std::{
    convert::TryFrom,
    fmt::{self, write},
};

use serde::{Deserialize, Serialize};

use crate::{
    context::Span,
    lexer::{FunctionLocals, LocalResolution},
};

use super::error::ParseError;

pub type Ident = (String, Span);

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Literal {
    Integer(i64),
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Number(n) => write!(f, "{:.4}", n),
            Literal::String(s) => write!(f, "{:?}", s),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Null => write!(f, "null"),
        }
    }
}

impl Literal {
    pub fn string(val: impl Into<String>) -> Self {
        Literal::String(val.into())
    }
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
    NotEq,
    Greater,
    GreaterEq,
    Less,
    LessEq,
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
    Delete,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Function {
    pub keyword_span: Span,
    pub arg_span: Span,
    pub num_args: u32,
    pub default_expr: Vec<Expr>,
    pub local_names: Vec<(String, u32)>,
    pub num_locals: u32,
    // Outer local idx, Inner local idx
    pub upvalues: Vec<(u32, u32)>,
    pub is_varargs: bool,
    pub body: StateRef,
}

impl Function {
    pub fn new(
        keyword_span: Span,
        arg_span: Span,
        num_args: u32,
        default_expr: Vec<Expr>,
        is_varargs: bool,
        locals: FunctionLocals,
        body: Statement,
    ) -> Self {
        Function {
            keyword_span,
            arg_span,
            num_args,
            default_expr,
            num_locals: locals.local_count(),
            local_names: locals
                .locals
                .into_iter()
                .map(|(name, idx)| (name.to_string(), idx))
                .collect(),
            upvalues: locals.upvalues,
            is_varargs,
            body: Box::new(body),
        }
    }
}

pub type StateRef = Box<Statement>;

#[derive(strum_macros::Display, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum StatementData {
    Block(Vec<Statement>),
    Expr(Expr),
    IfElse(Expr, StateRef, StateRef),
    While {
        while_kw: Span,
        cond: Expr,
        body: StateRef,
        is_do_while: bool,
    },
    Switch(Expr, Vec<(Expr, Statement)>, Option<StateRef>),
    // TODO: Implement this with while loop in parser
    For {
        init: StateRef,
        cond: Expr,
        incr: StateRef,
        body: StateRef,
    },
    Foreach {
        index_idx: Option<u32>,
        value_idx: u32,
        iterable: Expr,
        body: StateRef,
    },
    Break,
    Continue,
    Return(Expr),
    Yield(Expr),
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
        StatementData::While {
            while_kw: while_span,
            cond,
            body: Box::new(body),
            is_do_while: false,
        }
        .spanning(span)
    }

    pub fn do_while_loop(cond: Expr, body: Statement, do_span: Span, while_span: Span) -> Self {
        let span = do_span | while_span | body.span;
        StatementData::While {
            while_kw: do_span,
            cond,
            body: Box::new(body),
            is_do_while: true,
        }
        .spanning(span)
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
        index_idx: Option<u32>,
        value_idx: u32,
        iterable: Expr,
        body: Statement,
        for_span: Span,
    ) -> Self {
        let span = for_span | body.span;
        StatementData::Foreach {
            index_idx,
            value_idx,
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
    Local(u32, Span),
}

impl AssignTarget {
    pub fn span(&self) -> Span {
        match self {
            AssignTarget::Ident(ident) => ident.1,
            AssignTarget::ArrayAccess { span, .. } => *span,
            AssignTarget::FieldAccess(path, ident) => path.span | ident.1,
            AssignTarget::Local(_, span) => *span,
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

    pub fn local(id: u32, span: Span) -> Self {
        AssignTarget::Local(id, span)
    }
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
            ExprData::Local(id, span) => AssignTarget::local(id, span),
            _ => return Err(ParseError::invalid_assignment(span, data.to_string())),
        };
        Ok(target)
    }
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum CallTarget {
    Expr(ExprRef),
    FieldAccess(ExprRef, Ident),
}

impl From<Expr> for CallTarget {
    fn from(value: Expr) -> Self {
        match value.data {
            ExprData::FieldAccess(path, ident) => CallTarget::FieldAccess(path, ident),
            _ => CallTarget::Expr(Box::new(value)),
        }
    }
}

impl CallTarget {
    pub fn span(&self) -> Span {
        match self {
            CallTarget::Expr(expr) => expr.span,
            CallTarget::FieldAccess(path, ident) => path.span | ident.1,
        }
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
    pub op_span: Span,
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
        op_span: Span,
        lhs: ExprRef,
        rhs: ExprRef,
    },
    UnaryOp(UnaryOp, Span, ExprRef),
    UnaryRefOp(UnaryRefOp, Span, AssignTarget),
    FunctionCall {
        func: CallTarget,
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
    RawCall {
        func: ExprRef,
        this: ExprRef,
        args: Vec<Expr>,
    },
    Local(u32, Span),
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

    pub fn assign(target: AssignTarget, op_span: Span, value: Expr, kind: AssignKind) -> Self {
        let span = target.span() | value.span;
        let assign = Assign {
            target,
            op_span,
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

    pub fn binary_op(op: BinaryOp, op_span: Span, lhs: Expr, rhs: Expr) -> Self {
        let span = lhs.span | rhs.span;
        ExprData::BinaryOp {
            op,
            op_span,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
        .spanning(span)
    }

    pub fn unary_op(op: UnaryOp, expr: Expr, op_span: Span) -> Self {
        let span = op_span | expr.span;
        ExprData::UnaryOp(op, op_span, Box::new(expr)).spanning(span)
    }

    pub fn unary_ref_op(op: UnaryRefOp, expr: AssignTarget, op_span: Span) -> Self {
        let span = op_span | expr.span();
        ExprData::UnaryRefOp(op, op_span, expr).spanning(span)
    }

    pub fn function_call(func: Expr, args: Vec<Expr>, call_span: Span) -> Self {
        ExprData::FunctionCall {
            func: func.into(),
            args,
        }
        .spanning(call_span)
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

    pub(crate) fn raw_call(
        func: Expr,
        this: Expr,
        parameters: Vec<Expr>,
        rawcall_span: Span,
        end_span: Span,
    ) -> Self {
        ExprData::RawCall {
            func: Box::new(func),
            this: Box::new(this),
            args: parameters,
        }
        .spanning(rawcall_span | end_span)
    }

    pub fn ident<S: Into<String>>(ident: (S, Span)) -> Self {
        ExprData::Ident(ident.0.into()).spanning(ident.1)
    }

    pub fn local(id: u32, span: Span) -> Self {
        ExprData::Local(id, span).spanning(span)
    }
}

impl From<Expr> for Statement {
    fn from(expr: Expr) -> Self {
        Statement::expr(expr)
    }
}
