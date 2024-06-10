use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{context::Span, parser::ast::{Assign, AssignTarget, BinaryOp, Expr, ExprData, Statement, UnaryOp, UnaryRefOp}};

use super::{ArrayRef, Context, Function, FunctionRef, Object, ObjectRef, Value, WeakRef};

fn run(tree: Vec<Statement>) {
    let root = init_root();
    let mut function = Function {
        args: Vec::new(),
        is_varargs: false,
        body: tree,
        root: WeakRef::new(&root),
        env: root,
        locals: HashMap::new(),
    };
    let context = Context::new(&mut function);
}

fn init_root() -> ObjectRef {
    ObjectRef::new(Object {
        delegate: None,
        slots: HashMap::new(),
        is_class_inst: false
    })
}

fn run_statement(context: &mut Context, statement: &Statement) {
    match statement.data {
        _ => todo!()
    }
}

fn run_expression(context: &mut Context, expr: &Expr) -> Value {
    match &expr.data {
        ExprData::Literal(lit) => lit.into(),
        ExprData::TableDecl(table_decl) => run_table(context, table_decl),
        ExprData::ArrayDecl(array_decl) => ArrayRef::new(array_decl.iter().map(|expr| run_expression(context, expr)).collect()).into(),
        ExprData::FunctionDef(func_def) => todo!("How to clone from ast"),
        ExprData::ClassDef { parent, members } => todo!(),
        ExprData::Assign(assign) => run_assign(context, assign),
        ExprData::Ternary { cond, true_expr, false_expr } => {
            if run_expression(context, cond).truthy() {
                run_expression(context, true_expr)
            } else {
                run_expression(context, false_expr)
            }
        },
        ExprData::BinaryOp { op, lhs, rhs } => run_binary_op(context, op, lhs, rhs),
        ExprData::UnaryOp(op, val) => run_unary_op(context, op, val),
        ExprData::UnaryRefOp(op, val) => run_unary_ref_op(context, op, val),
        ExprData::FunctionCall { func, args } => todo!("Todo how to call function"),
        ExprData::ArrayAccess { array, index } => run_array_access(context, array, index),
        ExprData::This => context.infunc.env.clone().into(),
        ExprData::FieldAccess(target, field) => {
            let target = run_expression(context, target);
            match target {
                // TODO: Cloning here!
                Value::Object(obj) => obj.0.borrow().get_field(&Value::String(field.0.clone())),
                _ => panic!("Can't dereference non-object")
            }
        },
        ExprData::Globals => context.infunc.root.0.upgrade().map(|obj_ref| ObjectRef(obj_ref).into()).unwrap_or(Value::Null),
        ExprData::Ident(name) => {
            if let Some(value) = context.infunc.locals.get(name) {
                value.clone()
            }else {
                // TODO: Should look at root table last
                context.infunc.env.0.borrow().get_field(&Value::String(name.clone()))
            }
        },
        ExprData::Base => {
            let env = context.infunc.env.0.borrow();
            if env.is_class_inst {
                env.delegate.as_ref().map(|del| Value::Object(del.clone())).unwrap_or(Value::Null)
            } else {
                Value::Null
            }
        },
    }
}

fn run_unary_ref_op(context: &mut Context, op: &UnaryRefOp, val: &AssignTarget) -> Value {
    todo!()
}

fn run_table(context: &mut Context, table_decl: &[(Expr, Expr)]) -> Value {
    todo!()
}

fn run_array_access(context: &mut Context, array: &Expr, index: &Expr) -> Value {
    todo!()
}

fn run_unary_op(context: &mut Context, op: &UnaryOp, val: &Expr) -> Value {
    todo!()
}

fn run_binary_op(context: &mut Context, op: &BinaryOp, lhs: &Expr, rhs: &Expr) -> Value {
    todo!()
}

fn run_assign(context: &mut Context, assign: &Assign) -> Value {
    todo!()
}