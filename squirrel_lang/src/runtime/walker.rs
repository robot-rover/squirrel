use std::{collections::HashMap, f32::consts::E};

use crate::{
    context::Span,
    parser::ast::{self, Assign, AssignKind, AssignTarget, BinaryOp, Expr, ExprData, Ident, Statement, StatementData, UnaryOp, UnaryRefOp},
};

use super::{
    ArrayRef, Closure, ClosureRef, Context, ExecError, FuncRuntime, Object, ObjectRef, VMState, Value, WeakRef
};

type ExprResult = Result<Value, ExecError>;
enum FlowControl {
    Return(Span, Value),
    Yield(Span, Value),
    Break(Span),
    Continue(Span),
    Error(ExecError),
}
type FlowResult = Result<(), FlowControl>;

impl From<ExecError> for FlowControl {
    fn from(err: ExecError) -> Self {
        FlowControl::Error(err)
    }
}

fn run(tree: &ast::Function) -> ExprResult {
    let root = init_root();
    let root_closure = ClosureRef::new(tree, Vec::new(), WeakRef::new(&root), WeakRef::new(&root));
    let infunc = FuncRuntime::new(root_closure, Vec::new());
    let mut vm_state = VMState { root_table: root };
    let mut context = Context { infunc, vm_state: &mut vm_state };

    run_function(&mut context, &tree.body)
}

fn init_root() -> ObjectRef {
    ObjectRef::new(Object {
        delegate: None,
        slots: HashMap::new(),
        is_class_inst: false,
    })
}

fn run_statement(context: &mut Context, statement: &Statement) -> FlowResult {
    match &statement.data {
        StatementData::Block(stmts) => {
            for statement in stmts {
                run_statement(context, statement)?;
            }
        },
        StatementData::Expr(expr) => { run_expression(context, expr)?; },
        StatementData::IfElse(cond, if_true, if_false) => {
            if run_expression(context, cond)?.truthy() {
                run_statement(context, if_true)?;
            } else {
                run_statement(context, if_false)?;
            }
        },
        StatementData::While(cond, body) => {
            while run_expression(context, cond)?.truthy() {
                match run_statement(context, body) {
                    Ok(()) => {}
                    Err(FlowControl::Break(_span)) => break,
                    Err(FlowControl::Continue(_span)) => {},
                    Err(other) => return Err(other),
                }
            }
        },
        StatementData::DoWhile(cond, body) => {
            loop {
                match run_statement(context, body) {
                    Ok(()) => {}
                    Err(FlowControl::Break(_span)) => break,
                    Err(FlowControl::Continue(_span)) => {},
                    Err(other) => return Err(other),
                }
                if !run_expression(context, cond)?.truthy() {
                    break;
                }
            }
        },
        StatementData::Switch(val, cases, default) => run_case(context, val, cases, default.as_ref().map(|b| &**b))?,
        StatementData::For { init, cond, incr, body } => todo!(),
        StatementData::Foreach { index_id, value_id, iterable, body } => todo!(),
        StatementData::Break => return Err(FlowControl::Break(statement.span)),
        StatementData::Continue => return Err(FlowControl::Continue(statement.span)),
        StatementData::Return(val) => return Err(FlowControl::Return(statement.span, run_expression(context, val)?)),
        StatementData::Yield(val) => todo!("Yield not implemented"),
        StatementData::LocalDec(ident, val) => {
            let val = run_expression(context, val)?;
            context.infunc.locals.insert(ident.0.clone(), val);
        },
        StatementData::TryCatch(_, _, _) => todo!("Try Catch not implemented"),
        StatementData::Throw(_) => todo!("Throw not implemented"),
        StatementData::Const(_, _) => todo!("Const not implemented"),
        StatementData::Empty => {},
    }
    Ok(())
}

fn run_case(context: &mut Context, val: &Expr, cases: &[(Expr, Statement)], default: Option<&Statement>) -> FlowResult {
    todo!()
}

fn run_expression(context: &mut Context, expr: &Expr) -> ExprResult {
    match &expr.data {
        ExprData::Literal(lit) => Ok(lit.into()),
        ExprData::TableDecl(table_decl) => run_table(context, table_decl),
        ExprData::ArrayDecl(array_decl) => Ok(ArrayRef::new(
            array_decl
                .iter()
                .map(|expr| run_expression(context, expr))
                .collect::<Result<Vec<_>, _>>()?,
        )
        .into()),
        ExprData::FunctionDef(func_def) => Ok(ClosureRef::new(
            func_def,
            func_def
                .default_expr
                .iter()
                .map(|expr| run_expression(context, expr))
                .collect::<Result<Vec<_>, _>>()?,
            context.infunc.closure.0.env.clone(),
            WeakRef::new(&context.vm_state.root_table),
        )
        .into()),
        ExprData::ClassDef { parent, members } => run_class(context, parent.as_ref(), members),
        ExprData::Assign(assign) => {
            let val = run_expression(context, &assign.value)?;
            let is_newslot = match &assign.kind {
                AssignKind::Normal => false,
                AssignKind::NewSlot => true,
                AssignKind::Mult => todo!(),
                AssignKind::Div => todo!(),
                AssignKind::Add => todo!(),
                AssignKind::Sub => todo!(),
            };
            run_assign(context, &assign.target, val.clone(), is_newslot)?;
            Ok(val)
        },
        ExprData::Ternary {
            cond,
            true_expr,
            false_expr,
        } => {
            if run_expression(context, cond)?.truthy() {
                run_expression(context, true_expr)
            } else {
                run_expression(context, false_expr)
            }
        }
        ExprData::BinaryOp { op, lhs, rhs } => run_binary_op(context, op, lhs, rhs),
        ExprData::UnaryOp(op, val) => run_unary_op(context, op, val),
        ExprData::UnaryRefOp(op, val) => run_unary_ref_op(context, op, val),
        ExprData::FunctionCall { func, args } => run_function_call(context, func, args),
        ExprData::ArrayAccess { array, index } => run_array_access(context, array, index),
        ExprData::This => Ok(context.infunc.closure.0.env.clone().into()),
        ExprData::FieldAccess(target, field) => {
            let target = run_expression(context, target)?;
            run_field_access(context, &target, &field.0, expr.span)
        }
        ExprData::Globals => Ok(context
            .infunc
            .closure
            .0
            .root
            .0
            .upgrade()
            .map(|obj_ref| ObjectRef(obj_ref).into())
            .unwrap_or(Value::Null)),
        ExprData::Ident(name) => run_load_ident(context, name, expr.span),
        ExprData::Base => {
            let env = context.infunc.closure.0.env.0.upgrade();
            Ok(env.and_then(|obj| {
                let obj = obj.borrow();
                if obj.is_class_inst {
                    obj.delegate
                        .as_ref()
                        .map(|del| Value::Object(del.clone()))
                } else {
                    None
                }
            }).unwrap_or(Value::Null))
        }
    }
}

fn run_function_call(context: &mut Context, func: &AssignTarget, args: &[Expr]) -> ExprResult {
    let (env, func_val) = match func {
        AssignTarget::FieldAccess(target, field_name) => {
            let parent = run_expression(context, target)?;
            let func = run_field_access(context, &parent, &field_name.0, field_name.1)?;
            (parent, func)
        }
        AssignTarget::Ident(ident) => {
            let func = run_load_ident(context, &ident.0, ident.1)?;
            (context.infunc.closure.0.env.clone().into(), func)
        }
        AssignTarget::ArrayAccess { array, index, span } => {
            // TODO: Non array access (i.e. field expression access for tables)
            let array = run_expression(context, array)?;
            let array = match array {
                Value::Array(ArrayRef(arr)) => arr,
                _ => panic!("Can't index non-array"),
            };
            let index = run_expression(context, index)?;
            let index = match index {
                // TODO: Handle negative index
                Value::Integer(val) => val as usize,
                _ => panic!("Can't index with non-integer"),
            };
            let func = array
                .borrow()
                .get(index)
                .expect("Array index out of bounds")
                .clone();
            (context.infunc.closure.0.env.clone().into(), func)
        }
    };
    let env = match env {
        Value::Object(obj) => obj,
        _ => panic!("Can't call function on non-object"),
    };

    let func = match func_val {
        Value::Function(func) => func,
        _ => panic!("Can't call non-function"),
    };

    // TODO: Some better way to validate this
    let args = args
        .iter()
        .map(|expr| run_expression(context, expr))
        .collect::<Result<Vec<_>, _>>()?;

    let ast_fn = unsafe { func.0.ast_fn.as_ref() };
    let rt_func = FuncRuntime::new(func, args);
    let body = &ast_fn.body;
    let mut context = Context { infunc: rt_func, vm_state: context.vm_state};
    run_function(&mut context, body)
}

fn run_function(context: &mut Context, body: &Statement) -> ExprResult {
    match run_statement(context, body) {
        Ok(expr) => Ok(Value::Null),
        Err(FlowControl::Break(span)) | Err(FlowControl::Continue(span)) => {
            Err(ExecError::illegal_keyword(span))
        }
        Err(FlowControl::Return(span, val)) => Ok(val),
        Err(FlowControl::Yield(span, val)) => todo!("Yield not implemented"),
        Err(FlowControl::Error(err)) => Err(err),
    }
}

fn run_load_ident(context: &mut Context, ident: &str, span: Span) -> ExprResult {
    let local_match = context.infunc.locals.get(ident);
    if let Some(value) = local_match {
        return Ok(value.clone());
    }

    let key = Value::String(ident.to_string());
    let env_match = context
            .infunc
            .closure.0
            .env
            .0
            .upgrade().and_then(|env| env.borrow().get_field(&key));
    if let Some(value) = env_match {
        return Ok(value);
    }

    let root_match = context
        .infunc
        .closure
        .0
        .root
        .0
        .upgrade()
        .and_then(|root| root.borrow().get_field(&key));
    if let Some(value) = root_match {
        return Ok(value);
    }

    Err(ExecError::undefined_variable((ident.to_string(), span)))
}

fn run_field_access(context: &mut Context, target: &Value, field: &str, span: Span) -> ExprResult {
    match target {
        // TODO: Cloning field here!
        Value::Object(obj) => {
            let res = obj.0.borrow().get_field(&Value::String(field.to_string()));
            match res {
                Some(val) => Ok(val),
                None => Err(ExecError::undefined_variable((field.to_string(), span))),

            }
        }
        _ => panic!("Can't dereference non-object"),
    }
}

fn run_unary_ref_op(context: &mut Context, op: &UnaryRefOp, target: &AssignTarget) -> ExprResult {
    let val = get_assign_target(context, target)?;
    let new_val = match op {
        UnaryRefOp::PreIncr | UnaryRefOp::PostIncr => todo!(),
        UnaryRefOp::PreDecr | UnaryRefOp::PostDecr => false,
    };
    match op {
        UnaryRefOp::PreIncr | UnaryRefOp::PreDecr => {
        },
        UnaryRefOp::PostIncr | UnaryRefOp::PostDecr => {

        },
    }
    todo!()
}

fn run_class(context: &mut Context, parent: Option<&Ident>, table_decl: &[(Expr, Expr)]) -> ExprResult {
    todo!()
}

fn run_table(context: &mut Context, table_decl: &[(Expr, Expr)]) -> ExprResult {
    todo!()
}

fn run_array_access(context: &mut Context, array: &Expr, index: &Expr) -> ExprResult {
    todo!()
}

fn run_unary_op(context: &mut Context, op: &UnaryOp, val: &Expr) -> ExprResult {
    todo!()
}

fn run_binary_op(context: &mut Context, op: &BinaryOp, lhs: &Expr, rhs: &Expr) -> ExprResult {
    todo!()
}

fn get_assign_target(context: &mut Context, target: &AssignTarget) -> ExprResult {
    match target {
        AssignTarget::Ident(ident) => {
            todo!()
        },
        AssignTarget::ArrayAccess { array, index, span } => todo!(),
        AssignTarget::FieldAccess(_, _) => todo!(),
    }
}

fn run_assign(context: &mut Context, target: &AssignTarget, val: Value, is_newslot: bool) -> Result<(), ExecError> {
    match target {
        AssignTarget::Ident(ident) => {
            if let Some(val) = context.infunc.locals.get_mut(&ident.0) {
                assert!(is_newslot, "Can't create a local slot");
                *val = val.clone();
            } if is_newslot {
                if let Some(env) = context.infunc.closure.0.env.0.upgrade() {
                    env.borrow_mut().slots.insert(Value::String(ident.0.clone()), val);
                } else {
                    panic!("Setting field of null")
                }
            } else {
                // TODO: Inefficient
                let mut current = context.infunc.closure.0.env.0.upgrade();
                while let Some(strong_current) = current {
                    current = {
                        let mut obj = strong_current.borrow_mut();
                        if obj.slots.contains_key(&Value::String(ident.0.clone())) {
                            obj.slots.insert(Value::String(ident.0.clone()), val);
                            break;
                        }
                        obj.delegate.clone().map(|obj_ref| obj_ref.0)
                    }
                }
                panic!("Slot does not exist: {}", ident.0)
            }
        },
        AssignTarget::ArrayAccess { array, index, span } => todo!(),
        AssignTarget::FieldAccess(_, _) => todo!(),
    }
    Ok(())
}

fn run_ident(context: &mut Context, ident: &Ident) -> ExprResult {
    todo!()
}

#[cfg(test)]
mod tests {
    use crate::{parser::parse, test_foreach};
    use super::*;

    test_foreach!(sample_test);

    fn sample_test(file_name: &str, file_contents: &str) {
        let actual_ast = match parse(file_contents, file_name.to_string()) {
            Ok(ast) => ast,
            Err(err) => panic!("{}", err)
        };

        let result = match run(&actual_ast) {
            Ok(val) => val,
            Err(err) => panic!("{:?}", err)
        };

        // let expect_ast = exchange_data("parse", file_name, &actual_ast);
        // TODO: Have a more useful comparison for these trees
        // assert_eq!(actual_ast, expect_ast);
    }
}
