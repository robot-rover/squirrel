use std::{collections::HashMap};

use crate::{
    context::Span,
    parser::ast::{Assign, AssignTarget, BinaryOp, Expr, ExprData, Statement, UnaryOp, UnaryRefOp},
};

use super::{
    ArrayRef, Context, ExecError, FuncRuntime, FunctionRef, Object, ObjectRef, Value, WeakRef,
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

fn run(tree: Vec<Statement>) -> ExprResult {
    let root = init_root();
    let mut function = FuncRuntime {
        root: WeakRef::new(&root),
        env: root,
        locals: HashMap::new(),
    };
    let body = Statement::block(tree, Span::empty(), Span::empty());

    run_function(&mut function, &body)
}

fn init_root() -> ObjectRef {
    ObjectRef::new(Object {
        delegate: None,
        slots: HashMap::new(),
        is_class_inst: false,
    })
}

fn run_statement(context: &mut Context, statement: &Statement) -> FlowResult {
    match statement.data {
        _ => todo!(),
    }
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
        ExprData::FunctionDef(func_def) => Ok(FunctionRef::new(
            func_def,
            func_def
                .default_expr
                .iter()
                .map(|expr| run_expression(context, expr))
                .collect::<Result<Vec<_>, _>>()?,
        )
        .into()),
        ExprData::ClassDef { parent, members } => todo!(),
        ExprData::Assign(assign) => run_assign(context, assign),
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
        ExprData::This => Ok(context.infunc.env.clone().into()),
        ExprData::FieldAccess(target, field) => {
            let target = run_expression(context, target)?;
            run_field_access(context, &target, &field.0)
        }
        ExprData::Globals => Ok(context
            .infunc
            .root
            .0
            .upgrade()
            .map(|obj_ref| ObjectRef(obj_ref).into())
            .unwrap_or(Value::Null)),
        ExprData::Ident(name) => run_load_ident(context, name),
        ExprData::Base => {
            let env = context.infunc.env.0.borrow();
            Ok(if env.is_class_inst {
                env.delegate
                    .as_ref()
                    .map(|del| Value::Object(del.clone()))
                    .unwrap_or(Value::Null)
            } else {
                Value::Null
            })
        }
    }
}

fn run_function_call(context: &mut Context, func: &AssignTarget, args: &[Expr]) -> ExprResult {
    let (env, func_val) = match func {
        AssignTarget::FieldAccess(target, field_name) => {
            let parent = run_expression(context, target)?;
            let func = run_field_access(context, &parent, &field_name.0)?;
            (parent, func)
        }
        AssignTarget::Ident(ident) => {
            let func = run_load_ident(context, &ident.0)?;
            (context.infunc.env.clone().into(), func)
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
            (context.infunc.env.clone().into(), func)
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

    let mut rt_func = FuncRuntime::new(&func, args, env, context.infunc.root.clone());
    let ast_fn = unsafe { func.0.ast_fn.as_ref() };
    let body = &ast_fn.body;
    run_function(&mut rt_func, body)
}

fn run_function(rt_func: &mut FuncRuntime, body: &Statement) -> ExprResult {
    let context = &mut Context::new(rt_func);
    match run_statement(context, body) {
        Ok(expr) => Ok(Value::Null),
        Err(FlowControl::Break(span)) | Err(FlowControl::Continue(span)) => {
            Err(ExecError::illegal_keyword(span))
        }
        Err(FlowControl::Return(span, val)) => Ok(val),
        Err(FlowControl::Yield(span, val)) => panic!("Yield not implemented"),
        Err(FlowControl::Error(err)) => Err(err),
    }
}

fn run_load_ident(context: &mut Context, ident: &str) -> ExprResult {
    Ok(if let Some(value) = context.infunc.locals.get(ident) {
        value.clone()
    } else {
        // TODO: Should look at root table last
        context
            .infunc
            .env
            .0
            .borrow()
            .get_field(&Value::String(ident.to_string()))
    })
}

fn run_field_access(context: &mut Context, target: &Value, field: &str) -> ExprResult {
    match target {
        // TODO: Cloning field here!
        Value::Object(obj) => Ok(obj.0.borrow().get_field(&Value::String(field.to_string()))),
        _ => panic!("Can't dereference non-object"),
    }
}

fn run_unary_ref_op(context: &mut Context, op: &UnaryRefOp, val: &AssignTarget) -> ExprResult {
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

fn run_assign(context: &mut Context, assign: &Assign) -> ExprResult {
    todo!()
}
