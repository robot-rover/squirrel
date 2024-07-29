use hashbrown::HashMap;
use std::{
    borrow::BorrowMut,
    cell::RefCell,
    env,
    io::{self, Write},
    ops::Deref,
    rc::Rc,
};

use crate::{
    context::{Span, SquirrelError},
    parser::ast::{
        self, AssignKind, AssignTarget, BinaryOp, CallTarget, Expr, ExprData, Ident, Statement,
        StatementData, UnaryOp, UnaryRefOp,
    },
    util::{IntoOwned, WriteOption},
};

use super::{
    builtins::{self, tostring},
    value::{Class, Closure, HashValue, Instance, Object, TypeName, Value},
    CallInfo, Context, ExecError, ExprResult, FuncRuntime, VMState,
};
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

pub fn run<'a>(
    tree: &ast::Function,
    file_name: &str,
    stdout: Option<&mut dyn io::Write>,
    args: impl IntoIterator<Item = &'a str>,
) -> Result<(), SquirrelError> {
    let root = init_root();
    let root_closure = Closure::root(tree, Value::Object(root.clone()));
    let arg_vals = args.into_iter().map(|arg| Value::string(arg)).collect();
    let infunc = FuncRuntime::new(
        Rc::new(RefCell::new(root_closure)),
        arg_vals,
        Some(Value::Object(root.clone())),
        Span::empty(),
        Span::empty(),
    )
    .expect("Unable to create root function runtime");
    let mut vm_state = VMState {
        root_table: root,
        stdout: stdout.into(),
        stderr: WriteOption::Stderr(io::stderr()),
    };
    let mut context = Context {
        infunc,
        vm_state: &mut vm_state,
    };

    run_function(&mut context, &tree.body)
        .map(|_| ())
        .map_err(|err| err.with_context(file_name.to_string()))
}

fn init_root() -> Rc<RefCell<Object>> {
    Rc::new(RefCell::new(builtins::global::make_root_table()))
}

fn run_foreach<'a, K, O, I>(
    context: &mut Context,
    iter: I,
    index_local_idx: Option<&u32>,
    value_local_idx: &u32,
    body: &Statement,
) -> FlowResult
where
    I: Iterator<Item = (K, &'a Value)>,
    K: IntoOwned<O>,
    O: Into<Value>,
{
    for (key, val) in iter {
        if let Some(index_idx) = index_local_idx {
            context
                .infunc
                .set_local(*index_idx, key.into_owned().into())
        }
        context.infunc.set_local(*value_local_idx, val.clone());

        match run_statement(context, body) {
            Ok(()) | Err(FlowControl::Continue(_)) => {}
            Err(FlowControl::Break(_span)) => break,
            Err(other) => return Err(other),
        }
    }

    Ok(())
}

fn run_statement(context: &mut Context, statement: &Statement) -> FlowResult {
    match &statement.data {
        StatementData::Block(stmts) => {
            for statement in stmts {
                run_statement(context, statement)?;
            }
        }
        StatementData::Expr(expr) => {
            run_expression(context, expr)?;
        }
        StatementData::IfElse(cond, if_true, if_false) => {
            if run_expression(context, cond)?.truthy() {
                run_statement(context, if_true)?;
            } else {
                run_statement(context, if_false)?;
            }
        }
        StatementData::While { while_kw, cond, body, is_do_while } => {
            let mut keep_going = true;
            loop {
                if !*is_do_while {
                    keep_going = run_expression(context, cond)?.truthy();
                }
                match run_statement(context, body) {
                    Ok(()) => {}
                    Err(FlowControl::Break(_span)) => break,
                    Err(FlowControl::Continue(_span)) => {}
                    Err(other) => return Err(other),
                }
                if *is_do_while {
                    keep_going = run_expression(context, cond)?.truthy();
                }
                if !keep_going {
                    break;
                }
            }
        }
        StatementData::Switch(val, cases, default) => {
            run_case(context, val, cases, default.as_ref().map(|b| &**b))?
        }
        StatementData::For {
            init,
            cond,
            incr,
            body,
        } => {
            run_statement(context, init)?;
            while run_expression(context, cond)?.truthy() {
                match run_statement(context, body) {
                    Ok(()) => {}
                    Err(FlowControl::Break(_span)) => break,
                    Err(FlowControl::Continue(_span)) => {}
                    Err(other) => return Err(other),
                }
                run_statement(context, incr)?;
            }
        }
        StatementData::Foreach {
            index_idx,
            value_idx,
            iterable,
            body,
        } => {
            let iterable_span = iterable.span;
            let iterable = run_expression(context, iterable)?;
            match iterable {
                Value::String(_) => todo!(),
                Value::Object(obj) => {
                    let anchor = obj.borrow();
                    run_foreach::<&HashValue, HashValue, _>(
                        context,
                        anchor.slot_iter(),
                        index_idx.as_ref(),
                        value_idx,
                        body,
                    )?;
                }
                Value::Array(arr) => {
                    let anchor = arr.borrow();
                    run_foreach::<i64, i64, _>(
                        context,
                        anchor.iter().enumerate().map(|(i, v)| (i as i64, v)),
                        index_idx.as_ref(),
                        value_idx,
                        body,
                    )?;
                }
                other => {
                    return Err(FlowControl::Error(ExecError::uniterable_type(
                        other.type_str().to_string(),
                        iterable_span,
                    )))
                }
            };
        }
        StatementData::Break => return Err(FlowControl::Break(statement.span)),
        StatementData::Continue => return Err(FlowControl::Continue(statement.span)),
        StatementData::Return(val) => {
            return Err(FlowControl::Return(
                statement.span,
                run_expression(context, val)?,
            ))
        }
        StatementData::Yield(val) => todo!("Yield not implemented"),
        StatementData::TryCatch(_, _, _) => todo!("Try Catch not implemented"),
        StatementData::Throw(_) => todo!("Throw not implemented"),
        StatementData::Const(_, _) => todo!("Const not implemented"),
        StatementData::Empty => {}
    }
    Ok(())
}

fn run_case(
    context: &mut Context,
    val: &Expr,
    cases: &[(Expr, Statement)],
    default: Option<&Statement>,
) -> FlowResult {
    let cond = run_expression(context, val)?;
    let mut matched = false;
    for (case_val, body) in cases {
        // TODO: Does squirrel evaluate all case labels?
        if !matched {
            let case_val = run_expression(context, case_val)?;
            if cond == case_val {
                matched = true;
            }
        }
        if matched {
            match run_statement(context, body) {
                Ok(()) => {}
                Err(FlowControl::Break(_span)) => return Ok(()),
                Err(other) => return Err(other),
            }
        }
    }
    if let Some(default) = default {
        match run_statement(context, default) {
            Ok(()) => {}
            Err(FlowControl::Break(_span)) => return Ok(()),
            Err(other) => return Err(other),
        }
    }
    Ok(())
}

fn run_expression(context: &mut Context, expr: &Expr) -> ExprResult {
    match &expr.data {
        ExprData::Literal(lit) => Ok(lit.into()),
        ExprData::TableDecl(table_decl) => run_table(context, table_decl),
        ExprData::ArrayDecl(array_decl) => Ok(Value::array(
            array_decl
                .iter()
                .map(|expr| run_expression(context, expr))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        ExprData::FunctionDef(func_def) => Ok(Value::closure(Closure::new(
            func_def,
            func_def
                .default_expr
                .iter()
                .map(|expr| run_expression(context, expr))
                .collect::<Result<Vec<_>, _>>()?,
            &context.infunc,
            Value::Object(context.vm_state.root_table.clone()),
        ))),
        ExprData::ClassDef { parent, members } => run_class(context, parent.as_ref(), members),
        ExprData::Assign(assign) => {
            let mut val = run_expression(context, &assign.value)?;
            let (is_newslot, op) = match &assign.kind {
                AssignKind::Normal => (false, None),
                AssignKind::NewSlot => (true, None),
                AssignKind::Add => (false, Some(BinaryOp::Add)),
                AssignKind::Sub => (false, Some(BinaryOp::Sub)),
                AssignKind::Mult => (false, Some(BinaryOp::Mul)),
                AssignKind::Div => (false, Some(BinaryOp::Div)),
            };
            val = if let Some(op) = op {
                let target_val = get_assign_target(context, &assign.target)?;
                run_binary_op(
                    context,
                    &op,
                    assign.op_span,
                    target_val,
                    assign.target.span(),
                    val,
                    assign.value.span,
                )?
            } else {
                val
            };
            run_assign(context, &assign.target, val.clone(), is_newslot)?;
            Ok(val)
        }
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
        ExprData::BinaryOp {
            op,
            op_span,
            lhs,
            rhs,
        } => {
            let lhs_span = lhs.span;
            let lhs = run_expression(context, lhs)?;
            let rhs_span = rhs.span;
            let rhs = run_expression(context, rhs)?;
            run_binary_op(context, op, *op_span, lhs, lhs_span, rhs, rhs_span)
        }
        ExprData::UnaryOp(op, op_span, val) => {
            let val_span = val.span;
            let val = run_expression(context, val)?;
            run_unary_op(context, op, *op_span, val, val_span)
        }
        ExprData::UnaryRefOp(op, op_span, val) => run_unary_ref_op(context, op, *op_span, val),
        ExprData::FunctionCall { func, args } => run_function_call(
            context,
            func,
            args,
            args.iter()
                .map(|expr| expr.span)
                .fold(func.span(), |acc, span| acc | span),
        ),
        ExprData::RawCall { func, this, args } => {
            let func_span = func.span;
            let func = run_expression(context, func)?;
            let param_span = args
                .iter()
                .map(|expr| expr.span)
                .fold(func_span, |acc, span| acc | span);
            let this = run_expression(context, this)?;
            let args = args
                .iter()
                .map(|expr| run_expression(context, expr))
                .collect::<Result<Vec<_>, _>>()?;

            run_rawcall(context, func, this, args, func_span, param_span)
        }
        ExprData::ArrayAccess { array, index } => {
            let span = index.span;
            let array = run_expression(context, array)?;
            let index = run_expression(context, index)?;
            // run_array_access(context, &array, index, span)
            let hash_index = index.try_into().map_err(|unhash: Value| {
                ExecError::unhashable_type(unhash.type_str().to_string(), span)
            })?;
            array
                .get_field(&hash_index)
                .ok_or_else(|| ExecError::undefined_field(span, hash_index.into()))
        }
        ExprData::This => Ok(context.infunc.env.clone()),
        ExprData::FieldAccess(target, field) => {
            let target = run_expression(context, target)?;
            target
                .get_field_str(&field.0)
                .ok_or_else(|| ExecError::undefined_field(field.1, Value::string(&field.0)))
        }
        ExprData::Globals => Ok(context.infunc.closure.borrow().root.clone()),
        ExprData::Ident(name) => run_load_ident(context, name, expr.span),
        ExprData::Base => {
            todo!();
            return Ok(Value::Null);
        }
        ExprData::Local(idx, span) => {
            let val = context.infunc.locals[*idx as usize]
                .deref()
                .borrow()
                .clone();
            Ok(val)
        }
    }
}

fn run_function_call(
    context: &mut Context,
    func: &CallTarget,
    args: &[Expr],
    args_span: Span,
) -> ExprResult {
    let (env, func_val) = match func {
        CallTarget::FieldAccess(target, field_name) => {
            let parent = run_expression(context, target)?;
            let func = parent.get_field_str(&field_name.0).ok_or_else(|| {
                ExecError::undefined_field(field_name.1, Value::string(&field_name.0))
            })?;
            (parent, func)
        }
        CallTarget::Expr(expr) => {
            let func = run_expression(context, expr)?;
            (context.infunc.env.clone(), func)
        }
    };
    let args = args
        .iter()
        .map(|expr| run_expression(context, expr))
        .collect::<Result<Vec<_>, _>>()?;

    run_rawcall(context, func_val, env, args, func.span(), args_span)
}

macro_rules! run_metamethod {
    () => {};
}

pub fn run_rawcall(
    context: &mut Context,
    func_val: Value,
    env: Value,
    args: Vec<Value>,
    func_span: Span,
    call_span: Span,
) -> ExprResult {
    // TODO: Some better way to validate this
    match func_val {
        Value::Closure(func) => {
            let ast_fn = unsafe { func.borrow().ast_fn.as_ref() };
            // TODO: need to force func runtime to use our env here rather than the closure's
            let rt_func = FuncRuntime::new(func.clone(), args, Some(env), func_span, call_span)?;
            let body = &ast_fn.body;
            let mut context = Context {
                infunc: rt_func,
                vm_state: context.vm_state,
            };
            run_function(&mut context, body)
        }
        Value::NativeFn(func) => {
            let call_info = CallInfo {
                func_span,
                call_span,
            };
            func(context as *mut _, env, args, &call_info)
        }
        Value::Class(class) => {
            let instance = Instance::construct(class.clone())?;
            let constructor = instance.get_field_str("constructor");
            let instance_val = Value::instance(instance);
            if let Some(constructor) = constructor {
                run_rawcall(
                    context,
                    constructor,
                    instance_val.clone(),
                    args,
                    func_span,
                    call_span,
                )?;
            }
            Ok(instance_val)
        }
        Value::Object(obj) => {
            let call = obj.deref().borrow().get_field_str("_call");
            if let Some(call) = call {
                let mut call_args = vec![context.infunc.env.clone()];
                call_args.extend(args);
                run_rawcall(
                    context,
                    call,
                    Value::Object(obj),
                    call_args,
                    func_span,
                    call_span,
                )
            } else {
                Err(ExecError::missing_metamethod(
                    func_span,
                    call_span,
                    "call".to_string(),
                    "()".to_string(),
                ))
            }
        }
        Value::Instance(inst) => {
            let call = inst.deref().borrow().get_field_str("_call");
            if let Some(call) = call {
                let mut call_args = vec![context.infunc.env.clone()];
                call_args.extend(args);
                run_rawcall(
                    context,
                    call,
                    Value::Instance(inst),
                    call_args,
                    func_span,
                    call_span,
                )
            } else {
                Err(ExecError::missing_metamethod(
                    func_span,
                    call_span,
                    "call".to_string(),
                    "()".to_string(),
                ))
            }
        }
        other => Err(ExecError::general(
            func_span,
            format!("Can't call non-function {other:?}"),
        )),
    }
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
    let env_match = context.infunc.env.get_field_str(&ident);
    if let Some(value) = env_match {
        return Ok(value);
    }

    let root_match = context.infunc.closure.borrow().root.get_field_str(&ident);
    if let Some(value) = root_match {
        return Ok(value);
    }

    Err(ExecError::undefined_variable((ident.to_string(), span)))
}

fn run_delete(context: &mut Context, op_span: Span, target: &AssignTarget) -> ExprResult {
    match target {
        AssignTarget::Ident(ident) => todo!(),
        AssignTarget::ArrayAccess { array, index, span } => todo!(),
        AssignTarget::FieldAccess(parent, ident) => todo!(),
        AssignTarget::Local(local_idx, span) => todo!(),
    }
}

fn run_unary_ref_op(
    context: &mut Context,
    op: &UnaryRefOp,
    op_span: Span,
    target: &AssignTarget,
) -> ExprResult {
    let val = get_assign_target(context, target)?;
    let (is_add, return_new) = match op {
        UnaryRefOp::PreIncr => (true, true),
        UnaryRefOp::PreDecr => (false, true),
        UnaryRefOp::PostIncr => (true, false),
        UnaryRefOp::PostDecr => (false, false),
        UnaryRefOp::Delete => return run_delete(context, op_span, target),
    };
    let new_val = run_binary_op(
        context,
        if is_add {
            &BinaryOp::Add
        } else {
            &BinaryOp::Sub
        },
        op_span,
        val.clone(),
        target.span(),
        Value::Integer(1),
        op_span,
    )?;
    run_assign(context, target, new_val.clone(), false)?;
    Ok(if return_new { new_val } else { val })
}

fn run_class(
    context: &mut Context,
    parent: Option<&Ident>,
    table_decl: &[(Expr, Expr)],
) -> ExprResult {
    let delegate = parent.map(|parent| run_load_ident(context, &parent.0, parent.1));
    let parent = if let Some((parent, parent_span)) = parent {
        let parent = run_load_ident(context, parent, *parent_span)?;
        match parent {
            Value::Class(obj) => Some(obj.clone()),
            _ => return Err(ExecError::extending_non_class(*parent_span)),
        }
    } else {
        None
    };
    let slots = table_decl
        .iter()
        .map(|(k, v)| {
            let k = run_expression(context, k)?
                .try_into()
                .map_err(|unhash: Value| {
                    ExecError::unhashable_type(unhash.type_str().to_string(), k.span)
                })?;
            let v = run_expression(context, v)?;
            Ok((k, v))
        })
        .collect::<Result<HashMap<HashValue, Value>, ExecError>>()?;
    Ok(Value::class(Class::new(parent, slots)))
}

fn run_table(context: &mut Context, table_decl: &[(Expr, Expr)]) -> ExprResult {
    Ok(Value::object(Object::new(
        None,
        table_decl
            .iter()
            .map(|(key, val)| {
                let key_span = key.span;
                let key = run_expression(context, key)?;
                let key = key.try_into().map_err(|unhash: Value| {
                    ExecError::unhashable_type(unhash.type_str().to_string(), key_span)
                })?;
                let val = run_expression(context, val)?;
                Ok((key, val))
            })
            .collect::<Result<HashMap<_, _>, _>>()?,
    )))
}

fn run_unary_op(
    context: &mut Context,
    op: &UnaryOp,
    op_span: Span,
    val: Value,
    val_span: Span,
) -> ExprResult {
    let res = match op {
        UnaryOp::Neg => match val {
            Value::Float(val) => Value::Float(-val),
            Value::Integer(val) => Value::Integer(-val),
            Value::Instance(inst) => todo!("_unm"),
            Value::Object(inst) => todo!("_unm"),
            other => {
                return Err(ExecError::illegal_unary_op(
                    "-",
                    op_span,
                    (&other, val_span),
                ))
            }
        },
        UnaryOp::Not => Value::boolean(!val.truthy()),
        UnaryOp::BitNot => match val {
            Value::Integer(val) => Value::Integer(!val),
            other => {
                return Err(ExecError::illegal_unary_op(
                    "~",
                    op_span,
                    (&other, val_span),
                ))
            }
        },
        UnaryOp::TypeOf => Value::string(&val.type_str()),
        UnaryOp::Clone => match val {
            Value::Closure(_) => todo!(),
            Value::Array(arr) => Value::array(arr.deref().borrow().clone()),
            Value::Object(obj) => Value::object(obj.deref().borrow().clone()),
            other => other.clone(),
        },
        UnaryOp::Resume => todo!("Resume not implemented"),
    };
    Ok(res)
}

// lhs.get_field_str($meta).ok_or_else(|| ExecError::missing_metamethod())

macro_rules! stringify_or_value {
    ($val:tt) => {
        stringify!($val)
    };
    ($val:tt, $lit:literal) => {
        $lit
    };
}

macro_rules! compare_impl {
    ($result:ident, $op:tt, $lhs_span:ident, $op_span:ident) => {
        Ok($result)
    };
    ($result:ident, $op:tt, $lhs_span:ident, $op_span:ident, $compare:ident) => {
        match $result {
            Value::Integer(val) => Ok(Value::Boolean(val $op 0)),
            Value::Float(val) => Ok(Value::Boolean(val $op 0.0)),
            other => Err(ExecError::wrong_metamethod_return_type($lhs_span, $op_span, "_cmp".to_string(), "integer|float".to_string(), other.type_str().to_string())),
        }
    };
}

macro_rules! metamethod_impl {
    (($val_variant:ident $lhs:ident, $lhs_span:ident) $op:tt ($rhs:ident, $rhs_span:ident); $op_span:ident, $context:ident, lit $op_lit:ident, meta $meta:literal $($compare:ident)?) => {{
        let meta = $lhs.borrow().get_field_str($meta).ok_or_else(|| ExecError::missing_metamethod($lhs_span, $op_span, $meta.to_string(), $op_lit.to_string()))?;
        let result = run_rawcall($context, meta, Value::$val_variant($lhs), vec![$rhs], $lhs_span | $op_span, $lhs_span | $rhs_span)?;
        compare_impl!(result, $op, $lhs_span, $op_span $(, $compare)?)
    }};
}

macro_rules! define_op {
    ($lhs:ident $op:tt $rhs:ident; $op_span:ident, $context:ident $(, lit $op_lit:literal)? $(, meta $meta:literal $($compare:ident)?)?) => {
        define_op!($lhs $op $rhs; $op_span, $context, int Integer, float Float $(, lit $op_lit)? $(, meta $meta $($compare)?)?)
    };
    ($lhs:ident $op:tt $rhs:ident; $op_span:ident, $context:ident, both $result:ident $(, lit $op_lit:literal)? $(, meta $meta:literal $($compare:ident)?)?) => {
        define_op!($lhs $op $rhs; $op_span, $context, int $result, float $result $(, lit $op_lit)? $(, meta $meta $($compare)?)?)
    };
    ($lhs:ident $op:tt $rhs:ident; $op_span:ident, $context:ident $(, int $int_result:ident)? $(, float $float_result:ident)? $(, lit $op_lit:literal)? $(, meta $meta:literal $($compare:ident)?)?) => {
        {
            let op_lit = stringify_or_value!($op $(, $op_lit)?);
            let result = |v: Value| Result::<Value, ExecError>::Ok(v);
            match ($lhs, $rhs) {
                $(((Value::Integer(lhs), _), (Value::Integer(rhs), _)) => Ok(Value::$int_result(lhs $op rhs)),)?
                $(
                    ((Value::Float(lhs), _), (Value::Float(rhs), _)) => Ok(Value::$float_result(lhs $op rhs)),
                    ((Value::Integer(lhs), _), (Value::Float(rhs), _)) => Ok(Value::$float_result((lhs as f64) $op rhs)),
                    ((Value::Float(lhs), _), (Value::Integer(rhs), _)) => Ok(Value::$float_result(lhs $op (rhs as f64))),
                )?
                $(
                    ((Value::Instance(lhs), lhs_span), (rhs, rhs_span)) => metamethod_impl!((Instance lhs, lhs_span) $op (rhs, rhs_span); $op_span, $context, lit op_lit, meta $meta $($compare)?),
                    ((Value::Object(lhs), lhs_span), (rhs, rhs_span)) => metamethod_impl!((Object lhs, lhs_span) $op (rhs, rhs_span); $op_span, $context, lit op_lit, meta $meta $($compare)?),
                    $(
                        ((Value::String(s), _), (Value::String(t), _)) => {
                            let $compare = ();
                            result(Value::Boolean(s $op t))
                        },
                    )?
                )?
                (lhs, rhs) => Err(ExecError::illegal_binary_op(op_lit, $op_span, lhs, rhs)),
            }
        }
    };
}

fn run_binary_op(
    context: &mut Context,
    op: &BinaryOp,
    op_span: Span,
    lhs: Value,
    lhs_span: Span,
    rhs: Value,
    rhs_span: Span,
) -> ExprResult {
    let lhs = (lhs, lhs_span);
    let rhs = (rhs, rhs_span);
    match op {
        BinaryOp::Add => {
            if matches!(lhs.0, Value::String(_)) || matches!(rhs.0, Value::String(_)) {
                let lhs = tostring(
                    &mut *context,
                    lhs.0,
                    vec![],
                    &CallInfo {
                        func_span: op_span,
                        call_span: lhs_span,
                    },
                )?;
                let rhs = tostring(
                    &mut *context,
                    rhs.0,
                    vec![],
                    &CallInfo {
                        func_span: op_span,
                        call_span: rhs_span,
                    },
                )?;
                Ok(Value::string(&format!("{}{}", lhs, rhs)))
            } else {
                define_op!(lhs + rhs; op_span, context, meta "_add")
            }
        }
        BinaryOp::Sub => define_op!(lhs - rhs; op_span, context, meta "_sub"),
        BinaryOp::Mul => define_op!(lhs * rhs; op_span, context, meta "_mul"),
        BinaryOp::Div => define_op!(lhs / rhs; op_span, context, meta "_div"),
        BinaryOp::Mod => define_op!(lhs % rhs; op_span, context, meta "_modulo"),
        // TODO: Comparing non-numbers for equality (classes, arrays, functions, etc)
        BinaryOp::Eq => Ok(Value::Boolean(lhs.0 == rhs.0)),
        BinaryOp::NotEq => Ok(Value::Boolean(lhs.0 != rhs.0)),
        BinaryOp::Greater => {
            define_op!(lhs > rhs; op_span, context, both boolean, meta "_cmp" compare)
        }
        BinaryOp::GreaterEq => {
            define_op!(lhs >= rhs; op_span, context, both boolean, meta "_cmp" compare)
        }
        BinaryOp::Less => {
            define_op!(lhs < rhs; op_span, context, both boolean, meta "_cmp" compare)
        }
        BinaryOp::LessEq => {
            define_op!(lhs <= rhs; op_span, context, both boolean, meta "_cmp" compare)
        }
        BinaryOp::Compare => todo!("Implement compare and metamethod"),
        BinaryOp::And => Ok(if !lhs.0.truthy() { lhs.0 } else { rhs.0 }),
        BinaryOp::Or => Ok(if lhs.0.truthy() { lhs.0 } else { rhs.0 }),
        BinaryOp::BitAnd => define_op!(lhs & rhs; op_span, context, int Integer),
        BinaryOp::BitOr => define_op!(lhs | rhs; op_span, context, int Integer),
        BinaryOp::BitXor => define_op!(lhs ^ rhs; op_span, context, int Integer),
        BinaryOp::Shl => define_op!(lhs << rhs; op_span, context, int Integer),
        BinaryOp::Shr => match (lhs, rhs) {
            ((Value::Integer(l), _), (Value::Integer(r), _)) => {
                Ok(Value::Integer(((l as u64) >> r) as i64))
            }
            (lhs, rhs) => Err(ExecError::illegal_binary_op(">>", op_span, lhs, rhs)),
        },
        BinaryOp::AShr => define_op!(lhs >> rhs; op_span, context, int Integer, lit ">>>"),
        BinaryOp::In => todo!("In is not implemented"),
        BinaryOp::InstanceOf => todo!("Instanceof is not implemented"),
    }
}

fn get_assign_target(context: &mut Context, target: &AssignTarget) -> ExprResult {
    match target {
        AssignTarget::Ident(ident) => run_load_ident(context, &ident.0, ident.1),
        AssignTarget::ArrayAccess { array, index, span } => {
            let array = run_expression(context, array)?;
            let index = run_expression(context, index)?;
            let hash_index = index.try_into().map_err(|unhash: Value| {
                ExecError::unhashable_type(unhash.type_str().to_string(), *span)
            })?;
            array
                .get_field(&hash_index)
                .ok_or_else(|| ExecError::undefined_field(*span, hash_index.into()))
        }
        AssignTarget::FieldAccess(target, field) => {
            let target = run_expression(context, target)?;
            target
                .get_field_str(&field.0)
                .ok_or_else(|| ExecError::undefined_field(field.1, Value::string(&field.0)))
        }
        AssignTarget::Local(idx, span) => Ok(context.infunc.locals[*idx as usize]
            .deref()
            .borrow()
            .clone()),
    }
}

fn run_assign(
    context: &mut Context,
    target: &AssignTarget,
    val: Value,
    is_newslot: bool,
) -> Result<(), ExecError> {
    let span = target.span();
    let (target_obj, index) = match target {
        AssignTarget::Local(idx, span) => {
            assert!(!is_newslot, "Can't create a local slot");
            *context.infunc.locals[*idx as usize].deref().borrow_mut() = val;
            return Ok(());
        }
        AssignTarget::Ident(ident) => {
            let env = context.infunc.env.clone();
            let val = HashValue::string(&ident.0);
            (env, val)
        }
        AssignTarget::ArrayAccess { array, index, span } => {
            let array = run_expression(context, array)?;
            let index = run_expression(context, index)?;
            let hash_index: HashValue = index.try_into().map_err(|unhash: Value| {
                ExecError::unhashable_type(unhash.type_str().to_string(), *span)
            })?;
            (array, hash_index)
        }
        AssignTarget::FieldAccess(parent, field) => {
            let parent = run_expression(context, parent)?;
            let index = HashValue::string(&field.0);
            (parent, index)
        }
    };

    match (target_obj, index) {
        (Value::Object(obj), any) => {
            let mut obj = obj.deref().borrow_mut();
            obj.set_field(any, val, is_newslot, span)
        }
        (Value::Array(arr), HashValue::Integer(index)) => {
            // TODO: What happens with newslot operator?
            // TODO: Handle index out of bounds
            let mut arr = arr.deref().borrow_mut();
            arr[index as usize] = val;
            Ok(())
        }
        (Value::Array(_), other) => panic!("Cannot set non-integer index of array"),
        (Value::Instance(inst), key) => {
            let mut inst = inst.deref().borrow_mut();
            inst.set_field(key, val, is_newslot, span)?;
            Ok(())
        }
        _ => panic!("Can't assign to non-object"),
    }
}

#[cfg(test)]
mod tests {
    use std::iter;

    use super::*;
    use crate::{
        context::IntoSquirrelErrorContext, parser::parse, test_foreach, test_util::exchange_str,
    };

    test_foreach!(sample_test);

    fn sample_test(file_name: &str, file_contents: &str) {
        let test_name = format!("walker-{}", file_name.replace("/", "-"));
        let test_desc = format!("Walker test for {}", file_name);

        let actual_ast = match parse(file_contents, file_name.to_string()) {
            Ok(ast) => ast,
            Err(err) => panic!("{}", err),
        };

        let mut output = Vec::new();
        let result = match run(&actual_ast, file_name, Some(&mut output), iter::empty()) {
            Ok(val) => val,
            Err(err) => panic!("{}", err.with_context(file_contents)),
        };

        let actual_str = String::from_utf8(output).expect("Invalid UTF-8 in test output");
        #[cfg(not(miri))]
        {
            insta::with_settings!({filters => vec![
                (r"\((?<kind>[a-z]+) : 0x(?:0x)?[0-9a-f]+\)", "($kind : 0x<memory>)")
            ]}, {
                insta::assert_snapshot!(test_name, actual_str, &test_desc);
            });
        }
    }
}
