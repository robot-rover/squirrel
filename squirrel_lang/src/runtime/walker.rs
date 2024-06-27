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
};

use super::{
    builtins,
    value::{Closure, HashValue, Object, TypeName, Value},
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

pub fn run(
    tree: &ast::Function,
    file_name: &str,
    stdout: Option<&mut dyn io::Write>,
) -> Result<(), SquirrelError> {
    let root = init_root();
    let root_closure = Closure::root(tree, Value::Object(root.clone()));
    let arg_vals = env::args()
        .into_iter()
        .map(|arg| Value::string(&arg))
        .collect();
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
        stderr: super::WriteOption::Stderr(io::stderr()),
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
        StatementData::While(cond, body) => {
            while run_expression(context, cond)?.truthy() {
                match run_statement(context, body) {
                    Ok(()) => {}
                    Err(FlowControl::Break(_span)) => break,
                    Err(FlowControl::Continue(_span)) => {}
                    Err(other) => return Err(other),
                }
            }
        }
        StatementData::DoWhile(cond, body) => loop {
            match run_statement(context, body) {
                Ok(()) => {}
                Err(FlowControl::Break(_span)) => break,
                Err(FlowControl::Continue(_span)) => {}
                Err(other) => return Err(other),
            }
            if !run_expression(context, cond)?.truthy() {
                break;
            }
        },
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
            let iterable = run_expression(context, iterable)?;
            for idx in 0.. {
                if let Some(index_idx) = index_idx {
                    *context.infunc.locals[*index_idx as usize]
                        .deref()
                        .borrow_mut() = Value::Integer(idx);
                }
                let val = match &iterable {
                    Value::Array(arr) => {
                        let arr = arr.deref().borrow();
                        if idx < arr.len() as i64 {
                            arr[idx as usize].clone()
                        } else {
                            break;
                        }
                    }
                    _ => todo!(),
                };
                *context.infunc.locals[*value_idx as usize]
                    .deref()
                    .borrow_mut() = val;

                run_statement(context, body)?;
            }
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
        ExprData::RawCall {
            func,
            this,
            parameters,
        } => {
            let func_span = func.span;
            let func = run_expression(context, func)?;
            let param_span = parameters
                .iter()
                .map(|expr| expr.span)
                .fold(func_span, |acc, span| acc | span);
            let this = run_expression(context, this)?;
            run_rawcall(context, func, this, parameters, func_span, param_span)
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
            let val = &context.infunc.env;
            // Double check this is null and not an error
            if let Value::Object(obj) = val {
                let obj = obj.borrow();
                if obj.get_is_class_inst() {
                    if let Some(delegate) = obj.get_delegate() {
                        return Ok(Value::Object(delegate.clone()));
                    }
                }
            }
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
    run_rawcall(context, func_val, env, args, func.span(), args_span)
}

fn run_rawcall(
    context: &mut Context,
    func_val: Value,
    env: Value,
    args: &[Expr],
    func_span: Span,
    call_span: Span,
) -> ExprResult {
    // TODO: Some better way to validate this
    let args = args
        .iter()
        .map(|expr| run_expression(context, expr))
        .collect::<Result<Vec<_>, _>>()?;

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
        other => panic!("Can't call non-function {other:?}"),
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
            Value::Object(obj) => {
                let obj_ref = obj.borrow();
                if obj_ref.get_is_class_inst() {
                    Some(obj.clone())
                } else {
                    panic!("Not a class")
                }
            }
            _ => panic!("Not a class"),
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
    Ok(Value::object(Object::new(parent, slots, true)))
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
        false,
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
            Value::Float(val) => Value::float(-val),
            Value::Integer(val) => Value::Integer(-val),
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
            Value::Object(_) => todo!(),
            other => other.clone(),
        },
        UnaryOp::Resume => todo!("Resume not implemented"),
    };
    Ok(res)
}

macro_rules! promoting_op {
    ($lhs:ident, $rhs:ident, $op:tt; $op_lit:literal) => {
        match ($lhs, $rhs) {
            (Value::Integer($lhs), Value::Integer($rhs)) => Ok(Value::Integer($lhs $op $rhs)),
            (Value::Float($lhs), Value::Float($rhs)) => Ok(Value::float($lhs $op $rhs)),
            (Value::Integer($lhs), Value::Float($rhs)) => Ok(Value::float(($lhs as f64) $op $rhs)),
            (Value::Float($lhs), Value::Integer($rhs)) => Ok(Value::float($lhs $op ($rhs as f64))),
            (lhs, rhs) => Err(($op_lit, lhs, rhs)),
        }
    };
    ($lhs:ident, $rhs:ident, $op:tt, $result:tt; $op_lit:literal) => {
        match ($lhs, $rhs) {
            (Value::Integer($lhs), Value::Integer($rhs)) => Ok(Value::$result($lhs $op $rhs)),
            (Value::Float($lhs), Value::Float($rhs)) => Ok(Value::$result($lhs $op $rhs)),
            (Value::Integer($lhs), Value::Float($rhs)) => Ok(Value::$result(($lhs as f64) $op $rhs)),
            (Value::Float($lhs), Value::Integer($rhs)) => Ok(Value::$result($lhs $op ($rhs as f64))),
            (lhs, rhs) => Err((stringify!($op), lhs, rhs)),
        }
    };
}

macro_rules! int_op {
    ($lhs:ident, $rhs:ident, $op:tt; $op_lit:literal) => {
        int_op!($lhs, $rhs, $op, Integer; $op_lit)
    };
    ($lhs:ident, $rhs:ident, $op:tt, $result:tt; $op_lit:literal) => {
        match ($lhs, $rhs) {
            (Value::Integer($lhs), Value::Integer($rhs)) => Ok(Value::$result($lhs $op $rhs)),
            (lhs, rhs) => Err(($op_lit, lhs, rhs)),
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
    let res = match op {
        BinaryOp::Add => {
            let lhs_str: Result<Rc<str>, Value> = <Rc<str>>::typed_from(lhs);
            let rhs_str: Result<Rc<str>, Value> = <Rc<str>>::typed_from(rhs);
            match (lhs_str, rhs_str) {
                (Ok(ls), Ok(rs)) => Ok(Value::string(&format!("{}{}", ls, rs))),
                (Ok(ls), Err(r)) => Ok(Value::string(&format!("{}{}", ls, r))),
                (Err(l), Ok(rs)) => Ok(Value::string(&format!("{}{}", l, rs))),
                (Err(l), Err(r)) => promoting_op!(l, r, +; "+"),
            }
        }
        BinaryOp::Sub => promoting_op!(lhs, rhs, -; "-"),
        BinaryOp::Mul => promoting_op!(lhs, rhs, *; "*"),
        BinaryOp::Div => promoting_op!(lhs, rhs, /; "/"),
        BinaryOp::Mod => int_op!(lhs, rhs, %; "%"),
        // TODO: Comparing non-numbers for equality (classes, arrays, functions, etc)
        BinaryOp::Eq => promoting_op!(lhs, rhs, ==, boolean; "=="),
        BinaryOp::NotEq => promoting_op!(lhs, rhs, !=, boolean; "!="),
        BinaryOp::Greater => promoting_op!(lhs, rhs, >, boolean; ">"),
        BinaryOp::GreaterEq => promoting_op!(lhs, rhs, >=, boolean; ">="),
        BinaryOp::Less => promoting_op!(lhs, rhs, <, boolean; "<"),
        BinaryOp::LessEq => promoting_op!(lhs, rhs, <=, boolean; "<="),
        BinaryOp::Compare => promoting_op!(lhs, rhs, -; "-"),
        BinaryOp::And => Ok(if !lhs.truthy() {
            lhs.clone()
        } else {
            rhs.clone()
        }),
        BinaryOp::Or => Ok(if lhs.truthy() {
            lhs.clone()
        } else {
            rhs.clone()
        }),
        BinaryOp::BitAnd => int_op!(lhs, rhs, &; "&"),
        BinaryOp::BitOr => int_op!(lhs, rhs, |; "|"),
        BinaryOp::BitXor => int_op!(lhs, rhs, ^; "^"),
        BinaryOp::Shl => int_op!(lhs, rhs, <<; "<<"),
        BinaryOp::Shr => match (lhs, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => {
                Ok(Value::Integer(((lhs as u64) >> rhs) as i64))
            }
            (lhs, rhs) => Err((">>", lhs, rhs)),
        },
        BinaryOp::AShr => int_op!(lhs, rhs, >>; ">>>"),
        BinaryOp::In => todo!("In is not implemented"),
        BinaryOp::InstanceOf => todo!("Instanceof is not implemented"),
    };
    res.map_err(|(op, lhs, rhs)| {
        ExecError::illegal_binary_op(op, op_span, (lhs, lhs_span), (rhs, rhs_span))
    })
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
            obj.set_field(any, val, is_newslot)
                .map_err(|hash_index| ExecError::undefined_field(span, hash_index.into()))
        }
        (Value::Array(arr), HashValue::Integer(index)) => {
            // TODO: Handle index out of bounds
            let mut arr = arr.deref().borrow_mut();
            arr[index as usize] = val;
            Ok(())
        }
        (Value::Array(_), other) => panic!("Cannot set non-integer index of array"),
        _ => panic!("Can't assign to non-object"),
    }
}

#[cfg(test)]
mod tests {
    use indent::indent_all_with;

    use super::*;
    use crate::{
        context::IntoSquirrelErrorContext, parser::parse, test_foreach, test_util::exchange_str,
    };

    test_foreach!(sample_test);

    fn sample_test(file_name: &str, file_contents: &str) {
        let actual_ast = match parse(file_contents, file_name.to_string()) {
            Ok(ast) => ast,
            Err(err) => panic!("{}", err),
        };

        let mut output = Vec::new();
        let result = match run(&actual_ast, file_name, Some(&mut output)) {
            Ok(val) => val,
            Err(err) => panic!("{}", err.with_context(file_contents)),
        };

        let actual_str = String::from_utf8(output).expect("Invalid UTF-8 in test output");
        #[cfg(not(miri))]
        {
            let expect_str = exchange_str("outputs", file_name, &actual_str);
            // TODO: Have a more useful comparison for these trees
            if actual_str != expect_str {
                panic!(
                    "Output does not match expected output\nSource File: {}\nExpected:\n{}\nActual:\n{}",
                    file_name,
                    indent_all_with("| ", expect_str),
                    indent_all_with("| ", actual_str),
                );
            }
        }
    }
}
