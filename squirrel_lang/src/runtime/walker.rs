use std::collections::HashMap;

use crate::{
    context::Span,
    parser::ast::{
        self, AssignKind, AssignTarget, BinaryOp, Expr, ExprData, Ident, Statement, StatementData,
        UnaryOp, UnaryRefOp,
    },
};

use super::{
    ArrayRef, ClosureRef, Context, ExecError, FuncRuntime, Object, ObjectRef, VMState, Value,
    WeakRef, H64,
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
    let mut context = Context {
        infunc,
        vm_state: &mut vm_state,
    };

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
            index_id,
            value_id,
            iterable,
            body,
        } => todo!("Foreach is not implemented"),
        StatementData::Break => return Err(FlowControl::Break(statement.span)),
        StatementData::Continue => return Err(FlowControl::Continue(statement.span)),
        StatementData::Return(val) => {
            return Err(FlowControl::Return(
                statement.span,
                run_expression(context, val)?,
            ))
        }
        StatementData::Yield(val) => todo!("Yield not implemented"),
        StatementData::LocalDec(ident, val) => {
            let val = run_expression(context, val)?;
            context.infunc.locals.insert(ident.0.clone(), val);
        }
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
                run_binary_op(context, &op, target_val, val)?
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
        ExprData::BinaryOp { op, lhs, rhs } => {
            let lhs = run_expression(context, lhs)?;
            let rhs = run_expression(context, rhs)?;
            run_binary_op(context, op, lhs, rhs)
        }
        ExprData::UnaryOp(op, val) => {
            let val = run_expression(context, val)?;
            run_unary_op(context, op, val)
        }
        ExprData::UnaryRefOp(op, val) => run_unary_ref_op(context, op, val),
        ExprData::FunctionCall { func, args } => run_function_call(context, func, args),
        ExprData::ArrayAccess { array, index } => {
            let span = index.span;
            let array = run_expression(context, array)?;
            let index = run_expression(context, index)?;
            run_array_access(context, &array, index, span)
        }
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
            Ok(env
                .and_then(|obj| {
                    let obj = obj.borrow();
                    if obj.is_class_inst {
                        obj.delegate.as_ref().map(|del| Value::Object(del.clone()))
                    } else {
                        None
                    }
                })
                .unwrap_or(Value::Null))
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
    let mut context = Context {
        infunc: rt_func,
        vm_state: context.vm_state,
    };
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
        .closure
        .0
        .env
        .0
        .upgrade()
        .and_then(|env| env.borrow().get_field(&key));
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
        val.clone(),
        Value::Integer(1),
    )?;
    Ok(if return_new { new_val } else { val })
}

fn run_class(
    context: &mut Context,
    parent: Option<&Ident>,
    table_decl: &[(Expr, Expr)],
) -> ExprResult {
    todo!()
}

fn run_table(context: &mut Context, table_decl: &[(Expr, Expr)]) -> ExprResult {
    Ok(ObjectRef::new(Object {
        delegate: None,
        slots: table_decl
            .iter()
            .map(|(key, val)| {
                let key = run_expression(context, key)?;
                let val = run_expression(context, val)?;
                Ok((key, val))
            })
            .collect::<Result<HashMap<_, _>, _>>()?,
        is_class_inst: false,
    })
    .into())
}

fn run_array_access(context: &mut Context, array: &Value, index: Value, span: Span) -> ExprResult {
    match array {
        Value::String(string) => {
            let index = match index {
                Value::Integer(val) => val as usize,
                _ => panic!("Illegal operation"),
            };
            Ok(Value::String(
                string.chars().nth(index).unwrap().to_string(),
            ))
        }
        Value::Object(obj) => obj
            .0
            .borrow()
            .get_field(&index)
            .ok_or_else(|| panic!("Key not in object")),
        Value::Array(arr) => {
            let index = match index {
                Value::Integer(val) => val as usize,
                _ => panic!("Illegal operation"),
            };
            Ok(arr
                .0
                .borrow()
                .get(index)
                .cloned()
                .unwrap_or_else(|| panic!("Array index out of bounds")))
        }
        Value::Weak(weak) => {
            let obj = weak.0.upgrade().expect("Weak reference is null");
            let val = obj
                .borrow()
                .get_field(&index)
                .ok_or_else(|| panic!("Key not in object"));
            val
        }
        _ => panic!("illegal operation"),
    }
}

fn run_unary_op(context: &mut Context, op: &UnaryOp, val: Value) -> ExprResult {
    let res = match op {
        UnaryOp::Neg => match val {
            Value::Float(H64(val)) => Value::float(-val),
            Value::Integer(val) => Value::Integer(-val),
            _ => panic!("Illegal operation"),
        },
        UnaryOp::Not => match val {
            Value::Integer(val) => Value::Integer(if val == 0 { 1 } else { 0 }),
            _ => panic!("Illegal operation"),
        },
        UnaryOp::BitNot => match val {
            Value::Integer(val) => Value::Integer(!val),
            _ => panic!("Illegal operation"),
        },
        UnaryOp::TypeOf => match val {
            Value::Null => Value::String("null".to_string()),
            Value::Integer(_) => Value::String("integer".to_string()),
            Value::Float(_) => Value::String("float".to_string()),
            Value::String(_) => Value::String("string".to_string()),
            Value::Array(_) => Value::String("array".to_string()),
            Value::Function(_) => Value::String("function".to_string()),
            Value::Object(_) => Value::String("object".to_string()),
            _ => todo!(),
        },
        UnaryOp::Clone => match val {
            Value::Object(_) => todo!(),
            Value::Array(_) => todo!(),
            Value::Weak(_) => todo!(),
            other => other.clone(),
        },
        UnaryOp::Resume => todo!("Resume not implemented"),
    };
    Ok(res)
}

macro_rules! promoting_op {
    ($lhs:ident, $rhs:ident, $op:tt) => {
        match ($lhs, $rhs) {
            (Value::Integer($lhs), Value::Integer($rhs)) => Value::Integer($lhs $op $rhs),
            (Value::Float(H64($lhs)), Value::Float(H64($rhs))) => Value::float($lhs $op $rhs),
            (Value::Integer($lhs), Value::Float(H64($rhs))) => Value::float(($lhs as f64) $op $rhs),
            (Value::Float(H64($lhs)), Value::Integer($rhs)) => Value::float($lhs $op ($rhs as f64)),
            _ => panic!("Illegal operation"),
        }
    };
    ($lhs:ident, $rhs:ident, $op:tt, $result:tt) => {
        match ($lhs, $rhs) {
            (Value::Integer($lhs), Value::Integer($rhs)) => Value::$result($lhs $op $rhs),
            (Value::Float(H64($lhs)), Value::Float(H64($rhs))) => Value::$result($lhs $op $rhs),
            (Value::Integer($lhs), Value::Float(H64($rhs))) => Value::$result(($lhs as f64) $op $rhs),
            (Value::Float(H64($lhs)), Value::Integer($rhs)) => Value::$result($lhs $op ($rhs as f64)),
            _ => panic!("Illegal operation"),
        }
    };
}

macro_rules! int_op {
    ($lhs:ident, $rhs:ident, $op:tt) => {
        int_op!($lhs, $rhs, $op, Integer)
    };
    ($lhs:ident, $rhs:ident, $op:tt, $result:tt) => {
        match ($lhs, $rhs) {
            (Value::Integer($lhs), Value::Integer($rhs)) => Value::$result($lhs $op $rhs),
            _ => panic!("Illegal operation"),
        }
    };
}

fn run_binary_op(context: &mut Context, op: &BinaryOp, lhs: Value, rhs: Value) -> ExprResult {
    let res = match op {
        BinaryOp::Add => promoting_op!(lhs, rhs, +),
        BinaryOp::Sub => promoting_op!(lhs, rhs, -),
        BinaryOp::Mul => promoting_op!(lhs, rhs, *),
        BinaryOp::Div => promoting_op!(lhs, rhs, /),
        BinaryOp::Mod => int_op!(lhs, rhs, %),
        // TODO: Comparing non-numbers for equality (classes, arrays, functions, etc)
        BinaryOp::Eq => promoting_op!(lhs, rhs, ==, boolean),
        BinaryOp::Greater => promoting_op!(lhs, rhs, >, boolean),
        BinaryOp::Less => promoting_op!(lhs, rhs, <, boolean),
        BinaryOp::Compare => promoting_op!(lhs, rhs, -),
        BinaryOp::And => {
            if !lhs.truthy() {
                lhs
            } else {
                rhs
            }
        }
        BinaryOp::Or => {
            if lhs.truthy() {
                lhs
            } else {
                rhs
            }
        }
        BinaryOp::BitAnd => int_op!(lhs, rhs, &),
        BinaryOp::BitOr => int_op!(lhs, rhs, |),
        BinaryOp::BitXor => int_op!(lhs, rhs, ^),
        BinaryOp::Shl => int_op!(lhs, rhs, <<),
        BinaryOp::Shr => match (lhs, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => {
                Value::Integer(((lhs as u64) >> rhs) as i64)
            }
            _ => panic!("Illegal operation"),
        },
        BinaryOp::AShr => int_op!(lhs, rhs, >>),
        BinaryOp::In => todo!("In is not implemented"),
        BinaryOp::InstanceOf => todo!("Instanceof is not implemented"),
    };
    Ok(res)
}

fn get_assign_target(context: &mut Context, target: &AssignTarget) -> ExprResult {
    match target {
        AssignTarget::Ident(ident) => run_load_ident(context, &ident.0, ident.1),
        AssignTarget::ArrayAccess { array, index, span } => {
            let array = run_expression(context, array)?;
            let index = run_expression(context, index)?;
            run_array_access(context, &array, index, *span)
        }
        AssignTarget::FieldAccess(target, field) => {
            let target = run_expression(context, target)?;
            run_field_access(context, &target, &field.0, field.1)
        }
    }
}

fn run_assign(
    context: &mut Context,
    target: &AssignTarget,
    val: Value,
    is_newslot: bool,
) -> Result<(), ExecError> {
    match target {
        AssignTarget::Ident(ident) => {
            if let Some(val) = context.infunc.locals.get_mut(&ident.0) {
                assert!(is_newslot, "Can't create a local slot");
                *val = val.clone();
            }
            if is_newslot {
                if let Some(env) = context.infunc.closure.0.env.0.upgrade() {
                    env.borrow_mut()
                        .slots
                        .insert(Value::String(ident.0.clone()), val);
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
        }
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
    use super::*;
    use crate::{parser::parse, test_foreach};

    test_foreach!(sample_test);

    fn sample_test(file_name: &str, file_contents: &str) {
        let actual_ast = match parse(file_contents, file_name.to_string()) {
            Ok(ast) => ast,
            Err(err) => panic!("{}", err),
        };

        let result = match run(&actual_ast) {
            Ok(val) => val,
            Err(err) => panic!("{:?}", err),
        };

        // let expect_ast = exchange_data("parse", file_name, &actual_ast);
        // TODO: Have a more useful comparison for these trees
        // assert_eq!(actual_ast, expect_ast);
    }
}
