use serde::{Deserialize, Serialize};

use crate::{
    context::{
        BinaryOpContext, GetFieldContext, GetIdentContext, SetFieldContext, SetIdentContext,
    },
    impl_sub_inst,
    sq_error::Span,
    vm::{
        compiler::{self, FormatInst},
        error::{ExecError, ExecResult},
        runtime::VMState,
        value::{HashValue, SetFieldError, TypeName, Value},
    },
};

use super::{Const, FunIdx, Inst, InstCtx, Local, Reg, SubInst, SubInstGetContext, Tag};

macro_rules! impl_sub_sub_inst {
    ($par_var:ident $parent:ident::$var:ident($name:ident $(($($field:ty),+))?); $ctx:ty) => {
        #[derive(Clone, Copy, Debug, Serialize, Deserialize)]
        pub struct $name $(($($field),+))?;

        impl SubInstGetContext for $name {
            type Context = $ctx;
            fn get_ctx(inst: &InstCtx) -> Option<&Self::Context> {
                match inst {
                    InstCtx::$par_var($parent::$var(.., ctx)) => Some(ctx),
                    _ => None,
                }
            }
        }
    };
}

macro_rules! impl_sub_inst {
    ($par_var:ident $with_ctx:ident/$wo_ctx:ident {
        $($variant:ident($data:ident $(($($field:ty),+))?, $ctx:ty)),+ $(,)?
    }) => {
        $(
            impl_sub_sub_inst!($par_var $with_ctx::$variant($data $(($($field),+))?); $ctx);
        )+
        #[derive(Clone, Debug, Serialize, Deserialize)]
        pub enum $with_ctx {
            $($variant($data, $ctx)),+
        }
        #[derive(Clone, Copy, Debug, Serialize, Deserialize)]
        pub enum $wo_ctx {
            $($variant($data)),+
        }
        impl $with_ctx {
            pub fn strip(&self) -> $wo_ctx {
                match self {
                    $(Self::$variant(data, _) => $wo_ctx::$variant(*data)),+
                }
            }
        }
    }
}

impl_sub_inst!(Get InstGetCtx/InstGet {
    Getc(Getc(Const), GetIdentContext),
    Getf(Getf(Reg), GetFieldContext),
    Getfc(Getfc(Const), GetFieldContext),
    IsIn(IsIn(Reg), BinaryOpContext),
});

impl FormatInst for InstGet {
    fn fmt_inst(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        fun: &compiler::Function,
    ) -> std::fmt::Result {
        match self {
            InstGet::Getc(Getc(constant)) => write!(f, "{:5} {}", "get", constant.fmt_inst(fun)),
            InstGet::Getf(Getf(reg)) => write!(f, "{:5} {}", "getf", reg),
            InstGet::Getfc(Getfc(constant)) => write!(f, "{:5} {}", "getf", constant.fmt_inst(fun)),
            InstGet::IsIn(IsIn(reg)) => write!(f, "{:5} {}", "isin", reg),
        }
    }
}

impl InstGetCtx {
    pub fn get_span(&self) -> Span {
        match self {
            InstGetCtx::Getc(_, ctx) => ctx.get_span(),
            InstGetCtx::Getf(_, ctx) => ctx.get_span(),
            InstGetCtx::Getfc(_, ctx) => ctx.get_span(),
            InstGetCtx::IsIn(_, ctx) => ctx.get_span(),
        }
    }
}

impl InstCtx {
    pub fn getc(constant: Const, ctx: GetIdentContext) -> Self {
        Self::Get(InstGetCtx::Getc(Getc(constant), ctx))
    }

    pub fn getf(reg_idx: Reg, ctx: GetFieldContext) -> Self {
        Self::Get(InstGetCtx::Getf(Getf(reg_idx), ctx))
    }

    pub fn getfc(constant: Const, ctx: GetFieldContext) -> Self {
        Self::Get(InstGetCtx::Getfc(Getfc(constant), ctx))
    }

    pub fn isin(reg_idx: Reg, ctx: BinaryOpContext) -> Self {
        Self::Get(InstGetCtx::IsIn(IsIn(reg_idx), ctx))
    }
}

pub fn run_get(state: &mut VMState, inst: InstGet) -> ExecResult {
    let parent = match inst {
        InstGet::Getc(_) => state.frame().get_env(),
        InstGet::Getf(Getf(reg)) | InstGet::IsIn(IsIn(reg)) => state.frame().get_reg(reg),
        InstGet::Getfc(_) => state.get_acc(),
    };

    let field = match inst {
        InstGet::Getc(Getc(constant)) | InstGet::Getfc(Getfc(constant)) => {
            state.frame().get_func().get_constant(constant)
        }
        InstGet::Getf(_) | InstGet::IsIn(_) => state.get_acc(),
    };

    // TODO: Don't need to clone here?
    let field = match HashValue::try_from(field.clone()) {
        Ok(hv) => hv,
        Err(nhv) => match inst {
            InstGet::Getc(g) => panic!("Getc should be called with a string constant"),
            InstGet::Getf(g) => return Err(state.get_context(g).unhashable_type(nhv)),
            InstGet::Getfc(g) => return Err(state.get_context(g).unhashable_type(nhv)),
            InstGet::IsIn(_) => {
                // Just return false
                state.set_acc(Value::Boolean(false));
                return Ok(());
            }
        },
    };

    let value = match (inst, parent.get_field(&field)) {
        (InstGet::Getc(g), None) => return Err(state.get_context(g).undefined_variable()),
        (InstGet::Getf(g), None) => return Err(state.get_context(g).undefined_field(field.into())),
        (InstGet::Getfc(g), None) => {
            return Err(state.get_context(g).undefined_field(field.into()))
        }
        (InstGet::IsIn(g), option) => Value::boolean(option.is_some()),
        (_, Some(val)) => val,
    };

    state.set_acc(value);

    Ok(())
}

impl_sub_inst!(Set InstSetCtx/InstSet {
    Setc(Setc(Const, bool), SetIdentContext),
    Setf(Setf(Reg, bool), SetFieldContext),
});

impl FormatInst for InstSet {
    fn fmt_inst(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        fun: &compiler::Function,
    ) -> std::fmt::Result {
        match self {
            InstSet::Setc(Setc(constant, is_ns)) => {
                write!(f, "{:5} {} ns:{}", "set", constant.fmt_inst(fun), is_ns)
            }
            InstSet::Setf(Setf(reg, is_ns)) => write!(f, "{:5} {} ns:{}", "set", reg, is_ns),
        }
    }
}

impl InstSetCtx {
    pub fn get_span(&self) -> Span {
        match self {
            InstSetCtx::Setc(_, ctx) => ctx.get_span(),
            InstSetCtx::Setf(_, ctx) => ctx.get_span(),
        }
    }
}

impl InstCtx {
    pub fn setf(reg_idx: Reg, ctx: SetFieldContext, is_ns: bool) -> Self {
        Self::Set(InstSetCtx::Setf(Setf(reg_idx, is_ns), ctx))
    }

    pub fn setc(constant: Const, ctx: SetIdentContext, is_ns: bool) -> Self {
        Self::Set(InstSetCtx::Setc(Setc(constant, is_ns), ctx))
    }
}

fn run_set_get_hashable(
    state: &mut VMState,
    inst: InstSet,
    value: Value,
) -> Result<HashValue, ExecError> {
    match HashValue::try_from(value) {
        Ok(hv) => Ok(hv),
        Err(nhv) => match inst {
            InstSet::Setc(_) => panic!("Setc should be called with a string constant"),
            InstSet::Setf(s) => Err(state.get_context(s).unhashable_type(nhv)),
        },
    }
}

pub fn run_set(state: &mut VMState, inst: InstSet) -> ExecResult {
    let parent = match inst {
        InstSet::Setc(_) => state.frame().get_env().clone(),
        InstSet::Setf(Setf(reg, _is_ns)) => state.frame().get_reg(reg).clone(),
    };

    let field = match inst {
        InstSet::Setc(Setc(constant, _is_ns)) => {
            state.frame().get_func().get_constant(constant).clone()
        }
        InstSet::Setf(Setf(reg, _is_ns)) => state.frame().get_reg(reg.nth(1)).clone(),
    };

    let is_newslot = match inst {
        InstSet::Setc(Setc(_, is_ns)) => is_ns,
        InstSet::Setf(Setf(_, is_ns)) => is_ns,
    };

    // TODO: Need to clone here perhaps?
    let value = state.get_acc().clone();

    let res = match (parent, field) {
        (Value::Array(array), Value::Integer(index)) => {
            match array.borrow_mut().get_mut(index as usize) {
                Some(r) => {
                    *r = value;
                    Ok(())
                }
                None => {
                    return Err(match inst {
                        InstSet::Setc(g) => panic!("Setc should be called with a string constant"),
                        InstSet::Setf(g) => state
                            .get_context(g)
                            .index_out_of_bounds(index, array.borrow().len()),
                    })
                }
            }
        }
        (Value::Array(array), other) => {
            return Err(match inst {
                InstSet::Setc(g) => panic!("Setc should be called with a string constant"),
                InstSet::Setf(g) => state
                    .get_context(g)
                    .wrong_index_type(<i64 as TypeName>::type_name(), other),
            })
        }
        (Value::Table(table), field) => table.borrow_mut().set_field(
            run_set_get_hashable(state, inst, field)?,
            value,
            is_newslot,
        ),
        (Value::Class(class), field) => class.borrow_mut().set_field(
            run_set_get_hashable(state, inst, field)?,
            value,
            is_newslot,
        ),
        (Value::Instance(ins), field) => {
            ins.borrow_mut()
                .set_field(run_set_get_hashable(state, inst, field)?, value, is_newslot)
        }
        (other, _) => {
            return Err(match inst {
                InstSet::Setc(g) => state.get_context(g).cannot_modify_type(other),
                InstSet::Setf(g) => state.get_context(g).cannot_modify_type(other),
            })
        }
    };

    match res {
        Ok(()) => Ok(()),
        Err(SetFieldError::UndefinedField(field)) => Err(match inst {
            InstSet::Setc(g) => state.get_context(g).undefined_variable(),
            InstSet::Setf(g) => state.get_context(g).undefined_field(field),
        }),
        Err(SetFieldError::MutatingInstantiatedClass) => Err(match inst {
            InstSet::Setc(g) => state.get_context(g).mutating_instantiated_class(),
            InstSet::Setf(g) => state.get_context(g).mutating_instantiated_class(),
        }),
    }
}

impl_sub_inst!(Del InstDelCtx/InstDel {
    Delc(Delc(Const), SetIdentContext),
    Delf(Delf(Reg), SetFieldContext),
    Delfc(Delfc(Const), SetFieldContext),
});

impl FormatInst for InstDel {
    fn fmt_inst(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        fun: &compiler::Function,
    ) -> std::fmt::Result {
        match self {
            InstDel::Delc(Delc(constant)) => {
                write!(f, "{:5} {}", "delc", constant.fmt_inst(fun))
            }
            InstDel::Delf(Delf(reg)) => write!(f, "{:5} {}", "delf", reg),
            InstDel::Delfc(Delfc(constant)) => {
                write!(f, "{:5} {}", "delf", constant.fmt_inst(fun))
            }
        }
    }
}

impl InstDelCtx {
    pub fn get_span(&self) -> Span {
        match self {
            InstDelCtx::Delc(_, ctx) => ctx.get_span(),
            InstDelCtx::Delf(_, ctx) => ctx.get_span(),
            InstDelCtx::Delfc(_, ctx) => ctx.get_span(),
        }
    }
}

impl InstCtx {
    pub fn delc(constant: Const, ctx: SetIdentContext) -> Self {
        Self::Del(InstDelCtx::Delc(Delc(constant), ctx))
    }

    pub fn delf(reg: Reg, ctx: SetFieldContext) -> Self {
        Self::Del(InstDelCtx::Delf(Delf(reg), ctx))
    }

    pub fn delfc(constant: Const, ctx: SetFieldContext) -> Self {
        Self::Del(InstDelCtx::Delfc(Delfc(constant), ctx))
    }
}

pub fn run_del(state: &mut VMState, inst: InstDel) -> ExecResult {
    let parent = match inst {
        InstDel::Delc(_) => state.frame().get_env(),
        InstDel::Delf(Delf(reg)) => state.frame().get_reg(reg),
        InstDel::Delfc(_) => state.get_acc(),
    };

    let field = match inst {
        InstDel::Delc(Delc(constant)) | InstDel::Delfc(Delfc(constant)) => {
            state.frame().get_func().get_constant(constant)
        }
        InstDel::Delf(_) => state.get_acc(),
    };

    // TODO: Don't need to clone here?
    let field = match HashValue::try_from(field.clone()) {
        Ok(hv) => hv,
        Err(nhv) => match inst {
            InstDel::Delc(g) => panic!("Delc should be called with a string constant"),
            InstDel::Delf(g) => return Err(state.get_context(g).unhashable_type(nhv)),
            InstDel::Delfc(g) => return Err(state.get_context(g).unhashable_type(nhv)),
        },
    };

    let res = match (parent, field) {
        (Value::Table(table), field) => table.borrow_mut().del_field(field),
        (other, _) => {
            return Err(match inst {
                InstDel::Delc(g) => state.get_context(g).cannot_modify_type(other.clone()),
                InstDel::Delf(g) => state.get_context(g).cannot_modify_type(other.clone()),
                InstDel::Delfc(g) => state.get_context(g).cannot_modify_type(other.clone()),
            })
        }
    };

    match res {
        Ok(value) => {
            state.set_acc(value);
            Ok(())
        }
        Err(SetFieldError::UndefinedField(field)) => match inst {
            InstDel::Delc(g) => Err(state.get_context(g).undefined_variable()),
            InstDel::Delf(g) => Err(state.get_context(g).undefined_field(field)),
            InstDel::Delfc(g) => Err(state.get_context(g).undefined_field(field)),
        },
        Err(SetFieldError::MutatingInstantiatedClass) => match inst {
            InstDel::Delc(g) => Err(state.get_context(g).mutating_instantiated_class()),
            InstDel::Delf(g) => Err(state.get_context(g).mutating_instantiated_class()),
            InstDel::Delfc(g) => Err(state.get_context(g).mutating_instantiated_class()),
        },
    }
}
