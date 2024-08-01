use serde::{Deserialize, Serialize};

use crate::{
    context::Span,
    impl_sub_inst,
    vm::{
        error::{ExecError, ExecResult},
        runtime::VMState,
        value::{HashValue, SetFieldError, TypeName, Value},
    },
};

use super::{
    context::{
        BinaryOpContext, GetFieldContext, GetIdentContext, SetFieldContext, SetIdentContext,
    },
    Const, FunIdx, Inst, InstCtx, Local, Reg, SubInst, SubInstGetContext, Tag,
};

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
        InstGet::Getc(_) => state.frame().get_env().clone(),
        InstGet::Getf(Getf(reg)) | InstGet::IsIn(IsIn(reg)) => state.frame().get_reg(reg).clone(),
        InstGet::Getfc(_) => state.take_acc(),
    };

    let field = match inst {
        InstGet::Getc(Getc(constant)) | InstGet::Getfc(Getfc(constant)) => {
            state.frame().get_func().get_constant(constant).clone()
        }
        InstGet::Getf(_) | InstGet::IsIn(_) => state.take_acc(),
    };

    let field = match HashValue::try_from(field) {
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
    let value = state.take_acc();

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

    Ok(())
}
