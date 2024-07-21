use crate::parser::ast::{self, BinaryOp, Expr, ExprData, Statement, StatementData, UnaryOp};

use super::bytecode::Inst;

struct Function {
    code: Vec<Inst>,
    constants: Vec<ast::Literal>,
    varargs: bool,
    num_params: u32,
    locals: Vec<(String, u32)>,
    sub_functions: Vec<Function>,
}

#[derive(Default)]
struct FunctionBuilder {
    blocks: Vec<Vec<Inst>>,
    constants: Vec<ast::Literal>,
    functions: Vec<Function>,
    next_register: u32,
}

impl FunctionBuilder {
    fn block(&mut self) -> Block {
        let block_idx = self.blocks.len().try_into().unwrap();
        self.blocks.push(vec![]);
        Block(block_idx)
    }

    fn literal(&mut self, lit: &ast::Literal) -> Const {
        let idx = self.constants.len().try_into().unwrap();
        self.constants.push(lit.clone());
        Const(idx)
    }

    fn ident(&mut self, ident: &str) -> Const {
        let idx = self.constants.len().try_into().unwrap();
        self.constants.push(ast::Literal::String(ident.to_string()));
        Const(idx)
    }

    fn function(&mut self, fun: Function) -> FunIdx {
        let idx = self.functions.len().try_into().unwrap();
        self.functions.push(fun);
        FunIdx(idx)
    }

    fn reg(&mut self) -> Reg {
        let reg = self.next_register;
        self.next_register += 1;
        Reg(reg)
    }

    fn regs(&mut self, count: u32) -> impl Iterator<Item = Reg> {
        let reg = self.next_register;
        self.next_register += count;
        (reg..reg + count).map(Reg)
    }

    fn build(self) -> Function {
        todo!()
    }
}

macro_rules! newtype {
    ($name:ident) => {
        #[derive(Clone, Copy)]
        pub struct $name(u32);

        impl From<$name> for u32 {
            fn from(wrapper: $name) -> u32 {
                wrapper.0
            }
        }
    };
}

newtype!(Const);
newtype!(Reg);
newtype!(FunIdx);

pub struct Block(u32);

impl From<&Block> for u32 {
    fn from(wrapper: &Block) -> u32 {
        wrapper.0
    }
}
impl Block {
    fn inst(&self, fun: &mut FunctionBuilder, inst: Inst) {
        fun.blocks[self.0 as usize].push(inst);
    }

    fn insts(&self, fun: &mut FunctionBuilder, insts: (Inst, Option<Inst>)) {
       self.inst(fun, insts.0);
       if let Some(inst2) = insts.1 {
           self.inst(fun, inst2);
       }
    }
}

fn compile_function(
    function: &ast::Function,
) -> Function {
    let mut builder = FunctionBuilder::default();
    let block = builder.block();
    compile_statement(&mut builder, block, &function.body);
    todo!()
}

// --------------
//   Statements
// --------------

#[must_use]
fn compile_statement(fun: &mut FunctionBuilder, block: Block, body: &Statement) -> Block {
    match &body.data {
        StatementData::Block(stmts) => stmts.iter().fold(block, |block, body| compile_statement(fun, block, body)),
        StatementData::Expr(expr) => compile_expr(fun, block, expr),
        StatementData::IfElse(cond, ifbody, elsebody) => compile_ifelse(fun, block, cond, ifbody, elsebody),
        StatementData::While(cond, body) => compile_while(fun, block, cond, body, false),
        StatementData::DoWhile(cond, body) => compile_while(fun, block, cond, body, true),
        StatementData::Switch(_, _, _) => todo!(),
        StatementData::For { init, cond, incr, body } => todo!(),
        StatementData::Foreach { index_idx, value_idx, iterable, body } => todo!(),
        StatementData::Break => todo!(),
        StatementData::Continue => todo!(),
        StatementData::Return(_) => todo!(),
        StatementData::Yield(_) => todo!(),
        StatementData::TryCatch(_, _, _) => todo!(),
        StatementData::Throw(_) => todo!(),
        StatementData::Const(_, _) => todo!(),
        StatementData::Empty => block,
    }
}

#[must_use]
fn compile_while(fun: &mut FunctionBuilder, block: Block, cond: &Expr, body: &Statement, is_do_while: bool) -> Block {
    let cond_block = fun.block();
    let body_block = fun.block();
    let end_block = fun.block();

    let return_to_cond = Inst::jmp(&cond_block);

    // Previous Block
    block.inst(fun, Inst::jmp(if is_do_while { &body_block } else { &cond_block }));

    // Condition Block
    let end_cond_block = compile_expr(fun, cond_block, cond);
    end_cond_block.inst(fun, Inst::jt(&body_block));
    end_cond_block.inst(fun, Inst::jmp(&end_block));

    // Body Block
    let end_body_block = compile_statement(fun, body_block, body);
    end_body_block.inst(fun, return_to_cond);

    end_block
}

#[must_use]
fn compile_ifelse(fun: &mut FunctionBuilder, block: Block, cond: &Expr, ifbody: &Statement, elsebody: &Statement) -> Block {
    let if_block = fun.block();
    let else_block = fun.block();
    let end_block = fun.block();

    // Previous Block
    let end_cond_expr = compile_expr(fun, block, cond);
    end_cond_expr .inst(fun, Inst::jt(&if_block));
    end_cond_expr .inst(fun, Inst::jmp(&else_block));

    // If Block
    let end_if_block = compile_statement(fun, if_block, ifbody);
    end_if_block.inst(fun, Inst::jmp(&end_block));

    // Else Block
    let end_else_block = compile_statement(fun, else_block, elsebody);
    end_else_block.inst(fun, Inst::jmp(&end_block));

    end_block
}

// ---------------
//   Expressions
// ---------------

#[must_use]
fn compile_expr(fun: &mut FunctionBuilder, block: Block, expr: &Expr) -> Block {
    match &expr.data {
        ExprData::Literal(lit) => compile_literal(fun, block, lit),
        ExprData::TableDecl(_) => todo!(),
        ExprData::ArrayDecl(_) => todo!(),
        ExprData::FunctionDef(ast_fun) => {
            let fun_idx = fun.function(compile_function(ast_fun));
            block.inst(fun, Inst::loadf(fun_idx));
            block
        },
        ExprData::ClassDef { parent, members } => todo!(),
        ExprData::Assign(assign) => compile_assign(fun, block, assign),
        ExprData::Ternary { cond, true_expr, false_expr } => todo!(),
        ExprData::BinaryOp { op, op_span, lhs, rhs } => compile_binary_op(fun, block, op, lhs, rhs),
        ExprData::UnaryOp(op, _span, expr) => compile_unary_op(fun, block, op, expr),
        ExprData::UnaryRefOp(_, _, _) => todo!(),
        ExprData::FunctionCall { func, args } => compile_fn_call(fun, block, func, args),
        ExprData::ArrayAccess { array, index } => todo!(),
        ExprData::This => {
            block.inst(fun, Inst::this());
            block
        },
        ExprData::FieldAccess(parent, field) => {
            let field_const = fun.ident(&field.0);
            let block = compile_expr(fun, block, parent);
            block.inst(fun, Inst::getfc(field_const));
            block
        },
        ExprData::Globals => {
            block.inst(fun, Inst::root());
            block
        },
        ExprData::Ident(ident) => {
            let ident = fun.ident(&ident);
            block.inst(fun, Inst::getc(ident));
            block
        },
        ExprData::Base => todo!(),
        ExprData::RawCall { func, this, args } => compile_raw_call(fun, block, func, this, args),
        ExprData::Local(idx, _span) => {
            block.inst(fun, Inst::loadl(*idx));
            block
        },
    }
}

fn compile_unary_op(fun: &mut FunctionBuilder, block: Block, op: &ast::UnaryOp, expr: &Expr) -> Block {
    let block = compile_expr(fun, block, expr);
    let inst = match op {
        UnaryOp::Neg => Inst::neg(),
        UnaryOp::Not => Inst::lnot(),
        UnaryOp::BitNot => Inst::bnot(),
        UnaryOp::TypeOf => todo!("TypeOf will be an instrinsic"),
        UnaryOp::Clone => todo!("Clone will be an instrinsic"),
        UnaryOp::Resume => todo!("Resume will be an instrinsic"),
    };
    block.inst(fun, inst);
    block
}

fn compile_binary_op(fun: &mut FunctionBuilder, block: Block, op: &ast::BinaryOp, lhs: &Expr, rhs: &Expr) -> Block {
    let lhs_reg = fun.reg();
    let block = compile_expr(fun, block, lhs);
    block.inst(fun, Inst::storr(lhs_reg));

    if matches!(op, BinaryOp::And | BinaryOp::Or) {
        let end_block = fun.block();
        let decision_inst = match op {
            BinaryOp::And => Inst::jf(&end_block),
            BinaryOp::Or => Inst::jt(&end_block),
            _ => unreachable!(),
        };
        block.inst(fun, decision_inst);
        let block = compile_expr(fun, block, rhs);
        block.inst(fun, Inst::jmp(&end_block));
        return end_block;
    }

    let block = compile_expr(fun, block, rhs);

    let inst = match op {
        BinaryOp::Add => Inst::add(lhs_reg),
        BinaryOp::Sub => Inst::sub(lhs_reg),
        BinaryOp::Mul => Inst::mul(lhs_reg),
        BinaryOp::Div => Inst::div(lhs_reg),
        BinaryOp::Mod => Inst::modu(lhs_reg),
        BinaryOp::Eq => Inst::iseq(lhs_reg),
        BinaryOp::NotEq => Inst::isne(lhs_reg),
        BinaryOp::Greater => Inst::isgt(lhs_reg),
        BinaryOp::GreaterEq => Inst::isge(lhs_reg),
        BinaryOp::Less => Inst::islt(lhs_reg),
        BinaryOp::LessEq => Inst::isle(lhs_reg),
        BinaryOp::Compare => Inst::cmp(lhs_reg),
        BinaryOp::And => unreachable!(),
        BinaryOp::Or => unreachable!(),
        BinaryOp::BitAnd => Inst::band(lhs_reg),
        BinaryOp::BitOr => Inst::bor(lhs_reg),
        BinaryOp::BitXor => Inst::bxor(lhs_reg),
        BinaryOp::Shl => {
            block.inst(fun, Inst::brsh(lhs_reg));
            Inst::neg()
        },
        BinaryOp::Shr => Inst::brsh(lhs_reg),
        BinaryOp::AShr => Inst::bash(lhs_reg),
        BinaryOp::In => Inst::isin(lhs_reg),
        BinaryOp::InstanceOf => todo!("InstanceOf will be an instrinsic"),
    };

    block.inst(fun, inst);
    block
}

fn compile_raw_call(fun: &mut FunctionBuilder, block: Block, func: &Expr, this: &Expr, args: &[Expr]) -> Block {
    let fun_reg = fun.reg();
    let mut arg_regs = fun.regs((args.len() + 1).try_into().unwrap());
    let this_reg = arg_regs.next().unwrap();

    let block = compile_expr(fun, block, func);
    block.inst(fun, Inst::storr(fun_reg));
    let block = compile_expr(fun, block, this);
    block.inst(fun, Inst::storr(this_reg));
    let block = load_call_args(fun, block, arg_regs, args);

    block.inst(fun, Inst::loadr(fun_reg));
    block.insts(fun, Inst::call(this_reg, args.len().try_into().unwrap()));

    block
}

fn compile_fn_call(fun: &mut FunctionBuilder, block: Block, func: &ast::CallTarget, args: &[Expr]) -> Block {
    let fun_reg = fun.reg();
    let mut arg_regs = fun.regs((args.len() + 1).try_into().unwrap());
    let this_reg = arg_regs.next().unwrap();

    // This Object
    let block = match func {
        ast::CallTarget::Expr(fn_expr) => {
            block.inst(fun, Inst::this());
            block.inst(fun, Inst::storr(this_reg));
            let block = compile_expr(fun, block, fn_expr);
            block.inst(fun, Inst::storr(fun_reg));
            block
        },
        ast::CallTarget::FieldAccess(env, fn_field) => {
            // TODO: double check if parent["field"] should use parent as env
            let block = compile_expr(fun, block, env);
            block.inst(fun, Inst::storr(this_reg));
            let fn_field_const = fun.ident(&fn_field.0);
            block.inst(fun, Inst::getfc(fn_field_const));
            block.inst(fun, Inst::storr(fun_reg));
            block
        },
    };

    let block = load_call_args(fun, block, arg_regs, args);

    block.inst(fun, Inst::loadr(fun_reg));
    block.insts(fun, Inst::call(this_reg, args.len().try_into().unwrap()));
    block
}

fn load_call_args(fun: &mut FunctionBuilder, block: Block, arg_regs: impl Iterator<Item = Reg>, args: &[Expr]) -> Block {
    args.iter().zip(arg_regs).fold(block, |block, (arg, reg)| {
        let block = compile_expr(fun, block, arg);
        block.inst(fun, Inst::storr(reg));
        block
    })
}

#[must_use]
fn compile_assign(fun: &mut FunctionBuilder, block: Block, assign: &ast::Assign) -> Block {
    let is_ns = match assign.kind {
        ast::AssignKind::Normal => false,
        ast::AssignKind::NewSlot => true,
        ast::AssignKind::Mult => todo!(),
        ast::AssignKind::Div => todo!(),
        ast::AssignKind::Add => todo!(),
        ast::AssignKind::Sub => todo!(),
    };

    match &assign.target {
        ast::AssignTarget::Ident(ident) => {
            let field = fun.ident(&ident.0);
            let block = compile_expr(fun, block, &assign.value);
            block.inst(fun, Inst::setc(field, is_ns));
            block
        },
        ast::AssignTarget::ArrayAccess { array, index, span } => {
            let parent = fun.reg();
            let field = fun.reg();
            let block = compile_expr(fun, block, array);
            block.inst(fun, Inst::storr(parent));
            let block = compile_expr(fun, block, index);
            block.inst(fun, Inst::storr(field));
            let block = compile_expr(fun, block, &assign.value);
            block.inst(fun, Inst::setf(parent, is_ns));
            block
        },
        ast::AssignTarget::FieldAccess(parent, field) => {
            let parent_reg = fun.reg();
            let field_reg = fun.reg();
            let field_const = fun.ident(&field.0);
            let block = compile_expr(fun, block, parent);
            block.inst(fun, Inst::storr(parent_reg));
            block.inst(fun, Inst::loadc(field_const));
            block.inst(fun, Inst::storr(field_reg));
            let block = compile_expr(fun, block, &assign.value);
            block.inst(fun, Inst::setf(parent_reg, is_ns));
            block
        },
        ast::AssignTarget::Local(local_idx, _) => {
            assert!(!is_ns, "Cannot use new slot with local variable");
            let block = compile_expr(fun, block, &assign.value);
            block.inst(fun, Inst::storl(*local_idx));
            block
        },
    }
}

#[must_use]
fn compile_literal(fun: &mut FunctionBuilder, block: Block, lit: &ast::Literal) -> Block {
    let lit_idx = fun.literal(lit);
    block.inst(fun, Inst::loadc(lit_idx));
    block
}