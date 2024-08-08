use std::{any::Any, fmt};

use logos::source;
use serde::{Deserialize, Serialize};

use crate::{
    context::{
        BinaryOpContext, FnCallContext, GetFieldContext, GetIdentContext, NewClassContext,
        SetFieldContext, SetIdentContext, UnaryOpContext,
    },
    parser::ast::{
        self, AssignTarget, BinaryOp, Expr, ExprData, Statement, StatementData, UnaryOp,
    },
    sq_error::Span,
    vm::bytecode::{InstJump, InstJumpCtx, JumpKind},
};

use super::bytecode::{Block, Const, FunIdx, Inst, InstCtx, Local, PrimType, Reg, Tag};

pub trait FormatInst {
    fn fmt_inst(&self, f: &mut fmt::Formatter<'_>, fun: &Function) -> fmt::Result;
}

pub struct ConstFmt<'a>(Const, &'a Function);

impl<'a> fmt::Display for ConstFmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "consts[{}]({})",
            self.0.as_idx(),
            self.1.constants[self.0.as_idx()]
        )
    }
}

impl Const {
    pub fn fmt_inst<'a>(&self, fun: &'a Function) -> ConstFmt<'a> {
        ConstFmt(*self, fun)
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "r[{}]", self.as_idx())
    }
}

pub struct LocalFmt<'a>(Local, &'a Function);

impl<'a> fmt::Display for LocalFmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Show local name
        write!(f, "l[{}]", self.0.as_idx())
    }
}

impl Local {
    pub fn fmt_inst<'a>(&self, fun: &'a Function) -> LocalFmt<'a> {
        LocalFmt(*self, fun)
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "block[{}]", self.as_idx())
    }
}

impl fmt::Display for FunIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fun[{}]", self.as_idx())
    }
}

#[derive(Serialize, Deserialize)]
pub struct Function {
    pub code: Vec<InstCtx>,
    pub block_offsets: Vec<u32>,
    pub constants: Vec<ast::Literal>,
    pub is_varargs: bool,
    pub num_regs: u32,
    pub num_params: u32,
    pub locals: Vec<(String, u32)>,
    pub num_locals: u32,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct File {
    pub file_name: String,
    pub source: String,
    pub code: Function,
}

impl Function {
    fn write(&self, f: &mut fmt::Formatter<'_>, source_file: Option<&File>) -> fmt::Result {
        write!(f, "Function(",)?;
        let mut arg_iter = self.locals[..self.num_params as usize]
            .iter()
            .map(|(name, _)| name.as_str())
            .peekable();
        while let Some(arg_name) = arg_iter.next() {
            write!(f, "{}", arg_name)?;
            if arg_iter.peek().is_some() {
                write!(f, ", ")?;
            }
        }

        if self.is_varargs {
            if self.num_params > 0 {
                write!(f, ", ...")?;
            } else {
                write!(f, "...")?;
            }
        }

        write!(f, ")")?;

        if f.alternate() {
            write!(f, "\nConstants:")?;
            for (idx, constant) in self.constants.iter().enumerate() {
                write!(f, "\n  {}: {:?}", idx, constant)?;
            }

            let mut locals_sorted = self.locals[self.num_params as usize..]
                .iter()
                .cloned()
                .collect::<Vec<_>>();
            locals_sorted.sort_by_key(|(_, idx)| *idx);
            write!(f, "\nLocals:")?;
            for (local, local_idx) in locals_sorted {
                write!(f, "\n  {}: {}", local_idx, local)?;
            }
            write!(f, "\nRegisters: {}", self.num_regs)?;

            let block_idx_width = self.block_offsets.iter().max().unwrap().to_string().len();
            let mut blocks = self
                .block_offsets
                .iter()
                .map(|offset| *offset as usize)
                .enumerate()
                .collect::<Vec<_>>();
            blocks.sort_by_key(|(_, offset)| *offset);
            let mut block_iter = blocks.into_iter().peekable();
            write!(f, "\nCode:")?;
            let mut start_of_next_line = 0;
            for (offset, inst) in self.code.iter().enumerate() {
                let block_idx = if let Some(&(block_idx, block_offset)) = block_iter.peek() {
                    assert!(
                        block_offset >= offset,
                        "Block offset {} is less than current offset {}",
                        block_offset,
                        offset
                    );
                    if block_offset == offset {
                        block_iter.next();
                        Some(block_idx)
                    } else {
                        None
                    }
                } else {
                    None
                };

                if let Some(source_file) = source_file {
                    let start_offset = inst.get_span().start;
                    if start_offset >= start_of_next_line {
                        let line_end = source_file.source[start_offset..]
                            .find('\n')
                            .map(|end| start_offset + end)
                            .unwrap_or(source_file.source.len());
                        for line in source_file.source[start_of_next_line..line_end].lines() {
                            write!(f, "\n  ; {}", line)?;
                        }
                        start_of_next_line = line_end + 1;
                    };
                }
                if let Some(block_idx) = block_idx {
                    write!(f, "\n  {:width$}: ", block_idx, width = block_idx_width)?;
                } else {
                    write!(f, "\n  {:width$}  ", "", width = block_idx_width)?;
                }
                inst.strip().fmt_inst(f, self)?;
            }
        }

        Ok(())
    }

    pub fn wrap<'a>(&'a self, file: &'a File) -> FunctionDebug<'a> {
        FunctionDebug(self, file)
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write(f, None)
    }
}

pub struct FunctionDebug<'a>(&'a Function, &'a File);

impl<'a> fmt::Debug for FunctionDebug<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.write(f, Some(self.1))
    }
}

struct FunctionBuilder {
    blocks: Vec<Vec<InstCtx>>,
    constants: Vec<ast::Literal>,
    functions: Vec<Function>,
    next_register: u32,
    cont_blocks: Vec<Block>,
    break_blocks: Vec<Block>,
}

struct BlockBuilder {
    block: Block,
}

impl BlockBuilder {
    fn inst(&self, fun: &mut FunctionBuilder, inst: InstCtx) {
        fun.blocks[self.block.as_idx()].push(inst);
    }

    fn insts(&self, fun: &mut FunctionBuilder, insts: (InstCtx, Option<InstCtx>)) {
        self.inst(fun, insts.0);
        if let Some(inst2) = insts.1 {
            self.inst(fun, inst2);
        }
    }

    fn as_block(&self) -> Block {
        self.block
    }
}

impl FunctionBuilder {
    fn new() -> Self {
        Self {
            blocks: Vec::new(),
            constants: Vec::new(),
            functions: Vec::new(),
            next_register: 0,
            cont_blocks: Vec::new(),
            break_blocks: Vec::new(),
        }
    }

    fn enter_loop(&mut self, cont_block: Block, break_block: Block) {
        self.cont_blocks.push(cont_block);
        self.break_blocks.push(break_block)
    }

    fn exit_loop(&mut self) {
        self.cont_blocks
            .pop()
            .expect("Calls to enter_loop don't match calls to exit_loop");
        self.break_blocks
            .pop()
            .expect("Calls to enter_loop don't match calls to exit_loop");
    }

    fn enter_switch(&mut self, break_block: Block) {
        self.break_blocks.push(break_block)
    }

    fn exit_switch(&mut self) {
        self.break_blocks
            .pop()
            .expect("Calls to enter_switch don't match calls to exit_switch");
    }

    fn cont_target(&self) -> Block {
        self.cont_blocks
            .last()
            .expect("No loop to continue")
            .clone()
    }

    fn break_target(&self) -> Block {
        self.break_blocks.last().expect("No loop to break").clone()
    }

    fn block(&mut self) -> BlockBuilder {
        let block_idx: u32 = self.blocks.len().try_into().unwrap();
        self.blocks.push(vec![]);
        BlockBuilder {
            block: block_idx.into(),
        }
    }

    fn literal(&mut self, lit: &ast::Literal) -> Const {
        let idx: u32 = self.constants.len().try_into().unwrap();
        self.constants.push(lit.clone());
        idx.into()
    }

    fn ident(&mut self, ident: &str) -> Const {
        let idx: u32 = self.constants.len().try_into().unwrap();
        self.constants.push(ast::Literal::String(ident.to_string()));
        idx.into()
    }

    fn function(&mut self, fun: Function) -> FunIdx {
        let idx: u32 = self.functions.len().try_into().unwrap();
        self.functions.push(fun);
        idx.into()
    }

    fn reg(&mut self) -> Reg {
        let reg = self.next_register;
        self.next_register += 1;
        reg.into()
    }

    fn regs(&mut self, count: u32) -> impl Iterator<Item = Reg> {
        let reg = self.next_register;
        self.next_register += count;
        (reg..reg + count).map(Reg::from)
    }

    fn build(
        self,
        num_params: u32,
        locals: Vec<(String, u32)>,
        is_varargs: bool,
        num_locals: u32,
    ) -> Function {
        let mut blocks_to_take = self
            .blocks
            .into_iter()
            .map(|block| Some(block))
            .collect::<Vec<_>>();
        let mut code = Vec::new();
        let mut block_offsets = vec![0; blocks_to_take.len()];

        for block_idx in 0..blocks_to_take.len() {
            let mut next_block = match blocks_to_take[block_idx].take() {
                Some(block) => block,
                None => continue,
            };
            let mut next_block_idx = block_idx;

            loop {
                assert!(next_block.len() > 0, "Block {} is empty", next_block_idx);
                block_offsets[next_block_idx] = u32::try_from(code.len()).unwrap();
                code.append(&mut next_block);

                match code.last().unwrap() {
                    InstCtx::Jump(InstJumpCtx {
                        data:
                            InstJump {
                                kind: JumpKind::Always,
                                block,
                            },
                        ..
                    }) => {
                        let target_block = block.as_idx();
                        if target_block <= next_block_idx {
                            continue;
                        }
                        match blocks_to_take[target_block].take() {
                            Some(block) => {
                                code.pop(); // Get rid of jump
                                next_block = block;
                                next_block_idx = target_block;
                            }
                            None => break,
                        }
                    }
                    InstCtx::Ret(_) => break,
                    other => panic!("Block ended with non-jump/return {:?}", other),
                }
            }
        }

        assert_eq!(block_offsets[0], 0);
        assert!(block_offsets[1..].iter().all(|offset| *offset != 0));

        Function {
            code,
            block_offsets,
            constants: self.constants,
            is_varargs,
            num_params,
            num_regs: self.next_register,
            locals,
            num_locals,
            sub_functions: self.functions,
        }
    }
}

pub fn compile(ast: &ast::Function, file_name: String, source: String) -> File {
    File {
        file_name,
        source, // let upvalues = ast_fn
        //     .upvalues
        //     .iter()
        //     .cloned()
        //     .map(|(parent_idx, _this_idx)| parent_rt.locals[parent_idx as usize].clone())
        //     .collect();
        // Closure {
        //     ast_fn: NonNull::from(ast_fn),
        //     default_vals,
        //     root,
        //     env: None,
        //     upvalues,
        // }
        code: compile_function(ast),
    }
}

fn compile_function(function: &ast::Function) -> Function {
    if function.default_expr.len() > 0 {
        // TODO: Default Args
        todo!()
    }
    let mut builder = FunctionBuilder::new();
    let block = builder.block();
    let block = compile_statement(&mut builder, block, &function.body);
    if !matches!(
        builder.blocks[block.block.as_idx()].last(),
        Some(InstCtx::Ret(_))
    ) {
        block.inst(&mut builder, InstCtx::retn(function.body.span));
    }
    builder.build(
        function.num_args,
        function.local_names.clone(),
        function.is_varargs,
        function.num_locals,
    )
}

// --------------
//   Statements
// --------------

fn compile_statement(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    body: &Statement,
) -> BlockBuilder {
    match &body.data {
        StatementData::Block(stmts) => stmts
            .iter()
            .fold(block, |block, body| compile_statement(fun, block, body)),
        StatementData::Expr(expr) => compile_expr(fun, block, expr),
        StatementData::IfElse(cond, ifbody, elsebody) => {
            compile_ifelse(fun, block, cond, ifbody, elsebody)
        }
        StatementData::While {
            while_kw,
            cond,
            body,
            is_do_while,
        } => compile_while(fun, block, cond, body, *is_do_while, *while_kw),
        StatementData::Switch(_, _, _) => todo!(),
        StatementData::For {
            for_kw,
            init,
            cond,
            incr,
            body,
        } => compile_for(fun, block, init, cond, incr, body, *for_kw),
        StatementData::Foreach {
            index_idx,
            value_idx,
            iterable,
            body,
        } => todo!(),
        StatementData::Break => {
            block.inst(
                fun,
                InstCtx::Jump(InstJumpCtx {
                    data: InstJump {
                        kind: JumpKind::Always,
                        block: fun.break_target(),
                    },
                    ctx: body.span,
                }),
            );
            block
        }
        StatementData::Continue => {
            block.inst(
                fun,
                InstCtx::Jump(InstJumpCtx {
                    data: InstJump {
                        kind: JumpKind::Always,
                        block: fun.cont_target(),
                    },
                    ctx: body.span,
                }),
            );
            block
        }
        StatementData::Return(expr) => {
            let block = compile_expr(fun, block, expr);
            block.inst(fun, InstCtx::ret(expr.span));
            block
        }
        StatementData::Yield(_) => todo!(),
        StatementData::TryCatch(_, _, _) => todo!(),
        StatementData::Throw(_) => todo!(),
        StatementData::Const(constant, span) => todo!(),
        StatementData::Empty => block,
    }
}

fn compile_for(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    init: &Statement,
    cond: &Expr,
    incr: &Statement,
    body: &Statement,
    for_span: Span,
) -> BlockBuilder {
    let init_block = compile_statement(fun, block, init);
    let cond_block = fun.block();
    let cond_block_ref = cond_block.as_block();
    let body_block = fun.block();
    let end_block = fun.block();

    let return_to_cond = InstCtx::jmp(cond_block.as_block(), cond.span);

    // Previous Block
    init_block.inst(fun, InstCtx::jmp(cond_block.as_block(), for_span));

    // Condition Block
    let end_cond_block = compile_expr(fun, cond_block, cond);
    end_cond_block.inst(fun, InstCtx::jt(body_block.as_block(), cond.span));
    end_cond_block.inst(fun, InstCtx::jmp(end_block.as_block(), cond.span));

    // Body Block
    fun.enter_loop(cond_block_ref, end_block.as_block());
    let mid_body_block = compile_statement(fun, body_block, body);
    let end_body_block = compile_statement(fun, mid_body_block, incr);
    fun.exit_loop();
    end_body_block.inst(fun, return_to_cond);

    end_block
}

fn compile_while(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    cond: &Expr,
    body: &Statement,
    is_do_while: bool,
    while_span: Span,
) -> BlockBuilder {
    let cond_block = fun.block();
    let cond_block_ref = cond_block.as_block();
    let body_block = fun.block();
    let end_block = fun.block();

    let return_to_cond = InstCtx::jmp(cond_block.as_block(), cond.span);

    // Previous Block
    block.inst(
        fun,
        InstCtx::jmp(
            if is_do_while {
                body_block.as_block()
            } else {
                cond_block.as_block()
            },
            while_span,
        ),
    );

    // Condition Block
    let end_cond_block = compile_expr(fun, cond_block, cond);
    end_cond_block.inst(fun, InstCtx::jt(body_block.as_block(), cond.span));
    end_cond_block.inst(fun, InstCtx::jmp(end_block.as_block(), cond.span));

    // Body Block
    fun.enter_loop(cond_block_ref, end_block.as_block());
    let end_body_block = compile_statement(fun, body_block, body);
    fun.exit_loop();
    end_body_block.inst(fun, return_to_cond);

    end_block
}

fn compile_ifelse(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    cond: &Expr,
    ifbody: &Statement,
    elsebody: &Statement,
) -> BlockBuilder {
    let if_block = fun.block();
    let else_block = fun.block();
    let end_block = fun.block();

    // Previous Block
    let end_cond_expr = compile_expr(fun, block, cond);
    end_cond_expr.inst(fun, InstCtx::jt(if_block.as_block(), cond.span));
    end_cond_expr.inst(fun, InstCtx::jmp(else_block.as_block(), cond.span));

    // If Block
    let end_if_block = compile_statement(fun, if_block, ifbody);
    end_if_block.inst(fun, InstCtx::jmp(end_block.as_block(), ifbody.span));

    // Else Block
    let end_else_block = compile_statement(fun, else_block, elsebody);
    end_else_block.inst(fun, InstCtx::jmp(end_block.as_block(), elsebody.span));

    end_block
}

// ---------------
//   Expressions
// ---------------

fn compile_expr(fun: &mut FunctionBuilder, block: BlockBuilder, expr: &Expr) -> BlockBuilder {
    match &expr.data {
        ExprData::Literal(lit) => compile_literal(fun, block, lit, expr.span),
        ExprData::TableDecl(slots) => compile_table_decl(fun, block, slots, expr.span),
        ExprData::ArrayDecl(elements) => compile_array_decl(fun, block, elements, expr.span),
        ExprData::FunctionDef(ast_fun) => {
            let fun_idx = fun.function(compile_function(ast_fun));
            block.inst(fun, InstCtx::loadf(fun_idx, ast_fun.keyword_span));
            block
        }
        ExprData::ClassDef {
            parent,
            members,
            ctx,
        } => compile_class_def(fun, block, parent, members, ctx.clone()),
        ExprData::Assign(assign) => compile_assign(fun, block, assign),
        ExprData::Ternary {
            cond,
            true_expr,
            false_expr,
        } => compile_ternary(fun, block, cond, true_expr, false_expr),
        ExprData::BinaryOp {
            op,
            op_span,
            lhs,
            rhs,
        } => compile_binary_op(fun, block, op, lhs, rhs, expr.span),
        ExprData::UnaryOp(op, op_span, expr) => compile_unary_op(fun, block, op, expr, *op_span),
        ExprData::UnaryRefOp(op, span, target) => {
            compile_unary_ref_op(fun, block, op, target, *span)
        }
        ExprData::FunctionCall { func, args } => compile_fn_call(fun, block, func, args, expr.span),
        ExprData::ArrayAccess { array, index } => {
            let arr_reg = fun.reg();
            let block = compile_expr(fun, block, array);
            block.inst(fun, InstCtx::storr(arr_reg, array.span));
            let block = compile_expr(fun, block, index);
            block.inst(
                fun,
                InstCtx::getf(
                    arr_reg,
                    GetFieldContext {
                        parent_span: array.span,
                        field_span: index.span,
                    },
                ),
            );
            block
        }
        ExprData::This => {
            block.inst(fun, InstCtx::this(expr.span));
            block
        }
        ExprData::FieldAccess(parent, field) => {
            let field_const = fun.ident(&field.0);
            let parent_span = parent.span;
            let block = compile_expr(fun, block, parent);
            block.inst(
                fun,
                InstCtx::getfc(
                    field_const,
                    GetFieldContext {
                        parent_span,
                        field_span: field.1,
                    },
                ),
            );
            block
        }
        ExprData::Globals => {
            block.inst(fun, InstCtx::root(expr.span));
            block
        }
        ExprData::Ident(ident) => {
            let ident_const = fun.ident(&ident);
            block.inst(
                fun,
                InstCtx::getc(
                    ident_const,
                    GetIdentContext {
                        ident_span: expr.span,
                    },
                ),
            );
            block
        }
        ExprData::Base => {
            block.inst(fun, InstCtx::base(expr.span));
            block
        }
        ExprData::RawCall { func, this, args } => {
            compile_raw_call(fun, block, func, this, args, expr.span)
        }
        ExprData::Local(idx, span) => {
            block.inst(fun, InstCtx::loadl(Local::from(*idx), *span));
            block
        }
    }
}

fn compile_class_def(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    parent: &Option<(String, Span)>,
    members: &[(Expr, Expr)],
    ctx: NewClassContext,
) -> BlockBuilder {
    let span = ctx.class_body_span;
    let class_reg = fun.reg();
    let field_reg = fun.reg();
    if let Some(parent) = parent {
        let parent_const = fun.ident(&parent.0);
        block.inst(fun, InstCtx::loadc(parent_const, parent.1));
    }
    block.inst(fun, InstCtx::class(parent.is_some(), ctx));

    compile_set_fields(fun, block, members, field_reg, class_reg, span)
}

fn compile_array_decl(
    fun: &mut FunctionBuilder,
    mut block: BlockBuilder,
    elements: &[Expr],
    span: Span,
) -> BlockBuilder {
    let push_const = fun.ident("push");
    let fun_reg = fun.reg();
    let array_reg = fun.reg();
    let el_reg = fun.reg();
    block.inst(
        fun,
        InstCtx::array(span, elements.len().try_into().unwrap_or(u8::MAX)),
    );
    block.inst(fun, InstCtx::storr(array_reg, span));
    block.inst(
        fun,
        InstCtx::getfc(
            push_const,
            GetFieldContext {
                parent_span: span,
                field_span: span,
            },
        ),
    );
    block.inst(fun, InstCtx::storr(fun_reg, span));
    for el in elements {
        block = compile_expr(fun, block, el);
        block.inst(fun, InstCtx::storr(el_reg, el.span));
        block.inst(fun, InstCtx::loadr(fun_reg, span));
        block.inst(
            fun,
            InstCtx::call(
                array_reg,
                1,
                FnCallContext {
                    fn_span: span,
                    call_span: el.span,
                    args: vec![el.span],
                },
            ),
        );
    }

    block.inst(fun, InstCtx::loadr(array_reg, span));
    block
}

fn compile_table_decl(
    fun: &mut FunctionBuilder,
    mut block: BlockBuilder,
    slots: &[(Expr, Expr)],
    span: Span,
) -> BlockBuilder {
    let table_reg = fun.reg();
    let field_reg = fun.reg();
    block.inst(fun, InstCtx::loadp(PrimType::EmptyTable, span));
    block.inst(fun, InstCtx::storr(table_reg, span));

    compile_set_fields(fun, block, slots, field_reg, table_reg, span)
}

fn compile_set_fields(
    fun: &mut FunctionBuilder,
    mut block: BlockBuilder,
    slots: &[(Expr, Expr)],
    field_reg: Reg,
    parent_reg: Reg,
    span: Span,
) -> BlockBuilder {
    for (key, value) in slots {
        block = compile_expr(fun, block, key);
        block.inst(fun, InstCtx::storr(field_reg, key.span));
        block = compile_expr(fun, block, value);
        block.inst(
            fun,
            InstCtx::setf(
                parent_reg,
                SetFieldContext {
                    parent_span: span,
                    field_span: key.span,
                    assignment_span: key.span,
                    value_span: value.span,
                },
                true,
            ),
        );
    }

    block.inst(fun, InstCtx::loadr(parent_reg, span));

    block
}

fn compile_ternary(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    cond: &Expr,
    true_expr: &Expr,
    false_expr: &Expr,
) -> BlockBuilder {
    let block = compile_expr(fun, block, cond);
    let true_block = fun.block();
    let true_block = compile_expr(fun, true_block, true_expr);
    let false_block = fun.block();
    let false_block = compile_expr(fun, false_block, false_expr);
    block.inst(fun, InstCtx::jt(true_block.as_block(), cond.span));
    block.inst(fun, InstCtx::jmp(false_block.as_block(), cond.span));
    let end_block = fun.block();
    true_block.inst(fun, InstCtx::jmp(end_block.as_block(), true_expr.span));
    false_block.inst(fun, InstCtx::jmp(end_block.as_block(), false_expr.span));
    end_block
}

fn compile_delete(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    target: &ast::AssignTarget,
    span: Span,
) -> BlockBuilder {
    match target {
        AssignTarget::Ident(ident) => todo!(),
        AssignTarget::ArrayAccess { array, index, span } => todo!(),
        AssignTarget::FieldAccess(parent, field) => todo!(),
        AssignTarget::Local(_, _) => unreachable!(),
    }
}

fn compile_unary_ref_op_run(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    op: &ast::UnaryRefOp,
) -> BlockBuilder {
    todo!()
}

fn compile_unary_ref_op(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    op: &ast::UnaryRefOp,
    target: &ast::AssignTarget,
    span: Span,
) -> BlockBuilder {
    if matches!(op, ast::UnaryRefOp::Delete) {
        return compile_delete(fun, block, target, span);
    }

    let block = match target {
        AssignTarget::Ident((ident, ident_span)) => {
            let ident_const = fun.ident(&ident);
            block.inst(fun, InstCtx::loadc(ident_const, *ident_span));
            let block = compile_unary_ref_op_run(fun, block, op);
            block.inst(
                fun,
                InstCtx::setc(
                    ident_const,
                    SetIdentContext {
                        ident_span: *ident_span,
                        assignment_span: span,
                        value_span: *ident_span,
                    },
                    false,
                ),
            );
            block
        }
        AssignTarget::ArrayAccess { array, index, span } => {
            let arr_reg = fun.reg();
            let field_reg = fun.reg();
            let block = compile_expr(fun, block, array);
            block.inst(fun, InstCtx::storr(arr_reg, array.span));
            let block = compile_expr(fun, block, index);
            block.inst(fun, InstCtx::storr(field_reg, index.span));
            block.inst(
                fun,
                InstCtx::getf(
                    arr_reg,
                    GetFieldContext {
                        parent_span: array.span,
                        field_span: index.span,
                    },
                ),
            );
            let block = compile_unary_ref_op_run(fun, block, op);
            block.inst(
                fun,
                InstCtx::setf(
                    arr_reg,
                    SetFieldContext {
                        parent_span: array.span,
                        field_span: index.span,
                        assignment_span: *span,
                        value_span: array.span | index.span,
                    },
                    false,
                ),
            );
            block
        }
        AssignTarget::FieldAccess(parent, (field, field_span)) => {
            let field_const = fun.ident(&field);
            let parent_reg = fun.reg();
            let field_reg = fun.reg();
            let block = compile_expr(fun, block, parent);
            block.inst(
                fun,
                InstCtx::getf(
                    parent_reg,
                    GetFieldContext {
                        parent_span: parent.span,
                        field_span: *field_span,
                    },
                ),
            );
            let block = compile_unary_ref_op_run(fun, block, op);
            block.inst(fun, InstCtx::loadc(field_const, *field_span));
            block.inst(fun, InstCtx::storr(field_reg, *field_span));
            block.inst(
                fun,
                InstCtx::setf(
                    parent_reg,
                    SetFieldContext {
                        parent_span: parent.span,
                        field_span: *field_span,
                        assignment_span: span,
                        value_span: parent.span | *field_span,
                    },
                    false,
                ),
            );
            block
        }
        AssignTarget::Local(idx, span) => {
            block.inst(fun, InstCtx::loadl(Local::from(*idx), *span));
            let block = compile_unary_ref_op_run(fun, block, op);
            block.inst(fun, InstCtx::storl(Local::from(*idx), *span));
            block
        }
    };

    block
}

fn compile_unary_op(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    op: &ast::UnaryOp,
    expr: &Expr,
    op_span: Span,
) -> BlockBuilder {
    let block = compile_expr(fun, block, expr);
    let context = UnaryOpContext {
        op_span,
        val_span: expr.span,
    };
    let inst = match op {
        UnaryOp::Neg => InstCtx::neg(context),
        UnaryOp::Not => InstCtx::lnot(context),
        UnaryOp::BitNot => InstCtx::bnot(context),
        UnaryOp::TypeOf => todo!("TypeOf will be an instrinsic"),
        UnaryOp::Clone => todo!("Clone will be an instrinsic"),
        UnaryOp::Resume => todo!("Resume will be an instrinsic"),
    };
    block.inst(fun, inst);
    block
}

fn compile_binary_op(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    op: &ast::BinaryOp,
    lhs: &Expr,
    rhs: &Expr,
    op_span: Span,
) -> BlockBuilder {
    let lhs_reg = fun.reg();
    let block = compile_expr(fun, block, lhs);
    block.inst(fun, InstCtx::storr(lhs_reg, lhs.span));

    if matches!(op, BinaryOp::And | BinaryOp::Or) {
        let end_block = fun.block();
        let decision_inst = match op {
            BinaryOp::And => InstCtx::jf(end_block.as_block(), lhs.span),
            BinaryOp::Or => InstCtx::jt(end_block.as_block(), lhs.span),
            _ => unreachable!(),
        };
        block.inst(fun, decision_inst);
        let block = compile_expr(fun, block, rhs);
        block.inst(fun, InstCtx::jmp(end_block.as_block(), rhs.span));
        return end_block;
    }

    let block = compile_expr(fun, block, rhs);

    let context = BinaryOpContext {
        lhs_span: lhs.span,
        op_span,
        rhs_span: rhs.span,
    };

    let inst = match op {
        BinaryOp::Add => InstCtx::add(lhs_reg, context),
        BinaryOp::Sub => InstCtx::sub(lhs_reg, context),
        BinaryOp::Mul => InstCtx::mul(lhs_reg, context),
        BinaryOp::Div => InstCtx::div(lhs_reg, context),
        BinaryOp::Mod => InstCtx::modu(lhs_reg, context),
        BinaryOp::Eq => InstCtx::iseq(lhs_reg, context),
        BinaryOp::NotEq => InstCtx::isne(lhs_reg, context),
        BinaryOp::Greater => InstCtx::isgt(lhs_reg, context),
        BinaryOp::GreaterEq => InstCtx::isge(lhs_reg, context),
        BinaryOp::Less => InstCtx::islt(lhs_reg, context),
        BinaryOp::LessEq => InstCtx::isle(lhs_reg, context),
        BinaryOp::Compare => InstCtx::cmp(lhs_reg, context),
        BinaryOp::And => unreachable!(),
        BinaryOp::Or => unreachable!(),
        BinaryOp::BitAnd => InstCtx::band(lhs_reg, context),
        BinaryOp::BitOr => InstCtx::bor(lhs_reg, context),
        BinaryOp::BitXor => InstCtx::bxor(lhs_reg, context),
        BinaryOp::Shl => {
            block.inst(fun, InstCtx::brsh(lhs_reg, context));
            todo!("This is broken");
            InstCtx::neg(todo!())
        }
        BinaryOp::Shr => InstCtx::brsh(lhs_reg, context),
        BinaryOp::AShr => InstCtx::bash(lhs_reg, context),
        BinaryOp::In => InstCtx::isin(lhs_reg, context),
        BinaryOp::InstanceOf => todo!("InstanceOf will be an instrinsic"),
    };

    block.inst(fun, inst);
    block
}

fn compile_raw_call(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    func: &Expr,
    this: &Expr,
    args: &[Expr],
    call_span: Span,
) -> BlockBuilder {
    let fun_reg = fun.reg();
    let mut arg_regs = fun.regs((args.len() + 1).try_into().unwrap());
    let this_reg = arg_regs.next().unwrap();

    let block = compile_expr(fun, block, func);
    block.inst(fun, InstCtx::storr(fun_reg, func.span));
    let block = compile_expr(fun, block, this);
    block.inst(fun, InstCtx::storr(this_reg, this.span));
    let block = load_call_args(fun, block, arg_regs, args);

    block.inst(fun, InstCtx::loadr(fun_reg, func.span));
    block.inst(
        fun,
        InstCtx::call(
            this_reg,
            args.len().try_into().unwrap(),
            FnCallContext {
                fn_span: func.span,
                call_span,
                args: args.iter().map(|expr| expr.span).collect(),
            },
        ),
    );

    block
}

fn compile_fn_call(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    func: &ast::CallTarget,
    args: &[Expr],
    call_span: Span,
) -> BlockBuilder {
    let fun_reg = fun.reg();
    let mut arg_regs = fun.regs((args.len() + 1).try_into().unwrap());
    let this_reg = arg_regs.next().unwrap();

    // This Object
    let block = match func {
        ast::CallTarget::Expr(fn_expr) => {
            block.inst(fun, InstCtx::this(fn_expr.span));
            block.inst(fun, InstCtx::storr(this_reg, fn_expr.span));
            let block = compile_expr(fun, block, fn_expr);
            block.inst(fun, InstCtx::storr(fun_reg, fn_expr.span));
            block
        }
        ast::CallTarget::FieldAccess(env, fn_field) => {
            // TODO: double check if parent["field"] should use parent as env
            let block = compile_expr(fun, block, env);
            block.inst(fun, InstCtx::storr(this_reg, env.span));
            let fn_field_const = fun.ident(&fn_field.0);
            block.inst(
                fun,
                InstCtx::getfc(
                    fn_field_const,
                    GetFieldContext {
                        parent_span: env.span,
                        field_span: fn_field.1,
                    },
                ),
            );
            block.inst(fun, InstCtx::storr(fun_reg, env.span | fn_field.1));
            block
        }
    };

    let block = load_call_args(fun, block, arg_regs, args);

    block.inst(fun, InstCtx::loadr(fun_reg, func.span()));
    block.inst(
        fun,
        InstCtx::call(
            this_reg,
            args.len().try_into().unwrap(),
            FnCallContext {
                fn_span: func.span(),
                call_span,
                args: args.iter().map(|expr| expr.span).collect(),
            },
        ),
    );
    block
}

fn load_call_args(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    arg_regs: impl Iterator<Item = Reg>,
    args: &[Expr],
) -> BlockBuilder {
    args.iter().zip(arg_regs).fold(block, |block, (arg, reg)| {
        let block = compile_expr(fun, block, arg);
        block.inst(fun, InstCtx::storr(reg, arg.span));
        block
    })
}

fn compile_assign(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    assign: &ast::Assign,
) -> BlockBuilder {
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
            block.inst(
                fun,
                InstCtx::setc(
                    field,
                    SetIdentContext {
                        ident_span: ident.1,
                        assignment_span: assign.op_span,
                        value_span: assign.value.span,
                    },
                    is_ns,
                ),
            );
            block
        }
        ast::AssignTarget::ArrayAccess { array, index, span } => {
            let parent = fun.reg();
            let field = fun.reg();
            let block = compile_expr(fun, block, array);
            block.inst(fun, InstCtx::storr(parent, array.span));
            let block = compile_expr(fun, block, index);
            block.inst(fun, InstCtx::storr(field, index.span));
            let block = compile_expr(fun, block, &assign.value);
            block.inst(
                fun,
                InstCtx::setf(
                    parent,
                    SetFieldContext {
                        parent_span: array.span,
                        field_span: index.span,
                        assignment_span: assign.op_span,
                        value_span: assign.value.span,
                    },
                    is_ns,
                ),
            );
            block
        }
        ast::AssignTarget::FieldAccess(parent, field) => {
            let parent_reg = fun.reg();
            let field_reg = fun.reg();
            let field_const = fun.ident(&field.0);
            let block = compile_expr(fun, block, parent);
            block.inst(fun, InstCtx::storr(parent_reg, parent.span));
            block.inst(fun, InstCtx::loadc(field_const, field.1));
            block.inst(fun, InstCtx::storr(field_reg, field.1));
            let block = compile_expr(fun, block, &assign.value);
            block.inst(
                fun,
                InstCtx::setf(
                    parent_reg,
                    SetFieldContext {
                        parent_span: parent.span,
                        field_span: field.1,
                        assignment_span: assign.op_span,
                        value_span: assign.value.span,
                    },
                    is_ns,
                ),
            );
            block
        }
        ast::AssignTarget::Local(local_idx, span) => {
            assert!(!is_ns, "Cannot use new slot with local variable");
            let block = compile_expr(fun, block, &assign.value);
            block.inst(
                fun,
                InstCtx::storl((*local_idx).into(), assign.op_span | *span),
            );
            block
        }
    }
}

fn compile_literal(
    fun: &mut FunctionBuilder,
    block: BlockBuilder,
    lit: &ast::Literal,
    span: Span,
) -> BlockBuilder {
    let lit_idx = fun.literal(lit);
    block.inst(fun, InstCtx::loadc(lit_idx, span));
    block
}

#[cfg(test)]

mod tests {
    use crate::test_util::exchange_data;
    use crate::{parser::parse, test_foreach};

    use super::*;

    test_foreach!(sample_test);

    fn sample_test(file_name: &str, file_contents: &str) {
        let test_name = format!("compiler-{}", file_name.replace("/", "-"));
        let test_desc = format!("Compiler test for {}", file_name);

        let ast = match parse(file_contents, file_name.to_string()) {
            Ok(ast) => ast,
            Err(err) => panic!("{}", err),
        };

        let actual_code = compile_function(&ast);

        #[cfg(not(miri))]
        {
            insta::assert_yaml_snapshot!(test_name, actual_code, {
                // ".**.start" => "Start",
                // ".**.end" => "End",
            });
        }
    }
}
