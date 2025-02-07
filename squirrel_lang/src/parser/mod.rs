pub mod ast;
mod pratt;

use std::{convert::TryInto, iter};

use ast::{AssignKind, AssignTarget, CallTarget};
use error::{ParseError, ParseResult};
use pratt::{parse_expr, parse_expr_line, parse_expr_token};

use self::ast::{Expr, Function, Literal, Statement};

use crate::{
    lexer::{SpannedLexer, Token},
    sq_error::{Span, SquirrelError, SquirrelErrorRendered},
};

pub fn parse(contents: &str, path: String) -> Result<Function, SquirrelErrorRendered> {
    let mut lexer = SpannedLexer::new(contents, path);

    let (root_fn_body, fn_locals) = lexer.fn_scoped(iter::empty(), true, |lexer| {
        let stmts = parse_statements(lexer, |tok| tok == None)
            .map_err(|err| err.with_context(&lexer).render(lexer))?;
        Ok(Statement::block(stmts, Span::empty(), Span::empty()))
    })?;

    let f = Function::new(
        Span::empty(),
        Span::empty(),
        0,
        Vec::new(),
        true,
        fn_locals,
        root_fn_body,
    );

    assert!(
        !f.locals
            .iter()
            .any(|local| matches!(local, ast::Local::Upvalue(_))),
        "Root function cannot have upvalues"
    );

    Ok(f)
}

fn parse_block<'s>(lexer: &mut SpannedLexer<'s>) -> ParseResult<Statement> {
    lexer.scoped(|lexer| {
        let start_span = lexer.expect_token(Token::LeftCurlyBrace, true)?;
        let statements = parse_statements(lexer, |tok| tok == Some(&Token::RightCurlyBrace))?;
        let end_span = lexer.expect_token(Token::RightCurlyBrace, true)?;
        Ok(Statement::block(statements, start_span, end_span))
    })
}

fn parse_statements<'s, F: Fn(Option<&Token>) -> bool>(
    tokens: &mut SpannedLexer<'s>,
    end_token: F,
) -> ParseResult<Vec<Statement>> {
    let mut statements = Vec::new();
    loop {
        let next = tokens.try_peek_token(true)?.map(|(tok, _)| tok);
        if end_token(next) {
            break;
        }
        statements.push(parse_statement(tokens)?);
    }
    Ok(statements)
}

fn parse_statement<'s>(tokens: &mut SpannedLexer<'s>) -> ParseResult<Statement> {
    let (initial_token, _ctx) = tokens.peek_token(true)?;
    let stmt = match initial_token {
        Token::LeftCurlyBrace => parse_block(tokens)?,
        Token::Semicolon => {
            tokens.skip_token();
            Statement::empty_stmt()
        }
        Token::Class => parse_class(tokens)?.into(),
        Token::If => parse_if(tokens)?,
        Token::Return | Token::Yield => {
            let is_yield = matches!(initial_token, Token::Yield);
            parse_return_yield(tokens, is_yield)?
        }
        Token::Throw => {
            let throw_span = tokens.expect_token(Token::Throw, true)?;
            let expr = parse_expr_line(tokens)?;
            Statement::throw(expr, throw_span)
        }
        Token::Local => parse_local(tokens, true)?,
        Token::Function => {
            let fn_span = tokens.expect_token(Token::Function, true)?;
            parse_function(tokens, fn_span)?.into()
        }
        Token::For => parse_for(tokens)?,
        Token::ForEach => parse_foreach(tokens)?,
        Token::Switch => parse_switch(tokens)?,
        Token::While | Token::Do => parse_while(tokens)?,
        Token::Break => {
            let break_span = tokens.expect_token(Token::Break, true)?;
            Statement::break_stmt(break_span)
        }
        _ => parse_expr_line(tokens)?.into(),
    };
    Ok(stmt)
}

fn parse_while<'s>(tokens: &mut SpannedLexer<'s>) -> ParseResult<Statement> {
    let (loop_token, loop_token_span) = tokens.next_token(true)?;
    let is_do = match loop_token {
        Token::While => false,
        Token::Do => true,
        _ => unreachable!("This function should not be called with a non-loop token"),
    };
    let mut body = None;
    if is_do {
        body = Some(tokens.scoped(|tokens| parse_statement(tokens))?);
        tokens.expect_token(Token::While, true)?;
    }
    tokens.expect_token(Token::LeftParenthesis, true)?;
    let cond = parse_expr(tokens, |tok| tok == &Token::RightParenthesis, true)?;
    tokens.expect_token(Token::RightParenthesis, true)?;
    if !is_do {
        body = Some(tokens.scoped(|tokens| parse_statement(tokens))?);
    }

    Ok(Statement::while_loop(cond, body.unwrap(), loop_token_span))
}

fn parse_switch<'s>(tokens: &mut SpannedLexer<'s>) -> ParseResult<Statement> {
    let switch_span = tokens.expect_token(Token::Switch, true)?;
    tokens.expect_token(Token::LeftParenthesis, true)?;
    let expr = parse_expr(tokens, |tok| tok == &Token::RightParenthesis, true)?;
    tokens.expect_token(Token::RightParenthesis, true)?;
    tokens.expect_token(Token::LeftCurlyBrace, true)?;
    let mut cases = Vec::new();
    let mut default = None;
    // TODO: How do switch statements and local resolution interact
    let end_span = tokens.scoped(|tokens| loop {
        let (next, span) = tokens.next_token(true)?;
        match next {
            Token::Case => {
                if default.is_some() {
                    return Err(ParseError::syntax_error(
                        "Case statement is not allowed after default".to_string(),
                        span,
                    ));
                }
                let case_expr = parse_expr(tokens, |tok| tok == &Token::Colon, true)?;
                tokens.expect_token(Token::Colon, true)?;
                let body = parse_statements(tokens, |tok| {
                    tok == Some(&Token::Case) || tok == Some(&Token::Default)
                })?;
                cases.push((
                    case_expr,
                    Statement::block(body, Span::empty(), Span::empty()),
                ));
            }
            Token::Default => {
                tokens.expect_token(Token::Colon, true)?;
                let body = parse_statements(tokens, |tok| tok == Some(&Token::RightCurlyBrace))?;
                default = Some(Statement::block(body, Span::empty(), Span::empty()))
            }
            Token::RightCurlyBrace => {
                break Ok(span);
            }
            other => return Err(ParseError::unexpected_token(other, span)),
        }
    })?;
    Ok(Statement::switch(
        expr,
        cases,
        default,
        switch_span,
        end_span,
    ))
}

fn parse_foreach<'s>(tokens: &mut SpannedLexer<'s>) -> ParseResult<Statement> {
    let for_span = tokens.expect_token(Token::ForEach, true)?;
    tokens.expect_token(Token::LeftParenthesis, true)?;
    let (token, span) = tokens.next_token(true)?;
    let ident1 = match token {
        Token::Identifier(name) => (name, span),
        other => return Err(ParseError::unexpected_token(other, span)),
    };
    let ident2 = match tokens.peek_token(true)? {
        (Token::Comma, _) => {
            tokens.skip_token();
            let (token, ctx) = tokens.next_token(true)?;
            match token {
                Token::Identifier(name) => Some((name, ctx)),
                other => return Err(ParseError::unexpected_token(other, ctx)),
            }
        }
        _ => None,
    };
    tokens.expect_token(Token::In, true)?;
    let iterable = parse_expr(tokens, |tok| tok == &Token::RightParenthesis, true)?;
    tokens.expect_token(Token::RightParenthesis, true)?;
    let (index, value) = match ident2 {
        Some(ident2) => (Some(ident1), ident2),
        None => (None, ident1),
    };

    let index = index.map(|(name, _ctx)| tokens.lcl().add_local(&name));
    let value = tokens.lcl().add_local(&value.0);

    let body = tokens.scoped(|tokens| parse_statement(tokens))?;
    Ok(Statement::foreach(index, value, iterable, body, for_span))
}

fn parse_for<'s>(tokens: &mut SpannedLexer<'s>) -> ParseResult<Statement> {
    let for_span = tokens.expect_token(Token::For, true)?;
    tokens.expect_token(Token::LeftParenthesis, true)?;
    let init = match tokens.peek_token(true)?.0 {
        Token::Local => parse_local(tokens, false)?,
        Token::Semicolon => {
            tokens.skip_token();
            Statement::empty_stmt()
        }
        _ => parse_expr_token(tokens, Token::Semicolon)?.0.into(),
    };
    let cond = match tokens.peek_token(true)? {
        (Token::Semicolon, _) => {
            let semi_span = tokens.expect_token(Token::Semicolon, true)?;
            Expr::literal(Literal::Integer(1), semi_span)
        }
        _ => parse_expr_token(tokens, Token::Semicolon)?.0,
    };
    let step = match tokens.peek_token(true)? {
        (Token::RightParenthesis, _) => {
            tokens.skip_token();
            Statement::empty_stmt()
        }
        _ => parse_expr_token(tokens, Token::RightParenthesis)?.0.into(),
    };
    let body = tokens.scoped(|tokens| parse_statement(tokens))?;
    Ok(Statement::for_loop(init, cond, step, body, for_span))
}

fn parse_return_yield<'s>(tokens: &mut SpannedLexer<'s>, is_yield: bool) -> ParseResult<Statement> {
    let expect_token = if is_yield {
        Token::Yield
    } else {
        Token::Return
    };
    let kw_span = tokens.expect_token(expect_token, true)?;
    let expr = match tokens.peek_token(true)? {
        (Token::Newline, _) | (Token::Semicolon, _) => {
            tokens.skip_token();
            Expr::literal(Literal::Null, Span::empty())
        }
        _ => parse_expr_line(tokens)?,
    };
    if is_yield {
        Ok(Statement::yield_stmt(expr, kw_span))
    } else {
        Ok(Statement::return_stmt(expr, kw_span))
    }
}

fn parse_local<'s>(tokens: &mut SpannedLexer<'s>, stop_at_newline: bool) -> ParseResult<Statement> {
    let local_span = tokens.expect_token(Token::Local, true)?;
    let mut decls = Vec::new();
    loop {
        let (ident, ctx) = tokens.next_token(true)?;
        let ident = match ident {
            Token::Identifier(name) => (name, ctx),
            other => return Err(ParseError::unexpected_token(other, ctx)),
        };
        let val = if let (Token::Assign, assign_span) = tokens.peek_token(false)? {
            let assign_span = *assign_span;
            tokens.skip_token();
            let val = parse_expr(
                tokens,
                |tok| tok == &Token::Comma || tok == &Token::Newline || tok == &Token::Semicolon,
                false,
            )?;
            Some((assign_span, val))
        } else {
            None
        };
        decls.push((ident, val));
        let (term, ctx) = tokens.next_token(false)?;
        match term {
            Token::Comma => {}
            Token::Newline if stop_at_newline => break,
            Token::Semicolon => break,
            other => return Err(ParseError::unexpected_token(other, ctx)),
        }
    }
    debug_assert!(!decls.is_empty());
    let statements: Vec<Statement> = decls
        .into_iter()
        .filter_map(|(ident, val)| {
            let local_id = tokens.lcl().add_local(&ident.0);
            val.map(|(assign_span, val)| {
                Expr::assign(
                    AssignTarget::Local(local_id, ident.1),
                    assign_span,
                    val,
                    AssignKind::Normal,
                )
                .into()
            })
        })
        .collect::<Vec<_>>();
    let statement = if statements.len() == 1 {
        statements.into_iter().next().unwrap().into()
    } else {
        let end_span = statements
            .last()
            .map(|stmt| stmt.span)
            .unwrap_or(Span::empty());
        Statement::block(statements, local_span, end_span)
    };
    Ok(statement)
}

fn parse_if<'s>(tokens: &mut SpannedLexer<'s>) -> ParseResult<Statement> {
    let if_span = tokens.expect_token(Token::If, true)?;
    tokens.expect_token(Token::LeftParenthesis, true)?;
    let cond = parse_expr(tokens, |tok| tok == &Token::RightParenthesis, true)?;
    tokens.expect_token(Token::RightParenthesis, true)?;
    let body = tokens.scoped(|tokens| parse_statement(tokens))?;
    let else_body = if let Some((Token::Else, _)) = tokens.try_peek_token(true)? {
        tokens.skip_token();
        tokens.scoped(|tokens| parse_statement(tokens))?
    } else {
        Statement::empty_stmt()
    };

    Ok(Statement::if_else(cond, body, else_body, if_span))
}

fn parse_class_extends_body<'s>(
    tokens: &mut SpannedLexer<'s>,
    class_span: Span,
) -> ParseResult<Expr> {
    let parent = if let (Token::Extends, extends_kw) = tokens.peek_token(true)? {
        let extends_kw = *extends_kw;
        tokens.skip_token();
        let (parent_ident, ctx) = tokens.next_token(true)?;
        if let Token::Identifier(parent_name) = parent_ident {
            Some((extends_kw, (parent_name.to_string(), ctx)))
        } else {
            return Err(ParseError::unexpected_token(parent_ident, ctx));
        }
    } else {
        None
    };
    tokens.expect_token(Token::LeftCurlyBrace, true)?;
    let (members, end_span) = parse_table_or_class(tokens, &Token::Semicolon, true)?;
    Ok(Expr::class_def(parent, members, class_span, end_span))
}

fn parse_class<'s>(tokens: &mut SpannedLexer<'s>) -> ParseResult<Expr> {
    let class_span = tokens.expect_token(Token::Class, true)?;

    let target = if let (Token::Identifier(_), _) = tokens.peek_token(true)? {
        Some(parse_hier_path(tokens, Token::Period)?)
    } else {
        None
    };

    let body = parse_class_extends_body(tokens, class_span)?;

    Ok(if let Some(target) = target {
        Expr::assign(target, class_span, body, AssignKind::NewSlot)
    } else {
        body
    })
}

fn parse_table_or_class<'s>(
    tokens: &mut SpannedLexer<'s>,
    sep: &Token,
    allow_constructor_sugar: bool,
) -> ParseResult<(Vec<(Expr, Expr)>, Span)> {
    let mut members = Vec::new();
    let end_span = loop {
        let next = tokens.peek_token(true)?;
        match next {
            (Token::RightCurlyBrace, _) => {
                let end_span = tokens.expect_token(Token::RightCurlyBrace, true)?;
                break end_span;
            }
            (Token::Identifier(constructor), keyword_span) if *constructor == "constructor" => {
                let keyword_span = *keyword_span;
                if !allow_constructor_sugar {
                    return Err(ParseError::syntax_error(
                        format!("Cannot declare a constructor for a table"),
                        keyword_span,
                    ));
                }
                tokens.skip_token();
                let constructor = parse_function_args_body(tokens, keyword_span)?;
                members.push((
                    Expr::literal(Literal::string("constructor"), keyword_span),
                    Expr::function_def(constructor, keyword_span),
                ));
            }
            (Token::Function, keyword_span) => {
                let keyword_span = *keyword_span;
                let fn_span = tokens.expect_token(Token::Function, true)?;
                let ident = match tokens.next_token(true)? {
                    (Token::Identifier(name), ctx) => (name.to_string(), ctx),
                    other => return Err(ParseError::unexpected_token(other.0, other.1)),
                };
                let func = parse_function_args_body(tokens, keyword_span)?;
                members.push((
                    Expr::literal(Literal::string(ident.0), ident.1),
                    Expr::function_def(func, fn_span),
                ));
            }
            (token, span) if token == sep || token == &Token::Newline => {
                let span = *span;
                tokens.skip_token()
            }
            (_, span) => {
                let span = *span;
                members.push(parse_table_slot(tokens, sep)?);
            }
        }
    };
    Ok((members, end_span))
}

fn parse_table_slot<'s>(tokens: &mut SpannedLexer<'s>, sep: &Token) -> ParseResult<(Expr, Expr)> {
    let (init_token, ctx) = tokens.next_token(true)?;
    let key = match init_token {
        Token::Identifier(name) => Expr::literal(Literal::string(name), ctx),
        Token::LeftSquareBracket => {
            let key = parse_expr(tokens, |tok| tok == &Token::RightSquareBracket, true)?;
            tokens.expect_token(Token::RightSquareBracket, true)?;
            key
        }
        other => return Err(ParseError::unexpected_token(other, ctx)),
    };
    tokens.expect_token(Token::Assign, true)?;
    let value = parse_expr(
        tokens,
        |tok| tok == sep || tok == &Token::Newline || tok == &Token::RightCurlyBrace,
        false,
    )?;
    Ok((key, value))
}

fn parse_function<'s>(tokens: &mut SpannedLexer<'s>, keyword_span: Span) -> ParseResult<Expr> {
    let target = if let (Token::Identifier(_), _) = tokens.peek_token(true)? {
        Some(parse_hier_path(tokens, Token::DoubleColon)?)
    } else {
        None
    };
    let func = parse_function_args_body(tokens, keyword_span)?;
    let func_def = Expr::function_def(func, keyword_span);
    Ok(if let Some(target) = target {
        Expr::assign(target, keyword_span, func_def, AssignKind::NewSlot)
    } else {
        func_def
    })
}

fn parse_hier_path<'s>(tokens: &mut SpannedLexer<'s>, sep: Token) -> ParseResult<AssignTarget> {
    let mut elements = Vec::new();
    loop {
        let (token, span) = tokens.next_token(true)?;
        match token {
            Token::Identifier(name) => {
                elements.push((name, span));
            }
            other => return Err(ParseError::unexpected_token(other, span)),
        }
        match tokens.peek_token(true)? {
            (tok, _) if tok == &sep => tokens.skip_token(),
            _ => break,
        }
    }
    let mut iter = elements.into_iter();
    let mut target = Expr::ident(iter.next().unwrap());
    for (name, span) in iter {
        target = Expr::field_access(target, (name.to_string(), span));
    }

    Ok(target.try_into().expect("This should be infallible"))
}

fn parse_list<'s>(
    tokens: &mut SpannedLexer<'s>,
    start: Token<'s>,
    end: Token<'s>,
) -> ParseResult<(Vec<Expr>, Span)> {
    let start_span = tokens.expect_token(start, true)?;
    let mut args = Vec::new();
    let end_span = loop {
        let (next, _ctx) = tokens.peek_token(true)?;
        match next {
            end_tok if end_tok == &end => {
                let end_span = tokens.expect_token(end, true)?;
                break end_span;
            }
            _ => {
                if !args.is_empty() {
                    tokens.expect_token(Token::Comma, true)?;
                }
                let expr = parse_expr(tokens, |tok| tok == &end || tok == &Token::Comma, true)?;
                args.push(expr);
            }
        }
    };
    Ok((args, start_span | end_span))
}

fn parse_function_args_body<'s>(
    tokens: &mut SpannedLexer<'s>,
    keyword_span: Span,
) -> ParseResult<Function> {
    let (init_token, init_call_ctx) = tokens.next_token(true)?;
    match init_token {
        Token::LeftParenthesis => {}
        other => return Err(ParseError::unexpected_token(other, init_call_ctx)),
    };
    let mut args = Vec::new();
    let mut default_expr = Vec::new();
    let mut is_varargs = false;
    let end_call_ctx = loop {
        let (next, next_ctx) = tokens.next_token(true)?;
        match next {
            Token::RightParenthesis => break next_ctx,
            Token::Newline => {}
            Token::Varargs => {
                is_varargs = true;
            }
            Token::Identifier(name) if !is_varargs => {
                args.push((name, next_ctx));
                if let (Token::Assign, _) = tokens.peek_token(true)? {
                    tokens.skip_token();
                    let default_val = parse_expr(
                        tokens,
                        |tok| tok == &Token::Comma || tok == &Token::RightParenthesis,
                        true,
                    )?;
                    default_expr.push(default_val);
                } else if !default_expr.is_empty() {
                    return Err(ParseError::syntax_error(
                        format!("Cannot have non-default arguments after default arguments"),
                        next_ctx,
                    ));
                }

                let (term, ctx) = tokens.next_token(true)?;
                match term {
                    Token::Comma => {}
                    Token::RightParenthesis => break ctx,
                    other => return Err(ParseError::unexpected_token(other, ctx)),
                }
            }
            other => return Err(ParseError::unexpected_token(other, next_ctx)),
        }
    };

    let (body, fn_locals) = tokens.fn_scoped(
        args.iter().map(|(name, _span)| *name),
        is_varargs,
        |tokens| parse_statement(tokens),
    )?;
    Ok(Function::new(
        keyword_span,
        init_call_ctx | end_call_ctx,
        args.len() as u32,
        default_expr,
        is_varargs,
        fn_locals,
        body,
    ))
}

pub mod error {
    use crate::sq_error::{RsBacktrace, Span, SquirrelError};

    use crate::lexer::{SpannedLexer, Token};

    pub type ParseResult<T> = Result<T, ParseError>;
    #[derive(Debug)]
    pub enum ParseError {
        UnexpectedToken(String, Span, RsBacktrace),
        UnexpectedEof(RsBacktrace),
        SyntaxError(String, Span, RsBacktrace),
        ErrorContext(SquirrelError),
        InvalidAssignment {
            target: Span,
            target_kind: String,
            backtrace: RsBacktrace,
        },
    }

    impl From<SquirrelError> for ParseError {
        fn from(ctx: SquirrelError) -> Self {
            Self::ErrorContext(ctx)
        }
    }

    impl ParseError {
        pub fn with_context(self, ctx: &SpannedLexer) -> SquirrelError {
            match self {
                ParseError::UnexpectedToken(tok, span, backtrace) => SquirrelError::new(
                    ctx.get_file_id(),
                    span.into(),
                    format!("Unexpected token {:?}", tok),
                    backtrace,
                ),
                ParseError::UnexpectedEof(backtrace) => SquirrelError::new(
                    ctx.get_file_id(),
                    (ctx.current_offset()..ctx.current_offset()).into(),
                    format!("Unexpected eof"),
                    backtrace,
                ),
                ParseError::SyntaxError(message, span, backtrace) => SquirrelError::new(
                    ctx.get_file_id(),
                    span.into(),
                    format!("Syntax Error: {:?}", message),
                    backtrace,
                ),
                ParseError::InvalidAssignment {
                    target,
                    target_kind,
                    backtrace,
                } => SquirrelError::new(
                    ctx.get_file_id(),
                    target,
                    format!("Invalid assignment target: {}", target_kind),
                    backtrace,
                ),
                ParseError::ErrorContext(err) => err,
            }
        }

        pub fn unexpected_token<'s>(tok: Token<'s>, span: Span) -> Self {
            Self::UnexpectedToken(format!("{:?}", tok), span, RsBacktrace::new())
        }

        pub fn unexpected_eof() -> Self {
            Self::UnexpectedEof(RsBacktrace::new())
        }

        pub fn syntax_error(message: String, span: Span) -> Self {
            Self::SyntaxError(message, span, RsBacktrace::new())
        }

        pub fn invalid_assignment(target: Span, target_kind: String) -> Self {
            Self::InvalidAssignment {
                target,
                target_kind,
                backtrace: RsBacktrace::new(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test_foreach;
    use crate::test_util::exchange_data;

    use super::*;

    test_foreach!(sample_test);

    fn sample_test(file_name: &str, file_contents: &str) {
        let test_name = format!("parser-{}", file_name.replace("/", "-"));
        let test_desc = format!("Parser test for {}", file_name);

        let actual_ast = match parse(file_contents, file_name.to_string()) {
            Ok(ast) => ast,
            Err(err) => panic!("{}", err),
        };
        #[cfg(not(miri))]
        {
            insta::assert_yaml_snapshot!(test_name, actual_ast, {
                ".**.start" => "Start",
                ".**.end" => "End",
            });
        }
    }
}
