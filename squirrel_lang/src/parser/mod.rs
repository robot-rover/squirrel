pub mod ast;
mod pratt;

use std::convert::TryInto;

use ast::{AssignKind, AssignTarget};
use error::{ParseError, ParseResult};
use pratt::{parse_expr, parse_expr_line, parse_expr_token};

use self::ast::{Expr, Function, Literal, Statement};

use crate::{
    context::{IntoSquirrelErrorContext, Span, SquirrelErrorContext},
    lexer::{SpannedLexer, Token},
};

pub fn parse(contents: &str, path: String) -> Result<Function, SquirrelErrorContext> {
    let mut lexer = SpannedLexer::new(contents, path);
    let stmts = parse_statements(&mut lexer, |tok| tok == None)
        .map_err(|err| err.with_context(&lexer))
        .map_err(|err| err.with_context(contents))?;
    let body = Statement::block(stmts, Span::empty(), Span::empty());
    Ok(Function {
        keyword_span: Span::empty(),
        arg_span: Span::empty(),
        args: Vec::new(),
        default_expr: Vec::new(),
        is_varargs: true,
        body: Box::new(body),
    })
}

fn parse_block<'s>(lexer: &mut SpannedLexer<'s>) -> ParseResult<Statement> {
    let start_span = lexer.expect_token(Token::LeftCurlyBrace, true)?;
    let statements = parse_statements(lexer, |tok| tok == Some(&Token::RightCurlyBrace))?;
    let end_span = lexer.expect_token(Token::RightCurlyBrace, true)?;
    Ok(Statement::block(statements, start_span, end_span))
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

// TODO: This function is a mess, needs to be refactored into seperate functions
fn parse_statement<'s>(tokens: &mut SpannedLexer<'s>) -> ParseResult<Statement> {
    let (initial_token, _ctx) = tokens.peek_token(true)?;
    let stmt = match initial_token {
        Token::LeftCurlyBrace => parse_block(tokens)?,
        Token::Semicolon => {
            tokens.skip_token();
            Statement::empty_stmt()
        }
        Token::Class => parse_class(tokens)?,
        Token::If => parse_if(tokens)?,
        // TODO: Combine with Yield
        Token::Return => {
            let return_span = tokens.expect_token(Token::Return, true)?;
            let expr = match tokens.peek_token(true)? {
                (Token::Newline, _) | (Token::Semicolon, _) => {
                    tokens.skip_token();
                    Expr::literal(Literal::Null, Span::empty())
                }
                _ => parse_expr_line(tokens)?,
            };
            Statement::return_stmt(expr, return_span)
        }
        Token::Yield => {
            let return_span = tokens.expect_token(Token::Yield, true)?;
            let expr = match tokens.peek_token(true)? {
                (Token::Newline, _) | (Token::Semicolon, _) => {
                    tokens.skip_token();
                    Expr::literal(Literal::Null, Span::empty())
                }
                _ => parse_expr_line(tokens)?,
            };
            Statement::yield_stmt(expr, return_span)
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
        Token::For => {
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
            let body = parse_statement(tokens)?;
            Statement::for_loop(init, cond, step, body, for_span)
        }
        Token::ForEach => {
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
            let body = parse_statement(tokens)?;
            let (index, value) = match ident2 {
                Some(ident2) => (Some(ident1), ident2),
                None => (None, ident1),
            };
            Statement::foreach(index, value, iterable, body, for_span)
        }
        Token::Switch => {
            let switch_span = tokens.expect_token(Token::Switch, true)?;
            tokens.expect_token(Token::LeftParenthesis, true)?;
            let expr = parse_expr(tokens, |tok| tok == &Token::RightParenthesis, true)?;
            tokens.expect_token(Token::RightParenthesis, true)?;
            tokens.expect_token(Token::LeftCurlyBrace, true)?;
            let mut cases = Vec::new();
            let mut default = None;
            let end_span = loop {
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
                        let body =
                            parse_statements(tokens, |tok| tok == Some(&Token::RightCurlyBrace))?;
                        default = Some(Statement::block(body, Span::empty(), Span::empty()))
                    }
                    Token::RightCurlyBrace => {
                        break span;
                    }
                    other => return Err(ParseError::unexpected_token(other, span)),
                }
            };
            Statement::switch(expr, cases, default, switch_span, end_span)
        }
        Token::While => {
            let while_span = tokens.expect_token(Token::While, true)?;
            tokens.expect_token(Token::LeftParenthesis, true)?;
            let cond = parse_expr(tokens, |tok| tok == &Token::RightParenthesis, true)?;
            tokens.expect_token(Token::RightParenthesis, true)?;
            let body = parse_statement(tokens)?;
            Statement::while_loop(cond, body, while_span)
        }
        _ => parse_expr_line(tokens)?.into(),
    };
    Ok(stmt)
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
        let val = if let (Token::Assign, _) = tokens.peek_token(true)? {
            tokens.skip_token();
            parse_expr(
                tokens,
                |tok| tok == &Token::Comma || tok == &Token::Newline || tok == &Token::Semicolon,
                false,
            )?
        } else {
            Expr::literal(Literal::Null, Span::empty())
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
    let statements = decls
        .into_iter()
        .map(|(ident, val)| Statement::local_dec(ident, val, local_span))
        .collect::<Vec<_>>();
    let statement = if statements.len() == 1 {
        statements.into_iter().next().unwrap()
    } else {
        let end_span = statements.last().unwrap().span;
        Statement::block(statements, local_span, end_span)
    };
    Ok(statement)
}

fn parse_if<'s>(tokens: &mut SpannedLexer<'s>) -> ParseResult<Statement> {
    let if_span = tokens.expect_token(Token::If, true)?;
    tokens.expect_token(Token::LeftParenthesis, true)?;
    let cond = parse_expr(tokens, |tok| tok == &Token::RightParenthesis, true)?;
    tokens.expect_token(Token::RightParenthesis, true)?;
    let body = parse_statement(tokens)?;
    let else_body = if let Some((Token::Else, _)) = tokens.try_peek_token(true)? {
        tokens.skip_token();
        parse_statement(tokens)?
    } else {
        Statement::empty_stmt()
    };

    Ok(Statement::if_else(cond, body, else_body, if_span))
}

fn parse_class<'s>(tokens: &mut SpannedLexer<'s>) -> ParseResult<Statement> {
    let class_span = tokens.expect_token(Token::Class, true)?;
    let mut first_token_span = tokens.next_token(true)?;
    let mut decl_path = Vec::new();
    // Todo: need a parse in loop seperated by token function
    if let (Token::Identifier(name), name_span) = first_token_span {
        decl_path.push((name, name_span));
        loop {
            first_token_span = tokens.next_token(true)?;
            if let (Token::Period, _) = first_token_span {
                let (ident_token, ident_span) = tokens.next_token(true)?;
                let Token::Identifier(name) = ident_token else {
                    return Err(ParseError::unexpected_token(ident_token, ident_span));
                };
                decl_path.push((name, ident_span));
            } else {
                break;
            }
        }
    }
    let parent = if let (Token::Extends, _) = first_token_span {
        let (parent_ident, ctx) = tokens.next_token(true)?;
        if let Token::Identifier(parent_name) = parent_ident {
            let parent_option = Some((parent_name, ctx));
            first_token_span = tokens.next_token(true)?;
            parent_option
        } else {
            return Err(ParseError::unexpected_token(parent_ident, ctx));
        }
    } else {
        None
    };
    let (first_token, ctx) = first_token_span;
    if first_token != Token::LeftCurlyBrace {
        return Err(ParseError::unexpected_token(first_token, ctx));
    }
    let (members, end_span) = parse_table_or_class(tokens, &Token::Semicolon, true)?;
    let body = Expr::class_def(parent, members, class_span, end_span);
    if let Some(class_name) = decl_path.pop() {
        let base = decl_path
            .into_iter()
            .fold(None, |acc, (name, span)| match acc {
                Some(parent) => Some(Expr::field_access(parent, (name, span))),
                None => Some((name, span).into()),
            });
        let target = match base {
            Some(base_expr) => AssignTarget::FieldAccess(Box::new(base_expr), class_name),
            None => AssignTarget::Ident(class_name),
        };
        Ok(Expr::assign(target, class_span, body, AssignKind::NewSlot).into())
    } else {
        Ok(body.into())
    }
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
            (Token::Identifier(constructor), keyword_span) if constructor == "constructor" => {
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
                    Expr::literal(Literal::String("constructor".to_string()), keyword_span),
                    Expr::function_def(constructor, keyword_span),
                ));
            }
            (Token::Function, keyword_span) => {
                let keyword_span = *keyword_span;
                let fn_span = tokens.expect_token(Token::Function, true)?;
                let ident = match tokens.next_token(true)? {
                    (Token::Identifier(name), ctx) => (name, ctx),
                    other => return Err(ParseError::unexpected_token(other.0, other.1)),
                };
                let func = parse_function_args_body(tokens, keyword_span)?;
                members.push((
                    Expr::literal(Literal::String(ident.0), ident.1),
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
        Token::Identifier(name) => Expr::literal(Literal::String(name), ctx),
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

fn parse_function<'s>(
    tokens: &mut SpannedLexer<'s>,
    keyword_span: Span,
) -> ParseResult<Expr> {
    let target = match tokens.peek_token(true)?.0 {
        Token::Identifier(_) => Some(parse_hier_path(tokens)?),
        _ => None,
    };
    let func = parse_function_args_body(tokens, keyword_span)?;
    let func_def = Expr::function_def(func, keyword_span);
    // TODO: Share logic with class
    let fd = match target {
        Some(other) => Expr::assign(
            other,
            keyword_span,
            func_def,
            AssignKind::NewSlot,
        ),
        None => func_def,
    };
    Ok(fd)
}

fn parse_hier_path<'s>(tokens: &mut SpannedLexer<'s>) -> ParseResult<AssignTarget> {
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
            (Token::DoubleColon, _) => tokens.skip_token(),
            _ => break,
        }
    }
    let mut iter = elements.into_iter();
    let mut target = iter.next().unwrap().into();
    for (name, span) in iter {
        target = Expr::field_access(target, (name, span));
    }
    target.try_into()
}

fn parse_list<'s>(
    tokens: &mut SpannedLexer<'s>,
    start: Token,
    end: Token,
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

    let body = parse_statement(tokens)?;
    Ok(Function {
        args,
        arg_span: init_call_ctx | end_call_ctx,
        default_expr,
        is_varargs,
        body: Box::new(body),
        keyword_span,
    })
}

pub mod error {
    use crate::context::{Span, SqBacktrace, SquirrelError};

    use crate::lexer::{SpannedLexer, Token};

    pub type ParseResult<T> = Result<T, ParseError>;
    #[derive(Debug)]
    pub enum ParseError {
        UnexpectedToken(Token, Span, SqBacktrace),
        UnexpectedEof(SqBacktrace),
        SyntaxError(String, Span, SqBacktrace),
        ErrorContext(SquirrelError),
        InvalidAssignment {
            target: Span,
            target_kind: String,
            backtrace: SqBacktrace,
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
                    ctx.get_file_name().to_string(),
                    span.into(),
                    format!("Unexpected token {:?}", tok),
                    backtrace,
                ),
                ParseError::UnexpectedEof(backtrace) => SquirrelError::new(
                    ctx.get_file_name().to_string(),
                    (ctx.current_offset()..ctx.current_offset()).into(),
                    format!("Unexpected eof"),
                    backtrace,
                ),
                ParseError::SyntaxError(message, span, backtrace) => SquirrelError::new(
                    ctx.get_file_name().to_string(),
                    span.into(),
                    format!("Syntax Error: {:?}", message),
                    backtrace,
                ),
                ParseError::InvalidAssignment {
                    target,
                    target_kind,
                    backtrace,
                } => SquirrelError::new(
                    ctx.get_file_name().to_string(),
                    target,
                    format!("Invalid assignment target: {}", target_kind),
                    backtrace,
                ),
                ParseError::ErrorContext(err) => err,
            }
        }

        pub fn unexpected_token(tok: Token, span: Span) -> Self {
            Self::UnexpectedToken(tok, span, SqBacktrace::new())
        }

        pub fn unexpected_eof() -> Self {
            Self::UnexpectedEof(SqBacktrace::new())
        }

        pub fn syntax_error(message: String, span: Span) -> Self {
            Self::SyntaxError(message, span, SqBacktrace::new())
        }

        pub fn invalid_assignment(target: Span, target_kind: String) -> Self {
            Self::InvalidAssignment {
                target,
                target_kind,
                backtrace: SqBacktrace::new(),
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
        let actual_ast = match parse(file_contents, file_name.to_string()) {
            Ok(ast) => ast,
            Err(err) => panic!("{}", err),
        };
        #[cfg(not(miri))]
        {
            let expect_ast = exchange_data("parser", file_name, &actual_ast);

            // TODO: Have a more useful comparison for these trees
            assert_eq!(actual_ast, expect_ast);
        }
    }
}
