use std::convert::TryInto;

use crate::context::Span;
use crate::lexer::{SpannedLexer, Token};

use super::{
    ast::{AssignKind, BinaryOp, Expr, ExprData, Literal, UnaryOp, UnaryRefOp},
    error::{ParseError, ParseResult},
    parse_function, parse_list, parse_table_or_class,
};

pub fn parse_expr<'s, F: Fn(&Token) -> bool>(
    tokens: &mut SpannedLexer<'s>,
    is_term: F,
    ignore_newlines: bool,
) -> ParseResult<Expr> {
    parse_expr_bp(tokens, 0, &is_term, ignore_newlines)
}

pub fn parse_expr_token<'s>(
    tokens: &mut SpannedLexer<'s>,
    token: Token<'s>,
) -> ParseResult<(Expr, Span)> {
    let expr = parse_expr(tokens, |tok| tok == &token, true)?;
    let span = tokens.expect_token(token, true)?;
    Ok((expr, span))
}

pub fn parse_expr_line<'s>(tokens: &mut SpannedLexer<'s>) -> ParseResult<Expr> {
    let expr = parse_expr(
        tokens,
        |tok| tok == &Token::Newline || tok == &Token::Semicolon,
        false,
    )?;
    // Can't use expect_token here because it will panic if we hit EOF
    if let Some(Err(err)) = tokens.next() {
        return Err(err.into());
    }
    Ok(expr)
}

// TODO: Verify expr bp by testing sq reference implementation
fn get_prefix_bp(op: &Token) -> u16 {
    match op {
        Token::Minus => 21,
        Token::Plus => 21,
        Token::Not => 21,
        Token::BitNot => 21,
        Token::Typeof => 21,
        Token::Clone => 21,
        Token::Resume => 21,
        Token::Increment => 23,
        Token::Decrement => 23,
        _ => panic!("Token {:?} is not a prefix operator", op),
    }
}

fn get_bp(op: &Token) -> Option<(u16, u16)> {
    let bp = match op {
        // -- Postfix Operators --
        Token::Increment => (23, 24),
        Token::Decrement => (23, 24),
        // Note, Array Access and Function Call act as postfix operators
        // Member / Array Access and Function Call (25, 26)
        Token::Period | Token::LeftParenthesis | Token::LeftSquareBracket => (25, 26),
        // Multiply and Divide (19, 20)
        Token::Multiply | Token::Divide | Token::Modulus => (19, 20),
        // Addition and Subtraction (17, 18)
        Token::Plus | Token::Minus => (17, 18),
        // Shift Operators (15, 16)
        Token::RightShiftArith | Token::RightShift | Token::LeftShift => (15, 16),
        // Comparison Operators (13, 14)
        Token::LessThan
        | Token::LessThanEquals
        | Token::GreaterThan
        | Token::GreaterThanEquals
        | Token::InstaceOf => (13, 14),
        // Equality Operators (11, 12)
        Token::Equals | Token::DoesNotEqual | Token::Compare => (11, 12),
        // Bitwise And (9, 10)
        Token::BitAnd => (9, 10),
        // Bitwise Xor (7, 8)
        Token::BitXor => (7, 8),
        // Logical And and In (5, 6)
        Token::In | Token::And => (5, 6),
        Token::QuestionMark => (3, 4),
        // Assignment
        Token::Assign
        | Token::NewSlot
        | Token::PlusAssign
        | Token::MinusAssign
        | Token::MultiplyAssign
        | Token::DivideAssign => (2, 2),
        _ => return None,
    };
    Some(bp)
}

// TODO: Need to go through newlines if line starts with a binary operator
// TODO: Switch from recursion to shuntyard parsing
pub fn parse_expr_bp<'s, F: Fn(&Token<'s>) -> bool>(
    tokens: &mut SpannedLexer<'s>,
    min_bp: u16,
    is_term: &F,
    ignore_newlines: bool,
) -> ParseResult<Expr> {
    let (first_token, ctx) = tokens.next_token(true)?;
    let mut lhs = match first_token {
        // Needs to be less than the bp of period to not break field access
        Token::Identifier(name) => if min_bp < 25 {
            tokens
                .lcl()
                .maybe_reference_local(name)
                .map(|local_idx| Expr::local(local_idx, ctx))
        } else {
            None
        }
        .unwrap_or_else(|| Expr::ident((name, ctx))),
        Token::Integer(num) => Expr::literal(Literal::Integer(num), ctx),
        Token::Number(num) => Expr::literal(Literal::Number(num), ctx),
        Token::String(string) => Expr::literal(Literal::String(string), ctx),
        Token::Boolean(val) => Expr::literal(Literal::Integer(if val { 1 } else { 0 }), ctx),
        Token::Null => Expr::literal(Literal::Null, ctx),
        // Need the cast to fn to prevent infinite recursion during type resolution
        Token::LeftParenthesis => parse_expr_token(tokens, Token::RightParenthesis)?.0,
        Token::This => ExprData::This.spanning(ctx),
        Token::Base => ExprData::Base.spanning(ctx),
        // Prefix Unary Operators
        // DoubleColon should be attached to an identifier, so it has infinite binding power
        Token::DoubleColon => {
            let (next_tok, ctx2) = tokens.next_token(true)?;
            let name = if let Token::Identifier(name) = next_tok {
                name
            } else {
                return Err(ParseError::unexpected_token(next_tok, ctx2));
            };
            Expr::field_access(ExprData::Globals.spanning(ctx), (name.to_string(), ctx2))
        }
        Token::LeftCurlyBrace => {
            let (members, end_span) = parse_table_or_class(tokens, &Token::Comma, false)?;
            Expr::table_decl(members, ctx, end_span)
        }
        Token::Function => parse_function(tokens, ctx)?,
        Token::LeftSquareBracket => {
            tokens.stash((first_token, ctx)); // Put the bracket back
            let (elements, span) =
                parse_list(tokens, Token::LeftSquareBracket, Token::RightSquareBracket)?;
            Expr::array_decl(elements, span)
        }
        // Prefix Unary Operators
        Token::Minus
        | Token::Plus
        | Token::Not
        | Token::BitNot
        | Token::Typeof
        | Token::Clone
        | Token::Resume => {
            let op = match first_token {
                Token::Minus => UnaryOp::Neg,
                Token::Not => UnaryOp::Not,
                Token::BitNot => UnaryOp::BitNot,
                Token::Typeof => UnaryOp::TypeOf,
                Token::Clone => UnaryOp::Clone,
                Token::Resume => UnaryOp::Resume,
                _ => unreachable!(),
            };
            let rhs = parse_expr_bp(
                tokens,
                get_prefix_bp(&first_token),
                is_term,
                ignore_newlines,
            )?;
            Expr::unary_op(op, rhs, ctx)
        }
        Token::Increment | Token::Decrement => {
            let rhs = parse_expr_bp(
                tokens,
                get_prefix_bp(&first_token),
                is_term,
                ignore_newlines,
            )?;
            let op = if matches!(first_token, Token::Increment) {
                UnaryRefOp::PreIncr
            } else {
                UnaryRefOp::PreDecr
            };
            Expr::unary_ref_op(op, rhs.try_into()?, ctx)
        }
        Token::RawCall => {
            let (args, end_span) =
                parse_list(tokens, Token::LeftParenthesis, Token::RightParenthesis)?;
            let mut args_iter = args.into_iter();
            let func = args_iter.next();
            let env = args_iter.next();
            let (func, env) = match (func, env) {
                (Some(func), Some(env)) => (func, env),
                _ => {
                    return Err(ParseError::syntax_error(
                        "rawcall expects at least 2 arguments".to_string(),
                        ctx | end_span,
                    ))
                }
            };
            let args = args_iter.collect();

            Expr::raw_call(func, env, args, ctx, end_span)
        }
        other => return Err(ParseError::unexpected_token(other, ctx)),
    };

    loop {
        // If this is a line that ends the file, return the current expression
        if !ignore_newlines && tokens.try_peek_token(false)?.is_none() {
            return Ok(lhs);
        }
        // TODO: Handle newline behavior here
        let (op_token, ctx) = tokens.peek_token(ignore_newlines)?;
        let ctx = *ctx;
        let Some((l_bp, r_bp)) = get_bp(op_token) else {
            if is_term(op_token) {
                return Ok(lhs);
            } else {
                return Err(ParseError::unexpected_token(op_token.clone(), ctx));
            }
        };
        if l_bp < min_bp {
            break;
        }
        match op_token {
            Token::Increment | Token::Decrement => {
                let incr = matches!(op_token, Token::Increment);
                tokens.skip_token();
                lhs = Expr::unary_ref_op(
                    if incr {
                        UnaryRefOp::PostIncr
                    } else {
                        UnaryRefOp::PostDecr
                    },
                    lhs.try_into()?,
                    ctx,
                );
                continue;
            }
            Token::LeftParenthesis => {
                let (args, call_span) =
                    parse_list(tokens, Token::LeftParenthesis, Token::RightParenthesis)?;
                lhs = Expr::function_call(lhs, args, call_span);
                continue;
            }
            Token::LeftSquareBracket => {
                tokens.skip_token();
                let (index, end_span) = parse_expr_token(tokens, Token::RightSquareBracket)?;
                lhs = Expr::array_access(lhs, index, ctx, end_span);
                continue;
            }
            _ => {}
        }
        let (op_token, op_span) = tokens.next_token(true)?;
        if matches!(op_token, Token::QuestionMark) {
            let (true_expr, _colon_span) = parse_expr_token(tokens, Token::Colon)?;
            let false_expr = parse_expr_bp(tokens, r_bp, is_term, ignore_newlines)?;
            lhs = Expr::ternary(lhs, true_expr, false_expr);
            continue;
        };
        let rhs = parse_expr_bp(tokens, r_bp, is_term, ignore_newlines)?;
        lhs = match op_token {
            // Member Access
            Token::Period => {
                let Expr { data, span } = rhs;
                if let ExprData::Ident(name) = data {
                    Expr::field_access(lhs, (name, span))
                } else {
                    return Err(ParseError::syntax_error(
                        format!("Expected Identifier after '.'"),
                        op_span,
                    ));
                }
            }
            // TODO: Some of this should be a macro
            // Multiply and Divide
            Token::Multiply => Expr::binary_op(BinaryOp::Mul, op_span, lhs, rhs),
            Token::Divide => Expr::binary_op(BinaryOp::Div, op_span, lhs, rhs),
            Token::Modulus => Expr::binary_op(BinaryOp::Mod, op_span, lhs, rhs),
            // Addition and Subtraction
            Token::Plus => Expr::binary_op(BinaryOp::Add, op_span, lhs, rhs),
            Token::Minus => Expr::binary_op(BinaryOp::Sub, op_span, lhs, rhs),
            // Shift Operators
            Token::RightShiftArith => Expr::binary_op(BinaryOp::AShr, op_span, lhs, rhs),
            Token::RightShift => Expr::binary_op(BinaryOp::Shr, op_span, lhs, rhs),
            Token::LeftShift => Expr::binary_op(BinaryOp::Shl, op_span, lhs, rhs),
            // Comparison Operators
            // TODO: These give wrong values for NaNs
            Token::LessThan => Expr::binary_op(BinaryOp::Less, op_span, lhs, rhs),
            Token::LessThanEquals => Expr::unary_op(
                UnaryOp::Not,
                Expr::binary_op(BinaryOp::Greater, op_span, lhs, rhs),
                op_span,
            ),
            Token::GreaterThan => Expr::binary_op(BinaryOp::Greater, op_span, lhs, rhs),
            Token::GreaterThanEquals => Expr::unary_op(
                UnaryOp::Not,
                Expr::binary_op(BinaryOp::Less, op_span, lhs, rhs),
                op_span,
            ),
            Token::InstaceOf => Expr::binary_op(BinaryOp::InstanceOf, op_span, lhs, rhs),
            // Equality Operators
            Token::Equals => Expr::binary_op(BinaryOp::Eq, op_span, lhs, rhs),
            Token::DoesNotEqual => Expr::binary_op(BinaryOp::Eq, op_span, lhs, rhs),
            Token::Compare => Expr::binary_op(BinaryOp::Compare, op_span, lhs, rhs),
            // Bitwise And
            Token::BitAnd => Expr::binary_op(BinaryOp::BitAnd, op_span, lhs, rhs),
            // Bitwise Xor
            Token::BitXor => Expr::binary_op(BinaryOp::BitXor, op_span, lhs, rhs),
            // Logical And and In
            Token::In => Expr::binary_op(BinaryOp::In, op_span, lhs, rhs),
            Token::And => Expr::binary_op(BinaryOp::And, op_span, lhs, rhs),
            // Assignment Operators
            Token::Assign => Expr::assign(lhs.try_into()?, op_span, rhs, AssignKind::Normal),
            Token::NewSlot => Expr::assign(lhs.try_into()?, op_span, rhs, AssignKind::NewSlot),
            Token::PlusAssign => Expr::assign(lhs.try_into()?, op_span, rhs, AssignKind::Add),
            Token::MinusAssign => Expr::assign(lhs.try_into()?, op_span, rhs, AssignKind::Sub),
            Token::MultiplyAssign => Expr::assign(lhs.try_into()?, op_span, rhs, AssignKind::Mult),
            Token::DivideAssign => Expr::assign(lhs.try_into()?, op_span, rhs, AssignKind::Div),
            other => panic!("Token {:?} should not have a binding power", other),
        };
    }

    Ok(lhs)
}
