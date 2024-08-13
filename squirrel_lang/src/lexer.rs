use std::{
    borrow::Cow,
    convert::{self, identity},
    str::FromStr,
};

use ariadne::{Cache, Label, Report, ReportKind, Source};
use hashbrown::HashMap;
use logos::{Lexer, Logos, Skip};
use serde::{Deserialize, Serialize};

use crate::{
    parser::{ast, error::ParseError},
    sq_error::render_report,
};

use crate::sq_error::{Span, SquirrelError};

use self::error::{LexError, LexResult};

#[derive(Clone, Debug, PartialEq, Logos, Serialize, Deserialize)]
#[logos(extras = LexerContext)]
#[logos(error = LexError)]
#[logos(skip r"[ \t\r\f]+")]
pub enum Token<'s> {
    // Keywords
    #[token("base")]
    Base,
    #[token("break")]
    Break,
    #[token("case")]
    Case,
    #[token("catch")]
    Catch,
    #[token("class")]
    Class,
    #[token("clone")]
    Clone,
    #[token("continue")]
    Continue,
    #[token("const")]
    Const,
    #[token("default")]
    Default,
    #[token("delete")]
    Delete,
    #[token("else")]
    Else,
    #[token("enum")]
    Enum,
    #[token("extends")]
    Extends,
    #[token("for")]
    For,
    #[token("foreach")]
    ForEach,
    #[token("function")]
    Function,
    #[token("if")]
    If,
    #[token("in")]
    In,
    #[token("local")]
    Local,
    #[token("resume")]
    Resume,
    #[token("return")]
    Return,
    #[token("switch")]
    Switch,
    #[token("this")]
    This,
    #[token("throw")]
    Throw,
    #[token("try")]
    Try,
    #[token("typeof")]
    Typeof,
    #[token("do")]
    Do,
    #[token("while")]
    While,
    #[token("yield")]
    Yield,
    #[token("instanceof")]
    InstaceOf,
    #[token("static")]
    Static,
    #[token("rawcall")]
    RawCall,
    // Operators
    #[token("!")]
    Not,
    #[token("!=")]
    DoesNotEqual,
    #[token("||")]
    Or,
    #[token("==")]
    Equals,
    #[token("&&")]
    And,
    #[token(">=")]
    GreaterThanEquals,
    #[token("<=")]
    LessThanEquals,
    #[token(">")]
    GreaterThan,
    #[token("<")]
    LessThan,
    #[token("<=>")]
    Compare,
    #[token("+")]
    Plus,
    #[token("+=")]
    PlusAssign,
    #[token("-")]
    Minus,
    #[token("-=")]
    MinusAssign,
    #[token("/")]
    Divide,
    #[token("/=")]
    DivideAssign,
    #[token("*")]
    Multiply,
    #[token("*=")]
    MultiplyAssign,
    #[token("%")]
    Modulus,
    #[token("%=")]
    ModulusAssign,
    #[token("++")]
    Increment,
    #[token("--")]
    Decrement,
    #[token("<-")]
    NewSlot,
    #[token("=")]
    Assign,
    #[token("&")]
    BitAnd,
    #[token("^")]
    BitXor,
    #[token("|")]
    BitOr,
    #[token("~")]
    BitNot,
    #[token(">>")]
    RightShift,
    #[token("<<")]
    LeftShift,
    #[token(">>>")]
    RightShiftArith,
    // Misc Symbols
    #[token("{")]
    LeftCurlyBrace,
    #[token("}")]
    RightCurlyBrace,
    #[token("[")]
    LeftSquareBracket,
    #[token("]")]
    RightSquareBracket,
    #[token("(")]
    LeftParenthesis,
    #[token(")")]
    RightParenthesis,
    #[token(".")]
    Period,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token(";")]
    Semicolon,
    #[token("?")]
    QuestionMark,
    #[token("\n", |lex| {
        lex.extras.log_newlines(1);
    })]
    Newline,
    #[token("@")]
    AtSymbol,
    #[token("...")]
    Varargs,
    // Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
    Identifier(&'s str),
    // Literals
    #[token("\"", |lex| escape_str::<EscapedString>(lex, false))]
    #[token("@\"", |lex| escape_str::<VerbString>(lex, true))]
    #[token("__FILE__", |lex| lex.extras.file_name.clone())]
    String(String),
    #[regex(r"\d+", |lex| i64::from_str(lex.slice()))]
    #[regex(r"0\d+", |lex| i64::from_str_radix(&lex.slice()[1..], 8))]
    #[regex(r"0x[0-9a-fA-F]+", |lex| i64::from_str_radix(&lex.slice()[2..], 16))]
    #[regex(r"'([^'])*'", |lex| parse_char_lit(lex))]
    #[token("__LINE__", |lex| lex.extras.get_line() as i64)]
    Integer(i64),
    // Todo: handle missing number after decimal point
    #[regex(r"\d+\.\d*", |lex| f64::from_str(lex.slice()))]
    #[regex(r"\d+(\.\d*)?e[+-]?\d*", |lex| parse_sci(lex))]
    Number(f64),
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Boolean(bool),
    #[token("null")]
    Null,
    // Comments
    #[regex(r"//[^\n]*", |lex| {
        let _content = &lex.slice()[2..];
        Skip
    }
    )]
    #[regex(r"#[^\n]*", |lex| {
        let _content = &lex.slice()[1..];
        Skip
    })]
    #[regex(r"/\*([^*]|\n|\*[^/])*\*/", |lex| {
        let _content = trim_str(find_newlines(lex), 2, 2);
        Skip
    })]
    Comment,
    // Attributes
    // For now we are ignoring these
    #[regex(r"</[^(/>)]*/>", |_| Skip)]
    Attribute,
}

fn trim_str(s: &str, trim_start: usize, trim_end: usize) -> &str {
    &s[trim_start..s.len() - trim_end]
}

#[derive(Debug, PartialEq, Logos)]
pub enum EscapedString<'s> {
    #[regex(r"\\x[0-9a-fA-F]{2}", parse_char_hex)]
    #[regex(r"\\u[0-9a-fA-F]{4}", parse_char_hex)]
    #[regex(r"\\U[0-9a-fA-F]{8}", parse_char_hex)]
    // todo: Inefficient, need to have custom function to concatenate
    // todo: an iterator of str and char below, then we can have this hold a char instead
    Owned(String),
    #[regex(r"\\.", |lex| escape_lookup(lex.slice()))]
    #[regex(r#"[^\\"\n]+"#)]
    Fragment(&'s str),
    #[token("\"")]
    End,
    #[token("\n")]
    Newline,
}

#[derive(Debug, PartialEq, Logos)]
pub enum VerbString<'s> {
    #[regex(r#"[^"\n]+"#)]
    #[token("\"\"", |_| "\"")]
    Fragment(&'s str),
    #[token("\"")]
    End,
    #[token("\n")]
    Newline,
}
impl<'s> From<VerbString<'s>> for EscapedString<'s> {
    fn from(v: VerbString<'s>) -> Self {
        match v {
            VerbString::Fragment(s) => EscapedString::Fragment(s),
            VerbString::End => EscapedString::End,
            VerbString::Newline => EscapedString::Newline,
        }
    }
}

fn escape_lookup(s: &str) -> Option<&'static str> {
    // TODO: Check squirrel source code for other escapes
    match s {
        r"\a" => Some("\x07"),
        r"\b" => Some("\x08"),
        r"\f" => Some("\x0C"),
        r"\n" => Some("\n"),
        r"\r" => Some("\r"),
        r"\t" => Some("\t"),
        r"\v" => Some("\x0B"),
        r"\\" => Some("\\"),
        r"\'" => Some("\'"),
        r"\0" => Some("\0"),
        r#"\""# => Some("\""),
        _ => None,
    }
}

fn parse_char_hex<'s>(lexer: &mut Lexer<'s, EscapedString<'s>>) -> Result<String, ()> {
    let hex_str = &lexer.slice()[2..];
    let codepoint = u32::from_str_radix(hex_str, 16).unwrap();
    match char::from_u32(codepoint) {
        Some(c) => Ok(c.to_string()),
        None => Err(()),
    }
}

fn escape_str<'s, T>(lexer: &mut Lexer<'s, Token<'s>>, allow_newlines: bool) -> LexResult<String>
where
    T: Logos<'s, Source = str> + Into<EscapedString<'s>>,
    <T as Logos<'s>>::Extras: Default,
{
    let remainder = &lexer.source()[lexer.span().end..];
    let mut escape_lexer = T::lexer(remainder);
    let mut fragments: Vec<Cow<str>> = Vec::new();
    while let Some(fragment) = escape_lexer.next() {
        match fragment.map(Into::into) {
            Ok(EscapedString::Fragment(fragment)) => fragments.push(fragment.into()),
            Ok(EscapedString::End) => {
                lexer.bump(escape_lexer.span().end);
                return Ok(fragments.join(""));
            }
            Ok(EscapedString::Owned(c)) => fragments.push(c.into()),
            Ok(EscapedString::Newline) => {
                if allow_newlines {
                    lexer.extras.log_newlines(1);
                    fragments.push("\n".into())
                } else {
                    return Err(LexError::General(
                        "Newline in non-verbatim string".to_string(),
                    ));
                }
            }
            Err(_) => {
                return Err(LexError::General(format!(
                    "Illegal escape sequence in string: \"{}\"",
                    escape_lexer.slice()
                )))
            }
        }
    }

    Err(LexError::UnterminatedString)
}

fn find_newlines<'s>(lexer: &mut Lexer<'s, Token<'s>>) -> &'s str {
    let slice = lexer.slice();
    lexer
        .extras
        .log_newlines(slice.chars().filter(|&c| c == '\n').count() as u32);
    slice
}

fn parse_sci<'s>(lexer: &Lexer<'s, Token<'s>>) -> LexResult<f64> {
    // TODO: Error handling
    let s = lexer.slice();
    let e_loc = s.find('e').unwrap();
    let (base, exp) = s.split_at(e_loc);
    let base = f64::from_str(base)?;
    // Skip the 'e'
    let exp = i32::from_str(&exp[1..])?;
    Ok(base * 10f64.powi(exp))
}

fn parse_char_lit<'s>(lexer: &Lexer<'s, Token<'s>>) -> LexResult<i64> {
    let mut chars = lexer.slice().chars();
    let open_quote = chars.next().unwrap();
    debug_assert!(open_quote == '\'');
    let target = chars.next().unwrap();
    let close_quote = chars.next().unwrap();
    let end = chars.next();
    if close_quote != '\'' || end.is_some() {
        return Err(LexError::General("Invalid character literal".to_string()));
    }
    Ok(target as i64)
}

#[derive(Debug)]
pub struct FunctionLocals<'s> {
    // Locals defined in this function
    locals: Vec<(&'s str, u32, bool)>,
    // Outer local index, Inner local index
    upvalues: Vec<(u32, u32)>,
    visible_locals: Vec<HashMap<&'s str, u32>>,
}

impl<'s> FunctionLocals<'s> {
    const OUTSIDE_LOCAL: &'static str = "Cannot call when outside of local scope";
    pub fn local_count(&self) -> u32 {
        self.locals.len() as u32 + self.upvalues.len() as u32
    }

    fn new() -> Self {
        Self {
            locals: Vec::new(),
            upvalues: Vec::new(),
            visible_locals: Vec::new(),
        }
    }

    fn validate(&self) -> bool {
        let mut locals_found = vec![false; self.locals.len() + self.upvalues.len()];
        for idx in self
            .locals
            .iter()
            .map(|(_, idx, _)| *idx)
            .chain(self.upvalues.iter().map(|(_, idx)| *idx))
        {
            if let Some(iref) = locals_found.get_mut(idx as usize) {
                if *iref {
                    return false;
                }
                *iref = true;
            } else {
                return false;
            }
        }

        locals_found.into_iter().all(convert::identity)
    }

    fn add_local(&mut self, local: &'s str) -> u32 {
        let local_idx = self.local_count();
        self.visible_locals
            .last_mut()
            .expect(Self::OUTSIDE_LOCAL)
            .insert(local, local_idx);
        self.locals.push((local, local_idx, false));
        local_idx
    }

    fn add_upvalue(&mut self, upvalue: &'s str, parent_idx: u32) -> u32 {
        let upvalue_idx = self.local_count();
        self.visible_locals
            .first_mut()
            .expect(Self::OUTSIDE_LOCAL)
            .insert(upvalue, upvalue_idx);
        self.upvalues.push((parent_idx, upvalue_idx));
        upvalue_idx
    }

    pub fn to_locals(self) -> Vec<ast::Local> {
        let mut locals: Vec<Option<ast::Local>> = vec![None; self.local_count() as usize];
        for (name, idx, is_upvalue) in self.locals {
            let idx = idx as usize;
            assert_eq!(locals[idx], None);
            locals[idx] = Some(if is_upvalue {
                ast::Local::Local(name.to_owned())
            } else {
                ast::Local::NamedReg(name.to_owned())
            });
        }

        for (parent_idx, idx) in self.upvalues {
            let idx = idx as usize;
            assert_eq!(locals[idx], None);
            locals[idx] = Some(ast::Local::Upvalue(parent_idx));
        }

        locals
            .into_iter()
            .map(|o| o.expect("Local indicies are not contiguous"))
            .collect()
    }
}

pub struct LocalResolution<'s> {
    fn_scopes: Vec<FunctionLocals<'s>>,
}

impl<'s> LocalResolution<'s> {
    const OUTSIDE_GLOBAL: &'static str = "Cannot call add_local when outside of global scope";
    const OUTSIDE_LOCAL: &'static str = "Cannot call add_local when outside of local scope";

    pub fn add_local(&mut self, local: &'s str) -> u32 {
        self.fn_scopes
            .last_mut()
            .expect(Self::OUTSIDE_GLOBAL)
            .add_local(local)
    }

    pub fn maybe_reference_local(&mut self, local: &'s str) -> Option<u32> {
        let (mut local_idx, fn_idx) = self
            .fn_scopes
            .iter()
            .enumerate()
            .rev()
            .find_map(|(fn_idx, fn_scope)| {
                fn_scope
                    .visible_locals
                    .iter()
                    .find_map(|scope| scope.get(local).map(|idx| (*idx, fn_idx)))
            })?
            .clone();

        // Need to propogate this local upwards through upvalues
        let current_func_idx = self.fn_scopes.len() - 1;
        if current_func_idx != fn_idx {
            // This local is used as an upvalue
            self.fn_scopes[fn_idx].locals[local_idx as usize].2 = true;
        }
        for func_scope_idx in fn_idx..current_func_idx {
            let next_func_idx = func_scope_idx + 1;
            local_idx = self.fn_scopes[next_func_idx].add_upvalue(local, local_idx);
        }
        // println!("{}Resolved local reference: {local} -> {local_idx}", " ".repeat(self.visible_locals.len()));

        Some(local_idx)
    }

    fn enter_scope(&mut self) {
        self.fn_scopes
            .last_mut()
            .expect(Self::OUTSIDE_GLOBAL)
            .visible_locals
            .push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.fn_scopes
            .last_mut()
            .expect(Self::OUTSIDE_GLOBAL)
            .visible_locals
            .pop()
            .expect(Self::OUTSIDE_LOCAL);
    }

    fn enter_fn_scope(&mut self, args: impl IntoIterator<Item = &'s str>, is_varargs: bool) {
        let mut fl = FunctionLocals::new();
        fl.visible_locals.push(HashMap::new());
        self.fn_scopes.push(fl);
        args.into_iter().for_each(|arg| {
            self.add_local(arg);
        });
        if is_varargs {
            self.add_local("vargv");
        }
    }

    fn exit_fn_scope(&mut self) -> FunctionLocals<'s> {
        assert_eq!(
            self.fn_scopes.last().map(|fl| fl.visible_locals.len()),
            Some(1)
        );
        self.fn_scopes.pop().expect(Self::OUTSIDE_GLOBAL)
    }

    fn new() -> Self {
        Self {
            fn_scopes: Vec::new(),
        }
    }
}

pub struct SpannedLexer<'s> {
    file_id: usize,
    logos: logos::Lexer<'s, Token<'s>>,
    stored_next: Option<(Token<'s>, Span)>,
    locals: LocalResolution<'s>,
    source: Source<&'s str>,
}

impl<'s> Cache<usize> for SpannedLexer<'s> {
    type Storage = &'s str;

    fn fetch(
        &mut self,
        id: &usize,
    ) -> Result<&ariadne::Source<Self::Storage>, Box<dyn std::fmt::Debug + '_>> {
        if *id == self.file_id {
            Ok(&self.source)
        } else {
            Err(Box::new(format!(
                "Invalid file id: {} (expected: {})",
                id, self.file_id
            )))
        }
    }

    fn display<'a>(&self, id: &'a usize) -> Option<Box<dyn std::fmt::Display + 'a>> {
        if self.file_id == *id {
            Some(Box::new(self.logos.extras.file_name.clone()))
        } else {
            None
        }
    }
}

impl<'s> SpannedLexer<'s> {
    pub fn new(input: &'s str, file_name: String) -> Self {
        Self {
            file_id: 0,
            logos: Token::lexer_with_extras(input, LexerContext::new(file_name)),
            stored_next: None,
            locals: LocalResolution::new(),
            source: Source::from(input),
        }
    }

    pub fn get_file_id(&self) -> usize {
        self.file_id
    }

    pub fn lcl(&mut self) -> &mut LocalResolution<'s> {
        &mut self.locals
    }

    pub fn scoped<T, F: FnOnce(&mut Self) -> T>(&mut self, f: F) -> T {
        self.locals.enter_scope();
        let result = f(self);
        self.locals.exit_scope();
        result
    }

    pub fn fn_scoped<T, E, F>(
        &mut self,
        args: impl IntoIterator<Item = &'s str>,
        is_varargs: bool,
        f: F,
    ) -> Result<(T, FunctionLocals<'s>), E>
    where
        F: FnOnce(&mut Self) -> Result<T, E>,
    {
        self.locals.enter_fn_scope(args, is_varargs);
        let result = f(self);
        let fn_locals = self.locals.exit_fn_scope();
        let result = result?;
        assert!(
            fn_locals.validate(),
            "Function locals are invalid: {:?}",
            fn_locals
        );
        Ok((result, fn_locals))
    }

    pub fn get_file_name(&self) -> &str {
        &self.logos.extras.file_name
    }

    pub fn get_source(&self) -> &str {
        &self.logos.source()
    }

    pub fn current_line(&self) -> u32 {
        self.logos.extras.get_line()
    }

    pub fn current_offset(&self) -> usize {
        self.logos.span().end
    }
}

// TODO: Skip newlines is true in nearly every case. Maybe it should be the default behavior
impl<'s> SpannedLexer<'s> {
    fn next_internal(&mut self) -> Result<(), SquirrelError> {
        let token = self.logos.next();
        let op = match token {
            Some(Ok(token)) => {
                let span = self.logos.span().into();
                Some((token, span))
            }
            Some(Err(err)) => return Err(err.with_context(&self.logos, self.file_id)),
            None => None,
        };
        self.stored_next = op;
        Ok(())
    }

    fn peek(&mut self) -> Result<Option<&(Token<'s>, Span)>, SquirrelError> {
        if self.stored_next.is_none() {
            self.next_internal()?;
        }
        Ok(self.stored_next.as_ref())
    }

    pub fn skip_newlines(&mut self) {
        while let Ok(Some((Token::Newline, _))) = self.peek() {
            self.next();
        }
    }

    pub fn next_token(&mut self, skip_newlines: bool) -> Result<(Token<'s>, Span), ParseError> {
        if skip_newlines {
            self.skip_newlines();
        }
        self.next()
            .ok_or_else(ParseError::unexpected_eof)?
            .map_err(Into::into)
    }

    // TODO: Maybe this should be (&Token, Span)
    pub fn peek_token(&mut self, skip_newlines: bool) -> Result<&(Token<'s>, Span), ParseError> {
        self.try_peek_token(skip_newlines)
            .and_then(|op| op.ok_or_else(ParseError::unexpected_eof))
    }

    pub fn try_peek_token(
        &mut self,
        skip_newlines: bool,
    ) -> Result<Option<&(Token<'s>, Span)>, ParseError> {
        if skip_newlines {
            self.skip_newlines();
        }
        // First, fill stored_next
        if self.stored_next.is_none() {
            self.next_internal()?;
        }

        Ok(self.stored_next.as_ref())
    }

    pub fn expect_token(
        &mut self,
        expected: Token<'s>,
        skip_newlines: bool,
    ) -> Result<Span, ParseError> {
        let (token, span) = self.next_token(skip_newlines)?;
        if token == expected {
            Ok(span)
        } else {
            Err(ParseError::unexpected_token(token, span))
        }
    }

    pub fn skip_token(&mut self) {
        assert!(
            self.stored_next.is_some(),
            "Can only call skip_token after a peek() call"
        );
        self.stored_next.take();
    }

    pub fn stash(&mut self, tsp: (Token<'s>, Span)) {
        assert!(
            self.stored_next.is_none(),
            "Can only call stash after a next() call"
        );
        self.stored_next = Some(tsp);
    }

    pub fn dbg(&self, span: Span, label: &str) {
        let report = Report::build(ReportKind::Advice, self.get_file_name(), span.start)
            .with_message(label)
            .with_label(
                Label::new(span.in_file(self.get_file_name()))
                    .with_message("Here")
                    .with_color(ariadne::Color::Blue),
            )
            .finish();
        println!(
            "{}",
            render_report(&report, self.get_file_name(), self.logos.source())
        );
    }
}

impl<'s> Iterator for SpannedLexer<'s> {
    type Item = Result<(Token<'s>, Span), SquirrelError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.stored_next.take() {
            Some(Ok(next))
        } else {
            self.next_internal()
                .map(|_| self.stored_next.take())
                .transpose()
        }
    }
}

#[derive(Debug)]
pub struct LexerContext {
    line_num: u32,
    file_name: String,
}

impl LexerContext {
    pub fn get_line(&self) -> u32 {
        self.line_num
    }

    pub fn new(file_name: String) -> Self {
        Self {
            line_num: 1,
            file_name,
        }
    }
}

impl LexerContext {
    pub fn log_newlines(&mut self, count: u32) {
        self.line_num += count;
    }
}

mod error {
    use std::num::{ParseFloatError, ParseIntError};

    use logos::Lexer;

    use crate::sq_error::{RsBacktrace, SquirrelError};

    use super::Token;

    pub type LexResult<T> = Result<T, LexError>;
    #[derive(Debug, Clone, PartialEq)]
    pub enum LexError {
        ParseIntError(ParseIntError),
        ParseFloatError(ParseFloatError),
        UnknownToken,
        UnterminatedString,
        General(String),
    }

    impl Default for LexError {
        fn default() -> Self {
            LexError::UnknownToken
        }
    }

    impl LexError {
        pub fn with_context<'s>(
            self,
            lexer: &Lexer<'s, Token<'s>>,
            file_id: usize,
        ) -> SquirrelError {
            let token_location = lexer.span().into();
            let message = match self {
                LexError::ParseIntError(err) => {
                    format!("Failed to parse integer literal: {}", err)
                }
                LexError::ParseFloatError(err) => {
                    format!("Failed to parse float literal: {}", err)
                }
                LexError::UnknownToken => "Unknown token".to_string(),
                LexError::General(msg) => msg,
                LexError::UnterminatedString => "String missing termination character".to_string(),
            };
            SquirrelError::new(file_id, token_location, message, RsBacktrace::new())
        }
    }

    impl From<ParseIntError> for LexError {
        fn from(err: ParseIntError) -> Self {
            LexError::ParseIntError(err)
        }
    }

    impl From<ParseFloatError> for LexError {
        fn from(err: ParseFloatError) -> Self {
            LexError::ParseFloatError(err)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::error::LexResult;

    use crate::test_foreach;
    use crate::test_util::exchange_data;

    use super::*;

    fn lex(input: &str, file_name: String) -> Vec<Result<Token, LexError>> {
        Lexer::with_extras(input, LexerContext::new(file_name)).collect()
    }

    #[test]
    fn single_token_test() {
        let file_name = "single_token_test".to_string();

        println!("Checking Basic Tokens");
        let result = lex("base", file_name.clone());
        assert_eq!(vec![Ok(Token::Base)], result);

        let result = lex("true", file_name.clone());
        assert_eq!(vec![Ok(Token::Boolean(true))], result);

        // Strings
        println!("Checking Strings");
        let result = lex(r#""Hello World""#, file_name.clone());
        assert_eq!(vec![Ok(Token::String("Hello World".into()))], result);

        println!("Checking String Escapes");
        let result = lex(r#""Hello\"World""#, file_name.clone());
        assert_eq!(vec![Ok(Token::String(r#"Hello"World"#.into()))], result);

        let result = lex(r#""Hello\nWorld""#, file_name.clone());
        assert_eq!(vec![Ok(Token::String("Hello\nWorld".into()))], result);

        let result = lex("\"Hello\nWorld\"", file_name.clone());
        assert!(result.len() >= 1);
        assert!(matches!(result[0], Err(LexError::General(_))));

        println!("Checking Verbatim Strings");
        let result = lex("@\"Hello\\nWorld\nNewline\"", file_name.clone());
        assert_eq!(
            vec![Ok(Token::String("Hello\\nWorld\nNewline".into()))],
            result
        );

        println!("Checking Comments");
        let result = lex("// This is a comment", file_name.clone());
        assert_eq!(Vec::<LexResult<Token>>::new(), result);

        let result = lex("# This is a comment", file_name.clone());
        assert_eq!(Vec::<LexResult<Token>>::new(), result);

        let result = lex("  /* This is a\n\n comment */  ", file_name.clone());
        assert_eq!(Vec::<LexResult<Token>>::new(), result);

        println!("Checkint Integer Literals");
        let result = lex("123", file_name.clone());
        assert_eq!(vec![Ok(Token::Integer(123))], result);

        let result = lex("0123", file_name.clone());
        assert_eq!(vec![Ok(Token::Integer(1 * 8 * 8 + 2 * 8 + 3))], result);

        let result = lex("0x123", file_name.clone());
        assert_eq!(vec![Ok(Token::Integer(0x123))], result);

        let result = lex("'a'", file_name.clone());
        assert_eq!(vec![Ok(Token::Integer(97))], result);

        println!("Checking Float Literals");
        let result = lex("7.", file_name.clone());
        assert_eq!(vec![Ok(Token::Number(7.))], result);

        let result = lex("4.0", file_name.clone());
        assert_eq!(vec![Ok(Token::Number(4.))], result);

        let result = lex("4.e2", file_name.clone());
        assert_eq!(vec![Ok(Token::Number(4e2))], result);

        let result = lex("4.e-2", file_name.clone());
        assert_eq!(vec![Ok(Token::Number(4e-2))], result);
    }

    fn sample_test(file_name: &str, file_contents: &str) {
        let test_name = format!("lexer-{}", file_name.replace("/", "-"));
        let test_desc = format!("Lexer test for {}", file_name);

        let mut spanned_lexer = SpannedLexer::new(file_contents, file_name.to_string());
        let tokens = spanned_lexer.by_ref().collect::<Result<Vec<_>, _>>();
        let tokens = match tokens {
            Ok(tokens) => tokens,
            Err(e) => panic!("{}", e.render(&mut spanned_lexer)),
        };

        let actual_data = tokens
            .into_iter()
            .map(|(token, _)| token)
            .collect::<Vec<_>>();
        #[cfg(not(miri))]
        {
            insta::assert_yaml_snapshot!(test_name, actual_data, &test_desc);
        }
    }

    test_foreach!(sample_test);
}
