use std::str::FromStr;

use ariadne::{Label, Report, ReportKind};
use logos::{Lexer, Logos, Skip};
use serde::{Deserialize, Serialize};

use crate::parser::error::ParseError;

use crate::context::{DisplayReport, Span, SquirrelError};

use self::error::{LexError, LexResult};

#[derive(Clone, Debug, PartialEq, Logos, Serialize, Deserialize)]
#[logos(extras = LexerContext)]
#[logos(error = LexError)]
#[logos(skip r"[ \t\r\f]+")]
pub enum Token {
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
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
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
    #[regex(r#"[^\\"\n]+"#)]
    #[regex(r"\\.", |lex| escape_lookup(lex.slice()))]
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
        r"\n" => Some("\n"),
        r"\r" => Some("\r"),
        r"\t" => Some("\t"),
        r"\\" => Some("\\"),
        r#"\""# => Some("\""),
        _ => None,
    }
}

fn escape_str<'s, T>(lexer: &mut Lexer<'s, Token>, allow_newlines: bool) -> LexResult<String>
where
    T: Logos<'s, Source = str> + Into<EscapedString<'s>>,
    <T as Logos<'s>>::Extras: Default,
{
    let remainder = &lexer.source()[lexer.span().end..];
    let mut escape_lexer = T::lexer(remainder);
    let mut fragments: Vec<&str> = Vec::new();
    while let Some(fragment) = escape_lexer.next() {
        match fragment.map(Into::into) {
            Ok(EscapedString::Fragment(fragment)) => fragments.push(fragment),
            Ok(EscapedString::End) => {
                lexer.bump(escape_lexer.span().end);
                return Ok(fragments.join(""));
            }
            Ok(EscapedString::Newline) => {
                if allow_newlines {
                    lexer.extras.log_newlines(1);
                    fragments.push("\n")
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

// TODO: This would be faster if it is rolled into the escaping logic for strings
fn find_newlines<'s>(lexer: &mut Lexer<'s, Token>) -> &'s str {
    let slice = lexer.slice();
    lexer
        .extras
        .log_newlines(slice.chars().filter(|&c| c == '\n').count() as u32);
    slice
}

fn parse_sci<'s>(lexer: &Lexer<'s, Token>) -> LexResult<f64> {
    // TODO: Error handling
    let s = lexer.slice();
    let e_loc = s.find('e').unwrap();
    let (base, exp) = s.split_at(e_loc);
    let base = f64::from_str(base)?;
    // Skip the 'e'
    let exp = i32::from_str(&exp[1..])?;
    Ok(base * 10f64.powi(exp))
}

fn parse_char_lit<'s>(lexer: &Lexer<'s, Token>) -> LexResult<i64> {
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

pub struct SpannedLexer<'s> {
    logos: logos::Lexer<'s, Token>,
    stored_next: Option<(Token, Span)>,
}

impl<'s> SpannedLexer<'s> {
    pub fn new(input: &'s str, file_name: String) -> Self {
        Self {
            logos: Token::lexer_with_extras(input, LexerContext::new(file_name)),
            stored_next: None,
        }
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

impl SpannedLexer<'_> {
    fn next_internal(&mut self) -> Result<Option<(Token, Span)>, SquirrelError> {
        let token = self.logos.next();
        let op = match token {
            Some(Ok(token)) => {
                let span = self.logos.span().into();
                Some((token, span))
            }
            Some(Err(err)) => {
                return Err(err.with_context(&self.logos, self.get_file_name().to_string()))
            }
            None => None,
        };
        Ok(op)
    }

    fn peek(&mut self) -> Result<Option<&(Token, Span)>, SquirrelError> {
        if self.stored_next.is_none() {
            self.stored_next = self.next_internal()?;
        }
        Ok(self.stored_next.as_ref())
    }

    pub fn skip_newlines(&mut self) {
        while let Ok(Some((Token::Newline, _))) = self.peek() {
            self.next();
        }
    }

    pub fn next_token(&mut self, skip_newlines: bool) -> Result<(Token, Span), ParseError> {
        if skip_newlines {
            self.skip_newlines();
        }
        self.next()
            .ok_or_else(ParseError::unexpected_eof)?
            .map_err(Into::into)
    }

    pub fn peek_token(&mut self, skip_newlines: bool) -> Result<&(Token, Span), ParseError> {
        self.try_peek_token(skip_newlines)
            .and_then(|op| op.ok_or_else(ParseError::unexpected_eof))
    }

    pub fn try_peek_token(
        &mut self,
        skip_newlines: bool,
    ) -> Result<Option<&(Token, Span)>, ParseError> {
        if skip_newlines {
            self.skip_newlines();
        }
        // First, fill stored_next
        if self.stored_next.is_none() {
            self.stored_next = self.next_internal()?;
        }

        Ok(self.stored_next.as_ref())
    }

    pub fn expect_token(
        &mut self,
        expected: Token,
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
        self.next().unwrap().unwrap();
    }

    pub fn stash(&mut self, tsp: (Token, Span)) {
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
            DisplayReport::new(&report, self.get_file_name(), self.logos.source())
        );
    }
}

impl Iterator for SpannedLexer<'_> {
    type Item = Result<(Token, Span), SquirrelError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.stored_next.take() {
            Some(Ok(next))
        } else {
            match self.next_internal() {
                Ok(Some(next)) => Some(Ok(next)),
                Ok(None) => None,
                Err(err) => Some(Err(err)),
            }
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

    use crate::context::{SqBacktrace, SquirrelError};

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
            lexer: &Lexer<'s, Token>,
            file_name: String,
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
            SquirrelError::new(file_name, token_location, message, SqBacktrace::new())
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
    use crate::context::IntoSquirrelErrorContext;

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

    fn sample_test(sample_path: &str, sample_contents: &str) {
        let mut spanned_lexer = SpannedLexer::new(sample_contents, sample_path.to_string());
        let tokens = spanned_lexer.by_ref().collect::<Result<Vec<_>, _>>();
        let tokens = match tokens {
            Ok(tokens) => tokens,
            Err(e) => panic!("{:#}", e.with_context(spanned_lexer.get_source())),
        };

        let actual_data = tokens
            .into_iter()
            .map(|(token, _)| token)
            .collect::<Vec<_>>();
        #[cfg(not(miri))]
        {
            let expected_data = exchange_data("lexer", sample_path, &actual_data);
            for (idx, (expected, actual)) in expected_data
                .into_iter()
                .zip(actual_data.into_iter())
                .enumerate()
            {
                assert_eq!(expected, actual, "Token at index {} does not match", idx);
            }
        }
    }

    test_foreach!(sample_test);
}
