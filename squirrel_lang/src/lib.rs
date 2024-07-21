#![feature(ptr_metadata)]
#![feature(trace_macros)]
// TODO: Remove this
#![allow(unused)]

pub mod context;
pub mod lexer;
pub mod parser;
pub mod runtime;
pub mod util;
pub mod vm;

#[cfg(test)]
pub mod test_macro;
#[cfg(test)]
mod test_util;
