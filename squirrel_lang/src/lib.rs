#![feature(ptr_metadata)]
#![feature(trace_macros)]
// TODO: Remove this
#![allow(unused)]

pub mod context;
pub mod lexer;
pub mod parser;
pub mod sq_error;
pub mod util;
pub mod vm;
// pub mod walker;

#[cfg(test)]
pub mod test_macro;
#[cfg(test)]
mod test_util;
