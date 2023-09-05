#![allow(dead_code)] //? TODO for development
#![allow(unused_mut)] //? TODO for development
#![allow(unused_variables)] //? TODO for development
#![allow(unused_imports)] //? TODO for development

#![feature(return_position_impl_trait_in_trait)]
#![feature(generators, generator_trait, try_trait_v2)] // propane
#![feature(generic_arg_infer)] // const arrays in tests
#![feature(try_blocks)]

use std::ops::RangeInclusive;

mod data_repr;
mod util;
mod lexer;
mod source_bytes;
mod source_chars;

#[cfg(test)]
mod test_util;

//use std::fmt::{Debug, Formatter};

use anyhow::*;
fn main() -> Result<()> {

    Ok(())
}
