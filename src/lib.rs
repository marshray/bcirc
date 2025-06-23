// Copyright 2023-2025 Marsh J. Ray
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![allow(dead_code)] //? TODO for development
#![allow(non_snake_case)] //? TODO for development
#![allow(unused_mut)] //? TODO for development
#![allow(unused_variables)] //? TODO for development
#![allow(unused_imports)] //? TODO for development

mod ast;
mod config;
pub use crate::config::EvaluatorConfig;

mod data_repr;
mod file_content;
mod parser;
mod sources;
mod util;
mod values;

#[cfg(test)]
mod test_util;

pub const BCIRC_PATH_ENV_VAR_NAME: &str = "BCIRC_PATH";
