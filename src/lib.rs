// Copyright 2023 Marsh J. Ray
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![feature(iter_from_generator)]
#![feature(generators, generator_trait)]
#![feature(generic_arg_infer)] // const arrays in tests
#![feature(return_position_impl_trait_in_trait)]

#![allow(dead_code)] //? TODO for development
#![allow(unused_mut)] //? TODO for development
#![allow(unused_variables)] //? TODO for development
#![allow(unused_imports)] //? TODO for development
#![allow(non_snake_case)] //? TODO for development

mod ast;
mod config;
mod values;
mod file_content;
//mod lines;
mod parser;
// mod line_char_nums;
//mod source_bytes;
mod sources;
//mod source_chars;
// mod tokenizer;
mod util;

#[cfg(test)]
mod test_util;

use std::sync::{Arc, RwLock};

use thiserror::Error;

pub use crate::config::EvaluatorConfig;

pub const BCIRC_PATH_ENV_VAR_NAME: &str = "BCIRC_PATH";

#[derive(Error, Debug)]
pub enum EvaluatorConfigError {
    // #[error("couldn't open file: {0}")]
    // StdIoError(String),

    // #[error("UTF-8 encoding error: {0}")]
    // Utf8Error(String),

    // #[error("parse error")]
    // Parse(Vec<String>),

    #[error("Env var '{BCIRC_PATH_ENV_VAR_NAME}' not defined")]
    BcircPathEnvVarNotDefined,

    #[error("The existence of source path directory '{0}' can neither be confirmed nor denied: {1}")]
    PathDirTryExistsStdIoError(std::path::PathBuf, std::io::Error),

    #[error("The source path directory '{0}' does not exist.")]
    PathDirNotExist(std::path::PathBuf),

    #[error("The source path directory '{0}' is not a directory.")]
    PathDirIsNot(std::path::PathBuf),

    #[error("todo")]
    Todo,
}

/// Creates a default config from env vars.
#[allow(non_snake_case)]
pub fn make_default_EvaluatorConfig() -> Result<Arc<RwLock<EvaluatorConfig>>, EvaluatorConfigError> {
    EvaluatorConfig::new_default()
}
