// Copyright 2023 Marsh J. Ray
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![allow(dead_code)] //? TODO for development
#![allow(unused_mut)] //? TODO for development
#![allow(unused_variables)] //? TODO for development
#![allow(unused_imports)] //? TODO for development
#![allow(non_snake_case)] //? TODO for development

use std::{path::{Path, PathBuf}, sync::{Arc, Weak, RwLock}};

use serde::{Deserialize, Serialize};

use crate::EvaluatorConfigError;

fn verify_path_dir(dir_path: &Path) -> Result<bool, EvaluatorConfigError> {
    // See if it's empty.
    let dir_path_os_str = dir_path.as_os_str();
    if dir_path_os_str.is_empty() {
        return Err(EvaluatorConfigError::PathDirIsNot(dir_path.to_path_buf()));
    }

    let verified_dir = dir_path.try_exists()
        .map_err(|e| EvaluatorConfigError::PathDirTryExistsStdIoError(dir_path.to_path_buf(), e))?;

    if ! dir_path.is_dir() {        
        return Err(EvaluatorConfigError::PathDirNotExist(dir_path.to_path_buf()));
    }

    let verified_dir = verified_dir && dir_path.is_dir();
    Ok(verified_dir)
}

/// The overall configuration of our evaluator.
#[derive(Debug)]
pub struct EvaluatorConfig {
    /// The ordered list of paths to search when loading source files.
    source_search_paths: Vec<PathBuf>,
}

impl EvaluatorConfig {
    // Create some `SourceSearchPath`s, adding them to `v`. They will need their `weak_search_paths`
    // and `search_paths_ix` members set after it's known.
    fn source_search_paths() -> Result<Vec<PathBuf>, EvaluatorConfigError> {

        // Start with the "." current directory path.
        let mut search_paths = vec![".".into()];

        // Add any paths from the command line.
        //? TODO

        // Add the paths from the BCIRC_PATH env var.
        match std::env::var_os(crate::BCIRC_PATH_ENV_VAR_NAME) {
            Some(env_bcirc_paths) => {
                for bcirc_path in std::env::split_paths(&env_bcirc_paths) {
                    let path_buf = bcirc_path.to_owned();
                    let _ = verify_path_dir(&path_buf)?;
                    search_paths.push(path_buf);
                }
            }
            None => {
                return Err(EvaluatorConfigError::BcircPathEnvVarNotDefined);
            }
        }

        Ok(search_paths)
    }

    /// Creates a new, default, [`EvaluatorConfig`]. You should be able to call `Arc::get_mut()`.
    pub fn new_default() -> Result<Arc<RwLock<EvaluatorConfig>>, EvaluatorConfigError> {
        let source_search_paths = Self::source_search_paths()?;

        let ec = EvaluatorConfig {
            source_search_paths,
        };

        Ok(Arc::new(RwLock::new(ec)))
    }
}
