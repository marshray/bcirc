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

//? use use std::fmt::Display;
//? use std::ops::RangeInclusive;

use std::{path::PathBuf, sync::{Arc, RwLock}};

use serde::{Deserialize, Serialize};

use crate::EvaluatorConfig;

/// Information about a particular source file.
#[derive(Debug)]
pub struct SourceFileInfo {
    /// The file path, relative to the specified search path.
    file_path: PathBuf,

    /// The evaluator config.
    config: Arc<RwLock<EvaluatorConfig>>,

    /// The index into `config` `source_search_paths` by which this source file was loaded.
    source_search_paths_ix: usize,
}
