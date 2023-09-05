#![allow(dead_code)] //? TODO for development
#![allow(unused_mut)] //? TODO for development
#![allow(unused_variables)] //? TODO for development
#![allow(unused_imports)] //? TODO for development

use anyhow::*;
use serde::{Deserialize, Serialize};
//use thiserror::Error;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum IntegerRepr {
    I128(i128)
}
