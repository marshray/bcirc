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

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    serde::Deserialize,
    serde::Serialize
)]
#[repr(transparent)]
pub struct LineNumberOneBased(u64);
impl LineNumberOneBased {
    pub const fn zero() -> Self {
        LineNumberOneBased(0)
    }
    pub const fn one() -> Self {
        LineNumberOneBased(1)
    }
    pub fn inc(&mut self) -> Self {
        self.0 += 1;
        *self
    }
}

#[repr(transparent)]
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    serde::Deserialize,
    serde::Serialize
)]
pub struct CharNumberOneBased(u64);

impl CharNumberOneBased {
    pub const fn zero() -> Self {
        CharNumberOneBased(0)
    }
    pub const fn one() -> Self {
        CharNumberOneBased(1)
    }
    pub fn inc(&mut self) -> Self {
        self.0 += 1;
        *self
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct CharLoc {
    pub fo_start: u64,
    pub fo_last: u64,
    pub line_n: LineNumberOneBased,
    pub char_n: CharNumberOneBased,
}

impl CharLoc {
    pub fn new() -> CharLoc {
        Self {
            fo_start: 0,
            fo_last: 0,
            line_n: LineNumberOneBased::one(),
            char_n: CharNumberOneBased::one(),
        }
    }
}

impl Default for CharLoc {
    fn default() -> Self {
        Self::new()
    }
}
