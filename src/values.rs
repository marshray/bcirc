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

use anyhow::*;
use bitvec::prelude::*;
use serde::{Deserialize, Serialize};

/// An integer
#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Integer {
    I128(i128),
}

/// How a [`Bits`] works on the left and the right.
#[derive(Debug, Serialize, Deserialize)]
pub enum BitsExtent {
    /// Anchored.
    Anchored,

    /// Zero-extended.
    ZeroExtended,

    /// Sign-extended. This can only be applied on the left.
    SignExtended,
}

/// Representation of a [`Bits`] value.
#[derive(Debug, Serialize, Deserialize)]
pub struct Bits {
    /// Extents, left and right.
    extents: [BitsExtent; 2],
    defined_bits: BitVec,
}
