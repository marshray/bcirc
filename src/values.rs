// Copyright 2023 Marsh J. Ray
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![deny(elided_lifetimes_in_paths)]
#![deny(
    clippy::panic,
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::manual_assert
)]
#![allow(clippy::type_complexity, clippy::assertions_on_constants)]
#![allow(dead_code)] //? TODO for development
#![allow(non_snake_case)] //? TODO for development
#![allow(non_upper_case_globals)] //? TODO for development
#![allow(noop_method_call)] //? TODO for development
#![allow(non_camel_case_types)] //? TODO for development
#![allow(unused_mut)] //? TODO for development
#![allow(unused_braces)] //? TODO for development
#![allow(unused_assignments)] //? TODO for development
#![allow(unused_variables)] //? TODO for development
#![allow(unused_imports)] //? TODO for development
#![allow(unreachable_code)] //? TODO for development
#![allow(clippy::needless_lifetimes, clippy::let_and_return)] //? TODO for development

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
