// Copyright 2023-2025 Marsh J. Ray
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

pub(crate) const fn one_shl(ch: char) -> u128 {
    let ch = ch as u32;
    if ch < 128 {
        1_u128 << ch
    } else {
        assert!(ch < 128);
        0
    }
}

pub(crate) const fn fs_shl(n: u32, ch: char) -> u128 {
    let ch = ch as u32;
    assert!(n + ch < 128);
    ((1_u128 << n) - 1) << ch
}

pub(crate) const fn u128_ch_bit_test(u: u128, ch: char) -> bool {
    let ch = ch as u32;
    ch < 128 && ((u >> ch) & 1) != 0
}
