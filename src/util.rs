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

// use anyhow::*;
// use serde::{Deserialize, Serialize};
//use thiserror::Error;

/// Build an Option<> from a match expression.
///
/// let opt_ch: Option<char> = maybe_match!{
///     self, CharStruct { ch, .. } if ch != '\0' => ch
/// };
#[macro_export]
macro_rules! maybe_match {
    ($scrutinee:expr, $patt:pat $( if $guard:expr )? => $some_expr:expr) => {
        match $scrutinee {
            $patt $(if $guard)? => std::option::Option::Some($some_expr),
            _ => std::option::Option::None
        }
    };
}
