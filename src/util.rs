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
