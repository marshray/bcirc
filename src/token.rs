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

// use anyhow::{Context, Result, anyhow};
// use chumsky::{
//     combinator::To,
//     error::{RichPattern, RichReason},
//     prelude::*,
// };
// use num_bigint::BigInt;
// use self_cell::self_cell;
// use serde::Serialize;

use crate::{bits::*, file_content::FileContent, integer::Integer};

//#[derive(PartialOrd, Ord, Hash)]
#[derive(Clone, Debug, PartialEq, Eq)]
#[derive(serde::Serialize)]
pub enum Token<'src> {
    WhitespaceOrComment,

    IntegerLiteral(Integer),
    BitsLiteral(Bits),
    Identifier(&'src str),

    ExclamationMark,
    QuotationMark,
    Octothorpe,
    // Not using "dollar sign"
    PercentSign,
    Ampersand,
    //Apostrophe, // introduces IntegerLiteral
    ParenthesisLeft,
    ParenthesisRight,
    Asterisk,
    //PlusSign, // introduces positive IntegerLiteral
    Comma,
    //Minus, // introduces negative IntegerLiteral
    Period,
    ForwardSlash,
    Colon,
    Semicolon,
    LessThanSign,
    EqualSign,
    GreaterThanSign,
    QuestionMark,
    AtSign,
    SquareBracketLeft,
    SquareBracketRight,
    // Not using "circumflex accent" AKA "caret"
    Underscore,
    // Not using "grave accent"
    CurlyBracketLeft,
    VerticalBar,
    CurlyBracketRight,
    // Not using "tilde",
    /// Should produce an error
    InternalError,
}
