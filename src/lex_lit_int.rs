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

use anyhow::{Context, Result, anyhow};
use chumsky::{
    combinator::To,
    error::{RichPattern, RichReason},
    prelude::*,
};
use num_bigint::BigInt;
use self_cell::self_cell;
use serde::Serialize;

use crate::{
    integer::Integer,
    file_content::FileContent,
    token::Token,
    values::*,
    lexer::LexExtraErr,
};

pub(crate) fn lit_int<'src>() -> impl Parser<'src, &'src str, Token<'src>, LexExtraErr<'src>> {
    let base2 = lit_int_base_2();

    let base10 = just('\'').ignore_then(text::int(10).map_with(|s: &str, _e| {
        let i: i64 = s.parse().unwrap();
        let integer_value = Integer::I64(i);
        Token::IntegerLiteral(integer_value)
    }));

    let base16 = text::int(16).map_with(|s: &str, _e| {
        let i: i64 = s.parse().unwrap();
        let integer_value = Integer::I64(i);
        Token::IntegerLiteral(integer_value)
    });

    base2.or(base10).or(base16)
}

fn lit_int_base_2<'src>() -> impl Parser<'src, &'src str, Token<'src>, LexExtraErr<'src>> {
    let p = one_of("01").then(one_of("01_").repeated().to_slice());

    p.map(|(ch0, s): (char, &'src str)| {
        let mut i = Integer::from(0);
        let is_negative = false; //? TODO

        let signed_one = if is_negative {
            Integer::negative_one()
        } else {
            Integer::one()
        };

        for ch in std::iter::once(ch0).chain(s.chars()) {
            if ch != '_' {
                i.double_assign();
                if ch == '1' {
                    i += &signed_one;
                } else {
                    debug_assert_eq!(ch, '0');
                }
            }
        }

        Token::IntegerLiteral(i)
    })
}
