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
    bits::*, file_content::FileContent, integer::Integer, lexer::LexExtraErr, token::Token,
};

pub(crate) fn lit_int_base10<'src>() -> impl Parser<'src, &'src str, Token<'src>, LexExtraErr<'src>>
{
    let p = one_of("+-").or_not();
    let p = p.then(
        one_of("0123456789")
            .then(one_of("0123456789_").repeated())
            .to_slice(),
    );

    p.map(|pr: (Option<char>, &'src str)| {
        let (opt_ch, s) = pr;
        let is_negative = opt_ch.filter(|&ch| ch == '-').is_some();

        let ten: Integer = 10.into();
        let signed_ten = if is_negative {
            Integer::from(-10_i8)
        } else {
            ten
        };

        let mut i = Integer::from(0);
        for ch in s.chars() {
            if ch != '_' {
                debug_assert!('0' <= ch && ch <= '9');
                i *= 10_i64;
                i += (ch as isize - '0' as isize) as i64;
            }
        }

        if is_negative {
            i = -i;
        }

        Token::IntegerLiteral(i)
    })
}

pub(crate) fn lit_int_base2<'src>() -> impl Parser<'src, &'src str, Token<'src>, LexExtraErr<'src>>
{
    // The "'b" prefix is ignored
    let p = just("'b");
    let p = p.ignore_then(just('_').repeated());

    // This part is parsed to an integer.
    let q = one_of("01").then(one_of("01_").repeated());
    let q = q.to_slice();
    let q = q.map(|s: &'src str| {
        let mut i = Integer::from(0);
        for ch in s.chars() {
            if ch != '_' {
                i.double_assign();
                if ch == '1' {
                    i += 1;
                } else {
                    debug_assert_eq!(ch, '0');
                }
            }
        }

        Token::IntegerLiteral(i)
    });
    p.ignore_then(q)
}

pub(crate) fn lit_int_base16<'src>() -> impl Parser<'src, &'src str, Token<'src>, LexExtraErr<'src>>
{
    // The "'x prefix is ignored
    let p = just("'x");
    let p = p.ignore_then(just('_').repeated());

    // This part is parsed to an integer.
    let q = one_of("0123456789ABCDEFabcdef").then(one_of("0123456789ABCDEFabcdef_").repeated());
    let q = q.to_slice();
    let q = q.map(|s: &'src str| {
        let mut i = Integer::from(0);
        for ch in s.chars() {
            if ch != '_' {
                i *= 16_i64;
                match ch {
                    '0'..='9' => {
                        i += (ch as isize - '0' as isize) as i64;
                    }
                    'A'..='F' => {
                        i += (ch as isize - 'A' as isize) as i64 + 10;
                    }
                    'a'..='f' => {
                        i += (ch as isize - 'a' as isize) as i64 + 10;
                    }
                    _ => {
                        debug_assert_eq!(ch, '_');
                    }
                }
            }
        }

        Token::IntegerLiteral(i)
    });
    p.ignore_then(q)
}
