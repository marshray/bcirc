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

/// Build an Option<> from a conditional and an expression.
///
/// # Examples
///
/// ```
/// # use std::ops::Range;
/// # use bcirc::maybe_some;
/// struct X(usize);
/// impl X {
///     const VALID_RANGE: Range<usize> = 1_usize .. 101;
///     fn opt_x_from_usize(val: usize) -> Option<X> {
///         maybe_some!(Self::VALID_RANGE.contains(&val), X(val))
///     }
/// }
/// assert!(X::opt_x_from_usize(100).is_some());
/// assert!(X::opt_x_from_usize(101).is_none());
/// ```
///
#[macro_export]
macro_rules! maybe_some {
    ($e:expr, $f:expr) => {
        if $e {
            std::option::Option::Some($f)
        } else {
            std::option::Option::None
        }
    };
}

/// Like `std::matches`, but allows multiple patterns.
///
/// # Examples
///
/// ```
/// # use bcirc::matches_any;
/// fn get_message(chars: &[char; 2]) -> bool {
///     matches_any!(chars,
///         &[ a, _b ] if a == 'E',
///         &[ _a, b ] if b == 'H' )
/// }
/// assert_eq!(get_message(&['E', 'F']), true);
/// assert_eq!(get_message(&['F', 'G']), false);
/// assert_eq!(get_message(&['G', 'H']), true);
/// ```
#[macro_export]
macro_rules! matches_any {
    ($ex:expr, $( $pa:pat $(if $gu:expr)? ),*) => {
        match $ex {
            $( $pa $(if $gu)? => true, )*
            _ => false
        }
    };
}

/// Build an Option<> from a match expression.
///
/// # Examples
///
/// ```
/// # use bcirc::maybe_match;
/// struct CharStruct { ch: char };
/// let char_struct = CharStruct { ch: 'F' };
/// let opt_ch: Option<char> = maybe_match!(
///     char_struct, CharStruct { ch, .. } if ch != '\0' => ch
/// );
/// assert!(opt_ch.is_some());
/// ```
#[macro_export]
macro_rules! maybe_match {
    ($scrutinee:expr, $patt:pat $( if $guard:expr )? => $some_expr:expr) => {
        match $scrutinee {
            $patt $(if $guard)? => std::option::Option::Some($some_expr),
            _ => std::option::Option::None
        }
    };
}

/// A trait for fallible conversions when no Error type is specified.
/// TODO rename MaybeFrom?
pub trait OptFrom<T>: Sized {
    fn opt_from(value: T) -> Option<Self>;

    /// Produce a `std::result::Result<T, &'static str>` with a generic error message.
    fn try_from_ok_or_err(val: T) -> Result<Self, &'static str> {
        Self::opt_from(val).ok_or("Couldn't convert val.")
    }
}

/* Alas.

error[E0210]: type parameter `T` must be used as the type parameter for some local type (e.g., `MyStruct<T>`)
   = note: implementing a foreign trait is only possible if at least one of the types for which it is implemented is local
   = note: only traits defined in the current crate can be implemented for a type parameter

impl<T: OptFrom<U>, U> std::convert::TryFrom<U> for T
where
    T: OptFrom<U>,
{
    type Error = &'static str;

    fn try_from(val: U) -> Result<Self, Self::Error> {
        val.opt_from(val).ok_or(Err("OptFrom couldn't convert."))
    }
} */

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
