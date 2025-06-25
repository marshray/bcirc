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

use anyhow::*;
use const_default::ConstDefault;
use num_bigint::{BigInt, BigUint};
use serde::{Deserialize, Serialize};
use static_assertions::const_assert;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Integer {
    I64(i64),
    BigInt(BigInt),
}

impl Integer {
    /// [`Integer`] value `0`.
    pub const fn zero() -> Self {
        Integer::I64(0)
    }

    /// [`Integer`] value `1`.
    pub const fn one() -> Self {
        Integer::I64(1)
    }

    /// [`Integer`] value `-1`.
    pub const fn negative_one() -> Self {
        Integer::I64(-1)
    }

    /// Multiplies the [`Integer`] by `2`.
    pub fn double_assign(&mut self) {
        match self {
            self_ @ &mut Integer::I64(i) => {
                *self_ = match i as u64 >> 62 {
                    0 | 3 => {
                        // If the top two bits are the same, then we can just shift left.
                        Integer::I64(i << 1)
                    }
                    _ => {
                        // We need to upgrade to the BigInt representation.
                        let mut bi = BigInt::from(i);
                        bi <<= 1;
                        Integer::BigInt(bi)
                    }
                }
            }
            Integer::BigInt(bi) => {
                *bi <<= 1_u8;
            }
        }
    }

    /// Adds into [`self`] the [`i64`] value.
    pub fn add_assign_i64(&mut self, rhs: i64) {
        use std::ops::AddAssign;
        match self {
            self_ @ &mut Integer::I64(self_i64) => {
                *self_ = if let Some(sum_i64) = self_i64.checked_add(rhs) {
                    Integer::I64(sum_i64)
                } else {
                    let mut self_bi = BigInt::from(self_i64);
                    self_bi.add_assign(rhs);
                    Integer::BigInt(self_bi)
                };
            }
            Integer::BigInt(self_bi) => {
                self_bi.add_assign(rhs);
            }
        }
    }

    /// Multiplies into [`self`] the [`i64`] value.
    pub fn mul_assign_i64(&mut self, rhs: i64) {
        use std::ops::MulAssign;
        match self {
            self_ @ &mut Integer::I64(self_i64) => {
                *self_ = if let Some(product_i64) = self_i64.checked_mul(rhs) {
                    Integer::I64(product_i64)
                } else {
                    let mut self_bi = BigInt::from(self_i64);
                    self_bi.mul_assign(rhs);
                    Integer::BigInt(self_bi)
                };
            }
            Integer::BigInt(self_bi) => {
                self_bi.mul_assign(rhs);
            }
        }
    }
}

impl ConstDefault for Integer {
    /// Const default value for [`Integer`].
    const DEFAULT: Self = Self::zero();
}

impl Default for Integer {
    #[inline]
    fn default() -> Self {
        Self::DEFAULT
    }
}

impl std::cmp::PartialEq for Integer {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        #[inline(never)]
        fn f_eq(i1: i64, bi2: &BigInt) -> bool {
            if let Some(i2) = i64::try_from(bi2).ok() {
                i1 == i2
            } else {
                false
            }
        }

        match (self, other) {
            (&Integer::I64(i1), &Integer::I64(i2)) => i1 == i2,
            (Integer::BigInt(bi1), Integer::BigInt(bi2)) => bi1.eq(&bi2),
            (&Integer::I64(i1), Integer::BigInt(bi2)) => f_eq(i1, &bi2),
            (Integer::BigInt(bi1), &Integer::I64(i2)) => f_eq(i2, &bi1),
        }
    }
}

impl std::cmp::Eq for Integer {}

impl std::ops::AddAssign<i64> for Integer {
    #[inline]
    fn add_assign(&mut self, rhs: i64) {
        self.add_assign_i64(rhs);
    }
}

impl std::ops::AddAssign<&Self> for Integer {
    fn add_assign(&mut self, rhs: &Self) {
        match (self, rhs) {
            (self_ @ &mut Integer::I64(self_i64), &Integer::I64(i2)) => {
                *self_ = if let Some(sum_i64) = self_i64.checked_add(i2) {
                    Integer::I64(sum_i64)
                } else {
                    let mut self_bi = BigInt::from(self_i64);
                    self_bi.add_assign(i2);
                    Integer::BigInt(self_bi)
                };
            }
            (Integer::BigInt(self_bi), Integer::BigInt(bi2)) => {
                self_bi.add_assign(bi2);
            }
            (self_ @ &mut Integer::I64(i1), Integer::BigInt(bi2)) => {
                let mut self_bi = bi2.clone();
                self_bi.add_assign(i1);
                *self_ = Integer::BigInt(self_bi);
            }
            (Integer::BigInt(self_bi), &Integer::I64(i2)) => {
                self_bi.add_assign(i2);
            }
        }
    }
}

impl std::ops::MulAssign<i64> for Integer {
    #[inline]
    fn mul_assign(&mut self, rhs: i64) {
        self.mul_assign_i64(rhs);
    }
}

impl std::ops::MulAssign<&Self> for Integer {
    fn mul_assign(&mut self, rhs: &Self) {
        match (self, rhs) {
            (self_ @ &mut Integer::I64(self_i64), &Integer::I64(i2)) => {
                *self_ = if let Some(product_i64) = self_i64.checked_mul(i2) {
                    Integer::I64(product_i64)
                } else {
                    let mut self_bi = BigInt::from(self_i64);
                    self_bi.mul_assign(i2);
                    Integer::BigInt(self_bi)
                };
            }
            (Integer::BigInt(self_bi), Integer::BigInt(bi2)) => {
                self_bi.mul_assign(bi2);
            }
            (self_ @ &mut Integer::I64(i1), Integer::BigInt(bi2)) => {
                let mut self_bi = bi2.clone();
                self_bi.mul_assign(i1);
                *self_ = Integer::BigInt(self_bi);
            }
            (Integer::BigInt(self_bi), &Integer::I64(i2)) => {
                self_bi.mul_assign(i2);
            }
        }
    }
}

impl From<u8> for Integer {
    /// A [`u8`] can always be converted into a [`Integer`].
    fn from(src: u8) -> Self {
        Integer::I64(src as i64)
    }
}
impl From<u16> for Integer {
    /// A [`u16`] can always be converted into a [`Integer`].
    fn from(src: u16) -> Self {
        Integer::I64(src as i64)
    }
}
impl From<u32> for Integer {
    /// A [`u32`] can always be converted into a [`Integer`].
    fn from(src: u32) -> Self {
        Integer::I64(src as i64)
    }
}
impl From<usize> for Integer {
    /// A [`usize`] can always be converted into a [`Integer`].
    fn from(src: usize) -> Self {
        if let Some(i) = i64::try_from(src).ok() {
            Integer::I64(i)
        } else {
            Integer::BigInt(src.into())
        }
    }
}
impl From<u64> for Integer {
    /// A [`u64`] can always be converted into a [`Integer`].
    fn from(src: u64) -> Self {
        if let Some(i) = i64::try_from(src).ok() {
            Integer::I64(i)
        } else {
            Integer::BigInt(src.into())
        }
    }
}
impl From<u128> for Integer {
    /// A [`u128`] can always be converted into a [`Integer`].
    fn from(src: u128) -> Self {
        if let Some(i) = i64::try_from(src).ok() {
            Integer::I64(i)
        } else {
            Integer::BigInt(src.into())
        }
    }
}
impl From<i8> for Integer {
    /// A [`i8`] can always be converted into a [`Integer`].
    fn from(src: i8) -> Self {
        Integer::I64(src as i64)
    }
}
impl From<i16> for Integer {
    /// A [`i16`] can always be converted into a [`Integer`].
    fn from(src: i16) -> Self {
        Integer::I64(src as i64)
    }
}
impl From<i32> for Integer {
    /// A [`i32`] can always be converted into a [`Integer`].
    fn from(src: i32) -> Self {
        Integer::I64(src as i64)
    }
}
impl From<isize> for Integer {
    /// A [`isize`] can always be converted into a [`Integer`].
    fn from(src: isize) -> Self {
        const_assert!(size_of::<isize>() <= size_of::<i64>());
        Integer::I64(src as i64)
    }
}
impl From<i64> for Integer {
    /// A [`i64`] can always be converted into a [`Integer`].
    fn from(src: i64) -> Self {
        Integer::I64(src)
    }
}
impl From<i128> for Integer {
    /// A [`i128`] can always be converted into a [`Integer`].
    fn from(src: i128) -> Self {
        if let Some(i) = i64::try_from(src).ok() {
            Integer::I64(i)
        } else {
            Integer::BigInt(src.into())
        }
    }
}
impl From<BigInt> for Integer {
    /// A [`BigInt`] can always be converted into a [`Integer`].
    fn from(src: BigInt) -> Self {
        if let Some(i) = i64::try_from(&src).ok() {
            Integer::I64(i)
        } else {
            Integer::BigInt(src)
        }
    }
}
impl From<BigUint> for Integer {
    /// A [`BigUint`] can always be converted into a [`Integer`].
    fn from(src: BigUint) -> Self {
        if let Some(i) = i64::try_from(&src).ok() {
            Integer::I64(i)
        } else {
            Integer::BigInt(src.into())
        }
    }
}

#[cfg(test)]
mod t {
    use insta::assert_compact_debug_snapshot as iacds;

    use super::*;

    #[test]
    fn t1() {
        iacds!(Integer::zero(), @"I64(0)");
        iacds!(Integer::one(), @"I64(1)");
        iacds!(Integer::negative_one(), @"I64(-1)");

        let mut i = Integer::I64(2);
        iacds!(i, @"I64(2)");
    }

    #[test]
    fn t2() {
        let mut i = Integer::zero();
        i *= &Integer::one();
        iacds!(i, @"I64(0)");
        i *= &Integer::from(2_u8);
        iacds!(i, @"I64(0)");
    }

    #[test]
    fn t3() {
        let mut i = Integer::one();
        i *= &Integer::one();
        iacds!(i, @"I64(1)");
        i *= &Integer::from(2_u8);
        iacds!(i, @"I64(2)");
        i *= &Integer::from(2_u8);
        iacds!(i, @"I64(4)");
    }
}
