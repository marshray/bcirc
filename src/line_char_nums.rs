// Copyright 2023 Marsh J. Ray
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use serde::{Deserialize, Serialize};

use crate::maybe_some;
use crate::util::OptFrom;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize)]
pub struct LineNum(u64);

impl LineNum {
    const MIN_U64: u64 = 1;
    const MAX_U64: u64 = u64::MAX;

    /// Returns line number 1.
    pub const fn one() -> Self {
        Self(1u64)
    }

    /// Returns true iff line number is 1.
    #[allow(dead_code)]
    pub const fn is_one(&self) -> bool {
        self.0 == 1
    }

    #[allow(dead_code)]
    pub fn inc(&mut self) {
        self.0 += 1;
    }
}

impl OptFrom<u64> for LineNum {
    fn opt_from(val: u64) -> Option<Self> {
        maybe_some!(
            (LineNum::MIN_U64..=LineNum::MAX_U64).contains(&val),
            LineNum(val)
        )
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize)]
pub struct CharNum(u64);

impl CharNum {
    const MIN_U64: u64 = 1;
    const MAX_U64: u64 = u64::MAX;

    /// Returns char number 1.
    #[allow(dead_code)]
    pub const fn one() -> Self {
        Self(1u64)
    }

    /// Returns true iff char number is 1.
    pub const fn is_one(&self) -> bool {
        self.0 == 1
    }

    #[allow(dead_code)]
    pub fn inc(&mut self) {
        self.0 += 1;
    }
}

impl OptFrom<u64> for CharNum {
    fn opt_from(val: u64) -> Option<Self> {
        maybe_some!(
            (CharNum::MIN_U64..=CharNum::MAX_U64).contains(&val),
            CharNum(val)
        )
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize)]
pub struct LineCharNums([u64; 2]);

impl LineCharNums {
    /// Returns (line, char) number (1, 1).
    pub const fn one_one() -> Self {
        Self([1; 2])
    }

    /// Returns the (one-based) line number.
    pub fn line(&self) -> LineNum {
        LineNum(self.0[0])
    }

    /// Returns the (one-based) char number.
    pub fn char(&self) -> CharNum {
        CharNum(self.0[1])
    }

    /// Increments the line number, resets the char number to 1.
    pub fn inc_line(&mut self) {
        assert_ne!(self.0[0], LineNum::MAX_U64);
        self.0 = [self.0[0] + 1, 1];
    }

    /// Increments the char number. Does not affect the line number.
    pub fn inc_char(&mut self) {
        assert_ne!(self.0[1], CharNum::MAX_U64);
        self.0[1] += 1;
    }
}
