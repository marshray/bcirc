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

use std::ops::{Generator, GeneratorState, RangeInclusive};
use std::pin::Pin;

use anyhow::*;
use serde::{Deserialize, Serialize};

use crate::maybe_match;
use crate::source_bytes::{ByteOrEof, SourceByteReadResult};

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
pub struct LineNumberOneBased(u64);
impl LineNumberOneBased {
    pub fn inc(&mut self) {
        self.0 += 1;
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
pub struct CharNumberOneBased(u64);

impl CharNumberOneBased {
    pub fn inc(&mut self) {
        self.0 += 1;
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CharLoc {
    pub file_offset: RangeInclusive<u64>,
    pub line_n: LineNumberOneBased,
    pub char_n: CharNumberOneBased,
}

impl CharLoc {
    pub fn new() -> CharLoc {
        CharLoc {
            file_offset: 0u64..=0,
            line_n: LineNumberOneBased(1),
            char_n: CharNumberOneBased(1),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum CharError {
    InvalidUtf8(u8),
    UnexpectedEof,
    StdIoError(String),
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum CharResult {
    Char(char),
    Eol,
    Eof,
    CharError(CharError),
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SourceCharReadResult {
    pub loc: CharLoc,
    pub char_result: CharResult,
}

impl SourceCharReadResult {
    pub fn default_error() -> Self {
        Self {
            loc: CharLoc::new(),
            char_result: CharResult::Eof,
        }
    }
}

impl SourceCharReadResult {
    /// Returns if the result is a valid Eol.
    pub fn is_eol(&self) -> bool {
        matches!(
            *self,
            SourceCharReadResult {
                char_result: CharResult::Eol,
                ..
            }
        )
    }

    /// Returns if the result is a valid Eof.
    pub fn is_eof(&self) -> bool {
        matches!(
            *self,
            SourceCharReadResult {
                char_result: CharResult::Eof,
                ..
            }
        )
    }

    /// Returns if the result is an error.
    pub fn is_error(&self) -> bool {
        matches!(
            *self,
            SourceCharReadResult {
                char_result: CharResult::CharError(_),
                ..
            }
        )
    }

    /// Returns if the result is a char.
    pub fn is_char(&self) -> bool {
        matches!(
            *self,
            SourceCharReadResult {
                char_result: CharResult::Char(ch),
                ..
            }
        )
    }

    /// Returns if the result is a char or a valid Eol.
    pub fn is_char_or_eol(&self) -> bool {
        self.is_char() || self.is_eol()
    }

    /// Returns the char, if the SourceCharReadResult contains one.
    pub fn opt_char(&self) -> Option<char> {
        maybe_match!(*self, SourceCharReadResult { char_result: CharResult::Char(ch), .. } => ch)
    }

    /// Returns the char if the SourceCharReadResult contains one. Otherwise returns
    /// NUL (0 as char).
    pub fn char_or_nul(&self) -> char {
        self.opt_char().unwrap_or('\0')
    }
}

pub fn source_chars(
    source_bytes: &mut dyn Iterator<Item = SourceByteReadResult>,
) -> impl Iterator<Item = SourceCharReadResult>  + '_ {
    let mut source_char_read_result = SourceCharReadResult {
        loc: CharLoc::new(),
        char_result: CharResult::Eof,
    };

    //let mut file_offset_buf0 = 0u64;
    let mut utf8_buf = [0u8; 4];
    let mut utf8_buf_len = 0;
    let mut need_more_bytes_for_this_char = false;
    let mut track_prev_char_was_cr = false;

    //let mut generate_source_chars = move
    std::iter::from_generator(move || {
        for source_byte_read_result in source_bytes {
            source_char_read_result.loc.file_offset = if need_more_bytes_for_this_char {
                // Need more bytes for the current char
                *source_char_read_result.loc.file_offset.start()
            } else {
                // Begin a new char
                utf8_buf_len = 0;
                source_byte_read_result.file_offset
            }..=source_byte_read_result.file_offset;

            need_more_bytes_for_this_char = false;

            match source_byte_read_result.byte_or_eof {
                ByteOrEof::Byte(by) => {
                    assert!(utf8_buf_len < 4);
                    utf8_buf[utf8_buf_len] = by;
                    utf8_buf_len += 1;

                    match std::str::from_utf8(&utf8_buf[0..utf8_buf_len]) {
                        Result::Ok(ch_str) => {
                            assert_eq!(ch_str.len(), 1);
                            let ch = ch_str.chars().next().unwrap();

                            let prev_char_was_cr = track_prev_char_was_cr;
                            track_prev_char_was_cr = ch == '\r';

                            if prev_char_was_cr && ch == '\n' {
                                // Ignore LF after CR
                            } else {
                                let (yielding_eol, char_or_something) = if ch == '\r' || ch == '\n' {
                                    (true, CharResult::Eol)
                                } else {
                                    (false, CharResult::Char(ch))
                                };

                                let _char_or_eof = char_or_something.clone();
                                let scrr = SourceCharReadResult {
                                    char_result: char_or_something,
                                    ..source_char_read_result.clone()
                                };
                                //eprintln!("\nyielding {:?}", &scrr);

                                yield scrr;

                                if yielding_eol {
                                    //eprintln!("advancing to next line: {:?}", &_char_or_eof);
                                    // Advance to next line
                                    source_char_read_result.loc.char_n = CharNumberOneBased(1);
                                    source_char_read_result.loc.line_n.inc();
                                } else {
                                    //eprintln!("advancing to next char: {:?}", &_char_or_eof);
                                    // Advance to next char
                                    source_char_read_result.loc.char_n.inc();
                                }
                            }
                        }
                        Result::Err(utf8error) => {
                            match utf8error.error_len() {
                                None => {
                                    // Need more bytes for this char
                                    need_more_bytes_for_this_char = true;
                                    // continue
                                }
                                Some(len) => {
                                    // An invalid Utf8 byte was encountered
                                    debug_assert_eq!(utf8error.valid_up_to(), 0);

                                    let char_error = CharError::InvalidUtf8(by);
                                    yield SourceCharReadResult {
                                        char_result: CharResult::CharError(char_error),
                                        ..source_char_read_result.clone()
                                    };
                                }
                            } // match
                        } // Err
                    } // match from_utf8
                } // ByteOrEof::Byte
                ByteOrEof::StdIoError(err_str) => {
                    yield SourceCharReadResult {
                        char_result: CharResult::CharError(CharError::StdIoError(err_str)),
                        ..source_char_read_result.clone()
                    };
                }
                ByteOrEof::Eof => {
                    if utf8_buf_len != 0 {
                        yield SourceCharReadResult {
                            char_result: CharResult::CharError(CharError::UnexpectedEof),
                            ..source_char_read_result.clone()
                        };
                    } else {
                        if source_char_read_result.loc.char_n != CharNumberOneBased(1) {
                            yield SourceCharReadResult {
                                char_result: CharResult::Eol,
                                ..source_char_read_result.clone()
                            };
                            source_char_read_result.loc.char_n.inc();
                        }

                        yield SourceCharReadResult {
                            char_result: CharResult::Eof,
                            ..source_char_read_result.clone()
                        };
                    };
                }
            } // match
        } // for in source_bytes
    }) // from_generator
}

#[cfg(test)]
mod test {
    #[test]
    fn test() {
        use crate::source_bytes::source_bytes;
        use crate::source_chars::{source_chars, SourceCharReadResult};

        const TEST_DATA_SUBDIR: &'static str = "source_chars";

        crate::test_util::insta_glob(TEST_DATA_SUBDIR, |file_path, bx_bufread| {
            let mut source_bytes = source_bytes(bx_bufread);
            let mut source_chars = source_chars(&mut source_bytes);

            let results = source_chars.collect::<Vec<_>>();

            insta::assert_ron_snapshot!(results);
        });
    }
}
