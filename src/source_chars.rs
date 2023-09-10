// Copyright 2023 Marsh J. Ray
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use serde::{Deserialize, Serialize};

use crate::line_char_nums::{LineCharNums, LineNum};
use crate::matches_any;
use crate::maybe_match;
use crate::source_bytes::{ByteOrEof, SourceByteReadResult};
use crate::util::{one_shl, u128_ch_bit_test};

#[allow(dead_code)]
const fn is_cr_or_lf(ch: char) -> bool {
    const PUNCTUATION_FIRST_CHAR: u128 = one_shl('\r') | one_shl('\n');

    u128_ch_bit_test(PUNCTUATION_FIRST_CHAR, ch)
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum CharError {
    /// An invalid UTF-8 byte was encountered. The file offset in the containing structure refers
    /// to the offending byte. `len` indicates the total number of bytes in the invalid UTF-8
    /// sequence, so the relevant file offset range is `(fo + 1 - len) ..= fo`.
    InvalidUtf8 { val: u8, len: u8 },

    /// End-of-file was encountered before the current UTF-8 sequence was complete.
    /// The file offset in the containing structure refers to the total file size byte.
    /// `len` indicates the number of bytes that were successfully read in the UTF-8
    /// sequence, so the relevant file offset range is `(fo - len) .. fo`.
    UnexpectedEof { len: u8 },

    /// A `[std::io::Error]` was encountered.
    StdIoError(String),
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum CharResult {
    Char {
        ch: char,
        fo: u64,
        len: u8,
        line_char: LineCharNums,
    },

    Eol {
        fo: u64,
        len: u8,
        line_char: LineCharNums,
    },

    Eof {
        /// Total size of the file.
        file_size: u64,

        /// Last line number of the file.
        last_line: LineNum,
    },

    Error {
        ce: CharError,
        fo: u64,
        line_char: LineCharNums,
    },
}

impl CharResult {
    /// Returns if the result is a valid Eol.
    pub fn default_error() -> CharResult {
        CharResult::Eol {
            fo: 0,
            len: 0,
            line_char: LineCharNums::one_one(),
        }
    }

    /* /// Returns if the result is a valid Eol.
    pub fn is_eol(&self) -> bool {
        matches!(*self, CharResult::Eol { .. })
    } */

    /// Returns if the result is a valid Eof.
    pub fn is_eof(&self) -> bool {
        matches!(*self, CharResult::Eof { .. })
    }

    /// Returns if the result is an error.
    pub fn is_error(&self) -> bool {
        matches!(*self, CharResult::Error { .. })
    }

    /// Returns if the result is a char.
    pub fn is_char(&self) -> bool {
        matches!(*self, CharResult::Char { .. })
    }

    /// Returns if the result is a char or a valid Eol.
    pub fn is_char_or_eol(&self) -> bool {
        matches_any!(*self, CharResult::Char { .. }, CharResult::Eol { .. })
    }

    /// Returns the char, if the CharResult contains one.
    pub fn opt_char(&self) -> Option<char> {
        maybe_match!(*self, CharResult::Char { ch, .. } => ch)
    }

    /// Returns the char if the CharResult contains one. Otherwise returns
    /// NUL (0 as char).
    pub fn char_or_nul(&self) -> char {
        self.opt_char().unwrap_or('\0')
    }

    /// Returns if the result is a CR or LF.
    #[allow(dead_code)]
    pub fn is_cr_or_lf(&self) -> bool {
        matches!(*self, CharResult::Char { ch, .. } if is_cr_or_lf(ch))
    }

    /// Returns the line_char field, if this variant has one.
    pub fn opt_line_char(&self) -> Option<LineCharNums> {
        match self {
            CharResult::Char { line_char, .. } => Some(*line_char),
            CharResult::Eol { line_char, .. } => Some(*line_char),
            CharResult::Error { line_char, .. } => Some(*line_char),
            _ => None,
        }
    }
}

#[allow(dead_code)] //? TODO Hard to get this to be recognized as used for some reason.
pub fn source_chars(
    source_bytes: &mut dyn Iterator<Item = SourceByteReadResult>,
) -> impl Iterator<Item = CharResult> + '_ {
    // First pass just decodes UTF-8. Doesn't even track line numbers.
    // TODO OPT Another return type here would be simpler.
    let iter_1 = {
        let mut char_fo_start = 0u64;
        let mut utf8_buf = [0u8; 4];
        let mut utf8_buf_len = 0u8;
        let mut need_more_bytes_for_this_char = false;

        let line_char = LineCharNums::one_one();

        std::iter::from_generator(move || {
            'for_source_bytes: for source_byte_read_result in source_bytes {
                let fo_byte = source_byte_read_result.file_offset;

                if need_more_bytes_for_this_char {
                    // Just extend existing char
                    need_more_bytes_for_this_char = false;
                } else {
                    // Begin a new char
                    utf8_buf_len = 0;
                    char_fo_start = fo_byte;
                }

                match source_byte_read_result.byte_or_eof {
                    ByteOrEof::Byte(by) => {
                        // Shortcut lower-ASCII (non-UTF-8) chars when there's nothing in the UTF-8 buffer.
                        let ch: char = if utf8_buf_len == 0 && (by & 0x80) == 0 {
                            utf8_buf_len = 1;
                            by.into()
                        } else {
                            // See if we can interpret this byte to a char
                            assert!(utf8_buf_len < 4);
                            utf8_buf[utf8_buf_len as usize] = by;
                            utf8_buf_len += 1;

                            match std::str::from_utf8(&utf8_buf[0..utf8_buf_len as usize]) {
                                Result::Ok(ch_str) => {
                                    assert_eq!(ch_str.len(), 1);
                                    ch_str.chars().next().unwrap()
                                }
                                Result::Err(utf8error) => {
                                    match utf8error.error_len() {
                                        None => {
                                            // Need more bytes for this char
                                            need_more_bytes_for_this_char = true;
                                            continue 'for_source_bytes;
                                        }
                                        Some(_len) => {
                                            // An invalid Utf8 byte was encountered
                                            debug_assert_eq!(utf8error.valid_up_to(), 0);

                                            yield CharResult::Error {
                                                ce: CharError::InvalidUtf8 {
                                                    val: by,
                                                    len: utf8_buf_len,
                                                },
                                                fo: fo_byte,
                                                line_char,
                                            };
                                            break 'for_source_bytes;
                                        }
                                    } // match
                                } // Err
                            } // match from_utf8
                        };

                        yield CharResult::Char {
                            ch,
                            fo: char_fo_start,
                            len: utf8_buf_len,
                            line_char,
                        };
                    } // ByteOrEof::Byte

                    ByteOrEof::Eof => {
                        if utf8_buf_len != 0 {
                            yield CharResult::Error {
                                ce: CharError::UnexpectedEof { len: utf8_buf_len },
                                fo: char_fo_start,
                                line_char,
                            };
                        } else {
                            yield CharResult::Eof {
                                file_size: fo_byte,
                                last_line: LineNum::one(),
                            };
                        };
                    }

                    ByteOrEof::StdIoError(err_str) => {
                        yield CharResult::Error {
                            ce: CharError::StdIoError(err_str),
                            fo: fo_byte,
                            line_char,
                        };
                    }
                }
            }
        })
    };

    let iter_2 = {
        let mut opt_deferred_cr_result: Option<CharResult> = None;

        std::iter::from_generator(move || {
            'each_char_result: for mut this_char_result in iter_1 {
                // If we have deferred a CR
                if let Some(mut deferred_cr_result) = opt_deferred_cr_result.take() {
                    let mut skip_this_char_result = false;

                    // and an LF immediately following it,
                    if let (
                        CharResult::Eol {
                            fo: deferred_cr_fo,
                            len: deferred_cr_len,
                            ..
                        },
                        CharResult::Char {
                            ch: '\n',
                            fo: this_cr_fo,
                            len: this_cr_len,
                            ..
                        },
                    ) = (&mut deferred_cr_result, &this_char_result)
                    {
                        // Ignore the LF immediately following the CR, just extend the deferred CR's length.
                        assert_eq!(*deferred_cr_fo + *deferred_cr_len as u64, *this_cr_fo);
                        *deferred_cr_len = deferred_cr_len.checked_add(*this_cr_len).unwrap();

                        skip_this_char_result = true;
                    }

                    // Yield the deferred CR
                    yield deferred_cr_result;

                    // Maybe skip this char
                    if skip_this_char_result {
                        continue 'each_char_result;
                    }
                }

                // Convert CharResult::Char('\r' or '\n') to CharResult::Eol.
                if let CharResult::Char {
                    ch: ch @ '\r' | ch @ '\n',
                    fo,
                    len,
                    line_char,
                } = this_char_result
                {
                    this_char_result = CharResult::Eol { fo, len, line_char };

                    if ch == '\r' {
                        // Defer CR
                        opt_deferred_cr_result = Some(this_char_result);

                        continue 'each_char_result;
                    }
                }

                // Yield the non-deferred result
                yield this_char_result;
            }
        })
    };

    // This is the stream we give to callers.
    std::iter::from_generator(move || {
        let mut lc = LineCharNums::one_one();
        let mut ll = lc.line();

        for mut char_result in iter_2 {
            match &mut char_result {
                CharResult::Char { line_char, .. } => {
                    *line_char = lc;

                    // Advance to next char
                    lc.inc_char();
                }
                CharResult::Eol { line_char, .. } => {
                    // There was an Eol
                    ll = lc.line();

                    *line_char = lc;

                    // Advance to next line
                    lc.inc_line();
                }
                CharResult::Eof {
                    last_line,
                    ref file_size,
                } => {
                    *last_line = ll;

                    // Ensure we have an Eol before Eof.
                    if !lc.char().is_one() {
                        yield CharResult::Eol {
                            line_char: lc,
                            fo: *file_size,
                            len: 0,
                        };
                    }
                }
                CharResult::Error { line_char, .. } => {
                    *line_char = lc;
                }
            }

            yield char_result
        }
    })
}

#[cfg(test)]
mod test {
    #[test]
    fn test() {
        const TEST_DATA_SUBDIR: &str = "source_chars";

        crate::test_util::insta_glob(TEST_DATA_SUBDIR, |_file_path, bx_bufread| {
            let mut source_bytes = crate::source_bytes::source_bytes(bx_bufread);
            let source_chars = crate::source_chars::source_chars(&mut source_bytes);

            let results = source_chars.collect::<Vec<_>>();

            insta::assert_ron_snapshot!(results);
        });
    }
}
