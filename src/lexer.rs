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

use std::iter::FusedIterator;
use std::ops::RangeInclusive;

use serde::{Deserialize, Serialize};

use crate::line_char_nums::{LineCharNums, LineNum};
use crate::data_repr::IntegerRepr;
use crate::source_chars::{CharError, CharResult};
use crate::util::{fs_shl, one_shl, u128_ch_bit_test};

fn is_punctuation_single_char(ch: char) -> bool {
    #[rustfmt::skip]
    const PUNCTUATION_SINGLE_CHAR: u128 =
        one_shl('[') | one_shl(']') |
        one_shl('{') | one_shl('}') |
        one_shl('(') | one_shl(')') |
        one_shl('-') | one_shl('!') |
        one_shl('^') | one_shl('&') |
        one_shl('*') | one_shl(',') |
        one_shl(';');

    u128_ch_bit_test(PUNCTUATION_SINGLE_CHAR, ch)
}

fn is_punctuation_first_char(ch: char) -> bool {
    #[rustfmt::skip]
    const PUNCTUATION_FIRST_CHAR: u128 =
        one_shl('<') | one_shl('>') |
        one_shl('=') | one_shl('&');

    u128_ch_bit_test(PUNCTUATION_FIRST_CHAR, ch)
}

const DECIMAL_DIGIT_CHAR: u128 = fs_shl(10, '0');

fn is_decimal_digit(ch: char) -> bool {
    u128_ch_bit_test(DECIMAL_DIGIT_CHAR, ch)
}

fn decimal_digit_val(ch: char) -> u32 {
    debug_assert!(is_decimal_digit(ch));

    ch as u32 - b'0' as u32
}

fn is_comment_first_char(ch: char) -> bool {
    ch == '/'
}

#[rustfmt::skip]
const IDENTIFIER_FIRST_CHAR: u128 =
    fs_shl(26, 'A') | 
    fs_shl(26, 'a') |
    one_shl('_');

fn is_identifier_first_char(ch: char) -> bool {
    u128_ch_bit_test(IDENTIFIER_FIRST_CHAR, ch)
}

fn is_identifier_subsequent_char(ch: char) -> bool {
    const IDENTIFIER_SUBSEQUENT_CHAR: u128 = IDENTIFIER_FIRST_CHAR | DECIMAL_DIGIT_CHAR;
    u128_ch_bit_test(IDENTIFIER_SUBSEQUENT_CHAR, ch)
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Token {
    PunctuationSingleChar(char),
    Comment,
    LiteralInteger(IntegerRepr),
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct TokenInfo {
    pub linechar_range: RangeInclusive<LineCharNums>,
    pub token: Token,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum LexError {
    UnexpectedEof {
        // The file offset that could not be read.
        fo: u64,
        // The last line number and char number that was successfully read.
        line_char_last: LineCharNums,
    },

    UnexpectedChar {
        ch: char,
        fo: u64,
        len: u8,
        line_char: LineCharNums,
    },

    UnexpectedCharResult(CharResult),

    Todo,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum LexResult {
    Token(TokenInfo),

    Eod {
        // Total size of the file.
        file_size: u64,
        // Last line number of the file.
        last_line: LineNum,
    },

    Error(LexError),
}

impl LexResult {
    pub fn is_eod(&self) -> bool {
        matches!(self, &LexResult::Eod { .. })
    }
    pub fn is_error(&self) -> bool {
        matches!(self, &LexResult::Eod { .. })
    }
    pub fn is_final(&self) -> bool {
        !matches!(self, &LexResult::Token(_))
    }
}

struct Lexer<'a> {
    source_chars: &'a mut dyn Iterator<Item = CharResult>,
    read_results: [CharResult; 3],
    yielded_final: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source_chars: &'a mut dyn Iterator<Item = CharResult>) -> Self {
        let mut self_ = Self {
            source_chars,
            read_results: [
                CharResult::default_error(),
                CharResult::default_error(),
                CharResult::default_error(),
            ],
            yielded_final: false,
        };
        self_.consume_char();
        self_.consume_char();
        self_
    }

    // Convenient access to the char just consumed read result
    #[inline]
    fn prev_read_result(&self) -> &CharResult {
        &self.read_results[0]
    }

    // Convenient access to the current char read result
    #[inline]
    fn cur_read_result(&self) -> &CharResult {
        &self.read_results[1]
    }

    // Convenient access to the next char read result
    #[inline]
    fn peek_next_read_result(&self) -> &CharResult {
        &self.read_results[2]
    }

    /// Call this to consume a char.
    /// You don't have to call this if you're returning LexResult::is_final() (i.e., something other
    /// than a token).
    fn consume_char(&mut self) {
        eprintln!(
            "Lexer::read_next_char() consuming {:?}",
            &self.read_results[1]
        );

        self.read_results.rotate_left(1);

        eprintln!(
            "Lexer::read_next_char() cur is now {:?}",
            &self.read_results[1]
        );

        self.read_results[2] = if let Some(read_result) = self.source_chars.next() {
            read_result
        } else if self.cur_read_result().is_eof() || self.cur_read_result().is_error() {
            self.cur_read_result().clone()
        } else {
            CharResult::default_error()
        };
    }

    /// Consumes the current char.
    /// Returns None if the new read_result is Char or Eol.
    /// Otherwise, returns an appropriate LexResult.
    ///
    /// Call this when you need to advance to the next char in order to probably use it.
    /// But if you're simply advancing past the last char you are consuming, then
    /// you can just call `consume_char()`.
    ///
    /// Actually, since all tokens end on non-chars, you probably never need this.
    ///
    // fn consume_char_expecting_char_or_eol(&mut self) -> Option<LexResult> {
    //     self.consume_char();
    //     self.lexerror_unless_cur_char_or_eol()
    // }

    /// Returns None if the cur_read_result is Char or Eol.
    /// Otherwise, returns an appropriate LexResult.
    ///
    /// Call this before you need to interpret the current read_result as a char.
    ///
    fn lexerror_unless_cur_char_or_eol(&self) -> Option<LexResult> {
        let cur_read_result = self.cur_read_result();
        if cur_read_result.is_char_or_eol() {
            None
        } else {
            Some(LexResult::Error(LexError::UnexpectedCharResult(
                cur_read_result.clone(),
            )))
        }
    }

    /// Lexes the next token (or other result) from the initial state.
    fn lex_initial(&mut self) -> Option<LexResult> {
        eprintln!("Lexer::lex_initial()");

        match self.cur_read_result() {
            CharResult::Char {
                ch,
                fo,
                len,
                line_char,
            } => {
                let (ch, fo, len, line_char) = (*ch, *fo, *len, *line_char);
                if ch.is_whitespace() {
                    self.consume_char();
                    None
                } else if is_comment_first_char(ch) {
                    self.lex_comment_first_char(ch, line_char)
                } else if is_punctuation_single_char(ch) {
                    self.lex_punctuation_single_char(ch, line_char)
                } else if is_decimal_digit(ch) {
                    self.lex_literal_digit(ch, line_char)
                } else {
                    // Every other char is unexpected
                    // Don't consume
                    Some(LexResult::Error(LexError::UnexpectedChar {
                        ch,
                        fo,
                        len,
                        line_char,
                    }))
                }
            }
            CharResult::Eol { .. } => {
                self.consume_char();
                None
            }
            CharResult::Eof {
                file_size,
                last_line,
            } => {
                // Don't consume
                Some(LexResult::Eod {
                    file_size: *file_size,
                    last_line: *last_line,
                })
            }
            char_result => Some(LexResult::Error(LexError::UnexpectedCharResult(
                char_result.clone(),
            ))),
        }
    }

    /// Lexes a punctuation token comprised of a single char.
    fn lex_punctuation_single_char(
        &mut self,
        ch: char,
        line_char: LineCharNums,
    ) -> Option<LexResult> {
        assert!(is_punctuation_single_char(ch) || is_comment_first_char(ch));

        self.consume_char();

        Some(LexResult::Token(TokenInfo {
            linechar_range: line_char..=line_char,
            token: Token::PunctuationSingleChar(ch),
        }))
    }

    fn lex_comment_first_char(
        &mut self,
        mut ch: char,
        line_char: LineCharNums,
    ) -> Option<LexResult> {
        assert!(is_comment_first_char(ch));
        let first_line_char = line_char;

        // Peek at the next char of the "//"" or "/*"" sequence
        match self.peek_next_read_result().char_or_nul() {
            '/' => {
                // Consume first '/'
                self.consume_char();

                // Consume second '/' and any following chars
                loop {
                    self.consume_char();

                    if !self.cur_read_result().is_char() {
                        return None;
                    }
                }
            }
            '*' => {
                // Consume first '/'
                self.consume_char();

                // Consume '*' and any following chars
                loop {
                    let mut state = 0usize;
                    loop {
                        // Consume '*' and any following chars through "*/"
                        self.consume_char();

                        if state == 2 {
                            return None; // Success
                        }

                        if !self.cur_read_result().is_char_or_eol() {
                            return None; // Some kind of problem. Let the outer loop find it.
                        }

                        state = match (state, self.cur_read_result().char_or_nul()) {
                            (0, '*') => 1,
                            (1, '/') => 2,
                            _ => 0,
                        };
                    }
                }
            }
            _ => {
                // Don't consume first '/', just delegate to punctuation single char
                self.lex_punctuation_single_char(ch, line_char)
            }
        }
    }

    fn lex_literal_digit(&mut self, mut ch: char, line_char: LineCharNums) -> Option<LexResult> {
        assert!(is_decimal_digit(ch));

        let first_line_char = line_char;

        let mut last_line_char = line_char;
        let mut i: i128 = decimal_digit_val(ch).into();

        'more_digits: loop {
            self.consume_char();

            ch = self.cur_read_result().char_or_nul();
            if !is_decimal_digit(ch) {
                break 'more_digits;
            }

            i *= 10;
            i += i128::from(decimal_digit_val(ch));

            // `.unwrap()` should be justified because ch is known to be a decimal digit.
            last_line_char = self.cur_read_result().opt_line_char().unwrap();
        } // 'more_digits

        Some(LexResult::Token(TokenInfo {
            linechar_range: first_line_char..=last_line_char,
            token: Token::LiteralInteger(IntegerRepr::I128(i)),
        }))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult;

    fn next(&mut self) -> Option<LexResult> {
        eprintln!("Lexer::next()");

        if self.yielded_final {
            return None;
        }

        loop {
            let opt_lex_result = self.lex_initial();
            if let Some(ref lex_result) = opt_lex_result {
                self.yielded_final = lex_result.is_final();
                eprintln!("Lexer::yielded_final <- {:?}", self.yielded_final);

                eprintln!("Lexer::next() yielding {:?}", &lex_result);
                return opt_lex_result;
            }
        }
    }
}

impl<'a> std::iter::FusedIterator for Lexer<'a> {}

#[cfg(test)]
mod test {
    #[test]
    fn test() {
        const TEST_DATA_SUBDIR: &str = "lexer";

        crate::test_util::insta_glob(TEST_DATA_SUBDIR, |file_path, bx_bufread| {
            let mut source_bytes = crate::source_bytes::source_bytes(bx_bufread);
            let mut source_chars = crate::source_chars::source_chars(&mut source_bytes);

            let mut tokens = crate::lexer::Lexer::new(&mut source_chars);

            let results = tokens.collect::<Vec<_>>();

            insta::assert_ron_snapshot!(results);
        });
    }
}
