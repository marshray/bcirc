// Copyright 2023 Marsh J. Ray
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//? TODO The current nightly rustc seems to have problems with declaring code dead when it really isn't.
#![allow(dead_code)] //? TODO for development
// #![allow(unused_mut)] //? TODO for development
// #![allow(unused_variables)] //? TODO for development
// #![allow(unused_imports)] //? TODO for development

use std::ops::RangeInclusive;

use serde::{Deserialize, Serialize};

use crate::data_repr::IntegerRepr;
use crate::line_char_nums::{LineCharNums, LineNum};
use crate::source_chars::CharResult;
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
    Identifier(String),
    Whitespace,
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
        linecharnum: LineCharNums,
    },

    UnexpectedCharResult(CharResult),
    InternalError,
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
        matches!(self, &LexResult::Error(_))
    }
    
    pub fn is_final(&self) -> bool {
        // Anything other than a token is final
        !matches!(self, &LexResult::Token(_))
    }

    fn is_following_whitespace_significant(&self) -> bool {
        // Whitespace is only relevant immediately after + and - tokens.
        matches!(
            self,
            &LexResult::Token(TokenInfo {
                token: Token::PunctuationSingleChar('+' | '-'),
                ..
            })
        )
    }
}

#[derive(Debug, Clone)]
enum MaybeCharResult {
    CharResult(CharResult),
    PastEofCount(usize),
}

struct Lexer<'a> {
    bx_source_chars: Box<dyn Iterator<Item = MaybeCharResult> + 'a>,
    charresults: [CharResult; 3],
    linecharnumses: [LineCharNums; 3],
    prev_yielded_wants_whitespace: bool,
    yielded_final: bool,
}

macro_rules! consume_char {
    ($self:ident, $name:ident) => {{
        consume_char!(@common, false, $self, $name);
    }};
    (eol_is_edible; $self:ident, $name:ident) => {{
        consume_char!(@common, true, $self, $name);
    }};
    (@common, $eol_is_edible:expr, $self:ident, $name:ident) => {
        // #[rustfmt::skip]
        // eprintln!("Lexer::{}() consuming {:?}", stringify!($name), $self.cur_charresult());

        assert!(
                ($eol_is_edible && $self.cur_charresult().is_eol())
            || $self.cur_charresult().is_char());
        $self.consume_char();
    };
}

impl<'a> Lexer<'a> {
    pub fn new(source_chars: &'a mut dyn Iterator<Item = CharResult>) -> Self {
        // Set up source_chars to give a count sequence after the end.
        let past_end = (0..=usize::MAX).map(MaybeCharResult::PastEofCount);
        let source_chars = source_chars.map(MaybeCharResult::CharResult);
        let source_chars = source_chars.chain(past_end);

        let mut self_ = Self {
            bx_source_chars: Box::new(source_chars),
            charresults: [
                CharResult::default_error(),
                CharResult::default_error(),
                CharResult::default_error(),
            ],
            linecharnumses: [LineCharNums::max_max(); 3],
            prev_yielded_wants_whitespace: false,
            yielded_final: false,
        };
        self_.consume_char();
        self_.consume_char();
        self_
    }

    // Convenient access to the char just consumed read result
    #[inline]
    fn prev_charresult(&self) -> &CharResult {
        &self.charresults[0]
    }
    
    // Convenient access to the char just consumed's linecharnums
    #[inline]
    fn prev_linecharnums(&self) -> LineCharNums {
        self.linecharnumses[0]
    }

    // Convenient access to the current char read result
    #[inline]
    fn cur_charresult(&self) -> &CharResult {
        &self.charresults[1]
    }

    // Convenient access to the current char's linecharnums
    #[inline]
    fn cur_linecharnums(&self) -> LineCharNums {
        self.linecharnumses[1]
    }

    // Convenient access to the next char read result
    #[inline]
    fn peek_next_charresult(&self) -> &CharResult {
        &self.charresults[2]
    }

    // Convenient access to the next char's linecharnums
    #[inline]
    fn peek_next_linecharnums(&self) -> LineCharNums {
        self.linecharnumses[2]
    }

    /// Call this to consume a char.
    /// You don't have to call this if you're returning LexResult::is_final() (i.e., something other
    /// than a token).
    fn consume_char(&mut self) {
        self.charresults.rotate_left(1);
        self.linecharnumses.rotate_left(1);

        let source_char_result = self.bx_source_chars.next().unwrap();

        self.charresults[2] = match source_char_result {
            MaybeCharResult::CharResult(char_result) => char_result,
            MaybeCharResult::PastEofCount(past_eof_count) => {
                assert!(past_eof_count < 5);
                CharResult::default_error()
            }
        };

        // Try to get the current linecharnum. That really shouldn't fail, but if it does just
        // increment the value from the previous char.
        self.linecharnumses[2] = self.charresults[2].opt_linecharnums().unwrap_or(
            LineCharNums::max_max() );
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
        let cur_read_result = self.cur_charresult();
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
        let cur_read_result = self.cur_charresult();

        match cur_read_result {
            CharResult::Char {
                ch,
                fo,
                len,
                linecharnum,
            } => {
                let (ch, fo, len, linecharnum) = (*ch, *fo, *len, *linecharnum);

                if ch.is_whitespace() {
                    self.lex_whitespace(linecharnum)
                } else if is_comment_first_char(ch) {
                    self.lex_comment_first_char(linecharnum)
                } else if is_punctuation_single_char(ch) {
                    self.lex_punctuation_single_char(linecharnum)
                } else if is_decimal_digit(ch) {
                    self.lex_numeric_literal(linecharnum)
                } else if is_identifier_first_char(ch) {
                    self.lex_identifier()
                } else {
                    // Every other char is unexpected
                    // Don't consume
                    Some(LexResult::Error(LexError::UnexpectedChar {
                        ch,
                        fo,
                        len,
                        linecharnum,
                    }))
                }
            }
            CharResult::Eol { linecharnum, .. } => {
                self.lex_whitespace(*linecharnum)
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
            char_result => {
                //eprintln!("lex: char_result = {char_result:?}");

                Some(LexResult::Error(LexError::UnexpectedCharResult(
                    char_result.clone(),
                )))
            }
        }
    }

    /// Lexes whitespace.
    fn lex_whitespace(&mut self, first_linecharnums: LineCharNums) -> Option<LexResult> {
        debug_assert!(self.cur_charresult().is_whitespace());

        loop {
            consume_char!(eol_is_edible; self, lex_whitespace);

            if !self.cur_charresult().is_whitespace() {
                break;
            }
        }

        self.whitespace_if_significant(first_linecharnums)
    }

    /// Returns a whitespace token if that's significant here.
    /// Prev read result should be a whitespace char
    fn whitespace_if_significant(&mut self, first_linecharnums: LineCharNums) -> Option<LexResult> {
        if !self.prev_yielded_wants_whitespace {
            return None;
        }

        let last_linecharnums = self
            .cur_charresult()
            .opt_linecharnums()
            .unwrap_or(first_linecharnums);

        Some(LexResult::Token(TokenInfo {
            linechar_range: first_linecharnums..=last_linecharnums,
            token: Token::Whitespace,
        }))
    }

    /// Lexes a punctuation token comprised of a single char.
    fn lex_punctuation_single_char(
        &mut self,
        linecharnum: LineCharNums,
    ) -> Option<LexResult> {
        let opt_lexerror = self.lexerror_unless_cur_char_or_eol();
        if opt_lexerror.is_some() { return opt_lexerror; }

        let ch = self.cur_charresult().char_or_nul();

        if !(is_punctuation_single_char(ch) || is_comment_first_char(ch)) {
            return Some(LexResult::Error(LexError::InternalError));
        }

        consume_char!(self, lex_punctuation_single_char);

        Some(LexResult::Token(TokenInfo {
            linechar_range: linecharnum..=linecharnum,
            token: Token::PunctuationSingleChar(ch),
        }))
    }

    /// Lexes a token that starts with '/'.
    fn lex_comment_first_char(
        &mut self,
        first_linecharnums: LineCharNums,
    ) -> Option<LexResult> {
        let opt_lexerror = self.lexerror_unless_cur_char_or_eol();
        if opt_lexerror.is_some() { return opt_lexerror; }

        assert!(is_comment_first_char(self.cur_charresult().char_or_nul()));

        // Peek at the next char of the "//"" or "/*" sequence
        match self.peek_next_charresult().char_or_nul() {
            '/' => self.lex_line_comment(first_linecharnums),
            '*' => self.lex_block_comment(first_linecharnums),
            _ => self.lex_punctuation_single_char(first_linecharnums),
        }
    }

    fn lex_line_comment(
        &mut self,
        first_linecharnums: LineCharNums,
    ) -> Option<LexResult> {
        // Consume the two slashes promised to us by the caller.
        let mut cnt_slashes = 0_usize;
        while cnt_slashes < 2 {
            let opt_lexerror = self.lexerror_unless_cur_char_or_eol();
            if opt_lexerror.is_some() { return opt_lexerror; }

            let ch = self.cur_charresult().char_or_nul();
            if ch == '/' {
                cnt_slashes += 1;
            } else {
                return Some(LexResult::Error(LexError::InternalError));
            }

            consume_char!(self, lex_line_comment);
        }

        // Consume up to the Eol
        loop {
            let opt_lexerror = self.lexerror_unless_cur_char_or_eol();
            if opt_lexerror.is_some() { return opt_lexerror; }

            if self.cur_charresult().is_eol() {
                break;
            }

            consume_char!(self, lex_line_comment);
        }

        self.whitespace_if_significant(first_linecharnums)
    }

    fn lex_block_comment(
        &mut self,
        first_linecharnums: LineCharNums,
    ) -> Option<LexResult> {
        const CHAR_SEQ: [char; 4] = ['/', '*', '*', '/'];

        let mut state = 0_usize;
        while state != CHAR_SEQ.len() {
            let opt_lexerror = self.lexerror_unless_cur_char_or_eol();
            if opt_lexerror.is_some() { return opt_lexerror; }

            if self.cur_charresult().char_or_nul() == CHAR_SEQ[state] {
                state += 1;
            } else {
                // We didn't get the char we were hoping for. But those first two should have
                // been guaranteed by the caller.
                if state < 2 {
                    return Some(LexResult::Error(LexError::InternalError));
                }

                // Later char positions just restart at state 2.
                state = 2;
            }

            consume_char!(eol_is_edible; self, lex_block_comment);
        }

        self.whitespace_if_significant(first_linecharnums)
    }

    fn lex_numeric_literal(&mut self, linecharnum: LineCharNums) -> Option<LexResult> {
        let opt_lexerror = self.lexerror_unless_cur_char_or_eol();
        if opt_lexerror.is_some() { return opt_lexerror; }

        let mut ch = self.cur_charresult().char_or_nul();
        if !is_decimal_digit(ch) {
            return Some(LexResult::Error(LexError::InternalError));
        }

        let first_linecharnums = self.cur_linecharnums();

        let mut last_linecharnums = linecharnum;
        let mut i: i128 = decimal_digit_val(ch).into();

        'more_digits: loop {
            consume_char!(self, lex_numeric_literal);

            let opt_lexerror = self.lexerror_unless_cur_char_or_eol();
            if opt_lexerror.is_some() { return opt_lexerror; }
    
            ch = self.cur_charresult().char_or_nul();
            if !is_decimal_digit(ch) {
                break 'more_digits;
            }

            i *= 10;
            i += i128::from(decimal_digit_val(ch));

            // `.unwrap()` should be justified because ch is known to be a decimal digit.
            last_linecharnums = self.cur_charresult().opt_linecharnums().unwrap();
        } // 'more_digits

        Some(LexResult::Token(TokenInfo {
            linechar_range: first_linecharnums..=last_linecharnums,
            token: Token::LiteralInteger(IntegerRepr::I128(i)),
        }))
    }

    fn lex_identifier(&mut self) -> Option<LexResult> {
        let opt_lexerror = self.lexerror_unless_cur_char_or_eol();
        if opt_lexerror.is_some() { return opt_lexerror; }

        let ch = self.cur_charresult().char_or_nul();
        if !is_identifier_first_char(ch) {
            return Some(LexResult::Error(LexError::InternalError));
        }

        let first_linecharnums = self.cur_linecharnums();

        let mut last_linecharnums = first_linecharnums;
        let mut s: String = ch.into();

        loop {
            consume_char!(self, lex_identifier);

            let opt_lexerror = self.lexerror_unless_cur_char_or_eol();
            if opt_lexerror.is_some() { return opt_lexerror; }
    
            let ch = self.cur_charresult().char_or_nul();
            if !is_identifier_subsequent_char(ch) {
                break;
            }

            s.push(ch);

            last_linecharnums = self.cur_linecharnums();
        }

        Some(LexResult::Token(TokenInfo {
            linechar_range: first_linecharnums..=last_linecharnums,
            token: Token::Identifier(s),
        }))
    }}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult;

    fn next(&mut self) -> Option<LexResult> {
        if self.yielded_final {
            return None;
        }

        loop {
            let opt_lex_result = self.lex_initial();

            if let Some(ref lex_result) = opt_lex_result {
                self.prev_yielded_wants_whitespace =
                    lex_result.is_following_whitespace_significant();

                self.yielded_final = lex_result.is_final();

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

        crate::test_util::insta_glob(TEST_DATA_SUBDIR, |_file_path, bx_bufread| {
            //eprintln!("lexer::test::test() file_path: {:?}", _file_path);

            let mut source_bytes = crate::source_bytes::source_bytes(bx_bufread);
            let mut source_chars = crate::source_chars::source_chars(&mut source_bytes);

            let tokens = crate::lexer::Lexer::new(&mut source_chars);

            let results = tokens.collect::<Vec<_>>();

            insta::assert_ron_snapshot!(results);
        });
    }

    #[test]
    pub fn t_01() {
        let path = std::path::Path::new(r"src/test_data/lexer/identifiers.txt");
        let file = std::fs::File::open(path).unwrap();
        let bufreader = std::io::BufReader::new(file);
        let bx_bufread = Box::new(bufreader);
        let mut source_bytes = crate::source_bytes::source_bytes(bx_bufread);
        let mut source_chars = crate::source_chars::source_chars(&mut source_bytes);
        let tokens = crate::lexer::Lexer::new(&mut source_chars);
        let results = tokens.collect::<Vec<_>>();

        for (ix, lex_result) in results.iter().enumerate() {
            eprintln!("lex_result = {:?}", &lex_result);
            let last = ix + 1 == results.len();

            assert!(!lex_result.is_error());
            assert!(last == lex_result.is_eod());
            assert!(last == lex_result.is_final());
        }
    }
}
