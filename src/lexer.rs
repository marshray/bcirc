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

use std::collections;

use anyhow::{Context, Result, anyhow};
use chumsky::{
    combinator::To,
    error::{RichPattern, RichReason},
    prelude::*,
};
use self_cell::self_cell;
//? use serde::{Deserialize, Serialize};
use serde::Serialize;
//? use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    Apostrophe,
    ParenthesisLeft,
    ParenthesisRight,
    Asterisk,
    PlusSign,
    Comma,
    Minus,
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

use crate::{file_content::FileContent, values::*};

type LexExtraErr<'src> = extra::Err<Rich<'src, char>>;

fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<(Token<'src>, SimpleSpan)>, LexExtraErr<'src>>
{
    let whitespace = text::whitespace()
        .at_least(1)
        .map(|_| Token::WhitespaceOrComment);

    let line_comment = just("//")
        .ignore_then(any().and_is(just('\n').not()).repeated())
        .then_ignore(just('\n'))
        .map(|_| Token::WhitespaceOrComment);

    let block_comment = just("/*")
        .ignore_then(any().and_is(just("*/").not()).repeated())
        .then_ignore(just("*/"))
        .map(|_| Token::WhitespaceOrComment);

    // let line_comment = just("//")
    //     .then(any().and_is(just('\n').not()).repeated())
    //     .padded();
    // Single-character marks

    let single_char_mark =
        one_of("!\"#%&'()*+,-./:;<=>?@[]_{|}").map(|ch| match ch {
            '!' => Token::ExclamationMark,
            '"' => Token::QuotationMark,
            '#' => Token::Octothorpe,
            '%' => Token::PercentSign,
            '&' => Token::Ampersand,
            '\'' => Token::Apostrophe,
            '(' => Token::ParenthesisLeft,
            ')' => Token::ParenthesisRight,
            '*' => Token::Asterisk,
            '+' => Token::PlusSign,
            ',' => Token::Comma,
            '-' => Token::Minus,
            '.' => Token::Period,
            '/' => Token::ForwardSlash,
            ':' => Token::Colon,
            ';' => Token::Semicolon,
            '<' => Token::LessThanSign,
            '=' => Token::EqualSign,
            '>' => Token::GreaterThanSign,
            '?' => Token::QuestionMark,
            '@' => Token::AtSign,
            '[' => Token::SquareBracketLeft,
            ']' => Token::SquareBracketRight,
            '_' => Token::Underscore,
            '{' => Token::CurlyBracketLeft,
            '|' => Token::VerticalBar,
            '}' => Token::CurlyBracketRight,
            _ => {
                debug_assert_eq!(ch, '!');
                Token::InternalError
            }
        });

    let forward_slash = just('/')
        .then_ignore(just('*').not().rewind())
        .map(|_| Token::ForwardSlash);

    let identifier =
        text::ascii::ident::<'src, &'src str, LexExtraErr<'src>>().map(Token::Identifier);

    let int_literal = text::int(10).map(|s: &str| {
        let i: i128 = s.parse().unwrap();
        let integer_value = Integer::I128(i);
        Token::IntegerLiteral(integer_value)
    });

    let lexeme = whitespace
        .or(line_comment)
        .or(block_comment)
        .or(forward_slash)
        .or(single_char_mark)
        .or(identifier)
        .or(int_literal);

    let lexeme = lexeme
        .map_with(|tok, e| (tok, e.span()))
        .map(|pr| (!matches!(pr.0, Token::WhitespaceOrComment)).then_some(pr))
        .repeated()
        .flatten();

    lexeme.collect()
}

fn lex_failure_to_anyhow_error<'src>(errs: Vec<Rich<'src, char>>, src: &'src str) -> anyhow::Error {
    anyhow!("msg: {errs:?}")

    /*
    let strs = errs
        .iter()
        .map(|err| err.reason().to_string())
        .collect::<Vec<_>>();

    anyhow!("msg: {strs:?}")
    // */
    /*
    let msg = err.reason().to_string();

    let label = (
            err.found()                                                 //?
                .map(|c| c.to_string())                                 //?
                .unwrap_or_else(|| "end of input".to_string()),         //?
            *err.span(),                                                                                       //?
        );
    let extra_labels =
        err.contexts()                                            //?
            .map(|(l, s)| (format!("while parsing this {l}"), *s));

    anyhow!("msg: {msg}\nlabel: {label}\nextra_labels: {extra_labels}")
    // */
}

type VecTokensSpans<'src> = Vec<(Token<'src>, SimpleSpan)>;
type ResultVecTokensSpans<'src> = anyhow::Result<VecTokensSpans<'src>>;

fn lex_str<'src>(src: &'src str) -> ResultVecTokensSpans<'src> {
    let parse_result = lexer().parse(src);

    let r: Result<Vec<(Token<'_>, SimpleSpan)>, anyhow::Error> = parse_result
        .into_result()
        .map_err(|errs: Vec<Rich<'src, char>>| lex_failure_to_anyhow_error(errs, src));

    r
}

fn lex_file_content<'src>(file_content: &'src FileContent) -> ResultVecTokensSpans<'src> {
    lex_str(file_content.as_ref())
}

self_cell!(
    pub struct LexedFile {
        owner: FileContent,

        #[covariant]
        dependent: VecTokensSpans,
    }
);

impl serde::Serialize for LexedFile {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;

        let file_name = self.borrow_owner().filename_or_default();
        let tokens_spans = self.borrow_dependent();

        let mut s = serializer.serialize_struct("LexedFile", 2)?;
        s.serialize_field("file_name", &file_name)?;
        s.serialize_field("token_spans", tokens_spans)?;
        s.end()
    }
}

pub fn lex_file(file_path: &std::path::Path) -> Result<LexedFile> {
    let file_content: FileContent =
        FileContent::new(file_path).with_context(|| format!("File: {}", file_path.display()))?;
    //.map_err(|stdioerror| ParseFileError::StdIoError(stdioerror.to_string()))?;

    let lexed_file = LexedFile::try_new::<anyhow::Error>(file_content, lex_file_content);

    lexed_file
}

#[cfg(test)]
mod t {
    use super::{ResultVecTokensSpans, lex_file};

    #[test]
    fn t1() -> anyhow::Result<()> {
        const TEST_DATA_SUBDIR: &str = "lexer";

        crate::test_util::insta_glob_result(
            TEST_DATA_SUBDIR,
            |file_path: &std::path::Path,
             bx_bufread: Box<dyn std::io::BufRead>|
             -> anyhow::Result<()> {
                let lexing_result = super::lex_file(file_path).map_err(|e| e.to_string());

                insta::assert_ron_snapshot!(lexing_result);

                Ok(())
            },
        )
    }
}
