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

use std::{default, fmt::Display};

use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::{ast::AstItem, values::Integer};

type ChumskyError = chumsky::error::Simple<char>;

#[derive(Error, Debug, Serialize, Deserialize)]
pub enum ParseFileError {
    #[error("couldn't open file: {0}")]
    StdIoError(String),

    #[error("UTF-8 encoding error: {0}")]
    Utf8Error(String),

    #[error("parse error")]
    Parse(Vec<String>),

    #[error("todo")]
    Todo,
}

impl From<Vec<ChumskyError>> for ParseFileError {
    fn from(chumsky_errors: Vec<ChumskyError>) -> Self {
        let vs: Vec<String> = chumsky_errors.iter().map(|ce| ce.to_string()).collect();
        ParseFileError::Parse(vs)
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ParsingResult {
    cnt_tokens: usize,
}

// ParseFileError
use chumsky::{
    Parser,
    prelude::Simple,
    primitive::{end, filter},
    text::TextParser,
};

fn parser() -> impl Parser<char, AstItem, Error = Simple<char>> {
    // filter(|ch: &char| ch.is_ascii_digit())
    // .map(|ch| {
    //     let d = ch.to_digit(10).unwrap();
    //     Ast::IntegerLiteral(Integer::I128(d as i128))
    // })
    // .padded_by(filter(|ch: &char| ch.is_whitespace()).repeated())
    // .then_ignore(end())

    let integer_literal = chumsky::text::int(10)
        .map(|s: String| AstItem::IntegerLiteral(Integer::I128(s.parse().unwrap())))
        .padded();

    integer_literal.then_ignore(end())
}

pub fn parse<T>(content: T) -> Result<AstItem, ParseFileError>
where
    T: AsRef<str>,
{
    let parser = parser();

    let s: &str = content.as_ref();

    parser.parse(s).map_err(Into::into)
}

pub fn parse_file(file_path: &std::path::Path) -> Result<AstItem, ParseFileError> {
    use crate::file_content::FileContent;

    let content = FileContent::new(file_path)
        .map_err(|stdioerror| ParseFileError::StdIoError(stdioerror.to_string()))?;

    parse(content)
}

#[cfg(test)]
mod test {
    use super::{ParseFileError, parse_file};

    #[test]
    fn test() -> anyhow::Result<()> {
        const TEST_DATA_SUBDIR: &str = "parser";

        crate::test_util::insta_glob_result(
            TEST_DATA_SUBDIR,
            |file_path: &std::path::Path,
             bx_bufread: Box<dyn std::io::BufRead>|
             -> anyhow::Result<()> {
                //eprintln!("parser::test::test() file_path: {:?}", _file_path);
                let parsing_result = crate::parser::parse_file(file_path);

                insta::assert_ron_snapshot!(parsing_result);

                Ok(())
            },
        )
    }
}
