// Copyright 2023 Marsh J. Ray
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use std::io::{BufRead, Read};

use anyhow::*;
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub enum ByteOrEof {
    Byte(u8),
    StdIoError(String),
    Eof,
}

#[derive(Debug, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub struct SourceByteReadResult {
    pub file_offset: u64,
    pub byte_or_eof: ByteOrEof,
}

pub fn source_bytes(
    bx_bufread: Box<dyn std::io::BufRead>,
) -> impl Iterator<Item = SourceByteReadResult> {
    let mut bytes = bx_bufread.bytes();

    let mut bytes_eod = bytes
        .map(|result_by| Some(result_by))
        .chain(std::iter::once(None));

    bytes_eod.scan(0u64, |state, opt_stdioresult| {
        let file_offset = *state;
        *state += 1;

        Some(SourceByteReadResult {
            file_offset,
            byte_or_eof: match opt_stdioresult {
                Some(Result::Ok(by)) => ByteOrEof::Byte(by),
                Some(Result::Err(stdioerror)) => ByteOrEof::StdIoError(stdioerror.to_string()),
                None => ByteOrEof::Eof,
            },
        })
    })
}

#[cfg(test)]
mod test {
    #[test]
    fn test() {
        use crate::source_bytes::source_bytes;

        const TEST_DATA_SUBDIR: &'static str = "source_bytes";

        crate::test_util::insta_glob(TEST_DATA_SUBDIR, |file_path, bx_bufread| {
            let mut source_bytes = source_bytes(bx_bufread);

            let results = source_bytes.collect::<Vec<_>>();

            insta::assert_ron_snapshot!(results);
        });
    }
}
