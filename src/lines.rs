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
/*
use anyhow::*;

use serde::{Deserialize, Serialize};

pub fn lines(source: &str) -> Result<Vec<&str>> {
    let mut v_lines = vec![];

    let mut current_line: (&str, usize) = (source, 0);

    macro_rules! finish_line {
        () => {
            if current_line.1 != 0 {
                v_lines.push(&current_line.0[0..current_line.1]);
            }
        };
    }

    let mut track_prev_rest: &str = source;
    for ch in source.chars() {
        let rest = source;
        let prev_rest = track_prev_rest;
        track_prev_rest = rest;

        match ch {
            '\r' | '\n' => finish_line!(),
            _ => {
                if current_line.1 == 0 {
                    current_line.0 = prev_rest;
                }
                current_line.1 += 1;
            }
        }
    }

    finish_line!();

    Ok(v_lines)
}

#[cfg(test)]
mod test {
    #[test]
    fn test() {
        const TEST_DATA_SUBDIR: &str = "lines";

        crate::test_util::insta_glob(TEST_DATA_SUBDIR, |_file_path, bx_bufread| {
            eprintln!("lines::test::test() file_path: {:?}", _file_path);

            let mut source_bytes = crate::source_bytes::source_bytes(bx_bufread);
            let mut source_chars = crate::source_chars::source_chars(&mut source_bytes);

            let results = crate::lines::lines(&mut source_chars);

            insta::assert_ron_snapshot!(results);
        });
    }
}
*/