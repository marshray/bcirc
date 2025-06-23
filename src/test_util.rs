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

use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

//use anyhow::anyhow;

pub const TEST_DATA_DIR: &str = "test_data";
pub const GLOB_STR: &str = "*.{bin,txt}";

/// Calls `test_fn` for every file under `test_data_subdir` matching `GLOB_STR`.
///
/// `test_fn` returns `()`.
#[allow(dead_code)]
pub fn insta_glob<P: Into<PathBuf>, F: FnMut(&Path, Box<dyn BufRead>)>(
    test_data_subdir: P,
    mut test_fn: F,
) {
    insta_glob_result(test_data_subdir, |path, bx_bufread| {
        test_fn(path, bx_bufread);
        Ok(())
    })
    .unwrap();
}

/// Calls `test_fn` for every file under `test_data_subdir` matching `GLOB_STR`.
///
/// `test_fn` returns `anyhow::Result`.
pub fn insta_glob_result<
    P: Into<PathBuf>,
    F: FnMut(&Path, Box<dyn BufRead>) -> anyhow::Result<()>,
>(
    test_data_subdir: P,
    mut test_fn_returning_result: F,
) -> anyhow::Result<()> {
    let test_data_subdir: PathBuf = test_data_subdir.into();
    let snapshot_path = PathBuf::from(TEST_DATA_DIR).join(test_data_subdir);

    let mut vec_err_paths = Vec::<(std::path::PathBuf, anyhow::Error)>::new();

    insta::with_settings!({
        omit_expression => true,
        prepend_module_to_snapshot => false,
        snapshot_path => snapshot_path.clone(),
        sort_maps => true,
    }, {
        insta::glob!(
            snapshot_path,
            GLOB_STR,
            |file_path| {
                let file = File::open(file_path).unwrap();
                let bufreader = BufReader::new(file);

                //let mut src_bytes = Vec::<u8>::new();
                //bufreader.read_to_end(&mut src_bytes)?;
                // use::std::io::Seek;
                //bufreader.rewind()?;

                let bx_bufread = Box::new(bufreader);

                let result = file_specific_redactions(file_path, bx_bufread, &mut test_fn_returning_result);

                if let Err(error) = result {
                    vec_err_paths.push((file_path.into(), error));
                }
            }
        )
    });

    if !vec_err_paths.is_empty() {
        anyhow::bail!("one or more of the glob files errored")
    }
    Ok(())
}

fn file_specific_redactions<F: FnMut(&Path, Box<dyn BufRead>) -> anyhow::Result<()>>(
    file_path: &Path,
    bx_bufread: Box<BufReader<File>>,
    test_fn: &mut F,
) -> anyhow::Result<()> {
    let mut settings = insta::Settings::clone_current();

    // Text files can have their line endings modified by source control, so configure the
    // test settings to redact the `file_offset_range` field for test data file types other
    // than `.bin`.
    if file_path.extension().unwrap_or_default() != "bin" {
        let selector = "[].loc";
        let replacement = insta::dynamic_redaction(|mut value, _content_pathpath| {
            value.walk(&mut |content| -> bool {
                //eprintln!("Debug: {content:?}");
                // Struct("SourceFileCharLoc", [
                //     ("file_offset_range", ...),
                //     ...
                // ])
                use insta::_macro_support::Content;
                if let Content::Struct(_struct_type, vec) = content {
                    // struct_type: "SourceFileCharLoc"
                    //eprintln!("struct_type: {_struct_type:?}");
                    vec.retain(|(label, _)| *label != "file_offset_range");
                }

                false
            });
            value
        });

        settings.add_redaction(selector, replacement);
    }

    settings.bind(|| test_fn(file_path, bx_bufread))
}
