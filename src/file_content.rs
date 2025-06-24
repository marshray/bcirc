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
//#![allow(non_upper_case_globals)] //? TODO for development
#![allow(noop_method_call)] //? TODO for development
#![allow(non_camel_case_types)] //? TODO for development
//#![allow(unused_mut)] //? TODO for development
#![allow(unused_braces)] //? TODO for development
#![allow(unused_assignments)]
//? TODO for development
//#![allow(unused_variables)] //? TODO for development
//#![allow(unused_imports)] //? TODO for development
//#![allow(unreachable_code)] //? TODO for development
#![allow(clippy::needless_lifetimes, clippy::let_and_return)] //? TODO for development

use anyhow::Result;
use memmap2::Mmap;
use self_cell::self_cell;

#[derive(Debug)]
struct FileContentImplInner {
    pb: std::path::PathBuf,
    file: std::fs::File,
    mmap: Mmap,
}

#[derive(Debug)]
struct RefBytesStr<'a>(&'a [u8], &'a str);

self_cell!(
    struct FileContentImpl {
        owner: FileContentImplInner,

        #[covariant]
        dependent: RefBytesStr,
    }

    impl {Debug}
);

pub struct FileContent(FileContentImpl);

impl FileContent {
    /// Construts a new [`FileContent`].
    ///
    /// The file must be available for reading and `mmap`, and contain only valid `UTF-8`.
    pub fn new(path: &std::path::Path) -> Result<FileContent> {
        let pb = path.to_path_buf();

        let file = std::fs::File::open(path)?;

        let mmap = unsafe { Mmap::map(&file) }?;

        let fci = FileContentImpl::try_new::<anyhow::Error>(
            FileContentImplInner { pb, file, mmap },
            |owner| {
                let bytes = owner.mmap.as_ref();
                let s = std::str::from_utf8(bytes)?;
                Ok(RefBytesStr(bytes, s)) // RefBytesStr<'a>((&'a [u8], &'a str))
            },
        )?;

        Ok(FileContent(fci))
    }

    /// Returns the file [`Path`](std::path::Path).
    pub fn path(&self) -> &std::path::Path {
        self.0.borrow_owner().pb.as_path()
    }

    /// Returns the [`file_name`](std::path::Path::file_name) (converted with
    /// [`to_string_lossy()`](std::path::Path::to_string_lossy)), or an empty [`String`].
    pub fn filename_or_default(&self) -> String {
        self.path()
            .file_name()
            .map(|os_str| os_str.to_string_lossy().into_owned())
            .unwrap_or_default()
    }
}

impl AsRef<[u8]> for FileContent {
    fn as_ref(&self) -> &[u8] {
        self.0.borrow_dependent().0
    }
}

impl AsRef<str> for FileContent {
    fn as_ref(&self) -> &str {
        self.0.borrow_dependent().1
    }
}
