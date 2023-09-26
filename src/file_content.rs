// Copyright 2023 Marsh J. Ray
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![allow(dead_code)] //? TODO for development
// #![allow(unused_mut)] //? TODO for development
// #![allow(unused_variables)] //? TODO for development
// #![allow(unused_imports)] //? TODO for development
#![allow(non_snake_case)]

use anyhow::Result;
use memmap2::Mmap;
use self_cell::self_cell;

#[derive(Debug)]
struct FileContentImplInner {
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
    pub fn new(path: &std::path::Path) -> Result<FileContent> {
        let file = std::fs::File::open(path)?;

        let mmap = unsafe { Mmap::map(&file) }?;

        let fci = FileContentImpl::try_new::<anyhow::Error>(
            FileContentImplInner {
                file,
                mmap,
            },
            |owner| {
                let bytes = owner.mmap.as_ref();
                let s = std::str::from_utf8(bytes)?;
                Ok(RefBytesStr(bytes, s)) // RefBytesStr<'a>((&'a [u8], &'a str))
            }
        )?;

        Ok(FileContent(fci))
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
