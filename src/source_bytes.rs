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

use std::{
    sync::{Mutex, Arc},
    num::NonZeroU64,
    fs::File,
};

use memmap2::Mmap;

/// Types that make sense for file offsets.
pub trait FileOffset {}
impl FileOffset for u32 {}
impl FileOffset for u64 {}
impl FileOffset for usize {}

pub struct SourceBytes {
    file: File,
}

impl SourceBytes {
    pub fn new(file: std::fs::File) -> SourceBytes {
        SourceBytes {
            file,
        }
    }

    pub fn try_iter(&self) -> Result<SourceBytesIter, std::io::Error> {
        let file = self.file.try_clone()?;
        let flen = self.file.metadata()?.len();
        Ok(SourceBytesIter {
            file,
            flen,
            opt_mmap: None,
            fo: 0,
        })
    }
}

pub struct SourceBytesIter {
    file: File,
    flen: u64,
    opt_mmap: Option<(u64, Option<NonZeroU64>, Mmap)>,
    fo: u64,
}

impl SourceBytesIter {
}

impl<'a> Iterator for &'a SourceBytes {
    type Item = &'a [u8];

    /// Yields an empty slice on error or Eof.
    /// Call `error()` to determine which.
    fn next(&mut self) -> Option<Self::Item> {
        //Some(&self.buf[0..0]) //? TODO: 
        //;
        None //? TODO: 
    }

    //? TODO: skip()
}

/* 
#[cfg(test)]
#[derive(Deserialize, Serialize)]
pub enum MaybeSomeBytes<'a> {
    /// May contain some bytes. 
    SomeBytes(&'a [u8]),
    Eof,
    StdIoError(String),
}

pub struct SourceBytesIter<Fo>
where
    Fo: FileOffset
{}

impl<Fo> SourceBytesIter<Fo>
where
    Fo: FileOffset
{
}
 */
/* 
#[cfg(test)]
#[derive(Deserialize, Serialize)]
#[derive(Debug, PartialEq, Eq)]
pub struct SourceBytesReadResult<'a, Fo>
where
    Fo: FileOffset
{
    /// `Bytes`, `Eof`, or `StdIoError`.
    pub maybe_bytes: MaybeSomeBytes<'a>,
    
    /// If `maybe_bytes` is
    /// 
    /// - `Bytes`: the file offset of the first returned byte (or where it would be if empty)
    /// 
    /// - `Eof`: the size of the file, or
    /// 
    /// - `StdIoError`: the file offset where the error was encountered.
    pub file_offset: Fo,
}
*/
/*
#[allow(dead_code)]
pub fn source_bytes_bx_bufread<'a, Fo>(bx_bufread: Box<dyn BufRead>) -> SourceBytes<Fo> {
    let bytes = bx_bufread.bytes();

    let bytes_eod = bytes.map(Some).chain(std::iter::once(None));

    bytes_eod.scan(0u64, |state, opt_stdioresult| {
        let file_offset = *state;
        *state += 1;

        Some(SourceByteReadResult {
            file_offset,
            maybe_bytes: match opt_stdioresult {
                Some(Result::Ok(by)) => MaybeSomeBytes::Byte(by),
                Some(Result::Err(stdioerror)) => MaybeSomeBytes::StdIoError(stdioerror.to_string()),
                None => MaybeSomeBytes::Eof,
            },
        })
    })
}
*/
/* 
pub fn source_bytes_file(file_path: &std::path::Path) -> impl Iterator<Item = SourceByteReadResult> {
    let file = File::open(file_path).unwrap();
    let bufreader = BufReader::new(file);
    let bx_bufread = ;

    crate::source_bytes::source_bytes_bx_bufread(Box::new(bufreader))
}
 */
/* 
pub fn source_bytes_file_mmap(file_path: &std::path::Path) -> impl Iterator<Item = SourceByteReadResult> {
    use crate::file_content::FileContent;

    let content = FileContent::new(file_path)
        .map_err(|stdioerror| ParseFileError::StdIoError(stdioerror.to_string()))?;

    parse(content)
}
 */
#[cfg(test)]
mod test {
    #[test]
    fn test() {
/*
const TEST_DATA_SUBDIR: &str = "source_bytes";

let mut iter_n = 0_usize;
crate::test_util::insta_glob(TEST_DATA_SUBDIR, move |_file_path, bx_bufread| {
    iter_n += 1;
    let source_bytes = 
            match iter_n%2 {
                1 => {
                    crate::source_bytes::source_bytes_bx_bufread(bx_bufread)
                }
                0 => {
                    crate::source_bytes::source_bytes_bx_bufread(bx_bufread)
                    
                }
            };
            
            let results = source_bytes.collect::<Vec<_>>();
            
            insta::assert_ron_snapshot!(results);
        });
*/        
    }
}
