use std::fs::File;
use std::io::{BufRead, BufReader, Read, Seek};
use std::path::{Path, PathBuf};

pub const TEST_DATA_DIR: &'static str = "test_data";
pub const GLOB_STR: &'static str = "*.{bin,txt}";

/// Calls `test_fn` for every file under `test_data_subdir` matching `GLOB_STR`.
/// 
/// `test_fn` returns `()`.
pub fn insta_glob<P: Into<PathBuf>, F: FnMut(&Path, Box<dyn BufRead>)>(
    test_data_subdir: P,
    mut test_fn: F,
){
    insta_glob_result(test_data_subdir, |path, bx_bufread| {
        test_fn(path, bx_bufread);
        Ok(())
    })
}

/// Calls `test_fn` for every file under `test_data_subdir` matching `GLOB_STR`.
///
/// `test_fn` returns `anyhow::Result`.
pub fn insta_glob_result<P: Into<PathBuf>, F: FnMut(&Path, Box<dyn BufRead>) -> anyhow::Result<()>>(
    test_data_subdir: P,
    mut test_fn_returning_result: F,
) {
    let test_data_subdir: PathBuf = test_data_subdir.into();
    let snapshot_path = PathBuf::from(TEST_DATA_DIR).join(test_data_subdir);

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
                let mut file = File::open(file_path).unwrap();
                let mut bufreader = BufReader::new(file);

                //let mut src_bytes = Vec::<u8>::new();
                //bufreader.read_to_end(&mut src_bytes)?;
                //bufreader.rewind()?;

                let bx_bufread = Box::new(bufreader);

                file_specific_redactions(&file_path, bx_bufread, &mut test_fn_returning_result);
            }
        );
    });
}

fn file_specific_redactions<F: FnMut(&Path, Box<dyn BufRead>) -> anyhow::Result<()>>(
    file_path: &Path,
    bx_bufread: Box<BufReader<File>>,
    test_fn: &mut F,
) {
    let mut settings = insta::Settings::clone_current();

    // Text files can have their line endings modified by source control, so configure the
    // test settings to redact the `file_offset_range` field for test data file types other
    // than `.bin`.
    if file_path.extension().unwrap_or_default() != "bin" {
        let selector = "[].loc";
        let replacement = insta::dynamic_redaction(|mut value, path| {
            value.walk(&mut |content| -> bool {
                //eprintln!("Debug: {content:?}");
                // Struct("SourceFileCharLoc", [
                //     ("file_offset_range", ...),
                //     ...
                // ])
                use insta::_macro_support::Content;
                match content {
                    Content::Struct(_struct_type, ref mut vec) => {
                        // struct_type: "SourceFileCharLoc"
                        //eprintln!("struct_type: {_struct_type:?}");
                        vec.retain(|(label, _)| label != &"file_offset_range");
                    }
                    _ => {}
                }
                false
            });
            value
        });

        settings.add_redaction(selector, replacement);
    }

    settings.bind(|| test_fn(file_path, bx_bufread).unwrap());
}
