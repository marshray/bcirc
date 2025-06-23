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

use std::process::ExitCode;

use anyhow::{Context, Result};

fn main() -> ExitCode {
    if let Err(e) = run() {
        eprintln!("{e}");
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn run() -> Result<()> {
    // Set up the config
    let bcirc_config = bcirc::EvaluatorConfig::new()?;

    let config = bcirc_config
        .read()
        .map_err(|poison_error| anyhow::anyhow!("Reading Bcirc Config: {poison_error}"))?;

    eprintln!("bcirc config: {config:?}");

    Ok(())
}
