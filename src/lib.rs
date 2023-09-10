// Copyright 2023 Marsh J. Ray
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![feature(iter_from_generator)]
#![feature(generators, generator_trait)]
#![feature(generic_arg_infer)] // const arrays in tests
#![feature(return_position_impl_trait_in_trait)]

mod data_repr;
mod lexer;
mod line_char_nums;
mod source_bytes;
mod source_chars;
mod util;

#[cfg(test)]
mod test_util;
