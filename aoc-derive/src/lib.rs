// Based on the Apache-2.0/MIT licensed automod crate:
// https://github.com/dtolnay/automod

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::quote;
use regex::Regex;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use syn::{parse_macro_input, LitStr};

#[proc_macro]
pub fn impl_mod(input: TokenStream) -> TokenStream {
  let modules = get_modules(parse_macro_input!(input as LitStr));
  let uses = impl_uses(&modules);
  let available_days = impl_available_days(&modules);
  let solve = impl_solve(&modules);
  quote! {
      #uses
      pub const DAYS: &[u32] = &[#available_days];
      #solve
  }
  .into()
}

fn impl_uses(modules: &[(u32, String)]) -> TokenStream2 {
  modules
    .iter()
    .map(|(_, m)| {
      let ident = Ident::new(m, Span::call_site());
      quote! {
          pub mod #ident;
      }
    })
    .collect()
}

fn impl_available_days(modules: &[(u32, String)]) -> TokenStream2 {
  modules.iter().map(|(d, _)| quote! { #d, }).collect()
}

fn impl_solve(modules: &[(u32, String)]) -> TokenStream2 {
  let expanded: TokenStream2 = modules.iter().map(format_solve_match).collect();
  quote! {
      use std::time::{Duration, Instant};
      use anyhow::Result;
      pub fn solve(day: u32, input: &str) -> Result<((String, String), (Duration, Duration, Duration))> {
          match day {
              #expanded
              _ => panic!("Unavailable day: {}", day)
          }
      }
  }
}

fn format_solve_match(data: &(u32, String)) -> TokenStream2 {
  let (number, name) = data;
  let module = Ident::new(name, Span::call_site());
  quote! {
      #number => {
          let time = Instant::now();
          let data = #module::parse(&input)?;
          let parse_time = time.elapsed();

          let time = Instant::now();
          let result1 = #module::part1(&data);
          let part1_time = time.elapsed();

          let time = Instant::now();
          let result2 = #module::part2(&data);
          let part2_time = time.elapsed();

          Ok(
            (
              (format!("{}", result1), format!("{}", result2)),
              (parse_time, part1_time, part2_time)
            )
          )
      }
  }
}

fn get_modules(input: LitStr) -> Vec<(u32, String)> {
  let rel_path = input.value();
  let dir = match env::var_os("CARGO_MANIFEST_DIR") {
    Some(manifest_dir) => PathBuf::from(manifest_dir).join(rel_path),
    None => PathBuf::from(rel_path),
  };

  let mut modules = Vec::new();
  let p = Regex::new(r"d(?<day>\d{2})").unwrap();

  for entry in fs::read_dir(dir).unwrap() {
    let entry = entry.unwrap();
    if !entry.file_type().unwrap().is_file() {
      continue;
    }

    let file_name = entry.file_name();
    if file_name == "mod.rs" || file_name == "lib.rs" || file_name == "main.rs" {
      continue;
    }

    let path = Path::new(&file_name);
    if path.extension() == Some(OsStr::new("rs")) {
      let mut utf8 = file_name.into_string().unwrap();
      utf8.truncate(utf8.len() - ".rs".len());
      if let Some(res) = p.captures(&utf8) {
        let number = res["day"].parse::<u32>().unwrap();
        modules.push((number, utf8));
      }
    }
  }
  modules.sort();
  modules
}
