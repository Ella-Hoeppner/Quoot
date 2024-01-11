use std::{env, fs};

use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "core_lisp.pest"]
pub struct CoreLispParser;

#[test]
fn parse_numbers() {
  let valid_nums = ["1", "-1", "1.0", "-273.15", "50", "5.", ".0"];
  let invalid_nums = ["hi!!", "a5", "5a", "", ".", "5.0.0", ".0.0", "0..1"];

  println!("testing valids...");
  valid_nums.map(|num_string| {
    let successful_parse = CoreLispParser::parse(Rule::number, num_string);
    assert!(
      successful_parse.is_ok(),
      "{} shouldn't parse as a valid number!",
      num_string
    );
  });

  println!("testing invalids...");
  invalid_nums.map(|num_string| {
    let successful_parse = CoreLispParser::parse(Rule::number, num_string);
    assert!(
      successful_parse.is_err(),
      "{} should parse as a valid number!",
      num_string
    );
  });
}
