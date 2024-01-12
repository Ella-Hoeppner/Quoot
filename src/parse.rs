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

  valid_nums.map(|num_string| {
    let parse = CoreLispParser::parse(Rule::number, num_string);
    assert!(
      parse.is_ok(),
      "{} should parse as a valid number!",
      num_string
    );
  });

  invalid_nums.map(|num_string| {
    let parse = CoreLispParser::parse(Rule::number, num_string);
    assert!(
      parse.is_err(),
      "{} shouldn't parse as a valid number!",
      num_string
    );
  });
}

#[test]
fn parse_strings() {
  let valid_nums = [
    "\"ur so valid!\"",
    "\"\"",
    "\"\"",
    "\"\\\"\"",
    "\"\\\"escaped\\\"\"",
  ];
  let invalid_nums = [">:(", "5", "hello?", "\""];

  valid_nums.map(|str_string| {
    let parse = CoreLispParser::parse(Rule::string, str_string);
    assert!(
      parse.is_ok(),
      "{} should parse as a valid string!",
      str_string
    );
    let unwrapped_parse = parse.unwrap();
    let span = unwrapped_parse.clone().next().unwrap().as_span();
    assert!(
      span.end() == str_string.len(),
      "String {} does not get entirely consumed. String has length {} but \
      parsing terminates at character {}.\nParse tree:\n{:?}",
      str_string,
      str_string.len(),
      span.end(),
      unwrapped_parse
    );
  });

  invalid_nums.map(|str_string| {
    let parse = CoreLispParser::parse(Rule::string, str_string);
    assert!(
      parse.is_err(),
      "{} shouldn't parse as a valid string!",
      str_string
    );
  });
}
