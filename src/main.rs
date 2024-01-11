mod interpreter;
mod parse;
use interpreter::repl;
use parse::test_parser;

fn main() {
  //repl();
  test_parser();
}
