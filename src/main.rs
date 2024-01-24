mod interpreter;
mod new_parse;
use interpreter::repl;
use new_parse::test_parser;

fn main() {
  //repl();
  test_parser();
}
