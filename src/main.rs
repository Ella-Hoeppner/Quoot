mod interpreter;
mod library;
mod model;
mod parse;

use interpreter::repl;

fn main() {
  repl();
}
