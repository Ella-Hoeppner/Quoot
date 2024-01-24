use crate::parse::parse;
use crate::parse::QuootParseError;
use crate::parse::Sexp;
use std::io;
use std::io::Write;

fn print_prompt() {
  print!("> ");
  io::stdout().flush().unwrap();
}

fn eval(form: &str) -> Result<Sexp, QuootParseError> {
  parse(form)
}

pub fn repl() {
  println!("Malice repl started!\n");
  let mut input_buffer = String::new();
  let stdin = io::stdin();
  print_prompt();
  while stdin.read_line(&mut input_buffer).is_ok() {
    let trimmed_input = input_buffer.trim_end();
    if trimmed_input.eq("#EXIT") {
      println!("\nExiting Malice repl\n");
      break;
    }
    let eval_result = eval(trimmed_input);
    println!("{:?}", eval_result);
    input_buffer.clear();
    print_prompt();
  }
}
