use std::io;
use std::io::Write;

fn print_prompt() {
  print!("> ");
  io::stdout().flush().unwrap();
}

pub fn repl() {
  println!("Malice repl started!\n");
  let mut input_buffer = String::new();
  let stdin = io::stdin();
  print_prompt();
  while stdin.read_line(& mut input_buffer).is_ok() {
    let trimmed_input = input_buffer.trim_end();
    if trimmed_input.eq("#EXIT") {
      println!("\nExiting Malice repl\n");
      break
    }
    println!("You said \"{trimmed_input}\"");
    input_buffer.clear();
    print_prompt();
  }
}
