use crate::library::default_bindings;
use crate::model::{top_level_eval, Env, QuootEvalError, QuootValue};
use crate::parse::parse;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

pub fn repl() -> Result<()> {
  println!("\nQuoot repl started :D\n");
  let mut global_env = Env::from_bindings(default_bindings());

  let mut rl = DefaultEditor::new()?;
  if rl.load_history("history.txt").is_err() {
    println!("No Quoot repl history found");
  }
  loop {
    match rl.readline("> ") {
      Ok(line) => {
        let trimmed_input = line.trim_start().trim_end();
        rl.add_history_entry(line.as_str())?;
        match parse(trimmed_input) {
          Err(parse_error) => {
            println!("{:?}", QuootEvalError::Parse(parse_error))
          }
          Ok(form) => {
            match top_level_eval(&global_env, &QuootValue::from_sexp(&form)) {
              Err(e) => println!("{:?}", e),
              Ok((value, maybe_bindings)) => {
                println!("{}", value.to_string());
                if let Some(bindings) = maybe_bindings {
                  global_env.bind_all(bindings)
                };
              }
            }
          }
        }
      }
      Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
        println!("\nStopping Quoot repl. Bye!!!");
        break;
      }
      Err(err) => {
        println!("Error: {:?}", err);
        break;
      }
    }
  }
  rl.save_history("history.txt")?;
  Ok(())
}
