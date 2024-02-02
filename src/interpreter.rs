use crate::library::default_bindings;
use crate::model::Env;
use crate::model::QuootEvalError;
use crate::model::QuootFn;
use crate::model::QuootValue;
use crate::model::QuootValueList;
use crate::parse::parse;
use std::io;
use std::io::Write;

struct Interpreter {
  env: Env,
}

impl Interpreter {
  pub fn new(root_env: Env) -> Interpreter {
    Interpreter { env: root_env }
  }
  pub fn add_binding(&mut self, name: String, value: QuootValue) {
    self.env.bind(&name, value)
  }
  pub fn get_binding(
    &self,
    name: String,
  ) -> Result<QuootValue, QuootEvalError> {
    match self.env.get(&name) {
      Some(value) => Ok(value.to_owned()),
      None => Err(QuootEvalError::UnboundSymbolError(name)),
    }
  }
  pub fn apply(
    &self,
    f: QuootFn,
    args: QuootValueList,
  ) -> Result<QuootValue, QuootEvalError> {
    f(args)
  }
  pub fn eval(
    &mut self,
    value: QuootValue,
  ) -> Result<QuootValue, QuootEvalError> {
    match value {
      QuootValue::Symbol(name) => self.get_binding(name),
      QuootValue::List(values) => {
        let evaluated_values: &mut QuootValueList = &mut values
          .iter()
          .map(|v| self.eval(v.to_owned()))
          .into_iter()
          .collect::<Result<QuootValueList, QuootEvalError>>()?;
        match evaluated_values.pop_front() {
          None => Ok(QuootValue::List(QuootValueList::new())),
          Some(function) => match function {
            QuootValue::Fn(f) => self.apply(f, evaluated_values.to_owned()),
            other => {
              Err(QuootEvalError::AppliedUnapplicableError(other.to_string()))
            }
          },
        }
      }
      other => Ok(other),
    }
  }
  pub fn new_with_standard_env() -> Interpreter {
    Interpreter::new(Env::from_bindings(default_bindings()))
  }
}

fn print_prompt() {
  print!("> ");
  io::stdout().flush().unwrap();
}

pub fn repl() {
  println!("\nQuoot repl started :D\n");
  let interpreter = &mut Interpreter::new_with_standard_env();
  let mut input_buffer = String::new();
  let stdin = io::stdin();
  print_prompt();
  while stdin.read_line(&mut input_buffer).is_ok() {
    let trimmed_input = input_buffer.trim_start().trim_end();
    if trimmed_input.eq("#EXIT") {
      println!("\nQuoot repl stopped. bye!!\n");
      break;
    }
    match parse(trimmed_input) {
      Err(parse_error) => {
        println!("{:?}", QuootEvalError::Parse(parse_error))
      }
      Ok(form) => match interpreter.eval(QuootValue::from_sexp(&form)) {
        Err(e) => println!("{:?}", e),
        Ok(value) => {
          println!("{}", value.to_string())
        }
      },
    }
    input_buffer.clear();
    print_prompt();
  }
}
