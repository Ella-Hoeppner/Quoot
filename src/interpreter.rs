use crate::parse::parse;
use crate::parse::QuootParseError;
use crate::parse::Sexp;
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::io::Write;

#[derive(Clone)]
pub enum QuootValue {
  List(Vec<QuootValue>),
  Int(i64),
  Float(f64),
  String(String),
  Symbol(String),
}

impl QuootValue {
  pub fn from_token(token: &String) -> QuootValue {
    let chars: Vec<char> = token.chars().collect();
    if chars[0] == '"' {
      return QuootValue::String(chars[1..chars.len() - 1].iter().collect());
    }
    match token.parse::<i64>() {
      Ok(int) => return QuootValue::Int(int),
      Err(_) => match token.parse::<f64>() {
        Ok(float) => return QuootValue::Float(float),
        Err(_) => (),
      },
    }
    QuootValue::Symbol(token.clone())
  }
  pub fn from_sexp(sexp: &Sexp) -> QuootValue {
    match sexp {
      Sexp::List(sub_sexps) => QuootValue::List(
        sub_sexps
          .iter()
          .map(|sub_sexp| QuootValue::from_sexp(sub_sexp))
          .collect(),
      ),
      Sexp::Leaf(token) => QuootValue::from_token(token),
    }
  }
}

impl fmt::Display for QuootValue {
  fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      QuootValue::Symbol(token) => fmt.write_str(token)?,
      QuootValue::String(token) => {
        fmt.write_str("\"")?;
        fmt.write_str(token)?;
        fmt.write_str("\"")?;
      }
      QuootValue::Int(int) => fmt.write_str(&int.to_string())?,
      QuootValue::Float(float) => fmt.write_str(&float.to_string())?,
      QuootValue::List(sub_expressions) => {
        fmt.write_str("(")?;
        let mut separator = "";
        for sexp in sub_expressions {
          fmt.write_str(separator)?;
          fmt.write_str(&sexp.to_string())?;
          separator = " ";
        }
        fmt.write_str(")")?;
      }
    }
    Ok(())
  }
}

#[derive(Debug)]
pub enum QuootEvalError {
  Parse(QuootParseError),
  UnboundSymbolError(String),
}

#[derive(Default, Clone)]
struct Env {
  entries: HashMap<String, &'static QuootValue>,
}
impl Env {
  pub fn merge(self, other_env: Env) -> Env {
    Env {
      entries: self.entries.into_iter().chain(other_env.entries).collect(),
    }
  }
  pub fn get(self, name: &str) -> Option<&QuootValue> {
    self.entries.get(name).map(|e| e.to_owned())
  }
}

#[derive(Default)]
struct Interpreter {
  env: Env,
}

impl Interpreter {
  pub fn get_binding(
    &self,
    name: String,
  ) -> Result<QuootValue, QuootEvalError> {
    Err(QuootEvalError::UnboundSymbolError(name))
    /*match self.env.get(&name) {
      Some(value) => Ok(value.to_owned()),
      None => Err(QuootEvalError::UnboundSymbolError(name)),
    }*/
  }
  pub fn eval(&mut self, form: &str) -> Result<QuootValue, QuootEvalError> {
    match parse(form) {
      Err(parse_error) => Err(QuootEvalError::Parse(parse_error)),
      Ok(sexp) => {
        let parse_value = QuootValue::from_sexp(&sexp);
        Ok(parse_value)
        /*match parse_value {
          QuootValue::List(_) => todo!(),
          QuootValue::Int(x) => Ok(QuootValue::Int(x)),
          QuootValue::Float(x) => Ok(QuootValue::Float(x)),
          QuootValue::String(str) => Ok(QuootValue::String(str)),
          QuootValue::Symbol(name) => interpreter.get_binding(name),
        }*/
      }
    }
  }
}

fn print_prompt() {
  print!("> ");
  io::stdout().flush().unwrap();
}

pub fn repl() {
  println!("\nQuoot repl started :D\n");
  let interpreter = &mut Interpreter::default();
  let mut input_buffer = String::new();
  let stdin = io::stdin();
  print_prompt();
  while stdin.read_line(&mut input_buffer).is_ok() {
    let trimmed_input = input_buffer.trim_end();
    if trimmed_input.eq("#EXIT") {
      println!("\nQuoot repl stopped. bye!!\n");
      break;
    }
    match interpreter.eval(trimmed_input) {
      Err(e) => println!("{:?}", e),
      Ok(value) => {
        println!("{:?}", value.to_string())
      }
    }
    input_buffer.clear();
    print_prompt();
  }
}
