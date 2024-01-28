use crate::parse::parse;
use crate::parse::QuootParseError;
use crate::parse::Sexp;
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::io::Write;

#[derive(Clone)]
pub enum QuootValue {
  Nil,
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
      Sexp::Leaf(token) => {
        if token == "nil" {
          QuootValue::Nil
        } else {
          QuootValue::from_token(token)
        }
      }
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
      QuootValue::Nil => fmt.write_str("nil")?,
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
  bindings: HashMap<String, QuootValue>,
}
impl Env {
  pub fn bind(&mut self, name: &str, value: QuootValue) {
    self.bindings.insert(name.to_owned(), value);
  }
  pub fn get(&self, name: &str) -> Option<&QuootValue> {
    self.bindings.get(name).map(|e| e)
  }
}

#[derive(Default)]
struct Interpreter {
  env: Env,
}

impl Interpreter {
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
  pub fn eval(&mut self, form: &str) -> Result<QuootValue, QuootEvalError> {
    match parse(form) {
      Err(parse_error) => Err(QuootEvalError::Parse(parse_error)),
      Ok(sexp) => {
        let value = QuootValue::from_sexp(&sexp);
        match value {
          QuootValue::Symbol(name) => self.get_binding(name),
          QuootValue::List(_) => todo!(),
          _ => Ok(value),
        }
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
  interpreter.add_binding("x".to_owned(), QuootValue::Int(30));
  let mut input_buffer = String::new();
  let stdin = io::stdin();
  print_prompt();
  while stdin.read_line(&mut input_buffer).is_ok() {
    let trimmed_input = input_buffer.trim_start().trim_end();
    if trimmed_input.eq("#EXIT") {
      println!("\nQuoot repl stopped. bye!!\n");
      break;
    }
    match interpreter.eval(trimmed_input) {
      Err(e) => println!("{:?}", e),
      Ok(value) => {
        println!("{}", value.to_string())
      }
    }
    input_buffer.clear();
    print_prompt();
  }
}
