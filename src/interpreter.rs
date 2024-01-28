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
  Fn(&'static dyn Fn(Vec<&QuootValue>) -> Result<QuootValue, QuootEvalError>),
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
  pub fn type_string(&self) -> String {
    match self {
      QuootValue::Nil => "Nil",
      QuootValue::List(_) => "List",
      QuootValue::Int(_) => "Integer",
      QuootValue::Float(_) => "Float",
      QuootValue::String(_) => "String",
      QuootValue::Symbol(_) => "Symbol",
      QuootValue::Fn(_) => "Function",
    }
    .to_string()
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
      QuootValue::Fn(_) => fmt.write_str("<Function>")?,
    }
    Ok(())
  }
}

#[derive(Debug)]
pub enum QuootEvalError {
  Parse(QuootParseError),
  UnboundSymbolError(String),
  AppliedUnapplicableError,
  FunctionError(String),
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
  pub fn apply(
    &self,
    f: &'static dyn Fn(Vec<&QuootValue>) -> Result<QuootValue, QuootEvalError>,
    args: Vec<&QuootValue>,
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
        let evaluated_values = values
          .iter()
          .map(|v| self.eval(v.to_owned()))
          .into_iter()
          .collect::<Result<Vec<QuootValue>, QuootEvalError>>()?;
        match evaluated_values.first() {
          None => Ok(QuootValue::List(vec![])),
          Some(function) => match function {
            QuootValue::Fn(f) => {
              self.apply(f.clone(), evaluated_values[1..].iter().collect())
            }
            _ => todo!(),
          },
        }
      }
      other => Ok(other),
    }
  }
}

fn print_prompt() {
  print!("> ");
  io::stdout().flush().unwrap();
}

fn quoot_add(values: Vec<&QuootValue>) -> Result<QuootValue, QuootEvalError> {
  enum Summable {
    F(f64),
    I(i64),
  }
  let summables = values
    .iter()
    .map(|v| match v {
      QuootValue::Nil => Ok(Summable::I(0)),
      QuootValue::Int(i) => Ok(Summable::I(*i)),
      QuootValue::Float(f) => Ok(Summable::F(*f)),
      _ => Err(QuootEvalError::FunctionError(format!(
        "+: can't add type <{}>",
        v.type_string()
      ))),
    })
    .into_iter()
    .collect::<Result<Vec<Summable>, QuootEvalError>>()?;
  Ok(
    match summables.iter().fold(Summable::I(0), |a, b| match (a, b) {
      (Summable::I(a), Summable::I(b)) => Summable::I(a + b),
      (Summable::F(a), Summable::F(b)) => Summable::F(a + b),
      (Summable::I(i_a), Summable::F(f_b)) => Summable::F((i_a as f64) + f_b),
      (Summable::F(f_a), Summable::I(i_b)) => Summable::F(f_a + (*i_b as f64)),
    }) {
      Summable::F(f) => QuootValue::Float(f),
      Summable::I(i) => QuootValue::Int(i),
    },
  )
}

pub fn repl() {
  println!("\nQuoot repl started :D\n");
  let interpreter = &mut Interpreter::default();
  interpreter
    .add_binding("TAU".to_owned(), QuootValue::Float(6.283185307179586));
  interpreter.add_binding("+".to_owned(), QuootValue::Fn(&quoot_add));
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
