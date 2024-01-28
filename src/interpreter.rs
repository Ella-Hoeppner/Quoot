use crate::parse::parse;
use crate::parse::QuootParseError;
use crate::parse::Sexp;
use rpds::List;
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::io::Write;

#[derive(Clone)]
pub enum Num {
  Int(i64),
  Float(f64),
}

#[derive(Clone)]
pub enum QuootValue {
  Nil,
  List(List<QuootValue>),
  Num(Num),
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
      Ok(int) => return QuootValue::Num(Num::Int(int)),
      Err(_) => match token.parse::<f64>() {
        Ok(float) => return QuootValue::Num(Num::Float(float)),
        Err(_) => (),
      },
    }
    QuootValue::Symbol(token.clone())
  }
  pub fn from_sexp(sexp: &Sexp) -> QuootValue {
    match sexp {
      Sexp::List(sub_sexps) => QuootValue::List(
        sub_sexps.iter().rev().fold(List::new(), |list, sub_sexp| {
          list.push_front(QuootValue::from_sexp(sub_sexp))
        }),
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
      QuootValue::Num(num) => match num {
        Num::Int(_) => "Integer",
        Num::Float(_) => "Float",
      },
      QuootValue::String(_) => "String",
      QuootValue::Symbol(_) => "Symbol",
      QuootValue::List(_) => "List",
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
      QuootValue::Num(num) => match num {
        Num::Int(i) => fmt.write_str(&i.to_string()),
        Num::Float(f) => fmt.write_str(&format!(
          "{}{}",
          f,
          if f.fract() == 0.0 { "." } else { "" }
        )),
      }?,
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
          None => Ok(QuootValue::List(List::new())),
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
  let nums = values
    .iter()
    .map(|v| match v {
      QuootValue::Nil => Ok(Num::Int(0)),
      QuootValue::Num(num) => Ok(num.clone()),
      _ => Err(QuootEvalError::FunctionError(format!(
        "+: can't add type <{}>",
        v.type_string()
      ))),
    })
    .into_iter()
    .collect::<Result<Vec<Num>, QuootEvalError>>()?;
  Ok(QuootValue::Num(nums.iter().fold(
    Num::Int(0),
    |a, b| match (a, b) {
      (Num::Int(a), Num::Int(b)) => Num::Int(a + b),
      (Num::Float(a), Num::Float(b)) => Num::Float(a + b),
      (Num::Int(i_a), Num::Float(f_b)) => Num::Float((i_a as f64) + f_b),
      (Num::Float(f_a), Num::Int(i_b)) => Num::Float(f_a + (*i_b as f64)),
    },
  )))
}

fn quoot_multiply(
  values: Vec<&QuootValue>,
) -> Result<QuootValue, QuootEvalError> {
  let nums = values
    .iter()
    .map(|v| match v {
      QuootValue::Nil => Ok(Num::Int(0)),
      QuootValue::Num(num) => Ok(num.clone()),
      _ => Err(QuootEvalError::FunctionError(format!(
        "*: can't multiply type <{}>",
        v.type_string()
      ))),
    })
    .into_iter()
    .collect::<Result<Vec<Num>, QuootEvalError>>()?;
  Ok(QuootValue::Num(nums.iter().fold(
    Num::Int(1),
    |a, b| match (a, b) {
      (Num::Int(a), Num::Int(b)) => Num::Int(a * b),
      (Num::Float(a), Num::Float(b)) => Num::Float(a * b),
      (Num::Int(i_a), Num::Float(f_b)) => Num::Float((i_a as f64) * f_b),
      (Num::Float(f_a), Num::Int(i_b)) => Num::Float(f_a * (*i_b as f64)),
    },
  )))
}

pub fn repl() {
  println!("\nQuoot repl started :D\n");
  let interpreter = &mut Interpreter::default();
  interpreter.add_binding(
    "TAU".to_owned(),
    QuootValue::Num(Num::Float(6.283185307179586)),
  );
  interpreter.add_binding("+".to_owned(), QuootValue::Fn(&quoot_add));
  interpreter.add_binding("*".to_owned(), QuootValue::Fn(&quoot_multiply));
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
