use crate::parse::{QuootParseError, Sexp};
use imbl::Vector;
use std::fmt;

pub type QuootValueList = Vector<QuootValue>;
pub type QuootFn =
  &'static dyn Fn(QuootValueList) -> Result<QuootValue, QuootEvalError>;

#[derive(Clone, PartialEq)]
pub enum Num {
  Int(i64),
  Float(f64),
}

impl Num {
  pub fn floor(&self) -> i64 {
    match self {
      Num::Int(i) => *i,
      Num::Float(f) => *f as i64,
    }
  }
}

#[derive(Clone)]
pub enum QuootValue {
  Nil,
  Bool(bool),
  List(QuootValueList),
  Num(Num),
  String(String),
  Symbol(String),
  Fn(QuootFn),
}

impl PartialEq for QuootValue {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::List(a), Self::List(b)) => a == b,
      (Self::Num(a), Self::Num(b)) => a == b,
      (Self::String(a), Self::String(b)) => a == b,
      (Self::Symbol(a), Self::Symbol(b)) => a == b,
      _ => false,
    }
  }
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
      Sexp::List(sub_sexps) => {
        let v = &mut QuootValueList::new();
        sub_sexps
          .iter()
          .for_each(|sub_sexp| v.push_back(QuootValue::from_sexp(sub_sexp)));
        QuootValue::List(v.to_owned())
      }
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
      QuootValue::Bool(_) => "Bool",
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
  pub fn as_num(&self, error_prefix: &str) -> Result<Num, QuootEvalError> {
    match self {
      QuootValue::Nil => Ok(Num::Int(0)),
      QuootValue::Num(num) => Ok(num.clone()),
      _ => {
        return Err(QuootEvalError::FunctionError(format!(
          "{}: can't get num from type {}",
          error_prefix,
          self.type_string()
        )))
      }
    }
  }
  pub fn as_list(
    &self,
    error_prefix: &str,
  ) -> Result<QuootValueList, QuootEvalError> {
    match self {
      QuootValue::Nil => Ok(QuootValueList::new()),
      QuootValue::List(list) => Ok(list.clone()),
      _ => {
        return Err(QuootEvalError::FunctionError(format!(
          "{}: can't get list from type {}",
          error_prefix,
          self.type_string()
        )))
      }
    }
  }
  pub fn as_fn(&self, error_prefix: &str) -> Result<QuootFn, QuootEvalError> {
    match self {
      QuootValue::Fn(f) => Ok(f.clone()),
      _ => {
        return Err(QuootEvalError::FunctionError(format!(
          "{}: can't use type {} as a function",
          error_prefix,
          self.type_string()
        )))
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
      QuootValue::Bool(b) => {
        fmt.write_str(if *b { "true" } else { "false" })?
      }
    }
    Ok(())
  }
}

pub fn compose(f: QuootFn, g: QuootFn) -> QuootFn {
  Box::leak(Box::new(move |args| f(QuootValueList::unit(g(args)?))))
}

pub fn partial(f: QuootFn, prefix_args: QuootValueList) -> QuootFn {
  Box::leak(Box::new(move |args| {
    let cloned_args = &mut prefix_args.clone();
    cloned_args.append(args);
    f(cloned_args.to_owned())
  }))
}

#[derive(Debug)]
pub enum QuootEvalError {
  Parse(QuootParseError),
  UnboundSymbolError(String),
  AppliedUnapplicableError,
  FunctionError(String),
}
