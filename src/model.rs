use crate::parse::{QuootParseError, Sexp};
use imbl::Vector;
use std::{collections::HashMap, fmt};

pub type Bindings = HashMap<String, QuootValue>;

#[derive(Debug)]
pub enum QuootEvalError {
  Parse(QuootParseError),
  UnboundSymbolError(String),
  AppliedUnapplicableError(String),
  FunctionError(String),
}

#[derive(Default, Clone)]
pub struct Env {
  bindings: Bindings,
}
impl Env {
  pub fn from_bindings(bindings: Bindings) -> Env {
    Env { bindings }
  }
  pub fn bind(&mut self, name: &str, value: QuootValue) {
    self.bindings.insert(name.to_owned(), value);
  }
  pub fn get(&self, name: &str) -> Option<&QuootValue> {
    self.bindings.get(name).map(|e| e)
  }
}

pub type QuootValueList = Vector<QuootValue>;
pub type QuootFn =
  &'static dyn Fn(QuootValueList) -> Result<QuootValue, QuootEvalError>;
pub type QuootRealizerFn =
  &'static dyn Fn(QuootValueList) -> Result<Option<QuootValue>, QuootEvalError>;

#[derive(Clone, Copy, PartialEq)]
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
  pub fn add(a: Num, b: &Num) -> Num {
    match (a, b) {
      (Num::Int(a), Num::Int(b)) => Num::Int(a + b),
      (Num::Float(a), Num::Float(b)) => Num::Float(a + b),
      (Num::Int(a), Num::Float(b)) => Num::Float((a as f64) + b),
      (Num::Float(a), Num::Int(b)) => Num::Float(a + (*b as f64)),
    }
  }
  pub fn multiply(a: Num, b: &Num) -> Num {
    match (a, b) {
      (Num::Int(a), Num::Int(b)) => Num::Int(a * b),
      (Num::Float(a), Num::Float(b)) => Num::Float(a * b),
      (Num::Int(a), Num::Float(b)) => Num::Float((a as f64) * b),
      (Num::Float(a), Num::Int(b)) => Num::Float(a * (*b as f64)),
    }
  }
  pub fn min(a: Num, b: &Num) -> Num {
    match (a, b) {
      (Num::Int(a), Num::Int(b)) => Num::Int(a.min(*b)),
      (Num::Float(a), Num::Float(b)) => Num::Float(a.min(*b)),
      (Num::Int(a), Num::Float(b)) => {
        let b_derefed = *b;
        if (a as f64) <= b_derefed {
          Num::Int(a)
        } else {
          Num::Float(b_derefed)
        }
      }
      (Num::Float(a), Num::Int(b)) => {
        let b_derefed = *b;
        if (b_derefed as f64) <= a {
          Num::Int(b_derefed)
        } else {
          Num::Float(a)
        }
      }
    }
  }
  pub fn max(a: Num, b: &Num) -> Num {
    match (a, b) {
      (Num::Int(a), Num::Int(b)) => Num::Int(a.max(*b)),
      (Num::Float(a), Num::Float(b)) => Num::Float(a.max(*b)),
      (Num::Int(a), Num::Float(b)) => {
        let b_derefed = *b;
        if (a as f64) >= b_derefed {
          Num::Int(a)
        } else {
          Num::Float(b_derefed)
        }
      }
      (Num::Float(a), Num::Int(b)) => {
        let b_derefed = *b;
        if (b_derefed as f64) >= a {
          Num::Int(b_derefed)
        } else {
          Num::Float(a)
        }
      }
    }
  }
  pub fn numerical_equal(a: Num, b: &Num) -> bool {
    match (a, b) {
      (Num::Int(a), Num::Int(b)) => a == *b,
      (Num::Float(a), Num::Float(b)) => a == *b,
      (Num::Int(a), Num::Float(b)) => (a as f64) == *b,
      (Num::Float(a), Num::Int(b)) => {
        println!("{},{},{}", a, (*b as f64), a == (*b as f64));
        a == (*b as f64)
      }
    }
  }
}

#[derive(Clone)]
pub struct LazyQuootValueList {
  values: QuootValueList,
  fully_realized: bool,
  realizer: &'static dyn Fn(
    QuootValueList,
  ) -> Result<Option<QuootValue>, QuootEvalError>,
}

impl LazyQuootValueList {
  pub fn new(realizer: QuootRealizerFn) -> LazyQuootValueList {
    LazyQuootValueList {
      values: QuootValueList::new(),
      fully_realized: false,
      realizer,
    }
  }
  pub fn realized_len(&self) -> usize {
    self.values.len()
  }
  pub fn fully_realized(&self) -> bool {
    self.fully_realized
  }
  fn realize(
    &mut self,
  ) -> Result<Option<&mut LazyQuootValueList>, QuootEvalError> {
    if self.fully_realized {
      return Ok(None);
    }
    let new_value = (self.realizer)(self.values.clone())?;
    match new_value {
      None => {
        self.fully_realized = true;
        Ok(None)
      }
      Some(value) => {
        self.values.push_back(value);
        Ok(Some(self))
      }
    }
  }
  fn realize_to(
    &mut self,
    length: usize,
  ) -> Result<Option<&mut LazyQuootValueList>, QuootEvalError> {
    while self.values.len() < length {
      if self.realize()?.is_none() {
        return Ok(None);
      }
    }
    Ok(Some(self))
  }
  pub fn fully_realize(
    &mut self,
  ) -> Result<&mut LazyQuootValueList, QuootEvalError> {
    loop {
      if self.realize()?.is_none() {
        break;
      };
    }
    Ok(self)
  }
  pub fn get(
    &mut self,
    index: usize,
  ) -> Result<Option<QuootValue>, QuootEvalError> {
    self.realize_to(index + 1)?;
    Ok(self.values.get(index).map(|a| a.to_owned()))
  }
}

#[derive(Clone)]
pub enum QuootValue {
  Nil,
  Bool(bool),
  Num(Num),
  String(String),
  Symbol(String),
  Fn(QuootFn),
  List(QuootValueList),
  LazyList(LazyQuootValueList),
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
    if token == "nil" {
      return QuootValue::Nil;
    }
    if token == "true" {
      return QuootValue::Bool(true);
    }
    if token == "false" {
      return QuootValue::Bool(false);
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
      Sexp::Leaf(token) => QuootValue::from_token(token),
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
      QuootValue::LazyList(_) => "LazyList",
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
  pub fn as_bool(&self) -> bool {
    match self {
      QuootValue::Nil => false,
      QuootValue::Bool(b) => *b,
      _ => true,
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
      QuootValue::Bool(b) => {
        fmt.write_str(if *b { "true" } else { "false" })?
      }
      QuootValue::Fn(_) => fmt.write_str("<Function>")?,
      QuootValue::LazyList(_) => fmt.write_str("<LazyList>")?,
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
