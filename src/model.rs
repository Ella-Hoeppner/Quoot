use crate::parse::{QuootParseError, Sexp};
use imbl::{HashMap, Vector};
use std::{
  fmt,
  sync::{Arc, RwLock},
};

#[derive(Clone)]
pub enum QuootValue {
  Nil,
  Bool(bool),
  Num(Num),
  String(String),
  Symbol(String),
  List(QuootList),
  Op(QuootOp),
}

#[derive(Debug, PartialEq)]
pub enum QuootEvalError {
  Parse(QuootParseError),
  UnboundSymbolError(String),
  //AppliedUnapplicableError(String),
  OperatorError(String),
  OutOfBoundsError(i64, i64),
}

#[derive(Clone)]
pub enum QuootList {
  Strict(QuootStrictList),
  Lazy(QuootLazyList),
}

pub type QuootStrictList = Vector<QuootValue>;
pub type QuootOp = &'static dyn Fn(
  &Env,
  &QuootStrictList,
  bool,
) -> Result<QuootValue, QuootEvalError>;

pub type Bindings = HashMap<String, QuootValue>;

#[derive(Default, Clone)]
pub struct Env {
  bindings: Bindings,
}

#[derive(Clone, Copy, PartialEq)]
pub enum Num {
  Int(i64),
  Float(f64),
}

impl QuootList {
  pub fn as_strict(&self) -> Result<QuootStrictList, QuootEvalError> {
    match self {
      QuootList::Strict(list) => Ok(list.clone()),
      QuootList::Lazy(list) => Ok(list.as_strict()?),
    }
  }
  pub fn is_empty(&self) -> Result<bool, QuootEvalError> {
    Ok(match self {
      QuootList::Strict(list) => list.len() == 0,
      QuootList::Lazy(list) => {
        if list.realized_len() > 0 {
          true
        } else {
          !list.clone().realize()?
        }
      }
    })
  }
  pub fn get(&self, n: i64) -> Result<Option<QuootValue>, QuootEvalError> {
    match self {
      QuootList::Strict(strict_list) => {
        Ok(strict_list.get(n as usize).map(|e| e.clone()))
      }
      QuootList::Lazy(lazy_list) => lazy_list.get(n as usize),
    }
  }
  pub fn rest(&self) -> Result<QuootList, QuootEvalError> {
    match self {
      QuootList::Strict(strict_list) => {
        let list_clone = &mut strict_list.clone();
        list_clone.pop_front();
        Ok(QuootList::Strict(list_clone.to_owned()))
      }
      QuootList::Lazy(lazy_list) => lazy_list.rest(),
    }
  }
  pub fn deliteralize(mut list: QuootStrictList) -> QuootStrictList {
    match list.get(0) {
      Some(value) => match value {
        QuootValue::Symbol(name) => {
          if name == "#list" {
            list.pop_front();
            list
          } else {
            list
          }
        }
        _ => list,
      },
      None => list,
    }
  }
}

impl PartialEq for QuootList {
  fn eq(&self, other: &Self) -> bool {
    self.as_strict() == other.as_strict()
  }
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
        let v = &mut QuootStrictList::new();
        sub_sexps
          .iter()
          .for_each(|sub_sexp| v.push_back(QuootValue::from_sexp(sub_sexp)));
        QuootValue::List(QuootList::Strict(v.to_owned()))
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
      QuootValue::List(list) => match list {
        QuootList::Strict(_) => "List",
        QuootList::Lazy(_) => "LazyList",
      },
      QuootValue::Op(_) => "Operator",
    }
    .to_string()
  }
  pub fn as_num(&self, error_prefix: &str) -> Result<Num, QuootEvalError> {
    match self {
      QuootValue::Nil => Ok(Num::Int(0)),
      QuootValue::Num(num) => Ok(num.clone()),
      _ => {
        return Err(QuootEvalError::OperatorError(format!(
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
  ) -> Result<QuootList, QuootEvalError> {
    match self {
      QuootValue::Nil => Ok(QuootList::Strict(QuootStrictList::new())),
      QuootValue::List(list) => Ok(list.clone()),
      _ => {
        return Err(QuootEvalError::OperatorError(format!(
          "{}: can't get list from type {}",
          error_prefix,
          self.type_string()
        )))
      }
    }
  }
  pub fn as_op(&self, error_prefix: &str) -> Result<QuootOp, QuootEvalError> {
    match self {
      QuootValue::Op(f) => Ok(*f),
      QuootValue::List(list) => {
        let cloned_list = list.clone();
        Ok(Box::leak(Box::new(
          move |_env: &Env, args: &QuootStrictList, _eval_args| {
            if args.len() == 1 {
              let index = args.front().unwrap().as_num("<List>")?.floor();
              match cloned_list.get(index)? {
                Some(value) => Ok(value),
                None => Err(QuootEvalError::OutOfBoundsError(
                  index,
                  cloned_list.as_strict()?.len() as i64,
                )),
              }
            } else {
              Err(QuootEvalError::OperatorError(format!(
                "<List>: need 1 argument, got {}",
                args.len(),
              )))
            }
          },
        )))
      }
      _ => {
        return Err(QuootEvalError::OperatorError(format!(
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
      QuootValue::List(list) => match list.as_strict() {
        Ok(values) => {
          fmt.write_str("(")?;
          let mut separator = "";
          for value in values {
            fmt.write_str(separator)?;
            fmt.write_str(&value.to_string())?;
            separator = " ";
          }
          fmt.write_str(")")?;
        }
        Err(_) => return Err(fmt::Error),
      },
      QuootValue::Nil => fmt.write_str("nil")?,
      QuootValue::Bool(b) => {
        fmt.write_str(if *b { "true" } else { "false" })?
      }
      QuootValue::Op(_) => fmt.write_str("<Operator>")?,
    }
    Ok(())
  }
}

impl Env {
  pub fn from_bindings(bindings: Bindings) -> Env {
    Env { bindings }
  }
  pub fn bind(&mut self, name: &str, value: QuootValue) {
    self.bindings.insert(name.to_owned(), value);
  }
  pub fn get(&self, name: &str) -> Result<&QuootValue, QuootEvalError> {
    self
      .bindings
      .get(name)
      .ok_or(QuootEvalError::UnboundSymbolError(name.to_owned()))
  }
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
pub struct QuootLazyState {
  pub realized_values: QuootStrictList,
  pub builder_values: Option<QuootList>,
  pub captured_environment: Option<Env>,
  pub is_finished: bool,
}
impl QuootLazyState {
  pub fn new(
    prerealized_values: QuootStrictList,
    initial_builder_values: Option<QuootList>,
    env: Option<Env>,
  ) -> QuootLazyState {
    QuootLazyState {
      realized_values: prerealized_values,
      builder_values: initial_builder_values,
      captured_environment: env,
      is_finished: false,
    }
  }
}
pub type QuootLazyRealizer =
  &'static dyn Fn(&mut QuootLazyState) -> Result<(), QuootEvalError>;
#[derive(Clone)]
pub struct QuootLazyList {
  pub state: Arc<RwLock<QuootLazyState>>,
  realizer: QuootLazyRealizer,
}

impl QuootLazyList {
  pub fn new(
    realizer: QuootLazyRealizer,
    state: QuootLazyState,
  ) -> QuootLazyList {
    QuootLazyList {
      state: Arc::new(RwLock::new(state)),
      realizer,
    }
  }
  pub fn realized_len(&self) -> usize {
    (*(*self.state).read().unwrap()).realized_values.len()
  }
  fn realize(&self) -> Result<bool, QuootEvalError> {
    let state = &mut (*self.state).write().unwrap();
    if !state.is_finished {
      (self.realizer)(state)?
    }
    Ok(state.is_finished)
  }
  pub fn is_fully_realized(&self) -> bool {
    self.state.read().unwrap().is_finished
  }
  pub fn realize_to(
    &self,
    length: usize,
  ) -> Result<Option<&QuootLazyList>, QuootEvalError> {
    while (*self.state.read().unwrap()).realized_values.len() < length {
      if self.realize()? {
        return Ok(None);
      }
    }
    Ok(Some(self))
  }
  pub fn fully_realize(&self) -> Result<&QuootLazyList, QuootEvalError> {
    while !self.realize()? {}
    let state = &mut (*self.state).write().unwrap();
    state.is_finished = true;
    Ok(self)
  }
  pub fn get(
    &self,
    index: usize,
  ) -> Result<Option<QuootValue>, QuootEvalError> {
    self.realize_to(index + 1)?;
    Ok(
      self
        .state
        .read()
        .unwrap()
        .realized_values
        .get(index)
        .map(|e| e.clone()),
    )
  }
  pub fn rest(&self) -> Result<QuootList, QuootEvalError> {
    let state = (*self.state).read().unwrap();
    let cloned_values = &mut state.realized_values.clone();
    cloned_values.pop_front();
    if state.is_finished {
      Ok(QuootList::Strict(cloned_values.clone()))
    } else {
      Ok(QuootList::Lazy(QuootLazyList::new(
        &|state| match &state.builder_values {
          Some(builder_state) => {
            match builder_state.get(state.realized_values.len() as i64 + 1)? {
              Some(new_value) => state.realized_values.push_back(new_value),
              None => state.is_finished = true,
            }
            Ok(())
          }
          None => unreachable!(),
        },
        QuootLazyState::new(
          cloned_values.clone(),
          Some(QuootList::Lazy(self.clone())),
          None,
        ),
      )))
    }
  }
  pub fn as_strict(&self) -> Result<QuootStrictList, QuootEvalError> {
    self.fully_realize()?;
    Ok(self.state.read().unwrap().realized_values.clone())
  }
}

pub fn eval(
  env: &Env,
  value: &QuootValue,
) -> Result<QuootValue, QuootEvalError> {
  match value {
    QuootValue::Symbol(name) => env.get(&name).map(|v| v.clone()),
    QuootValue::List(list) => {
      let values = list.as_strict()?;
      match values.front() {
        None => Ok(QuootValue::List(QuootList::Strict(QuootStrictList::new()))),
        Some(first_value) => {
          let f = eval(env, first_value)?.as_op("eval")?;
          let cloned_values = &mut values.clone();
          cloned_values.pop_front();
          f(env, cloned_values, true)
        }
      }
    }
    other => Ok(other.to_owned()),
  }
}

pub fn maybe_eval(
  env: &Env,
  value: &QuootValue,
  should_eval: bool,
) -> Result<QuootValue, QuootEvalError> {
  if should_eval {
    eval(env, value)
  } else {
    Ok(value.to_owned())
  }
}
