use crate::parse::{ParseError, Sexp};
use imbl::{HashMap, Vector};
use std::{
  fmt,
  rc::Rc,
  sync::{Arc, RwLock},
};

#[derive(Clone)]
pub enum Value {
  Nil,
  Bool(bool),
  Num(Num),
  String(String),
  Symbol(String),
  List(List),
  Op(Op),
}

#[derive(Debug, PartialEq)]
pub enum EvalError {
  Parse(ParseError),
  UnboundSymbolError(String),
  OpError(String),
  OutOfBoundsError(String, i64, i64),
  DefineError(String),
}

#[derive(Clone)]
pub enum List {
  Strict(StrictList),
  Lazy(LazyList),
}

pub type StrictList = Vector<Value>;

#[derive(Clone)]
pub enum Op {
  Core(CoreOp),
  User(UserOp),
  Composition(Vec<Op>),
  Partial(Rc<Op>, StrictList),
  Applied(Rc<Op>),
  ListAccess(List),
}

impl Op {
  pub fn apply(
    &self,
    env: &Env,
    mut args: StrictList,
    eval_args: bool,
  ) -> Result<Value, EvalError> {
    match self {
      Op::Core(op) => (op.f)(env, args, eval_args),
      Op::ListAccess(list) => {
        if args.len() == 1 {
          let index = maybe_eval(env, args.pop_front().unwrap(), eval_args)?
            .as_num("<List>")?
            .floor();
          match list.get(index)? {
            Some(value) => Ok(value),
            None => Err(EvalError::OutOfBoundsError(
              "<List application>".to_string(),
              index,
              list.clone().to_strict()?.len() as i64,
            )),
          }
        } else {
          Err(EvalError::OpError(format!(
            "<List>: need 1 argument, got {}",
            args.len(),
          )))
        }
      }
      Op::User(op) => {
        if args.len() != op.arg_names.len() {
          return Err(EvalError::OpError(format!(
            "User Op{} needs {} arguments, got {}",
            if let Some(name) = &op.name {
              format!(" {}", name)
            } else {
              "".to_string()
            },
            op.arg_names.len(),
            args.len()
          )));
        }
        let maybe_evaled_args =
          maybe_eval_all(env, args, eval_args && op.evals_args)?;
        let mut body_env = op.env.clone();
        for (arg_name, arg_value) in
          op.arg_names.iter().zip(maybe_evaled_args.into_iter())
        {
          body_env.bind(arg_name, arg_value);
        }
        if let Some(name) = &op.name {
          body_env.bind(&name, Value::Op(Op::User(op.clone())))
        }
        let mut body_values = op
          .body
          .iter()
          .map(|value| eval(&body_env, value.clone()))
          .collect::<Result<Vec<Value>, EvalError>>()?;
        Ok(body_values.pop().unwrap())
      }
      Op::Composition(ops) => {
        let op = &ops[0];
        let mut value =
          op.apply(env, maybe_eval_all(env, args, eval_args)?, false)?;
        for op in &ops[1..] {
          value = op.apply(env, StrictList::unit(value), false)?;
        }
        Ok(value)
      }
      Op::Partial(op, prefix_args) => {
        let mut full_args = prefix_args.clone();
        full_args.append(args);
        op.apply(env, full_args, eval_args)
      }
      Op::Applied(op) => {
        if args.len() == 1 {
          op.apply(
            env,
            maybe_eval(env, args.pop_front().unwrap(), eval_args)?
              .to_list("apply")?
              .to_strict()?,
            eval_args,
          )
        } else {
          Err(EvalError::OpError(format!(
            "<Applied Op>: op constructed with 1-argument apply call needs 1 \
            argument, got {}",
            args.len(),
          )))
        }
      }
    }
  }
}

#[derive(Clone)]
pub struct UserOp {
  name: Option<String>,
  env: Env,
  arg_names: Vec<String>,
  body: StrictList,
  evals_args: bool,
}

impl UserOp {
  pub fn new(
    name: Option<String>,
    env: Env,
    arg_names: Vec<String>,
    body: StrictList,
    evals_args: bool,
  ) -> Self {
    Self {
      name,
      env,
      arg_names,
      body,
      evals_args,
    }
  }
}

#[derive(Clone)]
pub struct CoreOp {
  pub f: &'static fn(&Env, StrictList, bool) -> Result<Value, EvalError>,
}
impl CoreOp {
  pub fn new(
    f: &'static fn(&Env, StrictList, bool) -> Result<Value, EvalError>,
  ) -> Self {
    Self { f }
  }
}

pub type Bindings = HashMap<String, Value>;

#[derive(Default, Clone)]
pub struct Env {
  bindings: Bindings,
}

#[derive(Clone, Copy, PartialEq)]
pub enum Num {
  Int(i64),
  Float(f64),
}

impl List {
  pub fn to_strict(self) -> Result<StrictList, EvalError> {
    match self {
      List::Strict(list) => Ok(list),
      List::Lazy(list) => list.to_strict(),
    }
  }
  pub fn as_strict(&self) -> Result<StrictList, EvalError> {
    match self {
      List::Strict(list) => Ok(list.clone()),
      List::Lazy(list) => Ok(list.as_strict()?),
    }
  }
  pub fn is_empty(&self) -> Result<bool, EvalError> {
    Ok(match self {
      List::Strict(list) => list.len() == 0,
      List::Lazy(list) => {
        if list.realized_len() > 0 {
          true
        } else {
          !list.realize()?
        }
      }
    })
  }
  pub fn get(&self, n: i64) -> Result<Option<Value>, EvalError> {
    match self {
      List::Strict(strict_list) => {
        Ok(strict_list.get(n as usize).map(|e| e.clone()))
      }
      List::Lazy(lazy_list) => lazy_list.get(n as usize),
    }
  }
  pub fn rest(&self) -> Result<List, EvalError> {
    match self {
      List::Strict(strict_list) => {
        let mut list_clone = strict_list.clone();
        list_clone.pop_front();
        Ok(List::Strict(list_clone))
      }
      List::Lazy(lazy_list) => lazy_list.rest(),
    }
  }
  pub fn deliteralize(mut list: StrictList) -> StrictList {
    match list.get(0) {
      Some(value) => match value {
        Value::Symbol(name) => {
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

impl PartialEq for List {
  fn eq(&self, other: &Self) -> bool {
    self.as_strict() == other.as_strict()
  }
}

impl PartialEq for Value {
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

impl From<StrictList> for Value {
  fn from(value: StrictList) -> Self {
    Value::List(List::Strict(value))
  }
}

impl From<LazyList> for Value {
  fn from(value: LazyList) -> Self {
    Value::List(List::Lazy(value))
  }
}

impl From<i64> for Value {
  fn from(value: i64) -> Self {
    Value::Num(Num::Int(value))
  }
}

impl From<f64> for Value {
  fn from(value: f64) -> Self {
    Value::Num(Num::Float(value))
  }
}

impl From<usize> for Value {
  fn from(value: usize) -> Self {
    Value::from(value as i64)
  }
}

impl Value {
  pub fn from_token(token: &String) -> Value {
    let chars: Vec<char> = token.chars().collect();
    if chars[0] == '"' {
      return Value::String(chars[1..chars.len() - 1].iter().collect());
    }
    if token == "nil" {
      return Value::Nil;
    }
    if token == "true" {
      return Value::Bool(true);
    }
    if token == "false" {
      return Value::Bool(false);
    }
    match token.parse::<i64>() {
      Ok(int) => return Value::Num(Num::Int(int)),
      Err(_) => match token.parse::<f64>() {
        Ok(float) => return Value::Num(Num::Float(float)),
        Err(_) => (),
      },
    }
    Value::Symbol(token.to_string())
  }
  pub fn from_sexp(sexp: &Sexp) -> Value {
    match sexp {
      Sexp::List(sub_sexps) => {
        let mut v = StrictList::new();
        sub_sexps
          .iter()
          .for_each(|sub_sexp| v.push_back(Value::from_sexp(sub_sexp)));
        Value::List(List::Strict(v))
      }
      Sexp::Leaf(token) => Value::from_token(token),
    }
  }
  pub fn type_string(&self) -> String {
    match self {
      Value::Nil => "Nil",
      Value::Bool(_) => "Bool",
      Value::Num(num) => match num {
        Num::Int(_) => "Integer",
        Num::Float(_) => "Float",
      },
      Value::String(_) => "String",
      Value::Symbol(_) => "Symbol",
      Value::List(list) => match list {
        List::Strict(_) => "List",
        List::Lazy(_) => "LazyList",
      },
      Value::Op(_) => "Operator",
    }
    .to_string()
  }
  pub fn as_num(&self, error_prefix: &str) -> Result<Num, EvalError> {
    match self {
      Value::Nil => Ok(Num::Int(0)),
      Value::Num(num) => Ok(num.clone()),
      _ => {
        return Err(EvalError::OpError(format!(
          "{}: can't get num from type {}",
          error_prefix,
          self.type_string()
        )))
      }
    }
  }
  pub fn to_list(self, error_prefix: &str) -> Result<List, EvalError> {
    match self {
      Value::Nil => Ok(List::Strict(StrictList::new())),
      Value::List(list) => Ok(list),
      _ => Err(EvalError::OpError(format!(
        "{}: can't get list from type {}",
        error_prefix,
        self.type_string()
      ))),
    }
  }
  pub fn to_op(self, error_prefix: &str) -> Result<Op, EvalError> {
    match self {
      Value::Op(op) => Ok(op.clone()),
      Value::List(list) => Ok(Op::ListAccess(list)),
      _ => Err(EvalError::OpError(format!(
        "{}: can't use type {} as a function",
        error_prefix,
        self.type_string()
      ))),
    }
  }
  pub fn as_bool(&self) -> bool {
    match self {
      Value::Nil => false,
      Value::Bool(b) => *b,
      _ => true,
    }
  }
}

impl fmt::Display for Value {
  fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      Value::Symbol(token) => fmt.write_str(token)?,
      Value::String(token) => {
        fmt.write_str("\"")?;
        fmt.write_str(token)?;
        fmt.write_str("\"")?;
      }
      Value::Num(num) => match num {
        Num::Int(i) => fmt.write_str(&i.to_string()),
        Num::Float(f) => {
          if f.is_nan() {
            fmt.write_str("#nan")
          } else if f.is_finite() {
            fmt.write_str(&format!(
              "{}{}",
              f,
              if f.fract() == 0.0 { "." } else { "" }
            ))
          } else {
            fmt.write_str(if f.is_sign_positive() {
              "#inf"
            } else {
              "#-inf"
            })
          }
        }
      }?,
      Value::List(list) => match list.as_strict() {
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
      Value::Nil => fmt.write_str("nil")?,
      Value::Bool(b) => fmt.write_str(if *b { "true" } else { "false" })?,
      Value::Op(_) => fmt.write_str("<Operator>")?,
    }
    Ok(())
  }
}

impl Env {
  pub fn from_bindings(bindings: Bindings) -> Env {
    Env { bindings }
  }
  pub fn bind(&mut self, name: &str, value: Value) {
    self.bindings.insert(name.to_string(), value);
  }
  pub fn bind_all(&mut self, bindings: Bindings) {
    self.bindings = bindings.union(std::mem::take(&mut self.bindings));
  }
  pub fn get(&self, name: &str) -> Result<Value, EvalError> {
    self
      .bindings
      .get(name)
      .map(|v| v.clone())
      .ok_or(EvalError::UnboundSymbolError(name.to_string()))
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
  pub fn as_float(&self) -> f64 {
    match self {
      Num::Int(i) => *i as f64,
      Num::Float(f) => *f,
    }
  }
}

impl PartialOrd for Num {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    match (self, other) {
      (Num::Int(a), Num::Int(b)) => a.partial_cmp(b),
      (Num::Float(a), Num::Float(b)) => a.partial_cmp(b),
      (Num::Int(a), Num::Float(b)) => (*a as f64).partial_cmp(b),
      (Num::Float(a), Num::Int(b)) => a.partial_cmp(&(*b as f64)),
    }
  }
}

#[derive(Clone)]
pub struct LazyState {
  pub realized_values: StrictList,
  pub builder_values: Option<List>,
  pub captured_environment: Option<Env>,
  pub is_finished: bool,
}
impl LazyState {
  pub fn new(
    prerealized_values: StrictList,
    initial_builder_values: Option<List>,
    env: Option<Env>,
  ) -> LazyState {
    LazyState {
      realized_values: prerealized_values,
      builder_values: initial_builder_values,
      captured_environment: env,
      is_finished: false,
    }
  }
}
pub type LazyRealizer =
  &'static dyn Fn(&mut LazyState) -> Result<(), EvalError>;
#[derive(Clone)]
pub struct LazyList {
  pub state: Arc<RwLock<LazyState>>,
  realizer: LazyRealizer,
}

impl LazyList {
  pub fn new(realizer: LazyRealizer, state: LazyState) -> LazyList {
    LazyList {
      state: Arc::new(RwLock::new(state)),
      realizer,
    }
  }
  pub fn realized_len(&self) -> usize {
    (*(*self.state).read().unwrap()).realized_values.len()
  }
  fn realize(&self) -> Result<bool, EvalError> {
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
  ) -> Result<Option<&LazyList>, EvalError> {
    while (*self.state.read().unwrap()).realized_values.len() < length {
      if self.realize()? {
        return Ok(None);
      }
    }
    Ok(Some(self))
  }
  pub fn fully_realize(&self) -> Result<&LazyList, EvalError> {
    while !self.realize()? {}
    (*self.state).write().unwrap().is_finished = true;
    Ok(self)
  }
  pub fn get(&self, index: usize) -> Result<Option<Value>, EvalError> {
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
  pub fn rest(&self) -> Result<List, EvalError> {
    let state = (*self.state).read().unwrap();
    let mut cloned_values = state.realized_values.clone();
    cloned_values.pop_front();
    if state.is_finished {
      Ok(List::Strict(cloned_values))
    } else {
      Ok(List::Lazy(LazyList::new(
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
        LazyState::new(cloned_values, Some(List::Lazy(self.clone())), None),
      )))
    }
  }
  pub fn to_strict(self) -> Result<StrictList, EvalError> {
    self.fully_realize()?;
    Ok(std::mem::take(
      &mut self.state.write().unwrap().realized_values,
    ))
  }
  pub fn as_strict(&self) -> Result<StrictList, EvalError> {
    self.fully_realize()?;
    Ok(self.state.read().unwrap().realized_values.clone())
  }
}

pub fn eval(env: &Env, value: Value) -> Result<Value, EvalError> {
  match value {
    Value::Symbol(name) => env.get(&name),
    Value::List(list) => {
      let mut values = list.to_strict()?;
      match values.pop_front() {
        None => Ok(Value::List(List::Strict(StrictList::new()))),
        Some(first_value) => {
          let op = eval(env, first_value)?.to_op("eval")?;
          op.apply(env, values, true)
        }
      }
    }
    other => Ok(other),
  }
}

pub fn top_level_eval(
  env: &Env,
  mut value: Value,
) -> Result<(Value, Option<Bindings>), EvalError> {
  let mut binding_name: Option<String> = None;
  if let Value::List(List::Strict(list)) = &value {
    if let Some(Value::Symbol(name)) = list.get(0) {
      if name == "def" {
        if list.len() == 3 {
          let mut value_iter = list.clone().into_iter().skip(1);
          let binding_name_value: Value = value_iter.next().unwrap();
          if let Value::Symbol(def_binding_name) = binding_name_value {
            binding_name = Some(def_binding_name.to_string());
            value = value_iter.next().unwrap();
          } else {
            return Err(EvalError::DefineError(format!(
              "def: second argument must be a symbol, got {}",
              binding_name_value.type_string()
            )));
          }
        } else {
          return Err(EvalError::DefineError(format!(
            "def: need 2 arguments, got {}",
            list.len() - 1
          )));
        }
      }
    }
  }
  let evaled_value = eval(env, value)?;
  let binding = binding_name.map(|name| {
    let mut bindings = Bindings::new();
    bindings.insert(name, evaled_value.clone());
    bindings
  });
  Ok((evaled_value, binding))
}

pub fn maybe_eval(
  env: &Env,
  value: Value,
  should_eval: bool,
) -> Result<Value, EvalError> {
  if should_eval {
    eval(env, value)
  } else {
    Ok(value)
  }
}

pub fn maybe_eval_all(
  env: &Env,
  values: StrictList,
  eval_args: bool,
) -> Result<StrictList, EvalError> {
  if eval_args {
    Ok(StrictList::from(
      values
        .into_iter()
        .map(|value| eval(env, value))
        .collect::<Result<Vec<Value>, EvalError>>()?,
    ))
  } else {
    Ok(values)
  }
}
