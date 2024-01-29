use crate::parse::parse;
use crate::parse::QuootParseError;
use crate::parse::Sexp;
use rpds::List;
use std::cmp::min;
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::io::Write;

#[derive(Clone)]
pub enum Num {
  Int(i64),
  Float(f64),
}

impl Num {
  fn floor(&self) -> i64 {
    match self {
      Num::Int(i) => *i,
      Num::Float(f) => *f as i64,
    }
  }
}

type QuootFn =
  &'static dyn Fn(List<QuootValue>) -> Result<QuootValue, QuootEvalError>;

#[derive(Clone)]
pub enum QuootValue {
  Nil,
  List(List<QuootValue>),
  Num(Num),
  String(String),
  Symbol(String),
  Fn(QuootFn),
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
  ) -> Result<List<QuootValue>, QuootEvalError> {
    match self {
      QuootValue::Nil => Ok(List::new()),
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
    }
    Ok(())
  }
}

pub fn compose(f: QuootFn, g: QuootFn) -> QuootFn {
  Box::leak(Box::new(move |args| f(List::new().push_front(g(args)?))))
}

pub fn partial(f: QuootFn, prefix_args: List<QuootValue>) -> QuootFn {
  let reversed_args = prefix_args.reverse();
  Box::leak(Box::new(move |args| {
    f(reversed_args
      .iter()
      .fold(args, |expanded_args, prefix_arg| {
        expanded_args.push_front(prefix_arg.to_owned())
      }))
  }))
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
    f: QuootFn,
    args: List<QuootValue>,
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
        let evaluated_values: List<QuootValue> = values
          .iter()
          .map(|v| self.eval(v.to_owned()))
          .into_iter()
          .collect::<Result<List<QuootValue>, QuootEvalError>>()?;
        match evaluated_values.first() {
          None => Ok(QuootValue::List(List::new())),
          Some(function) => match function {
            QuootValue::Fn(f) => self.apply(
              f.clone(),
              match evaluated_values.drop_first() {
                Some(list) => list,
                None => List::new(),
              },
            ),
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

fn value_sum(
  values: List<QuootValue>,
  error_message_name: &str,
) -> Result<Num, QuootEvalError> {
  Ok(
    values
      .iter()
      .map(|v| v.as_num(error_message_name))
      .into_iter()
      .collect::<Result<Vec<Num>, QuootEvalError>>()?
      .iter()
      .fold(Num::Int(0), |a, b| match (a, b) {
        (Num::Int(a), Num::Int(b)) => Num::Int(a + b),
        (Num::Float(a), Num::Float(b)) => Num::Float(a + b),
        (Num::Int(a), Num::Float(b)) => Num::Float((a as f64) + b),
        (Num::Float(a), Num::Int(b)) => Num::Float(a + (*b as f64)),
      }),
  )
}

fn value_product(
  values: List<QuootValue>,
  error_message_name: &str,
) -> Result<Num, QuootEvalError> {
  Ok(
    values
      .iter()
      .map(|v| v.as_num(error_message_name))
      .into_iter()
      .collect::<Result<Vec<Num>, QuootEvalError>>()?
      .iter()
      .fold(Num::Int(1), |a, b| match (a, b) {
        (Num::Int(a), Num::Int(b)) => Num::Int(a * b),
        (Num::Float(a), Num::Float(b)) => Num::Float(a * b),
        (Num::Int(a), Num::Float(b)) => Num::Float((a as f64) * b),
        (Num::Float(a), Num::Int(b)) => Num::Float(a * (*b as f64)),
      }),
  )
}

fn transpose(
  values: List<QuootValue>,
  error_message_name: &str,
) -> Result<Vec<List<QuootValue>>, QuootEvalError> {
  if values.len() == 0 {
    Ok(vec![])
  } else {
    let lists: Vec<List<QuootValue>> = values
      .reverse()
      .iter()
      .map(|value| value.as_list(error_message_name))
      .collect::<Vec<Result<List<QuootValue>, QuootEvalError>>>()
      .into_iter()
      .collect::<Result<Vec<List<QuootValue>>, QuootEvalError>>()?;
    let transposed_lists: Vec<List<QuootValue>> = lists
      .first()
      .unwrap()
      .iter()
      .map(|v| List::new().push_front(v.clone()))
      .collect();
    Ok(lists[1..].iter().fold(transposed_lists, |lists, new_list| {
      let values: Vec<QuootValue> =
        new_list.iter().map(|v| v.clone()).collect();
      (0..min(lists.len(), values.len()))
        .map(|i: usize| lists[i].push_front(values[i].clone()))
        .collect()
    }))
  }
}

fn quoot_add(args: List<QuootValue>) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::Num(value_sum(args, "+")?))
}

fn quoot_multiply(
  args: List<QuootValue>,
) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::Num(value_product(args, "*")?))
}

fn quoot_subtract(
  args: List<QuootValue>,
) -> Result<QuootValue, QuootEvalError> {
  match args.first() {
    None => Err(QuootEvalError::FunctionError(
      "-: must supply at least one argument".to_owned(),
    )),
    Some(value) => {
      let first_num = value.as_num("-")?;
      match args.drop_first() {
        None => unreachable!(),
        Some(other_values) => Ok(QuootValue::Num(if other_values.len() == 0 {
          match first_num {
            Num::Int(i) => Num::Int(-i),
            Num::Float(f) => Num::Float(-f),
          }
        } else {
          match (first_num, value_sum(other_values, "-")?) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a - b),
            (Num::Float(a), Num::Float(b)) => Num::Float(a - b),
            (Num::Int(a), Num::Float(b)) => Num::Float((a as f64) - b),
            (Num::Float(a), Num::Int(b)) => Num::Float(a - (b as f64)),
          }
        })),
      }
    }
  }
}

fn quoot_divide(args: List<QuootValue>) -> Result<QuootValue, QuootEvalError> {
  match args.first() {
    None => Err(QuootEvalError::FunctionError(
      "/: must supply at least one argument".to_owned(),
    )),
    Some(value) => {
      let first_num = value.as_num("/")?;
      match args.drop_first() {
        None => unreachable!(),
        Some(other_values) => Ok(QuootValue::Num(if other_values.len() == 0 {
          match first_num {
            Num::Int(i) => Num::Float(1.0 / (i as f64)),
            Num::Float(f) => Num::Float(1.0 / f),
          }
        } else {
          match (first_num, value_product(other_values, "/")?) {
            (Num::Int(a), Num::Int(b)) => Num::Float((a as f64) / (b as f64)),
            (Num::Float(a), Num::Float(b)) => Num::Float(a / b),
            (Num::Int(a), Num::Float(b)) => Num::Float((a as f64) / b),
            (Num::Float(a), Num::Int(b)) => Num::Float(a / (b as f64)),
          }
        })),
      }
    }
  }
}

fn quoot_modulo(args: List<QuootValue>) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let dividend = args.first().unwrap().as_num("mod")?;
    let divisor = args.drop_first().unwrap().first().unwrap().as_num("mod")?;
    Ok(QuootValue::Num(match (dividend, divisor) {
      (Num::Int(a), Num::Int(b)) => Num::Int(a % b),
      (Num::Float(a), Num::Float(b)) => Num::Float(a % b),
      (Num::Int(a), Num::Float(b)) => Num::Float((a as f64) % b),
      (Num::Float(a), Num::Int(b)) => Num::Float(a % (b as f64)),
    }))
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "mod: need 2 arguments, got {}",
      args.len()
    )))
  }
}

fn quoot_quotient(
  args: List<QuootValue>,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let dividend = args.first().unwrap().as_num("quot")?;
    let divisor = args.drop_first().unwrap().first().unwrap().as_num("quot")?;
    Ok(QuootValue::Num(match (dividend, divisor) {
      (Num::Int(a), Num::Int(b)) => Num::Int(a / b),
      (Num::Float(a), Num::Float(b)) => Num::Int((a / b) as i64),
      (Num::Int(a), Num::Float(b)) => Num::Int(((a as f64) / b) as i64),
      (Num::Float(a), Num::Int(b)) => Num::Int((a / (b as f64)) as i64),
    }))
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "quot: need 2 arguments, got {}",
      args.len()
    )))
  }
}

fn quoot_list_constructor(
  args: List<QuootValue>,
) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::List(args))
}

fn quoot_count(args: List<QuootValue>) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 1 {
    match args.first().unwrap() {
      QuootValue::Nil => Ok(QuootValue::Num(Num::Int(0))),
      QuootValue::List(list) => {
        Ok(QuootValue::Num(Num::Int(list.len() as i64)))
      }
      v => Err(QuootEvalError::FunctionError(format!(
        "count: can't count type <{}>",
        v.type_string()
      ))),
    }
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "count: need 1 argument, got {}",
      args.len()
    )))
  }
}

fn quoot_cons(args: List<QuootValue>) -> Result<QuootValue, QuootEvalError> {
  match args.first() {
    None => Err(QuootEvalError::FunctionError(
      "cons: need at least 1 argument, got 0".to_string(),
    )),
    Some(element) => {
      let rest_args = args.drop_first().unwrap();
      match rest_args.len() {
        0 => Ok(QuootValue::List(List::new().push_front(element.clone()))),
        1 => {
          let second_arg = rest_args.first().unwrap();
          match second_arg {
            QuootValue::List(list) => {
              Ok(QuootValue::List(list.push_front(element.clone())))
            }
            QuootValue::Nil => {
              Ok(QuootValue::List(List::new().push_front(element.clone())))
            }
            _ => Err(QuootEvalError::FunctionError(format!(
              "cons: cannot cons into a <{}>",
              second_arg.type_string()
            ))),
          }
        }
        n => Err(QuootEvalError::FunctionError(format!(
          "cons: need 1 or 2 arguments, got {}",
          n
        ))),
      }
    }
  }
}

fn quoot_concat(args: List<QuootValue>) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 0 {
    Ok(QuootValue::List(List::new()))
  } else {
    let mut lists = args
      .iter()
      .map(|v| v.as_list("concat"))
      .into_iter()
      .collect::<Result<Vec<List<QuootValue>>, QuootEvalError>>(
    )?;
    let mut concat_list = lists.pop().unwrap();
    for list in lists.iter().rev() {
      concat_list = list
        .reverse()
        .iter()
        .fold(concat_list, |l, e| l.push_front(e.to_owned()));
    }
    Ok(QuootValue::List(concat_list))
  }
}

fn quoot_nth(args: List<QuootValue>) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    match args.first().unwrap() {
      QuootValue::Nil => Ok(QuootValue::Nil),
      QuootValue::List(list) => {
        let n = args
          .drop_first()
          .unwrap()
          .first()
          .unwrap()
          .as_num("nth")?
          .floor();
        if n < list.len() as i64 {
          let mut list_copy = list.clone();
          for i in 0..n {
            list_copy = list_copy.drop_first().unwrap()
          }
          Ok(list_copy.first().unwrap().clone())
        } else {
          Err(QuootEvalError::FunctionError(format!(
            "nth: can't get value at index {} in list of length {}",
            n,
            list.len()
          )))
        }
      }
      other => Err(QuootEvalError::FunctionError(format!(
        "nth: cannot get nth value from <{}>",
        other.type_string()
      ))),
    }
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "nth: need 2 arguments, got {}",
      args.len()
    )))
  }
}

fn quoot_transpose(
  args: List<QuootValue>,
) -> Result<QuootValue, QuootEvalError> {
  let transposition = transpose(args, "transpose")?;
  Ok(QuootValue::List(
    transposition.iter().rev().fold(List::new(), |list, v| {
      list.push_front(QuootValue::List(v.to_owned()))
    }),
  ))
}

fn quoot_apply(args: List<QuootValue>) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Err(QuootEvalError::FunctionError(
      "apply: need 1 or 2 arguments, got 0".to_string(),
    )),
    1 => match args.first().unwrap() {
      QuootValue::Fn(f) => f(List::new()),
      other => Err(QuootEvalError::FunctionError(format!(
        "apply: cannot invoke type <{}>",
        other.type_string()
      ))),
    },
    2 => match args.first().unwrap() {
      QuootValue::Fn(f) => {
        let f_arg_list = args
          .drop_first()
          .unwrap()
          .first()
          .unwrap()
          .as_list("apply")?;
        f(f_arg_list)
      }
      other => Err(QuootEvalError::FunctionError(format!(
        "apply: cannot invoke type <{}>",
        other.type_string()
      ))),
    },
    n => Err(QuootEvalError::FunctionError(format!(
      "apply: need 1 or 2 arguments, got {}",
      n
    ))),
  }
}

fn quoot_map(args: List<QuootValue>) -> Result<QuootValue, QuootEvalError> {
  if args.len() < 2 {
    Err(QuootEvalError::FunctionError(format!(
      "map: need at least 2 arguments, got {}",
      args.len()
    )))
  } else {
    let f = match args.first().unwrap() {
      QuootValue::Fn(f) => f,
      other => {
        return Err(QuootEvalError::FunctionError(format!(
          "map: first arugment must be a function, got type <{}>",
          other.type_string()
        )))
      }
    };
    let arg_lists = transpose(args.drop_first().unwrap(), "map")?;
    let results: Vec<QuootValue> = arg_lists
      .iter()
      .rev()
      .map(|arg_list| f(arg_list.to_owned()))
      .collect::<Vec<Result<QuootValue, QuootEvalError>>>()
      .into_iter()
      .collect::<Result<Vec<QuootValue>, QuootEvalError>>()?;
    Ok(QuootValue::List(
      results
        .iter()
        .fold(List::new(), |list, value| list.push_front(value.to_owned())),
    ))
  }
}

fn quoot_identity(
  args: List<QuootValue>,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 1 {
    Ok(args.first().unwrap().clone())
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "identity: needed 1 argument, received {}",
      args.len()
    )))
  }
}

fn quoot_compose(args: List<QuootValue>) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Ok(QuootValue::Fn(&quoot_identity)),
    1 => Ok(QuootValue::Fn(args.first().unwrap().as_fn("compose")?)),
    n => {
      let fns = args
        .iter()
        .map(|arg| arg.as_fn("compose"))
        .collect::<Vec<Result<QuootFn, QuootEvalError>>>()
        .into_iter()
        .collect::<Result<Vec<QuootFn>, QuootEvalError>>()?;
      let mut f: QuootFn = fns[0];
      for i in 1..n {
        f = compose(f, fns[i]);
      }
      Ok(QuootValue::Fn(f))
    }
  }
}

fn quoot_partial(args: List<QuootValue>) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Err(QuootEvalError::FunctionError(
      "partial needs at least 1 argument, got 0".to_owned(),
    )),
    1 => Ok(QuootValue::Fn(args.first().unwrap().as_fn("partial")?)),
    n => {
      let f = args.first().unwrap().as_fn("partial")?;
      Ok(QuootValue::Fn(partial(f, args.drop_first().unwrap())))
    }
  }
}

pub fn repl() {
  println!("\nQuoot repl started :D\n");
  let interpreter = &mut Interpreter::default();
  interpreter.add_binding(
    "TAU".to_owned(),
    QuootValue::Num(Num::Float(6.283185307179586)),
  );
  interpreter.add_binding("+".to_owned(), QuootValue::Fn(&quoot_add));
  interpreter.add_binding("-".to_owned(), QuootValue::Fn(&quoot_subtract));
  interpreter.add_binding("*".to_owned(), QuootValue::Fn(&quoot_multiply));
  interpreter.add_binding("/".to_owned(), QuootValue::Fn(&quoot_divide));
  interpreter.add_binding("mod".to_owned(), QuootValue::Fn(&quoot_modulo));
  interpreter.add_binding("quot".to_owned(), QuootValue::Fn(&quoot_quotient));
  interpreter
    .add_binding("list".to_owned(), QuootValue::Fn(&quoot_list_constructor));
  interpreter.add_binding("count".to_owned(), QuootValue::Fn(&quoot_count));
  interpreter.add_binding("cons".to_owned(), QuootValue::Fn(&quoot_cons));
  interpreter.add_binding("concat".to_owned(), QuootValue::Fn(&quoot_concat));
  interpreter.add_binding("nth".to_owned(), QuootValue::Fn(&quoot_nth));
  interpreter
    .add_binding("transpose".to_owned(), QuootValue::Fn(&quoot_transpose));
  interpreter
    .add_binding("identity".to_owned(), QuootValue::Fn(&quoot_identity));
  interpreter.add_binding("apply".to_owned(), QuootValue::Fn(&quoot_apply));
  interpreter.add_binding("partial".to_owned(), QuootValue::Fn(&quoot_partial));
  interpreter.add_binding("|".to_owned(), QuootValue::Fn(&quoot_partial));
  interpreter.add_binding("compose".to_owned(), QuootValue::Fn(&quoot_compose));
  interpreter.add_binding(".".to_owned(), QuootValue::Fn(&quoot_compose));
  interpreter.add_binding("map".to_owned(), QuootValue::Fn(&quoot_map));
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
