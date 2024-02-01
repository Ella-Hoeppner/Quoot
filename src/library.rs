use crate::model::compose;
use crate::model::partial;
use crate::model::Num;
use crate::model::QuootEvalError;
use crate::model::QuootFn;
use crate::model::QuootValue;
use crate::model::QuootValueList;
use std::cmp::{max, min};

fn value_sum(
  values: QuootValueList,
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
  values: QuootValueList,
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

pub fn quoot_inc(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 1 {
    Ok(QuootValue::Num(
      match args.front().unwrap().as_num("inc")? {
        Num::Int(i) => Num::Int(i + 1),
        Num::Float(f) => Num::Float(f + 1.0),
      },
    ))
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "inc: needed 1 argument, got {}",
      args.len()
    )))
  }
}

pub fn quoot_dec(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 1 {
    Ok(QuootValue::Num(
      match args.front().unwrap().as_num("inc")? {
        Num::Int(i) => Num::Int(i - 1),
        Num::Float(f) => Num::Float(f - 1.0),
      },
    ))
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "inc: needed 1 argument, got {}",
      args.len()
    )))
  }
}

pub fn quoot_add(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::Num(value_sum(args, "+")?))
}

pub fn quoot_multiply(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::Num(value_product(args, "*")?))
}

pub fn quoot_subtract(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  let cloned_args = &mut args.clone();
  match cloned_args.pop_front() {
    None => Err(QuootEvalError::FunctionError(
      "-: must supply at least one argument".to_owned(),
    )),
    Some(value) => {
      let first_num = value.as_num("-")?;
      Ok(QuootValue::Num(if cloned_args.is_empty() {
        match first_num {
          Num::Int(i) => Num::Int(-i),
          Num::Float(f) => Num::Float(-f),
        }
      } else {
        match (first_num, value_sum(cloned_args.to_owned(), "-")?) {
          (Num::Int(a), Num::Int(b)) => Num::Int(a - b),
          (Num::Float(a), Num::Float(b)) => Num::Float(a - b),
          (Num::Int(a), Num::Float(b)) => Num::Float((a as f64) - b),
          (Num::Float(a), Num::Int(b)) => Num::Float(a - (b as f64)),
        }
      }))
    }
  }
}

pub fn quoot_divide(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  let cloned_args = &mut args.clone();
  match cloned_args.pop_front() {
    None => Err(QuootEvalError::FunctionError(
      "/: must supply at least one argument".to_owned(),
    )),
    Some(value) => {
      let first_num = value.as_num("/")?;
      Ok(QuootValue::Num(if cloned_args.is_empty() {
        match first_num {
          Num::Int(i) => Num::Float(1.0 / (i as f64)),
          Num::Float(f) => Num::Float(1.0 / f),
        }
      } else {
        match (first_num, value_product(cloned_args.to_owned(), "/")?) {
          (Num::Int(a), Num::Int(b)) => Num::Float((a as f64) / (b as f64)),
          (Num::Float(a), Num::Float(b)) => Num::Float(a / b),
          (Num::Int(a), Num::Float(b)) => Num::Float((a as f64) / b),
          (Num::Float(a), Num::Int(b)) => Num::Float(a / (b as f64)),
        }
      }))
    }
  }
}

pub fn quoot_modulo(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let dividend = args.front().unwrap().as_num("mod")?;
    let divisor = args.get(1).unwrap().as_num("mod")?;
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

pub fn quoot_quotient(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let dividend = args.front().unwrap().as_num("quot")?;
    let divisor = args.get(1).unwrap().as_num("quot")?;
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

pub fn quoot_equal(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::Bool(match args.len() {
    0 => true,
    n => {
      let cloned_args = &mut args.clone();
      let first = cloned_args.pop_front().unwrap();
      while let Some(arg) = cloned_args.pop_front() {
        if first != arg {
          return Ok(QuootValue::Bool(false));
        }
      }
      true
    }
  }))
}

pub fn quoot_list_constructor(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::List(args))
}

pub fn quoot_count(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 1 {
    match args.front().unwrap() {
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

pub fn quoot_cons(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  match args.front() {
    None => Err(QuootEvalError::FunctionError(
      "cons: need at least 1 argument, got 0".to_string(),
    )),
    Some(element) => match args.len() {
      1 => Ok(QuootValue::List(QuootValueList::unit(element.clone()))),
      2 => {
        let second_arg = args.get(1).unwrap();
        match second_arg {
          QuootValue::List(list) => {
            let list_clone = &mut list.clone();
            list_clone.push_front(element.to_owned());
            Ok(QuootValue::List(list_clone.to_owned()))
          }
          QuootValue::Nil => {
            Ok(QuootValue::List(QuootValueList::unit(element.clone())))
          }
          _ => Err(QuootEvalError::FunctionError(format!(
            "cons: cannot cons onto a <{}>",
            second_arg.type_string()
          ))),
        }
      }
      n => Err(QuootEvalError::FunctionError(format!(
        "cons: need 1 or 2 arguments, got {}",
        n
      ))),
    },
  }
}

pub fn quoot_concat(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 0 {
    Ok(QuootValue::List(QuootValueList::new()))
  } else {
    let cloned_args = &mut args.clone();
    let concat_list =
      &mut cloned_args.pop_front().unwrap().as_list("concat")?;
    while let Some(list) = cloned_args.pop_front() {
      concat_list.append(list.as_list("concat")?);
    }
    Ok(QuootValue::List(concat_list.to_owned()))
  }
}

pub fn quoot_get(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    match args.front().unwrap() {
      QuootValue::Nil => Ok(QuootValue::Nil),
      QuootValue::List(list) => {
        let n = args.get(1).unwrap().as_num("get")?.floor() as usize;
        match list.get(n) {
          None => Err(QuootEvalError::FunctionError(format!(
            "get: can't get value at index {} in list of length {}",
            n,
            list.len()
          ))),
          Some(value) => Ok(value.to_owned()),
        }
      }
      other => Err(QuootEvalError::FunctionError(format!(
        "get: cannot get value from <{}>",
        other.type_string()
      ))),
    }
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "get: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_take(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let n = max(0, args.front().unwrap().as_num("take")?.floor()) as usize;
    let list = args.get(1).unwrap().as_list("take")?;
    Ok(QuootValue::List(if n >= list.len() {
      list
    } else {
      list.take(n)
    }))
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "take: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_drop(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let n = max(0, args.front().unwrap().as_num("take")?.floor()) as usize;
    Ok(QuootValue::List(
      args.get(1).unwrap().as_list("take")?.skip(n),
    ))
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "take: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_range(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => todo!(),
    1 => {
      let n = args.front().unwrap().as_num("range")?.floor();
      let list = &mut QuootValueList::new();
      for i in 0..n {
        list.push_back(QuootValue::Num(Num::Int(i)));
      }
      Ok(QuootValue::List(list.to_owned()))
    }
    n => Err(QuootEvalError::FunctionError(format!(
      "range: need 1 or 2 arguments, got {}",
      n
    ))),
  }
}

pub fn quoot_apply(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Err(QuootEvalError::FunctionError(
      "apply: need 1 or 2 arguments, got 0".to_string(),
    )),
    1 => match args.front().unwrap() {
      QuootValue::Fn(f) => f(QuootValueList::new()),
      other => Err(QuootEvalError::FunctionError(format!(
        "apply: cannot invoke type <{}>",
        other.type_string()
      ))),
    },
    2 => match args.front().unwrap() {
      QuootValue::Fn(f) => {
        let f_arg_list = args.get(1).unwrap().as_list("apply")?;
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

pub fn quoot_map(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  if args.len() < 2 {
    Err(QuootEvalError::FunctionError(format!(
      "map: need at least 2 arguments, got {}",
      args.len()
    )))
  } else {
    todo!()
  }
}

pub fn quoot_identity(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 1 {
    Ok(args.front().unwrap().clone())
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "identity: needed 1 argument, received {}",
      args.len()
    )))
  }
}

pub fn quoot_compose(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Ok(QuootValue::Fn(&quoot_identity)),
    1 => Ok(QuootValue::Fn(args.front().unwrap().as_fn("compose")?)),
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

pub fn quoot_partial(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Err(QuootEvalError::FunctionError(
      "partial needs at least 1 argument, got 0".to_owned(),
    )),
    1 => Ok(QuootValue::Fn(args.front().unwrap().as_fn("partial")?)),
    n => {
      let cloned_args = &mut args.clone();
      let f = cloned_args.pop_front().unwrap().as_fn("partial")?;
      Ok(QuootValue::Fn(partial(f, cloned_args.to_owned())))
    }
  }
}

pub fn quoot_is_nil(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match args.front().unwrap() {
      QuootValue::Nil => true,
      _ => false,
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "nil? needs 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_bool(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match args.front().unwrap() {
      QuootValue::Bool(_) => true,
      _ => false,
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "bool? needs 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_list(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match args.front().unwrap() {
      QuootValue::List(_) => true,
      _ => false,
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "list? needs 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_num(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match args.front().unwrap() {
      QuootValue::Num(_) => true,
      _ => false,
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "num? needs 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_string(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match args.front().unwrap() {
      QuootValue::String(_) => true,
      _ => false,
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "str? needs 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_symbol(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match args.front().unwrap() {
      QuootValue::Symbol(_) => true,
      _ => false,
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "symbol? needs 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_fn(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match args.front().unwrap() {
      QuootValue::Fn(_) => true,
      _ => false,
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "fn? needs 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_empty(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match args.front().unwrap() {
      QuootValue::Nil => true,
      QuootValue::List(list) => list.is_empty(),
      other => {
        return Err(QuootEvalError::FunctionError(format!(
          "empty? needs a collection, cannot check whether type {} is empty",
          other.type_string()
        )))
      }
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "empty? needs 1 argument, got {}",
      n
    ))),
  }
}
