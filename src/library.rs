use crate::model::compose;
use crate::model::partial;
use crate::model::Num;
use crate::model::QuootEvalError;
use crate::model::QuootFn;
use crate::model::QuootValue;
use crate::model::QuootValueList;
use rpds::List;
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

fn transpose(
  values: QuootValueList,
  error_message_name: &str,
) -> Result<Vec<QuootValueList>, QuootEvalError> {
  if values.len() == 0 {
    Ok(vec![])
  } else {
    let lists: Vec<QuootValueList> = values
      .reverse()
      .iter()
      .map(|value| value.as_list(error_message_name))
      .collect::<Vec<Result<QuootValueList, QuootEvalError>>>()
      .into_iter()
      .collect::<Result<Vec<QuootValueList>, QuootEvalError>>()?;
    let transposed_lists: Vec<QuootValueList> = lists
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

pub fn quoot_inc(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 1 {
    Ok(QuootValue::Num(
      match args.first().unwrap().as_num("inc")? {
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
      match args.first().unwrap().as_num("inc")? {
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

pub fn quoot_divide(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
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

pub fn quoot_modulo(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
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

pub fn quoot_quotient(
  args: QuootValueList,
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

pub fn quoot_equal(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::Bool(match args.len() {
    0 => true,
    n => {
      let first = args.first().unwrap();
      let mut rest_args = args.drop_first().unwrap();
      for _ in 1..n {
        if first != rest_args.first().unwrap() {
          return Ok(QuootValue::Bool(false));
        }
        rest_args = rest_args.drop_first().unwrap();
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

pub fn quoot_cons(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
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

pub fn quoot_concat(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 0 {
    Ok(QuootValue::List(List::new()))
  } else {
    let mut lists = args
      .iter()
      .map(|v| v.as_list("concat"))
      .into_iter()
      .collect::<Result<Vec<QuootValueList>, QuootEvalError>>()?;
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

pub fn quoot_nth(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
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

pub fn quoot_transpose(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
  let transposition = transpose(args, "transpose")?;
  Ok(QuootValue::List(
    transposition.iter().rev().fold(List::new(), |list, v| {
      list.push_front(QuootValue::List(v.to_owned()))
    }),
  ))
}

pub fn quoot_take(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let n = max(0, args.first().unwrap().as_num("take")?.floor()) as usize;
    let mut list = args
      .drop_first()
      .unwrap()
      .first()
      .unwrap()
      .as_list("take")?;
    Ok(QuootValue::List(if n >= list.len() {
      list
    } else {
      let mut taken_list: QuootValueList = List::new();
      for i in 0..n {
        taken_list = taken_list.push_front(list.first().unwrap().clone());
        list = list.drop_first().unwrap();
      }
      taken_list.reverse()
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
    let n = max(0, args.first().unwrap().as_num("drop")?.floor()) as usize;
    let mut list = args
      .drop_first()
      .unwrap()
      .first()
      .unwrap()
      .as_list("drop")?;
    Ok(QuootValue::List(if n >= list.len() {
      List::new()
    } else {
      for i in 0..n {
        list = list.drop_first().unwrap();
      }
      list
    }))
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "drop: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_range(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => todo!(),
    1 => {
      let n = args.first().unwrap().as_num("range")?.floor();
      Ok(QuootValue::List(
        (0..n).rev().fold(List::new(), |list, i| {
          list.push_front(QuootValue::Num(Num::Int(i)))
        }),
      ))
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

pub fn quoot_map(args: QuootValueList) -> Result<QuootValue, QuootEvalError> {
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

pub fn quoot_identity(
  args: QuootValueList,
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

pub fn quoot_compose(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
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

pub fn quoot_partial(
  args: QuootValueList,
) -> Result<QuootValue, QuootEvalError> {
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
