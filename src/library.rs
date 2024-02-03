use crate::model::{
  eval, Bindings, Env, Num, QuootEvalError, QuootFn, QuootLazyList, QuootList,
  QuootStrictList, QuootValue,
};

fn fold_nums<F: FnMut(Num, &Num) -> Num>(
  values: QuootStrictList,
  error_message_name: &str,
  init_value: Num,
  folder: F,
) -> Result<Num, QuootEvalError> {
  Ok(
    values
      .iter()
      .map(|v| v.as_num(error_message_name))
      .into_iter()
      .collect::<Result<Vec<Num>, QuootEvalError>>()?
      .iter()
      .fold(init_value, folder),
  )
}

fn value_sum(
  values: QuootStrictList,
  error_message_name: &str,
) -> Result<Num, QuootEvalError> {
  Ok(fold_nums(
    values,
    error_message_name,
    Num::Int(0),
    Num::add,
  )?)
}

fn value_product(
  values: QuootStrictList,
  error_message_name: &str,
) -> Result<Num, QuootEvalError> {
  Ok(fold_nums(
    values,
    error_message_name,
    Num::Int(1),
    Num::multiply,
  )?)
}

fn value_min(
  values: QuootStrictList,
  error_message_name: &str,
) -> Result<Num, QuootEvalError> {
  Ok(fold_nums(
    values,
    error_message_name,
    Num::Float(f64::INFINITY),
    Num::min,
  )?)
}

fn value_max(
  values: QuootStrictList,
  error_message_name: &str,
) -> Result<Num, QuootEvalError> {
  Ok(fold_nums(
    values,
    error_message_name,
    Num::Float(f64::NEG_INFINITY),
    Num::max,
  )?)
}

fn eval_all(
  env: &Env,
  values: &QuootStrictList,
) -> Result<QuootStrictList, QuootEvalError> {
  Ok(QuootStrictList::from(
    values
      .iter()
      .map(|value| eval(env, value))
      .collect::<Vec<Result<QuootValue, QuootEvalError>>>()
      .into_iter()
      .collect::<Result<Vec<QuootValue>, QuootEvalError>>()?,
  ))
}

pub fn compose(f: QuootFn, g: QuootFn) -> QuootFn {
  Box::leak(Box::new(move |env: &Env, args: &QuootStrictList| {
    f(env, &QuootStrictList::unit(g(env, args)?))
  }))
}

pub fn partial(f: QuootFn, prefix_args: QuootStrictList) -> QuootFn {
  Box::leak(Box::new(move |env: &Env, args: &QuootStrictList| {
    let cloned_args = &mut prefix_args.clone();
    cloned_args.append(args.to_owned());
    f(env, cloned_args)
  }))
}

pub fn quoot_let(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let bindings = args.front().unwrap();
    match bindings {
      QuootValue::List(QuootList::Strict(list)) => {
        let list_clone = &mut list.clone();
        match list_clone.pop_front() {
          Some(value) => {
            if value.eq(&QuootValue::Symbol("#list".to_owned())) {
              if list_clone.len() % 2 == 0 {
                let sub_env = &mut env.clone();
                while let Some(binding_name) = list_clone.pop_front() {
                  match binding_name {
                    QuootValue::Symbol(name) => {
                      let binding_value = list_clone.pop_front().unwrap();
                      sub_env.bind(&name, binding_value);
                    }
                    other => {
                      return Err(QuootEvalError::FunctionError(format!(
                        "let: names must be symbols, got <{}>",
                        other.type_string()
                      )))
                    }
                  }
                }
                eval(sub_env, args.get(1).unwrap())
              } else {
                Err(QuootEvalError::FunctionError(format!(
                  "let: first argument needs an even number of forms"
                )))
              }
            } else {
              Err(QuootEvalError::FunctionError(format!(
                "let: first argument must be a list literal, got list"
              )))
            }
          }
          None => Err(QuootEvalError::FunctionError(format!(
            "let: first argument must be a list literal, got ()",
          ))),
        }
      }
      other => Err(QuootEvalError::FunctionError(format!(
        "let: first argument must be a list literal, got <{}>",
        other.type_string()
      ))),
    }
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "let: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_inc(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 1 {
    Ok(QuootValue::Num(
      match eval(env, args.front().unwrap())?.as_num("inc")? {
        Num::Int(i) => Num::Int(i + 1),
        Num::Float(f) => Num::Float(f + 1.0),
      },
    ))
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "inc: need 1 argument, got {}",
      args.len()
    )))
  }
}

pub fn quoot_dec(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 1 {
    Ok(QuootValue::Num(
      match eval(env, args.front().unwrap())?.as_num("inc")? {
        Num::Int(i) => Num::Int(i - 1),
        Num::Float(f) => Num::Float(f - 1.0),
      },
    ))
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "inc: need 1 argument, got {}",
      args.len()
    )))
  }
}

pub fn quoot_add(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::Num(value_sum(eval_all(env, args)?, "+")?))
}

pub fn quoot_multiply(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::Num(value_product(eval_all(env, args)?, "*")?))
}

pub fn quoot_subtract(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  let args_clone = &mut args.clone();
  match args_clone.pop_front() {
    None => Err(QuootEvalError::FunctionError(
      "-: need at least one argument".to_owned(),
    )),
    Some(value) => {
      let first_num = eval(env, &value)?.as_num("-")?;
      Ok(QuootValue::Num(if args_clone.is_empty() {
        match first_num {
          Num::Int(i) => Num::Int(-i),
          Num::Float(f) => Num::Float(-f),
        }
      } else {
        match (first_num, value_sum(eval_all(env, args_clone)?, "-")?) {
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
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  let args_clone = &mut args.clone();
  match args_clone.pop_front() {
    None => Err(QuootEvalError::FunctionError(
      "/: need at least 1 argument".to_owned(),
    )),
    Some(value) => {
      let first_num = eval(env, &value)?.as_num("/")?;
      Ok(QuootValue::Num(if args_clone.is_empty() {
        match first_num {
          Num::Int(i) => Num::Float(1.0 / (i as f64)),
          Num::Float(f) => Num::Float(1.0 / f),
        }
      } else {
        match (first_num, value_product(eval_all(env, args_clone)?, "/")?) {
          (Num::Int(a), Num::Int(b)) => Num::Float((a as f64) / (b as f64)),
          (Num::Float(a), Num::Float(b)) => Num::Float(a / b),
          (Num::Int(a), Num::Float(b)) => Num::Float((a as f64) / b),
          (Num::Float(a), Num::Int(b)) => Num::Float(a / (b as f64)),
        }
      }))
    }
  }
}

pub fn quoot_min(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Err(QuootEvalError::FunctionError(
      "min: need at least 1 argument".to_owned(),
    )),
    _ => Ok(QuootValue::Num(value_min(eval_all(env, args)?, "min")?)),
  }
}

pub fn quoot_max(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Err(QuootEvalError::FunctionError(
      "max: need at least 1 argument".to_owned(),
    )),
    _ => Ok(QuootValue::Num(value_max(eval_all(env, args)?, "max")?)),
  }
}

pub fn quoot_modulo(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let dividend = eval(env, args.front().unwrap())?.as_num("mod")?;
    let divisor = eval(env, args.get(1).unwrap())?.as_num("mod")?;
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
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let dividend = eval(env, args.front().unwrap())?.as_num("quot")?;
    let divisor = eval(env, args.get(1).unwrap())?.as_num("quot")?;
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

pub fn quoot_equal(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::Bool(match args.len() {
    0 => true,
    _ => {
      let values = &mut eval_all(env, args)?;
      let first = values.pop_front().unwrap();
      while let Some(arg) = values.pop_front() {
        if first != arg {
          return Ok(QuootValue::Bool(false));
        }
      }
      true
    }
  }))
}

pub fn quoot_numerical_equal(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::Bool(match args.len() {
    0 => true,
    _ => {
      let values = &mut eval_all(env, args)?;
      let first = values.pop_front().unwrap().as_num("==")?;
      while let Some(arg) = values.pop_front() {
        if !Num::numerical_equal(first, &arg.as_num("==")?) {
          return Ok(QuootValue::Bool(false));
        }
      }
      true
    }
  }))
}

pub fn quoot_list_constructor(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::List(QuootList::Strict(eval_all(env, args)?)))
}

pub fn quoot_count(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 1 {
    match eval(env, args.front().unwrap())? {
      QuootValue::Nil => Ok(QuootValue::Num(Num::Int(0))),
      QuootValue::List(QuootList::Strict(list)) => {
        Ok(QuootValue::Num(Num::Int(list.len() as i64)))
      }
      QuootValue::List(QuootList::Lazy(list)) => {
        let cloned_list = &mut list.clone();
        let realized_list = cloned_list.fully_realize()?;
        Ok(QuootValue::Num(Num::Int(
          realized_list.realized_len() as i64
        )))
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

pub fn quoot_cons(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::List(QuootList::Strict(QuootStrictList::unit(
      eval(env, args.front().unwrap())?,
    )))),
    2 => {
      let first_arg = eval(env, args.front().unwrap())?;
      let second_arg = eval(env, args.get(1).unwrap())?;
      match second_arg {
        QuootValue::List(QuootList::Strict(list)) => {
          let list_clone = &mut list.clone();
          list_clone.push_front(first_arg);
          Ok(QuootValue::List(QuootList::Strict(list_clone.to_owned())))
        }
        QuootValue::Nil => Ok(QuootValue::List(QuootList::Strict(
          QuootStrictList::unit(first_arg),
        ))),
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
  }
}

pub fn quoot_concat(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 0 {
    Ok(QuootValue::List(QuootList::Strict(QuootStrictList::new())))
  } else {
    let values = &mut eval_all(env, args)?;
    let concat_list =
      &mut values.pop_front().unwrap().as_list("concat")?.to_strict()?;
    while let Some(list) = values.pop_front() {
      concat_list.append(list.as_list("concat")?.to_strict()?);
    }
    Ok(QuootValue::List(QuootList::Strict(concat_list.to_owned())))
  }
}

pub fn quoot_get(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    match eval(env, args.front().unwrap())? {
      QuootValue::Nil => Ok(QuootValue::Nil),
      QuootValue::List(QuootList::Strict(list)) => {
        let n =
          eval(env, args.get(1).unwrap())?.as_num("get")?.floor() as usize;
        match list.get(n) {
          None => Err(QuootEvalError::FunctionError(format!(
            "get: index {} is out of bounds, list has length {}",
            n,
            list.len()
          ))),
          Some(value) => Ok(value.to_owned()),
        }
      }
      QuootValue::List(QuootList::Lazy(list)) => {
        let n =
          eval(env, args.get(1).unwrap())?.as_num("get")?.floor() as usize;
        match list.clone().get(n)? {
          None => Err(QuootEvalError::FunctionError(format!(
            "get: index {} is out of bounds, list has length {}",
            n,
            list.realized_len()
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

pub fn quoot_take(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let n =
      0.max(eval(env, args.front().unwrap())?.as_num("take")?.floor()) as usize;
    let list = eval(env, args.get(1).unwrap())?
      .as_list("take")?
      .to_strict()?;
    Ok(QuootValue::List(QuootList::Strict(if n >= list.len() {
      list
    } else {
      list.take(n)
    })))
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "take: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_drop(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    Ok(QuootValue::List(QuootList::Strict(
      eval(env, args.get(1).unwrap())?
        .as_list("drop")?
        .to_strict()?
        .skip(
          0.max(eval(env, args.front().unwrap())?.as_num("drop")?.floor())
            as usize,
        ),
    )))
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "drop: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_range(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Ok(QuootValue::List(QuootList::Lazy(QuootLazyList::new(
      &|values| Ok(Some(QuootValue::Num(Num::Int(values.len() as i64)))),
    )))),
    1 => {
      let list = &mut QuootStrictList::new();
      for i in 0..eval(env, args.front().unwrap())?.as_num("range")?.floor() {
        list.push_back(QuootValue::Num(Num::Int(i)));
      }
      Ok(QuootValue::List(QuootList::Strict(list.to_owned())))
    }
    n => Err(QuootEvalError::FunctionError(format!(
      "range: need 0 or 1 arguments, got {}",
      n
    ))),
  }
}

pub fn quoot_apply(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Err(QuootEvalError::FunctionError(
      "apply: need 1 or 2 arguments, got 0".to_string(),
    )),
    1 => match eval(env, args.front().unwrap())? {
      QuootValue::Fn(f) => f(env, &QuootStrictList::new()),
      other => Err(QuootEvalError::FunctionError(format!(
        "apply: cannot invoke type <{}>",
        other.type_string()
      ))),
    },
    2 => match eval(env, args.front().unwrap())? {
      QuootValue::Fn(f) => f(
        env,
        &eval(env, args.get(1).unwrap())?
          .as_list("apply")?
          .to_strict()?,
      ),
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

pub fn quoot_identity(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 1 {
    Ok(eval(env, &args.front().unwrap().clone())?)
  } else {
    Err(QuootEvalError::FunctionError(format!(
      "identity: need 1 argument, got {}",
      args.len()
    )))
  }
}

pub fn quoot_compose(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Ok(QuootValue::Fn(&quoot_identity)),
    1 => Ok(QuootValue::Fn(
      eval(env, args.front().unwrap())?.as_fn("compose")?,
    )),
    n => {
      let fns = args
        .iter()
        .map(|arg| eval(env, arg)?.as_fn("compose"))
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
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Err(QuootEvalError::FunctionError(
      "partial: need at least 1 argument, got 0".to_owned(),
    )),
    1 => Ok(QuootValue::Fn(
      eval(env, args.front().unwrap())?.as_fn("partial")?,
    )),
    n => {
      let values = &mut eval_all(env, args)?;
      let f = values.pop_front().unwrap().as_fn("partial")?;
      Ok(QuootValue::Fn(partial(f, values.to_owned())))
    }
  }
}

pub fn quoot_is_nil(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match eval(env, args.front().unwrap())? {
      QuootValue::Nil => true,
      _ => false,
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "nil?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_bool(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match eval(env, args.front().unwrap())? {
      QuootValue::Bool(_) => true,
      _ => false,
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "bool?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_list(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match eval(env, args.front().unwrap())? {
      QuootValue::List(_) => true,
      _ => false,
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "list?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_num(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match eval(env, args.front().unwrap())? {
      QuootValue::Num(_) => true,
      _ => false,
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "num?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_string(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match eval(env, args.front().unwrap())? {
      QuootValue::String(_) => true,
      _ => false,
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "str?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_symbol(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match eval(env, args.front().unwrap())? {
      QuootValue::Symbol(_) => true,
      _ => false,
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "symbol?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_fn(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match eval(env, args.front().unwrap())? {
      QuootValue::Fn(_) => true,
      _ => false,
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "fn?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_empty(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(match eval(env, args.front().unwrap())? {
      QuootValue::Nil => true,
      QuootValue::List(list) => list.is_empty()?,
      other => {
        return Err(QuootEvalError::FunctionError(format!(
          "empty?: cannot check whether type {} is empty",
          other.type_string()
        )))
      }
    })),
    n => Err(QuootEvalError::FunctionError(format!(
      "empty?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_first(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => match eval(env, args.front().unwrap())? {
      QuootValue::Nil => Ok(QuootValue::Nil),
      QuootValue::List(QuootList::Strict(list)) => Ok({
        match list.front() {
          None => QuootValue::Nil,
          Some(value) => value.to_owned(),
        }
      }),
      QuootValue::List(QuootList::Lazy(list)) => Ok({
        let list_clone = &mut list.clone();
        list_clone.realize_to(1)?;
        match list_clone.get(0)? {
          None => QuootValue::Nil,
          Some(value) => value.to_owned(),
        }
      }),
      other => {
        return Err(QuootEvalError::FunctionError(format!(
          "first: cannot get the first element of type <{}>",
          other.type_string()
        )))
      }
    },
    n => Err(QuootEvalError::FunctionError(format!(
      "first: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_last(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => match eval(env, args.front().unwrap())? {
      QuootValue::Nil => Ok(QuootValue::Nil),
      QuootValue::List(QuootList::Strict(list)) => Ok(match list.last() {
        None => QuootValue::Nil,
        Some(value) => value.to_owned(),
      }),
      QuootValue::List(QuootList::Lazy(list)) => Ok({
        let list_clone = &mut list.clone();
        list_clone.fully_realize()?;
        match list_clone.get(list_clone.realized_len() - 1)? {
          None => QuootValue::Nil,
          Some(value) => value.to_owned(),
        }
      }),
      other => {
        return Err(QuootEvalError::FunctionError(format!(
          "last: cannot get the last element of type <{}>",
          other.type_string()
        )))
      }
    },
    n => Err(QuootEvalError::FunctionError(format!(
      "last: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_reverse(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => {
      let list = &mut eval(env, args.front().unwrap())?
        .as_list("reverse")?
        .to_strict()?;
      let new_list = &mut QuootStrictList::new();
      while let Some(value) = list.pop_front() {
        new_list.push_front(value);
      }
      Ok(QuootValue::List(QuootList::Strict(new_list.clone())))
    }
    n => Err(QuootEvalError::FunctionError(format!(
      "reverse: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_bool(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      eval(env, args.front().unwrap())?.as_bool(),
    )),
    n => Err(QuootEvalError::FunctionError(format!(
      "bool: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_int(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Num(Num::Int(
      eval(env, args.front().unwrap())?.as_num("int")?.floor(),
    ))),
    n => Err(QuootEvalError::FunctionError(format!(
      "int: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_abs(
  env: &Env,
  args: &QuootStrictList,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Num(
      match eval(env, args.front().unwrap())?.as_num("abs")? {
        Num::Int(i) => Num::Int(i.abs()),
        Num::Float(f) => Num::Float(f.abs()),
      },
    )),
    n => Err(QuootEvalError::FunctionError(format!(
      "abs: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn default_bindings() -> Bindings {
  let bindings = &mut Bindings::new();
  bindings.insert(
    "TAU".to_owned(),
    QuootValue::Num(Num::Float(6.283185307179586)),
  );
  bindings.insert("let".to_owned(), QuootValue::Fn(&quoot_let));
  bindings.insert("inc".to_owned(), QuootValue::Fn(&quoot_inc));
  bindings.insert("dec".to_owned(), QuootValue::Fn(&quoot_dec));
  bindings.insert("+".to_owned(), QuootValue::Fn(&quoot_add));
  bindings.insert("*".to_owned(), QuootValue::Fn(&quoot_multiply));
  bindings.insert("=".to_owned(), QuootValue::Fn(&quoot_equal));
  bindings.insert("==".to_owned(), QuootValue::Fn(&quoot_numerical_equal));
  bindings.insert("-".to_owned(), QuootValue::Fn(&quoot_subtract));
  bindings.insert("/".to_owned(), QuootValue::Fn(&quoot_divide));
  bindings.insert("min".to_owned(), QuootValue::Fn(&quoot_min));
  bindings.insert("max".to_owned(), QuootValue::Fn(&quoot_max));
  bindings.insert("mod".to_owned(), QuootValue::Fn(&quoot_modulo));
  bindings.insert("quot".to_owned(), QuootValue::Fn(&quoot_quotient));
  bindings.insert("list".to_owned(), QuootValue::Fn(&quoot_list_constructor));
  bindings.insert("#list".to_owned(), QuootValue::Fn(&quoot_list_constructor));
  bindings.insert("count".to_owned(), QuootValue::Fn(&quoot_count));
  bindings.insert("cons".to_owned(), QuootValue::Fn(&quoot_cons));
  bindings.insert("concat".to_owned(), QuootValue::Fn(&quoot_concat));
  bindings.insert("get".to_owned(), QuootValue::Fn(&quoot_get));
  bindings.insert("take".to_owned(), QuootValue::Fn(&quoot_take));
  bindings.insert("drop".to_owned(), QuootValue::Fn(&quoot_drop));
  bindings.insert("range".to_owned(), QuootValue::Fn(&quoot_range));
  bindings.insert("identity".to_owned(), QuootValue::Fn(&quoot_identity));
  bindings.insert("apply".to_owned(), QuootValue::Fn(&quoot_apply));
  bindings.insert("partial".to_owned(), QuootValue::Fn(&quoot_partial));
  bindings.insert("|".to_owned(), QuootValue::Fn(&quoot_partial));
  bindings.insert("compose".to_owned(), QuootValue::Fn(&quoot_compose));
  bindings.insert(".".to_owned(), QuootValue::Fn(&quoot_compose));
  bindings.insert("nil?".to_owned(), QuootValue::Fn(&quoot_is_nil));
  bindings.insert("bool?".to_owned(), QuootValue::Fn(&quoot_is_bool));
  bindings.insert("list?".to_owned(), QuootValue::Fn(&quoot_is_list));
  bindings.insert("num?".to_owned(), QuootValue::Fn(&quoot_is_num));
  bindings.insert("str?".to_owned(), QuootValue::Fn(&quoot_is_string));
  bindings.insert("symbol?".to_owned(), QuootValue::Fn(&quoot_is_symbol));
  bindings.insert("fn?".to_owned(), QuootValue::Fn(&quoot_is_fn));
  bindings.insert("empty?".to_owned(), QuootValue::Fn(&quoot_is_empty));
  bindings.insert("bool".to_owned(), QuootValue::Fn(&quoot_bool));
  bindings.insert("int".to_owned(), QuootValue::Fn(&quoot_int));
  bindings.insert("abs".to_owned(), QuootValue::Fn(&quoot_abs));
  bindings.insert("first".to_owned(), QuootValue::Fn(&quoot_first));
  bindings.insert("last".to_owned(), QuootValue::Fn(&quoot_last));
  bindings.insert("reverse".to_owned(), QuootValue::Fn(&quoot_reverse));
  bindings.to_owned()
}
