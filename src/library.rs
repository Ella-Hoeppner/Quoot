use std::{rc::Rc, time::Instant};

use crate::model::{
  eval, maybe_eval, maybe_eval_all, Bindings, CoreOp, Env, EvalError, LazyList,
  LazyState, List, Num, Op, StrictList, UserOp, Value,
};

fn fold_nums<F: FnMut(Num, &Num) -> Num>(
  values: StrictList,
  error_message_name: &str,
  init_value: Num,
  folder: F,
) -> Result<Num, EvalError> {
  Ok(
    values
      .iter()
      .map(|v| v.as_num(error_message_name))
      .collect::<Result<Vec<Num>, EvalError>>()?
      .iter()
      .fold(init_value, folder),
  )
}

fn value_sum(
  values: StrictList,
  error_message_name: &str,
) -> Result<Num, EvalError> {
  Ok(fold_nums(
    values,
    error_message_name,
    Num::Int(0),
    Num::add,
  )?)
}

fn value_product(
  values: StrictList,
  error_message_name: &str,
) -> Result<Num, EvalError> {
  Ok(fold_nums(
    values,
    error_message_name,
    Num::Int(1),
    Num::multiply,
  )?)
}

fn value_min(
  values: StrictList,
  error_message_name: &str,
) -> Result<Num, EvalError> {
  Ok(fold_nums(
    values,
    error_message_name,
    Num::Float(f64::INFINITY),
    Num::min,
  )?)
}

fn value_max(
  values: StrictList,
  error_message_name: &str,
) -> Result<Num, EvalError> {
  Ok(fold_nums(
    values,
    error_message_name,
    Num::Float(f64::NEG_INFINITY),
    Num::max,
  )?)
}

pub fn quoot_let(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() < 2 {
    return Err(EvalError::OpError(format!(
      "let: need at least 2 arguments, got {}",
      args.len()
    )));
  }
  let mut args_iter = args.into_iter();
  match args_iter.next().unwrap() {
    Value::List(List::Strict(list)) => {
      let binding_list = List::deliteralize(list);
      if binding_list.len() % 2 == 0 {
        let mut binding_iter = binding_list.into_iter();
        let mut let_env = env.clone();
        while let Some(binding_name) = binding_iter.next() {
          match binding_name {
            Value::Symbol(name) => {
              let binding_value =
                maybe_eval(&let_env, binding_iter.next().unwrap(), eval_args)?;
              let_env.bind(&name, binding_value);
            }
            other => {
              return Err(EvalError::OpError(format!(
                "let: names must be symbols, got <{}>",
                other.type_string()
              )))
            }
          }
        }
        Ok(
          args_iter
            .map(|value| maybe_eval(&let_env, value, eval_args))
            .collect::<Result<Vec<Value>, EvalError>>()?
            .pop()
            .unwrap(),
        )
      } else {
        Err(EvalError::OpError(format!(
          "let: first argument needs an even number of forms"
        )))
      }
    }
    other => Err(EvalError::OpError(format!(
      "let: first argument must be a list literal, got <{}>",
      other.type_string()
    ))),
  }
}

pub fn quoot_eval(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => eval(env, maybe_eval(env, args.pop_front().unwrap(), eval_args)?),
    2 => todo!(),
    _ => Err(EvalError::OpError(format!(
      "eval: need 1 or 2 arguments, got {}",
      args.len()
    ))),
  }
}

pub fn quoot_quote(
  _env: &Env,
  mut args: StrictList,
  _eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(args.pop_front().unwrap()),
    _ => Err(EvalError::OpError(format!(
      "quote: need 1 argument, got {}",
      args.len()
    ))),
  }
}

pub fn parse_args_names(
  names: List,
  error_prefix: &str,
) -> Result<Vec<String>, EvalError> {
  List::deliteralize(names.to_strict()?)
    .into_iter()
    .map(|value| match value {
      Value::Symbol(name) => Ok(name),
      other => Err(EvalError::OpError(format!(
        "{}: arg list must contain symbols, got {}",
        error_prefix, other
      ))),
    })
    .collect::<Result<Vec<String>, EvalError>>()
}

pub fn quoot_op(
  env: &Env,
  mut args: StrictList,
  _eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() < 2 {
    return Err(EvalError::OpError(format!(
      "op: need at least 2 arguments, got {}",
      args.len()
    )));
  }
  let (op_name, arg_names): (Option<String>, Vec<String>) = {
    let first_arg = args.pop_front().unwrap();
    match first_arg {
      Value::Symbol(name) => (
        Some(name),
        parse_args_names(args.pop_front().unwrap().to_list("fn")?, "fn")?,
      ),
      Value::List(list) => (None, parse_args_names(list, "fn")?),
      other => {
        return Err(EvalError::OpError(format!(
          "op: first argument must be a symbol or list literal, got a <{}>",
          other.type_string()
        )))
      }
    }
  };
  Ok(Value::Op(Op::User(UserOp::new(
    op_name,
    env.clone(),
    arg_names,
    args,
    false,
  ))))
}

pub fn quoot_fn(
  env: &Env,
  mut args: StrictList,
  _eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() < 2 {
    return Err(EvalError::OpError(format!(
      "fn: need at least 2 arguments, got {}",
      args.len()
    )));
  }
  let (fn_name, arg_names): (Option<String>, Vec<String>) = {
    let first_arg = args.pop_front().unwrap();
    match first_arg {
      Value::Symbol(name) => (
        Some(name),
        parse_args_names(args.pop_front().unwrap().to_list("fn")?, "fn")?,
      ),
      Value::List(list) => (None, parse_args_names(list, "fn")?),
      other => {
        return Err(EvalError::OpError(format!(
          "fn: first argument must be a symbol or list literal, got a <{}>",
          other.type_string()
        )))
      }
    }
  };
  Ok(Value::Op(Op::User(UserOp::new(
    fn_name,
    env.clone(),
    arg_names,
    args,
    true,
  ))))
}

pub fn quoot_inc(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 1 {
    Ok(Value::Num(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)?
        .as_num("inc")?
      {
        Num::Int(i) => Num::Int(i + 1),
        Num::Float(f) => Num::Float(f + 1.0),
      },
    ))
  } else {
    Err(EvalError::OpError(format!(
      "inc: need 1 argument, got {}",
      args.len()
    )))
  }
}

pub fn quoot_dec(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 1 {
    Ok(Value::Num(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)?
        .as_num("inc")?
      {
        Num::Int(i) => Num::Int(i - 1),
        Num::Float(f) => Num::Float(f - 1.0),
      },
    ))
  } else {
    Err(EvalError::OpError(format!(
      "inc: need 1 argument, got {}",
      args.len()
    )))
  }
}

pub fn quoot_add(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  Ok(Value::Num(value_sum(
    maybe_eval_all(env, args, eval_args)?,
    "+",
  )?))
}

pub fn quoot_multiply(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  Ok(Value::Num(value_product(
    maybe_eval_all(env, args, eval_args)?,
    "*",
  )?))
}

pub fn quoot_subtract(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.pop_front() {
    None => Err(EvalError::OpError(
      "-: need at least one argument".to_string(),
    )),
    Some(value) => {
      let first_num = maybe_eval(env, value, eval_args)?.as_num("-")?;
      Ok(Value::Num(if args.is_empty() {
        match first_num {
          Num::Int(i) => Num::Int(-i),
          Num::Float(f) => Num::Float(-f),
        }
      } else {
        match (
          first_num,
          value_sum(maybe_eval_all(env, args, eval_args)?, "-")?,
        ) {
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
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.pop_front() {
    None => Err(EvalError::OpError(
      "/: need at least 1 argument".to_string(),
    )),
    Some(value) => {
      let first_num = maybe_eval(env, value, eval_args)?.as_num("/")?;
      Ok(Value::Num(if args.is_empty() {
        Num::Float(1.0 / first_num.as_float())
      } else {
        Num::Float(
          first_num.as_float()
            / value_product(maybe_eval_all(env, args, eval_args)?, "/")?
              .as_float(),
        )
      }))
    }
  }
}

pub fn quoot_min(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    0 => Err(EvalError::OpError(
      "min: need at least 1 argument".to_string(),
    )),
    _ => Ok(Value::Num(value_min(
      maybe_eval_all(env, args, eval_args)?,
      "min",
    )?)),
  }
}

pub fn quoot_max(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    0 => Err(EvalError::OpError(
      "max: need at least 1 argument".to_string(),
    )),
    _ => Ok(Value::Num(value_max(
      maybe_eval_all(env, args, eval_args)?,
      "max",
    )?)),
  }
}

pub fn quoot_modulo(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 2 {
    let mut args_iter = args.into_iter();
    let dividend =
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?.as_num("mod")?;
    let divisor =
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?.as_num("mod")?;
    Ok(Value::Num(match (dividend, divisor) {
      (Num::Int(a), Num::Int(b)) => Num::Int(a % b),
      (Num::Float(a), Num::Float(b)) => Num::Float(a % b),
      (Num::Int(a), Num::Float(b)) => Num::Float((a as f64) % b),
      (Num::Float(a), Num::Int(b)) => Num::Float(a % (b as f64)),
    }))
  } else {
    Err(EvalError::OpError(format!(
      "mod: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_quotient(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 2 {
    let mut args_iter = args.into_iter();
    let dividend =
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?.as_num("mod")?;
    let divisor =
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?.as_num("mod")?;
    Ok(Value::Num(match (dividend, divisor) {
      (Num::Int(a), Num::Int(b)) => Num::Int(a / b),
      (Num::Float(a), Num::Float(b)) => Num::Int((a / b) as i64),
      (Num::Int(a), Num::Float(b)) => Num::Int(((a as f64) / b) as i64),
      (Num::Float(a), Num::Int(b)) => Num::Int((a / (b as f64)) as i64),
    }))
  } else {
    Err(EvalError::OpError(format!(
      "quot: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_equal(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  Ok(Value::Bool(match args.len() {
    0 => true,
    _ => {
      let values = maybe_eval_all(env, args, eval_args)?;
      let first = &values[0];
      for i in 1..values.len() {
        if first != &values[i] {
          return Ok(Value::Bool(false));
        }
      }
      true
    }
  }))
}

pub fn quoot_numerical_equal(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  Ok(Value::Bool(match args.len() {
    0 => true,
    _ => {
      let values = maybe_eval_all(env, args, eval_args)?;
      let first = &values[0].as_num("==");
      for i in 1..values.len() {
        if first != &values[i].as_num("==") {
          return Ok(Value::Bool(false));
        }
      }
      true
    }
  }))
}

pub fn quoot_list_constructor(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  Ok(Value::from(maybe_eval_all(env, args, eval_args)?))
}

pub fn quoot_count(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 1 {
    match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
      Value::Nil => Ok(Value::from(0i64)),
      Value::List(List::Strict(list)) => Ok(Value::from(list.len())),
      Value::List(List::Lazy(list)) => {
        Ok(Value::from(list.fully_realize()?.realized_len()))
      }
      v => Err(EvalError::OpError(format!(
        "count: can't count type <{}>",
        v.type_string()
      ))),
    }
  } else {
    Err(EvalError::OpError(format!(
      "count: need 1 argument, got {}",
      args.len()
    )))
  }
}

pub fn quoot_cons(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::from(StrictList::unit(maybe_eval(
      env,
      args.pop_front().unwrap(),
      eval_args,
    )?))),
    2 => {
      let mut args_iter = args.into_iter();
      let first_arg = maybe_eval(env, args_iter.next().unwrap(), eval_args)?;
      let second_arg = maybe_eval(env, args_iter.next().unwrap(), eval_args)?;
      match second_arg {
        Value::List(List::Strict(mut strict_list)) => {
          strict_list.push_front(first_arg);
          Ok(Value::from(strict_list))
        }
        Value::List(List::Lazy(lazy_list)) => Ok(Value::from(LazyList::new(
          &|lazy_state| {
            let original_list = lazy_state
              .builder_values
              .as_ref()
              .unwrap()
              .get(0)
              .unwrap()
              .unwrap()
              .to_list("cons")?;
            match original_list
              .get(lazy_state.realized_values.len() as i64 - 1)?
            {
              Some(value) => lazy_state.realized_values.push_back(value),
              None => lazy_state.is_finished = true,
            }
            Ok(())
          },
          LazyState::new(
            {
              let state = lazy_list.state.read().unwrap();
              let mut prerealized_values = state.realized_values.clone();
              prerealized_values.push_front(first_arg);
              prerealized_values
            },
            Some(List::Strict(StrictList::unit(Value::List(List::Lazy(
              lazy_list,
            ))))),
            Some(env.clone()),
          ),
        ))),
        Value::Nil => Ok(Value::from(StrictList::unit(first_arg))),
        _ => Err(EvalError::OpError(format!(
          "cons: cannot cons onto a <{}>",
          second_arg.type_string()
        ))),
      }
    }
    n => Err(EvalError::OpError(format!(
      "cons: need 1 or 2 arguments, got {}",
      n
    ))),
  }
}

pub fn quoot_concat(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 0 {
    Ok(Value::from(StrictList::new()))
  } else {
    let mut values = maybe_eval_all(env, args, eval_args)?;
    let mut concat_list = StrictList::new();
    while let Some(list_value) = values.pop_front() {
      match list_value.to_list("concat")? {
        List::Strict(strict_list) => concat_list.append(strict_list),
        List::Lazy(lazy_list) => {
          if lazy_list.is_fully_realized() {
            concat_list.append(lazy_list.to_strict()?)
          } else {
            concat_list
              .append(lazy_list.state.read().unwrap().realized_values.clone());
            return Ok(Value::from(LazyList::new(
              &|lazy_state| {
                let mut builder_values_strict =
                  lazy_state.builder_values.as_ref().unwrap().as_strict()?;
                let mut outer_index = builder_values_strict
                  .get(0)
                  .unwrap()
                  .as_num("concat")?
                  .floor();
                let mut inner_index = builder_values_strict
                  .get(1)
                  .unwrap()
                  .as_num("concat")?
                  .floor();
                loop {
                  if outer_index as usize >= builder_values_strict.len() - 2 {
                    lazy_state.is_finished = true;
                    break;
                  }
                  let original_list = builder_values_strict
                    .get(outer_index as usize + 2)
                    .unwrap()
                    .clone()
                    .to_list("concat")?;
                  match original_list.get(inner_index)? {
                    Some(value) => {
                      lazy_state.realized_values.push_back(value);
                      builder_values_strict.set(0, Value::from(outer_index));
                      builder_values_strict
                        .set(1, Value::from(inner_index + 1));
                      lazy_state.builder_values =
                        Some(List::Strict(builder_values_strict));
                      break;
                    }
                    None => {
                      outer_index += 1;
                      inner_index = 0;
                    }
                  }
                }
                Ok(())
              },
              LazyState::new(
                concat_list,
                Some(List::Strict({
                  let mut state_values = StrictList::from(vec![
                    Value::from(0i64),
                    Value::from(lazy_list.realized_len()),
                    Value::from(lazy_list),
                  ]);
                  state_values.append(values);
                  state_values
                })),
                Some(env.clone()),
              ),
            )));
          }
        }
      }
    }
    Ok(Value::from(concat_list))
  }
}

pub fn quoot_get(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 2 {
    let mut args_iter = args.into_iter();
    match maybe_eval(env, args_iter.next().unwrap(), eval_args)? {
      Value::Nil => Ok(Value::Nil),
      Value::List(list) => {
        let index = maybe_eval(env, args_iter.next().unwrap(), eval_args)?
          .as_num("get")?
          .floor();
        match list.get(index)? {
          Some(value) => Ok(value),
          None => Err(EvalError::OutOfBoundsError(
            "get".to_string(),
            index,
            list.to_strict()?.len() as i64,
          )),
        }
      }
      other => Err(EvalError::OpError(format!(
        "get: cannot get value from <{}>",
        other.type_string()
      ))),
    }
  } else {
    Err(EvalError::OpError(format!(
      "get: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_update(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 3 {
    let mut args_iter = args.into_iter();
    let list = maybe_eval(env, args_iter.next().unwrap(), eval_args)?
      .to_list("update")?;
    let index = maybe_eval(env, args_iter.next().unwrap(), eval_args)?
      .as_num("update")?
      .floor() as usize;
    let op =
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?.to_op("update")?;
    match list {
      List::Strict(strict_list) => {
        if index >= strict_list.len() {
          return Err(EvalError::OutOfBoundsError(
            "update".to_string(),
            index as i64,
            strict_list.len() as i64,
          ));
        }
        let updated_value =
          op.apply(env, StrictList::unit(strict_list[index].clone()), false)?;
        let mut list_clone = strict_list.clone();
        list_clone.set(index, updated_value);
        Ok(Value::from(list_clone))
      }
      List::Lazy(lazy_list) => {
        lazy_list.realize_to(index + 1)?;
        if index >= lazy_list.realized_len() {
          return Err(EvalError::OutOfBoundsError(
            "update".to_string(),
            index as i64,
            lazy_list.realized_len() as i64,
          ));
        }
        let lazy_state_clone = lazy_list.state.read().unwrap().clone();
        let updated_value = op.apply(
          env,
          StrictList::unit(lazy_state_clone.realized_values[index].clone()),
          false,
        )?;
        let mut realized_values_clone = lazy_state_clone.realized_values;
        realized_values_clone.set(index, updated_value);
        Ok(Value::from(LazyList::new(
          &|lazy_state| {
            let original_list = lazy_state.builder_values.as_ref().unwrap();
            let index = lazy_state.realized_values.len() as i64;
            match original_list.get(index)? {
              Some(value) => lazy_state.realized_values.push_back(value),
              None => lazy_state.is_finished = true,
            }
            Ok(())
          },
          LazyState::new(
            realized_values_clone,
            Some(List::Lazy(lazy_list)),
            lazy_state_clone.captured_environment,
          ),
        )))
      }
    }
  } else {
    Err(EvalError::OpError(format!(
      "update: need 3 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_take(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 2 {
    let mut args_iter = args.into_iter();
    let n = 0.max(
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?
        .as_num("take")?
        .floor(),
    ) as usize;
    Ok(Value::List(
      match maybe_eval(env, args_iter.next().unwrap(), eval_args)?
        .to_list("take")?
      {
        List::Strict(strict_list) => List::Strict(if n >= strict_list.len() {
          strict_list
        } else {
          strict_list.take(n)
        }),
        List::Lazy(lazy_list) => List::Lazy(LazyList::new(
          &|lazy_state| {
            let builder_values = lazy_state.builder_values.as_ref().unwrap();
            let n = builder_values
              .get(0)
              .unwrap()
              .unwrap()
              .as_num("take")?
              .floor();
            if lazy_state.realized_values.len() as i64 >= n {
              lazy_state.is_finished = true;
            } else {
              let original_list =
                builder_values.get(1).unwrap().unwrap().to_list("take")?;
              match original_list.get(lazy_state.realized_values.len() as i64)?
              {
                Some(value) => {
                  lazy_state.realized_values.push_back(value);
                }
                None => lazy_state.is_finished = true,
              };
            }
            Ok(())
          },
          LazyState::new(
            {
              let state = lazy_list.state.read().unwrap();
              state
                .realized_values
                .take(n.min(state.realized_values.len()))
            },
            Some(List::Strict(StrictList::from(vec![
              Value::from(n),
              Value::from(lazy_list),
            ]))),
            Some(env.clone()),
          ),
        )),
      },
    ))
  } else {
    Err(EvalError::OpError(format!(
      "take: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_drop(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 2 {
    let mut args_iter = args.into_iter();
    let n = 0.max(
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?
        .as_num("take")?
        .floor(),
    ) as usize;
    Ok(Value::List(
      match maybe_eval(env, args_iter.next().unwrap(), eval_args)?
        .to_list("drop")?
      {
        List::Strict(strict_list) => {
          List::Strict(strict_list.skip(0.max(n) as usize))
        }
        List::Lazy(lazy_list) => List::Lazy(LazyList::new(
          &|lazy_state| {
            let builder_values = lazy_state.builder_values.as_ref().unwrap();
            let n = builder_values
              .get(0)
              .unwrap()
              .unwrap()
              .as_num("take")?
              .floor();
            let original_list =
              builder_values.get(1).unwrap().unwrap().to_list("take")?;
            match original_list
              .get(n + lazy_state.realized_values.len() as i64)?
            {
              Some(value) => {
                lazy_state.realized_values.push_back(value);
              }
              None => lazy_state.is_finished = true,
            };
            Ok(())
          },
          LazyState::new(
            {
              let state = lazy_list.state.read().unwrap();
              state
                .realized_values
                .skip(n.min(state.realized_values.len()))
            },
            Some(List::Strict(StrictList::from(vec![
              Value::from(n),
              Value::from(lazy_list),
            ]))),
            Some(env.clone()),
          ),
        )),
      },
    ))
  } else {
    Err(EvalError::OpError(format!(
      "drop: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_strict(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::from(
      maybe_eval(env, args.pop_front().unwrap(), eval_args)?
        .to_list("strict")?
        .to_strict()?,
    )),
    n => Err(EvalError::OpError(format!(
      "strict: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_range(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    0 => Ok(Value::from(LazyList::new(
      &|state| {
        state
          .realized_values
          .push_back(Value::from(state.realized_values.len()));
        Ok(())
      },
      LazyState::new(StrictList::new(), None, None),
    ))),
    1 => {
      let end = maybe_eval(env, args.pop_front().unwrap(), eval_args)?
        .as_num("range")?
        .floor();
      Ok(Value::from(
        (0..end).map(Value::from).collect::<StrictList>(),
      ))
    }
    2 => {
      let mut args_iter = args.into_iter();
      let start = maybe_eval(env, args_iter.next().unwrap(), eval_args)?
        .as_num("range")?
        .floor();
      let end = maybe_eval(env, args_iter.next().unwrap(), eval_args)?
        .as_num("range")?
        .floor();
      Ok(Value::from(
        (start..end).map(Value::from).collect::<StrictList>(),
      ))
    }
    n => Err(EvalError::OpError(format!(
      "range: need 0, 1, or 2 arguments, got {}",
      n
    ))),
  }
}

pub fn quoot_apply(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    0 => Err(EvalError::OpError(
      "apply: need 1 or 2 arguments, got 0".to_string(),
    )),
    1 => Ok(Value::Op(Op::Applied(Rc::from(
      maybe_eval(env, args.pop_front().unwrap(), eval_args)?.to_op("apply")?,
    )))),
    2 => {
      let mut args_iter = args.into_iter();
      match maybe_eval(env, args_iter.next().unwrap(), eval_args)? {
        Value::Op(op) => op.apply(
          env,
          maybe_eval(env, args_iter.next().unwrap(), eval_args)?
            .to_list("apply")?
            .to_strict()?,
          false,
        ),
        other => Err(EvalError::OpError(format!(
          "apply: cannot invoke type <{}>",
          other.type_string()
        ))),
      }
    }
    n => Err(EvalError::OpError(format!(
      "apply: need 1 or 2 arguments, got {}",
      n
    ))),
  }
}

pub fn quoot_identity(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 1 {
    Ok(maybe_eval(env, args.pop_front().unwrap(), eval_args)?)
  } else {
    Err(EvalError::OpError(format!(
      "identity: need 1 argument, got {}",
      args.len()
    )))
  }
}

pub fn quoot_compose(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    0 => Err(EvalError::OpError(format!(
      "compose: need at least 1 argument, got 0"
    ))),
    1 => Ok(Value::Op(
      maybe_eval(env, args.pop_front().unwrap(), eval_args)?
        .to_op("compose")?,
    )),
    _ => {
      let ops = args
        .into_iter()
        .rev()
        .map(|arg| maybe_eval(env, arg, eval_args)?.to_op("compose"))
        .collect::<Vec<Result<Op, EvalError>>>()
        .into_iter()
        .collect::<Result<Vec<Op>, EvalError>>()?;
      Ok(Value::Op(Op::Composition(ops)))
    }
  }
}

pub fn quoot_partial(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    0 => Err(EvalError::OpError(
      "partial: need at least 1 argument, got 0".to_string(),
    )),
    1 => Ok(Value::Op(
      maybe_eval(env, args.pop_front().unwrap(), eval_args)?
        .to_op("partial")?,
    )),
    _ => {
      let mut values = maybe_eval_all(env, args, eval_args)?;
      let f = values.pop_front().unwrap().to_op("partial")?;
      Ok(Value::Op(Op::Partial(Rc::from(f), values)))
    }
  }
}

pub fn quoot_is_nil(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
        Value::Nil => true,
        _ => false,
      },
    )),
    n => Err(EvalError::OpError(format!(
      "nil?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_bool(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
        Value::Bool(_) => true,
        _ => false,
      },
    )),
    n => Err(EvalError::OpError(format!(
      "bool?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_list(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
        Value::List(_) => true,
        _ => false,
      },
    )),
    n => Err(EvalError::OpError(format!(
      "list?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_num(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
        Value::Num(_) => true,
        _ => false,
      },
    )),
    n => Err(EvalError::OpError(format!(
      "num?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_nan(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
        Value::Num(Num::Float(f)) => f.is_nan(),
        _ => false,
      },
    )),
    n => Err(EvalError::OpError(format!(
      "nan?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_inf(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
        Value::Num(Num::Float(f)) => f.is_infinite(),
        _ => false,
      },
    )),
    n => Err(EvalError::OpError(format!(
      "inf?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_zero(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
        Value::Num(num) => match num {
          Num::Int(i) => i == 0,
          Num::Float(f) => f == 0.,
        },
        _ => false,
      },
    )),
    n => Err(EvalError::OpError(format!(
      "zero?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_neg(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
        Value::Num(num) => match num {
          Num::Int(i) => i < 0,
          Num::Float(f) => f < 0.,
        },
        _ => false,
      },
    )),
    n => Err(EvalError::OpError(format!(
      "neg?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_pos(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
        Value::Num(num) => match num {
          Num::Int(i) => i > 0,
          Num::Float(f) => f > 0.,
        },
        _ => false,
      },
    )),
    n => Err(EvalError::OpError(format!(
      "pos?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_string(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
        Value::String(_) => true,
        _ => false,
      },
    )),
    n => Err(EvalError::OpError(format!(
      "str?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_symbol(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
        Value::Symbol(_) => true,
        _ => false,
      },
    )),
    n => Err(EvalError::OpError(format!(
      "symbol?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_op(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
        Value::Op(_) => true,
        _ => false,
      },
    )),
    n => Err(EvalError::OpError(format!(
      "op?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_empty(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
        Value::Nil => true,
        Value::List(list) => list.is_empty()?,
        other => {
          return Err(EvalError::OpError(format!(
            "empty?: cannot check whether type {} is empty",
            other.type_string()
          )))
        }
      },
    )),
    n => Err(EvalError::OpError(format!(
      "empty?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_even(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)?
        .as_num("even?")?
      {
        Num::Int(i) => i % 2 == 0,
        Num::Float(f) => f % 2.0 == 0.0,
      },
    )),
    n => Err(EvalError::OpError(format!(
      "even?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_first(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
      Value::Nil => Ok(Value::Nil),
      Value::List(List::Strict(list)) => {
        Ok(list.front().map(Value::clone).unwrap_or(Value::Nil))
      }
      Value::List(List::Lazy(list)) => Ok({
        list.realize_to(1)?;
        match list.get(0)? {
          None => Value::Nil,
          Some(value) => value,
        }
      }),
      other => {
        return Err(EvalError::OpError(format!(
          "first: cannot get the first element of type <{}>",
          other.type_string()
        )))
      }
    },
    n => Err(EvalError::OpError(format!(
      "first: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_last(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => match maybe_eval(env, args.pop_front().unwrap(), eval_args)? {
      Value::Nil => Ok(Value::Nil),
      Value::List(List::Strict(list)) => {
        Ok(list.last().map(Value::clone).unwrap_or(Value::Nil))
      }
      Value::List(List::Lazy(list)) => Ok({
        list.fully_realize()?;
        list.get(list.realized_len() - 1)?.unwrap_or(Value::Nil)
      }),
      other => {
        return Err(EvalError::OpError(format!(
          "last: cannot get the last element of type <{}>",
          other.type_string()
        )))
      }
    },
    n => Err(EvalError::OpError(format!(
      "last: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_rest(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::List(
      maybe_eval(env, args.pop_front().unwrap(), eval_args)?
        .to_list("rest")?
        .rest()?,
    )),
    n => Err(EvalError::OpError(format!(
      "first: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_reverse(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => {
      let mut list = maybe_eval(env, args.pop_front().unwrap(), eval_args)?
        .to_list("reverse")?
        .to_strict()?;
      let mut new_list = StrictList::new();
      while let Some(value) = list.pop_front() {
        new_list.push_front(value);
      }
      Ok(Value::from(new_list))
    }
    n => Err(EvalError::OpError(format!(
      "reverse: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_bool(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      maybe_eval(env, args.pop_front().unwrap(), eval_args)?.as_bool(),
    )),
    n => Err(EvalError::OpError(format!(
      "bool: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_not(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Bool(
      !maybe_eval(env, args.pop_front().unwrap(), eval_args)?.as_bool(),
    )),
    n => Err(EvalError::OpError(format!(
      "bool: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_and(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    0 => Ok(Value::Bool(true)),
    _ => {
      let mut args_iter = args.into_iter();
      let mut value: Value =
        maybe_eval(env, args_iter.next().unwrap(), eval_args)?;
      loop {
        if !value.as_bool() {
          return Ok(value);
        }
        match args_iter.next() {
          Some(arg) => value = maybe_eval(env, arg, eval_args)?,
          None => return Ok(value),
        }
      }
    }
  }
}

pub fn quoot_or(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    0 => Ok(Value::Bool(true)),
    _ => {
      let mut args_iter = args.into_iter();
      let mut value: Value =
        maybe_eval(env, args_iter.next().unwrap(), eval_args)?;
      loop {
        if value.as_bool() {
          return Ok(value);
        }
        match args_iter.next() {
          Some(arg) => value = maybe_eval(env, arg, eval_args)?,
          None => return Ok(Value::Bool(false)),
        }
      }
    }
  }
}

pub fn quoot_int(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::from(
      maybe_eval(env, args.pop_front().unwrap(), eval_args)?
        .as_num("int")?
        .floor(),
    )),
    n => Err(EvalError::OpError(format!(
      "int: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_abs(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Num(
      match maybe_eval(env, args.pop_front().unwrap(), eval_args)?
        .as_num("abs")?
      {
        Num::Int(i) => Num::Int(i.abs()),
        Num::Float(f) => Num::Float(f.abs()),
      },
    )),
    n => Err(EvalError::OpError(format!(
      "abs: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_sqrt(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => Ok(Value::Num(Num::Float(
      maybe_eval(env, args.pop_front().unwrap(), eval_args)?
        .as_num("sqrt")?
        .as_float()
        .sqrt(),
    ))),
    n => Err(EvalError::OpError(format!(
      "sqrt: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_filter(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 2 {
    let mut args_iter = args.into_iter();
    let predicate =
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?.to_op("filter")?;
    let list = maybe_eval(env, args_iter.next().unwrap(), eval_args)?
      .to_list("filter")?;
    let mut initial_builder_values = StrictList::new();
    initial_builder_values.push_back(Value::from(0i64));
    initial_builder_values.push_back(Value::List(list));
    initial_builder_values.push_back(Value::Op(predicate));
    Ok(Value::from(LazyList::new(
      &|state| {
        let builder_values =
          state.builder_values.as_ref().unwrap().as_strict()?;
        let mut index =
          builder_values.front().unwrap().as_num("filter")?.floor();
        let original_list =
          builder_values.get(1).unwrap().clone().to_list("filter")?;
        let predicate =
          builder_values.get(2).unwrap().clone().to_op("filter")?;
        while let Some(value) = original_list.get(index)? {
          if predicate
            .apply(
              &state.captured_environment.as_ref().unwrap(),
              StrictList::unit(value.clone()),
              false,
            )?
            .as_bool()
          {
            let mut new_builder_values = StrictList::new();
            new_builder_values.push_back(Value::from(index + 1));
            new_builder_values.push_back(Value::List(original_list));
            new_builder_values.push_back(Value::Op(predicate));
            state.builder_values = Some(List::Strict(new_builder_values));
            state.realized_values.push_back(value);
            return Ok(());
          }
          index += 1;
        }
        state.is_finished = true;
        Ok(())
      },
      LazyState::new(
        StrictList::new(),
        Some(List::Strict(initial_builder_values)),
        Some(env.clone()),
      ),
    )))
  } else {
    Err(EvalError::OpError(format!(
      "filter: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_map(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() > 1 {
    let mut args_iter = args.into_iter();
    let mut map_values =
      vec![maybe_eval(env, args_iter.next().unwrap(), eval_args)?];
    while let Some(arg) = args_iter.next() {
      map_values.push(Value::List(
        maybe_eval(env, arg, eval_args)?.to_list("map")?,
      ));
    }
    Ok(Value::from(LazyList::new(
      &|state| {
        let index = state.realized_values.len() as i64;
        let builder_values =
          state.builder_values.as_ref().unwrap().as_strict()?;
        let op = builder_values.get(0).unwrap().clone().to_op("map")?;
        let mut op_args: Vec<Value> = vec![];
        for i in 1..builder_values.len() {
          match builder_values
            .get(i)
            .unwrap()
            .clone()
            .to_list("map")?
            .get(index)?
          {
            Some(value) => {
              op_args.push(value);
            }
            None => {
              state.is_finished = true;
              break;
            }
          }
        }
        if !state.is_finished {
          state.realized_values.push_back(op.apply(
            &state.captured_environment.as_ref().unwrap(),
            StrictList::from(op_args),
            false,
          )?);
        }
        Ok(())
      },
      LazyState::new(
        StrictList::new(),
        Some(List::Strict(StrictList::from(map_values))),
        Some(env.clone()),
      ),
    )))
  } else {
    Err(EvalError::OpError(format!(
      "map: need at least 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_greater(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 2 {
    let mut args_iter = args.into_iter();
    let a =
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?.as_num(">")?;
    let b =
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?.as_num(">")?;
    Ok(Value::Bool(a > b))
  } else {
    Err(EvalError::OpError(format!(
      ">: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_greater_or_equal(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 2 {
    let mut args_iter = args.into_iter();
    let a =
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?.as_num(">")?;
    let b =
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?.as_num(">")?;
    Ok(Value::Bool(a >= b))
  } else {
    Err(EvalError::OpError(format!(
      ">=: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_less(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 2 {
    let mut args_iter = args.into_iter();
    let a =
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?.as_num(">")?;
    let b =
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?.as_num(">")?;
    Ok(Value::Bool(a < b))
  } else {
    Err(EvalError::OpError(format!(
      "<: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_less_or_equal(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 2 {
    let mut args_iter = args.into_iter();
    let a =
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?.as_num(">")?;
    let b =
      maybe_eval(env, args_iter.next().unwrap(), eval_args)?.as_num(">")?;
    Ok(Value::Bool(a <= b))
  } else {
    Err(EvalError::OpError(format!(
      "<=: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_if(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  if args.len() == 3 {
    let mut args_iter = args.into_iter();
    if !maybe_eval(env, args_iter.next().unwrap(), eval_args)?.as_bool() {
      args_iter.next();
    }
    Ok(maybe_eval(env, args_iter.next().unwrap(), eval_args)?)
  } else {
    Err(EvalError::OpError(format!(
      "if: need 3 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_rand(
  _env: &Env,
  _args: StrictList,
  _eval_args: bool,
) -> Result<Value, EvalError> {
  Ok(Value::Num(Num::Float(rand::random::<f64>())))
}

pub fn quoot_repeat(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => {
      let value = maybe_eval(env, args.pop_front().unwrap(), eval_args)?;
      Ok(Value::from(LazyList::new(
        &|lazy_state| {
          lazy_state.realized_values.push_back(
            lazy_state.builder_values.as_ref().unwrap().as_strict()?[0].clone(),
          );
          Ok(())
        },
        LazyState::new(
          StrictList::new(),
          Some(List::Strict(StrictList::unit(value))),
          Some(env.clone()),
        ),
      )))
    }
    2 => {
      let mut args_iter = args.into_iter();
      let value = maybe_eval(env, args_iter.next().unwrap(), eval_args)?;
      let count = maybe_eval(env, args_iter.next().unwrap(), eval_args)?
        .as_num("repeat")?
        .floor();
      let mut values: Vec<Value> = vec![];
      for _ in 0..count {
        values.push(value.clone());
      }
      Ok(Value::from(StrictList::from(values)))
    }
    n => Err(EvalError::OpError(format!(
      "repeatedly: need 1 or 2 arguments, got {}",
      n
    ))),
  }
}

pub fn quoot_repeatedly(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => {
      let generator_op = maybe_eval(env, args.pop_front().unwrap(), eval_args)?
        .to_op("repeatedly")?;
      Ok(Value::from(LazyList::new(
        &|lazy_state| {
          let generator_op =
            lazy_state.builder_values.as_ref().unwrap().as_strict()?[0]
              .clone()
              .to_op("repeatedly")?;
          let value = generator_op.apply(
            &lazy_state.captured_environment.as_ref().unwrap(),
            StrictList::new(),
            false,
          )?;
          lazy_state.realized_values.push_back(value);
          Ok(())
        },
        LazyState::new(
          StrictList::new(),
          Some(List::Strict(StrictList::unit(Value::Op(generator_op)))),
          Some(env.clone()),
        ),
      )))
    }
    2 => {
      let mut args_iter = args.into_iter();
      let generator_op = maybe_eval(env, args_iter.next().unwrap(), eval_args)?
        .to_op("repeatedly")?;
      let count = maybe_eval(env, args_iter.next().unwrap(), eval_args)?
        .as_num("repeatedly")?
        .floor();
      let mut values: Vec<Value> = vec![];
      for _ in 0..count {
        values.push(generator_op.apply(&env, StrictList::new(), false)?)
      }
      Ok(Value::from(StrictList::from(values)))
    }
    n => Err(EvalError::OpError(format!(
      "repeatedly: need 1 or 2 arguments, got {}",
      n
    ))),
  }
}

pub fn quoot_iterate(
  env: &Env,
  args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    2 => {
      let mut args_iter = args.into_iter();
      let op = maybe_eval(env, args_iter.next().unwrap(), eval_args)?
        .to_op("iterate")?;
      let initial_value =
        maybe_eval(env, args_iter.next().unwrap(), eval_args)?;
      Ok(Value::from(LazyList::new(
        &|lazy_state| {
          let generator_op =
            lazy_state.builder_values.as_ref().unwrap().as_strict()?[0]
              .clone()
              .to_op("iterate")?;
          let value = generator_op.apply(
            &lazy_state.captured_environment.as_ref().unwrap(),
            StrictList::unit(
              lazy_state.realized_values.last().unwrap().clone(),
            ),
            false,
          )?;
          lazy_state.realized_values.push_back(value);
          Ok(())
        },
        LazyState::new(
          StrictList::unit(initial_value),
          Some(List::Strict(StrictList::unit(Value::Op(op)))),
          Some(env.clone()),
        ),
      )))
    }
    3 => {
      let mut args_iter = args.into_iter();
      let op = maybe_eval(env, args_iter.next().unwrap(), eval_args)?
        .to_op("iterate")?;
      let mut value = maybe_eval(env, args_iter.next().unwrap(), eval_args)?;
      let count = maybe_eval(env, args_iter.next().unwrap(), eval_args)?
        .as_num("iterate")?
        .floor();
      for _ in 0..count as usize {
        value = op.apply(env, StrictList::unit(value), false)?;
      }
      Ok(value)
    }
    n => Err(EvalError::OpError(format!(
      "repeatedly: need 2 or 3 arguments, got {}",
      n
    ))),
  }
}

pub fn quoot_time(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => {
      let timer = Instant::now();
      let value = maybe_eval(env, args.pop_front().unwrap(), eval_args)?;
      println!("{:.2?}", timer.elapsed());
      Ok(value)
    }
    n => Err(EvalError::OpError(format!(
      "time: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_print(
  env: &Env,
  mut args: StrictList,
  eval_args: bool,
) -> Result<Value, EvalError> {
  match args.len() {
    1 => {
      let value = maybe_eval(env, args.pop_front().unwrap(), eval_args)?;
      println!("{}", value);
      Ok(value)
    }
    n => Err(EvalError::OpError(format!(
      "print: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn default_bindings() -> Bindings {
  let mut bindings = Bindings::new();
  [
    ("TAU", 6.283185307179586),
    ("#inf", f64::INFINITY),
    ("#-inf", -f64::INFINITY),
    ("#nan", f64::NAN),
  ]
  .iter()
  .for_each(|(name, value)| {
    bindings.insert(name.to_string(), Value::from(*value as f64));
  });
  let operator_bindings: &[(
    &str,
    fn(&Env, StrictList, bool) -> Result<Value, EvalError>,
  )] = &[
    ("time", quoot_time),
    ("print", quoot_print),
    ("let", quoot_let),
    ("eval", quoot_eval),
    ("quote", quoot_quote),
    ("op", quoot_op),
    ("fn", quoot_fn),
    ("inc", quoot_inc),
    ("dec", quoot_dec),
    ("+", quoot_add),
    ("*", quoot_multiply),
    ("=", quoot_equal),
    ("==", quoot_numerical_equal),
    ("-", quoot_subtract),
    ("/", quoot_divide),
    ("min", quoot_min),
    ("max", quoot_max),
    ("mod", quoot_modulo),
    ("quot", quoot_quotient),
    ("list", quoot_list_constructor),
    ("#list", quoot_list_constructor),
    ("count", quoot_count),
    ("cons", quoot_cons),
    ("concat", quoot_concat),
    ("get", quoot_get),
    ("update", quoot_update),
    ("take", quoot_take),
    ("drop", quoot_drop),
    ("strict", quoot_strict),
    ("range", quoot_range),
    ("identity", quoot_identity),
    ("apply", quoot_apply),
    ("partial", quoot_partial),
    ("|", quoot_partial),
    ("compose", quoot_compose),
    (".", quoot_compose),
    ("nil?", quoot_is_nil),
    ("bool?", quoot_is_bool),
    ("list?", quoot_is_list),
    ("num?", quoot_is_num),
    ("nan?", quoot_is_nan),
    ("inf?", quoot_is_inf),
    ("zero?", quoot_is_zero),
    ("neg?", quoot_is_neg),
    ("pos?", quoot_is_pos),
    ("str?", quoot_is_string),
    ("symbol?", quoot_is_symbol),
    ("op?", quoot_is_op),
    ("empty?", quoot_is_empty),
    ("even?", quoot_is_even),
    ("bool", quoot_bool),
    ("not", quoot_not),
    ("and", quoot_and),
    ("or", quoot_or),
    ("int", quoot_int),
    ("abs", quoot_abs),
    ("sqrt", quoot_sqrt),
    ("first", quoot_first),
    ("last", quoot_last),
    ("rest", quoot_rest),
    ("reverse", quoot_reverse),
    ("filter", quoot_filter),
    ("map", quoot_map),
    (">", quoot_greater),
    (">=", quoot_greater_or_equal),
    ("<", quoot_less),
    ("<=", quoot_less_or_equal),
    ("if", quoot_if),
    ("rand", quoot_rand),
    ("repeatedly", quoot_repeatedly),
    ("repeat", quoot_repeat),
    ("iterate", quoot_iterate),
  ];
  operator_bindings.iter().for_each(|(name, op)| {
    bindings.insert(name.to_string(), Value::Op(Op::Core(CoreOp::new(op))));
  });
  bindings
}
