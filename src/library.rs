use std::time::Instant;

use crate::model::{
  eval, maybe_eval, Bindings, Env, Num, QuootEvalError, QuootLazyList,
  QuootLazyState, QuootList, QuootOp, QuootStrictList, QuootValue,
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

fn maybe_eval_all(
  env: &Env,
  values: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootStrictList, QuootEvalError> {
  if eval_args {
    Ok(QuootStrictList::from(
      values
        .iter()
        .map(|value| eval(env, value))
        .collect::<Vec<Result<QuootValue, QuootEvalError>>>()
        .into_iter()
        .collect::<Result<Vec<QuootValue>, QuootEvalError>>()?,
    ))
  } else {
    Ok(values.clone())
  }
}

pub fn partial(op: QuootOp, prefix_args: QuootStrictList) -> QuootOp {
  QuootOp::new(Box::leak(Box::new(
    move |_op_self: &QuootOp,
          env: &Env,
          args: &QuootStrictList,
          eval_args: bool| {
      let mut cloned_args = prefix_args.clone();
      cloned_args.append(args.to_owned());
      (op.f)(&op, env, &cloned_args, eval_args)
    },
  )))
}

pub fn quoot_let(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let bindings = args.front().unwrap();
    match bindings {
      QuootValue::List(QuootList::Strict(list)) => {
        let mut list_clone = QuootList::deliteralize(list.clone());
        if list_clone.len() % 2 == 0 {
          let mut sub_env = env.clone();
          while let Some(binding_name) = list_clone.pop_front() {
            match binding_name {
              QuootValue::Symbol(name) => {
                let binding_value =
                  maybe_eval(env, &list_clone.pop_front().unwrap(), eval_args)?;
                sub_env.bind(&name, binding_value);
              }
              other => {
                return Err(QuootEvalError::OperatorError(format!(
                  "let: names must be symbols, got <{}>",
                  other.type_string()
                )))
              }
            }
          }
          maybe_eval(&sub_env, args.get(1).unwrap(), eval_args)
        } else {
          Err(QuootEvalError::OperatorError(format!(
            "let: first argument needs an even number of forms"
          )))
        }
      }
      other => Err(QuootEvalError::OperatorError(format!(
        "let: first argument must be a list literal, got <{}>",
        other.type_string()
      ))),
    }
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "let: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_eval(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => eval(env, &maybe_eval(env, args.front().unwrap(), eval_args)?),
    2 => todo!(),
    _ => Err(QuootEvalError::OperatorError(format!(
      "inc: need 1 or 2 arguments, got {}",
      args.len()
    ))),
  }
}

pub fn quoot_quote(
  _op_self: &QuootOp,
  _env: &Env,
  args: &QuootStrictList,
  _eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(args.front().unwrap().to_owned()),
    _ => Err(QuootEvalError::OperatorError(format!(
      "quote: need 1 argument, got {}",
      args.len()
    ))),
  }
}

pub fn quoot_operator(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  _eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    2 => {
      let env_clone = env.clone();
      let arg_names = QuootList::deliteralize(
        args.front().unwrap().as_list("operator")?.as_strict()?,
      )
      .iter()
      .map(|value| match value.clone() {
        QuootValue::Symbol(name) => Ok(name),
        other => Err(QuootEvalError::OperatorError(format!(
          "operator: first argument must be a list of symbols, found a <{}> in \
          list",
          other.type_string()
        ))),
      })
      .collect::<Vec<Result<String, QuootEvalError>>>()
      .into_iter()
      .collect::<Result<Vec<String>, QuootEvalError>>()?;
      let body = args.get(1).unwrap().to_owned();
      Ok(QuootValue::Op(QuootOp::new(Box::leak(Box::new(
        move |_op_self: &QuootOp,
              _application_env: &Env,
              application_args: &QuootStrictList,
              _application_eval_args: bool| {
          if application_args.len() != arg_names.len() {
            return Err(QuootEvalError::OperatorError(format!(
              "<Operator>: operator needs {} arguments, got {}",
              arg_names.len(),
              application_args.len()
            )));
          }
          let mut body_env = env_clone.clone();
          for i in 0..arg_names.len() {
            body_env
              .bind(arg_names[i].as_str(), application_args[i].to_owned());
          }
          eval(&body_env, &body)
        },
      )))))
    }
    _ => Err(QuootEvalError::OperatorError(format!(
      "operator: need 2 arguments, got {}",
      args.len()
    ))),
  }
}

pub fn quoot_fn(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  _eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    2 => {
      let env_clone = env.clone();
      let arg_names = QuootList::deliteralize(
        args.front().unwrap().as_list("fn")?.as_strict()?,
      )
      .iter()
      .map(|value| match value {
        QuootValue::Symbol(name) => Ok(name.clone()),
        other => Err(QuootEvalError::OperatorError(format!(
          "fn: with 2 arguments, first argument must be a list of symbols, \
          found a <{}> in list",
          other.type_string()
        ))),
      })
      .collect::<Vec<Result<String, QuootEvalError>>>()
      .into_iter()
      .collect::<Result<Vec<String>, QuootEvalError>>()?;
      let body = args.get(1).unwrap().to_owned();
      Ok(QuootValue::Op(QuootOp::new(Box::leak(Box::new(
        move |_op_self: &QuootOp,
              application_env: &Env,
              application_args: &QuootStrictList,
              application_eval_args: bool| {
          if application_args.len() != arg_names.len() {
            return Err(QuootEvalError::OperatorError(format!(
              "<Function>: fn needs {} arguments, got {}",
              arg_names.len(),
              application_args.len()
            )));
          }
          let maybe_evaled_args = maybe_eval_all(
            application_env,
            application_args,
            application_eval_args,
          )?;
          let mut body_env = env_clone.clone();
          for i in 0..arg_names.len() {
            body_env
              .bind(arg_names[i].as_str(), maybe_evaled_args[i].to_owned());
          }
          eval(&body_env, &body)
        },
      )))))
    }
    3 => {
      let env_clone = env.clone();
      let fn_name = match args.front().unwrap() {
        QuootValue::Symbol(name) => name.to_string(),
        other => {
          return Err(QuootEvalError::OperatorError(format!(
            "fn: with 3 arguments, first argument must be a list of symbols, \
            got a <{}>",
            other.type_string()
          )))
        }
      };
      let arg_names = QuootList::deliteralize(
        args.get(1).unwrap().as_list("fn")?.as_strict()?,
      )
      .iter()
      .map(|value| match value {
        QuootValue::Symbol(name) => Ok(name.clone()),
        other => Err(QuootEvalError::OperatorError(format!(
          "fn: with 3 arguments, second argument must be a list of symbols, \
          found a <{}> in list",
          other.type_string()
        ))),
      })
      .collect::<Vec<Result<String, QuootEvalError>>>()
      .into_iter()
      .collect::<Result<Vec<String>, QuootEvalError>>()?;
      let body = args.get(2).unwrap().to_owned();
      let op = QuootOp::new(Box::leak(Box::new(
        move |op_self: &QuootOp,
              application_env: &Env,
              application_args: &QuootStrictList,
              application_eval_args: bool| {
          if application_args.len() != arg_names.len() {
            return Err(QuootEvalError::OperatorError(format!(
              "<Function>: fn needs {} arguments, got {}",
              arg_names.len(),
              application_args.len()
            )));
          }
          let maybe_evaled_args = maybe_eval_all(
            application_env,
            application_args,
            application_eval_args,
          )?;
          let mut body_env = env_clone.clone();
          body_env.bind(&fn_name, QuootValue::Op(op_self.clone()));
          for i in 0..arg_names.len() {
            body_env
              .bind(arg_names[i].as_str(), maybe_evaled_args[i].to_owned());
          }
          eval(&body_env, &body)
        },
      )));
      Ok(QuootValue::Op(op))
    }
    _ => Err(QuootEvalError::OperatorError(format!(
      "fn: need 2 or 3 arguments, got {}",
      args.len()
    ))),
  }
}

pub fn quoot_inc(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 1 {
    Ok(QuootValue::Num(
      match maybe_eval(env, args.front().unwrap(), eval_args)?.as_num("inc")? {
        Num::Int(i) => Num::Int(i + 1),
        Num::Float(f) => Num::Float(f + 1.0),
      },
    ))
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "inc: need 1 argument, got {}",
      args.len()
    )))
  }
}

pub fn quoot_dec(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 1 {
    Ok(QuootValue::Num(
      match maybe_eval(env, args.front().unwrap(), eval_args)?.as_num("inc")? {
        Num::Int(i) => Num::Int(i - 1),
        Num::Float(f) => Num::Float(f - 1.0),
      },
    ))
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "inc: need 1 argument, got {}",
      args.len()
    )))
  }
}

pub fn quoot_add(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::Num(value_sum(
    maybe_eval_all(env, args, eval_args)?,
    "+",
  )?))
}

pub fn quoot_multiply(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::Num(value_product(
    maybe_eval_all(env, args, eval_args)?,
    "*",
  )?))
}

pub fn quoot_subtract(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  let mut args_clone = args.clone();
  match args_clone.pop_front() {
    None => Err(QuootEvalError::OperatorError(
      "-: need at least one argument".to_owned(),
    )),
    Some(value) => {
      let first_num = maybe_eval(env, &value, eval_args)?.as_num("-")?;
      Ok(QuootValue::Num(if args_clone.is_empty() {
        match first_num {
          Num::Int(i) => Num::Int(-i),
          Num::Float(f) => Num::Float(-f),
        }
      } else {
        match (
          first_num,
          value_sum(maybe_eval_all(env, &args_clone, eval_args)?, "-")?,
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
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  let mut args_clone = args.clone();
  match args_clone.pop_front() {
    None => Err(QuootEvalError::OperatorError(
      "/: need at least 1 argument".to_owned(),
    )),
    Some(value) => {
      let first_num = maybe_eval(env, &value, eval_args)?.as_num("/")?;
      Ok(QuootValue::Num(if args_clone.is_empty() {
        Num::Float(1.0 / first_num.as_float())
      } else {
        Num::Float(
          first_num.as_float()
            / value_product(maybe_eval_all(env, &args_clone, eval_args)?, "/")?
              .as_float(),
        )
      }))
    }
  }
}

pub fn quoot_min(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Err(QuootEvalError::OperatorError(
      "min: need at least 1 argument".to_owned(),
    )),
    _ => Ok(QuootValue::Num(value_min(
      maybe_eval_all(env, args, eval_args)?,
      "min",
    )?)),
  }
}

pub fn quoot_max(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Err(QuootEvalError::OperatorError(
      "max: need at least 1 argument".to_owned(),
    )),
    _ => Ok(QuootValue::Num(value_max(
      maybe_eval_all(env, args, eval_args)?,
      "max",
    )?)),
  }
}

pub fn quoot_modulo(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let dividend =
      maybe_eval(env, args.front().unwrap(), eval_args)?.as_num("mod")?;
    let divisor =
      maybe_eval(env, args.get(1).unwrap(), eval_args)?.as_num("mod")?;
    Ok(QuootValue::Num(match (dividend, divisor) {
      (Num::Int(a), Num::Int(b)) => Num::Int(a % b),
      (Num::Float(a), Num::Float(b)) => Num::Float(a % b),
      (Num::Int(a), Num::Float(b)) => Num::Float((a as f64) % b),
      (Num::Float(a), Num::Int(b)) => Num::Float(a % (b as f64)),
    }))
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "mod: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_quotient(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let dividend =
      maybe_eval(env, args.front().unwrap(), eval_args)?.as_num("quot")?;
    let divisor =
      maybe_eval(env, args.get(1).unwrap(), eval_args)?.as_num("quot")?;
    Ok(QuootValue::Num(match (dividend, divisor) {
      (Num::Int(a), Num::Int(b)) => Num::Int(a / b),
      (Num::Float(a), Num::Float(b)) => Num::Int((a / b) as i64),
      (Num::Int(a), Num::Float(b)) => Num::Int(((a as f64) / b) as i64),
      (Num::Float(a), Num::Int(b)) => Num::Int((a / (b as f64)) as i64),
    }))
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "quot: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_equal(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::Bool(match args.len() {
    0 => true,
    _ => {
      let values = maybe_eval_all(env, args, eval_args)?;
      let first = &values[0];
      for i in 1..values.len() {
        if first != &values[i] {
          return Ok(QuootValue::Bool(false));
        }
      }
      true
    }
  }))
}

pub fn quoot_numerical_equal(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::Bool(match args.len() {
    0 => true,
    _ => {
      let values = maybe_eval_all(env, args, eval_args)?;
      let first = &values[0].as_num("==");
      for i in 1..values.len() {
        if first != &values[i].as_num("==") {
          return Ok(QuootValue::Bool(false));
        }
      }
      true
    }
  }))
}

pub fn quoot_list_constructor(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::from(maybe_eval_all(env, args, eval_args)?))
}

pub fn quoot_count(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 1 {
    match maybe_eval(env, args.front().unwrap(), eval_args)? {
      QuootValue::Nil => Ok(QuootValue::from(0i64)),
      QuootValue::List(QuootList::Strict(list)) => {
        Ok(QuootValue::from(list.len()))
      }
      QuootValue::List(QuootList::Lazy(list)) => {
        Ok(QuootValue::from(list.fully_realize()?.realized_len()))
      }
      v => Err(QuootEvalError::OperatorError(format!(
        "count: can't count type <{}>",
        v.type_string()
      ))),
    }
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "count: need 1 argument, got {}",
      args.len()
    )))
  }
}

pub fn quoot_cons(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::from(QuootStrictList::unit(maybe_eval(
      env,
      args.front().unwrap(),
      eval_args,
    )?))),
    2 => {
      let first_arg = maybe_eval(env, args.front().unwrap(), eval_args)?;
      let second_arg = maybe_eval(env, args.get(1).unwrap(), eval_args)?;
      match second_arg {
        QuootValue::List(QuootList::Strict(strict_list)) => {
          let mut list_clone = strict_list.clone();
          list_clone.push_front(first_arg);
          Ok(QuootValue::from(list_clone))
        }
        QuootValue::List(QuootList::Lazy(lazy_list)) => {
          Ok(QuootValue::from(QuootLazyList::new(
            &|lazy_state| {
              let original_list = lazy_state
                .builder_values
                .as_ref()
                .unwrap()
                .get(0)
                .unwrap()
                .unwrap()
                .as_list("cons")?;
              match original_list
                .get(lazy_state.realized_values.len() as i64 - 1)?
              {
                Some(value) => lazy_state.realized_values.push_back(value),
                None => lazy_state.is_finished = true,
              }
              Ok(())
            },
            QuootLazyState::new(
              {
                let state = lazy_list.state.read().unwrap();
                let mut prerealized_values = state.realized_values.clone();
                prerealized_values.push_front(first_arg);
                prerealized_values
              },
              Some(QuootList::Strict(QuootStrictList::unit(QuootValue::List(
                QuootList::Lazy(lazy_list),
              )))),
              Some(env.clone()),
            ),
          )))
        }
        QuootValue::Nil => {
          Ok(QuootValue::from(QuootStrictList::unit(first_arg)))
        }
        _ => Err(QuootEvalError::OperatorError(format!(
          "cons: cannot cons onto a <{}>",
          second_arg.type_string()
        ))),
      }
    }
    n => Err(QuootEvalError::OperatorError(format!(
      "cons: need 1 or 2 arguments, got {}",
      n
    ))),
  }
}

pub fn quoot_concat(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 0 {
    Ok(QuootValue::from(QuootStrictList::new()))
  } else {
    let mut values = maybe_eval_all(env, args, eval_args)?;
    let mut concat_list = QuootStrictList::new();
    while let Some(list_value) = values.pop_front() {
      match list_value.as_list("concat")? {
        QuootList::Strict(strict_list) => concat_list.append(strict_list),
        QuootList::Lazy(lazy_list) => {
          if lazy_list.is_fully_realized() {
            concat_list.append(lazy_list.as_strict()?)
          } else {
            concat_list
              .append(lazy_list.state.read().unwrap().realized_values.clone());
            return Ok(QuootValue::from(QuootLazyList::new(
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
                    .as_list("concat")?;
                  match original_list.get(inner_index)? {
                    Some(value) => {
                      lazy_state.realized_values.push_back(value);
                      builder_values_strict
                        .set(0, QuootValue::from(outer_index));
                      builder_values_strict
                        .set(1, QuootValue::from(inner_index + 1));
                      lazy_state.builder_values =
                        Some(QuootList::Strict(builder_values_strict));
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
              QuootLazyState::new(
                concat_list,
                Some(QuootList::Strict({
                  let mut state_values = QuootStrictList::from(vec![
                    QuootValue::from(0i64),
                    QuootValue::from(lazy_list.realized_len()),
                    QuootValue::from(lazy_list),
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
    Ok(QuootValue::from(concat_list))
  }
}

pub fn quoot_get(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    match maybe_eval(env, args.front().unwrap(), eval_args)? {
      QuootValue::Nil => Ok(QuootValue::Nil),
      QuootValue::List(list) => {
        let index = maybe_eval(env, args.get(1).unwrap(), eval_args)?
          .as_num("get")?
          .floor();
        match list.get(index)? {
          Some(value) => Ok(value),
          None => Err(QuootEvalError::OutOfBoundsError(
            "get".to_owned(),
            index,
            list.as_strict()?.len() as i64,
          )),
        }
      }
      other => Err(QuootEvalError::OperatorError(format!(
        "get: cannot get value from <{}>",
        other.type_string()
      ))),
    }
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "get: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_update(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 3 {
    let list = maybe_eval(env, &args[0], eval_args)?.as_list("update")?;
    let index = maybe_eval(env, &args[1], eval_args)?
      .as_num("update")?
      .floor() as usize;
    let op = maybe_eval(env, &args[2], eval_args)?.as_op("update")?;
    match list {
      QuootList::Strict(strict_list) => {
        if index >= strict_list.len() {
          return Err(QuootEvalError::OutOfBoundsError(
            "update".to_owned(),
            index as i64,
            strict_list.len() as i64,
          ));
        }
        let updated_value = (op.f)(
          &op,
          env,
          &QuootStrictList::unit(strict_list[index].clone()),
          false,
        )?;
        let mut list_clone = strict_list.clone();
        list_clone.set(index, updated_value);
        Ok(QuootValue::from(list_clone))
      }
      QuootList::Lazy(lazy_list) => {
        lazy_list.realize_to(index + 1)?;
        if index >= lazy_list.realized_len() {
          return Err(QuootEvalError::OutOfBoundsError(
            "update".to_owned(),
            index as i64,
            lazy_list.realized_len() as i64,
          ));
        }
        let lazy_state = lazy_list.state.read().unwrap();
        let updated_value = (op.f)(
          &op,
          env,
          &QuootStrictList::unit(lazy_state.realized_values[index].clone()),
          false,
        )?;
        let mut realized_clone = lazy_state.realized_values.clone();
        realized_clone.set(index, updated_value);
        Ok(QuootValue::from(QuootLazyList::new(
          &|lazy_state| {
            let original_list = lazy_state.builder_values.as_ref().unwrap();
            let index = lazy_state.realized_values.len() as i64;
            match original_list.get(index)? {
              Some(value) => lazy_state.realized_values.push_back(value),
              None => lazy_state.is_finished = true,
            }
            Ok(())
          },
          QuootLazyState::new(
            realized_clone,
            Some(QuootList::Lazy(lazy_list.to_owned())),
            lazy_state.captured_environment.to_owned(),
          ),
        )))
      }
    }
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "update: need 3 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_take(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let n = 0.max(
      maybe_eval(env, args.front().unwrap(), eval_args)?
        .as_num("take")?
        .floor(),
    ) as usize;
    Ok(QuootValue::List(
      match maybe_eval(env, args.get(1).unwrap(), eval_args)?.as_list("take")? {
        QuootList::Strict(strict_list) => {
          QuootList::Strict(if n >= strict_list.len() {
            strict_list
          } else {
            strict_list.take(n)
          })
        }
        QuootList::Lazy(lazy_list) => QuootList::Lazy(QuootLazyList::new(
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
                builder_values.get(1).unwrap().unwrap().as_list("take")?;
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
          QuootLazyState::new(
            {
              let state = lazy_list.state.read().unwrap();
              state
                .realized_values
                .take(n.min(state.realized_values.len()))
            },
            Some(QuootList::Strict(QuootStrictList::from(vec![
              QuootValue::from(n),
              QuootValue::from(lazy_list),
            ]))),
            Some(env.clone()),
          ),
        )),
      },
    ))
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "take: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_drop(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let n = 0.max(
      maybe_eval(env, args.front().unwrap(), eval_args)?
        .as_num("take")?
        .floor(),
    ) as usize;
    Ok(QuootValue::List(
      match maybe_eval(env, args.get(1).unwrap(), eval_args)?.as_list("drop")? {
        QuootList::Strict(strict_list) => {
          QuootList::Strict(strict_list.skip(0.max(n) as usize))
        }
        QuootList::Lazy(lazy_list) => QuootList::Lazy(QuootLazyList::new(
          &|lazy_state| {
            let builder_values = lazy_state.builder_values.as_ref().unwrap();
            let n = builder_values
              .get(0)
              .unwrap()
              .unwrap()
              .as_num("take")?
              .floor();
            let original_list =
              builder_values.get(1).unwrap().unwrap().as_list("take")?;
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
          QuootLazyState::new(
            {
              let state = lazy_list.state.read().unwrap();
              state
                .realized_values
                .skip(n.min(state.realized_values.len()))
            },
            Some(QuootList::Strict(QuootStrictList::from(vec![
              QuootValue::from(n),
              QuootValue::from(lazy_list),
            ]))),
            Some(env.clone()),
          ),
        )),
      },
    ))
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "drop: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_strict(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::from(
      maybe_eval(env, args.front().unwrap(), eval_args)?
        .as_list("strict")?
        .as_strict()?,
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "strict: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_range(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Ok(QuootValue::from(QuootLazyList::new(
      &|state| {
        state
          .realized_values
          .push_back(QuootValue::from(state.realized_values.len()));
        Ok(())
      },
      QuootLazyState::new(QuootStrictList::new(), None, None),
    ))),
    1 => {
      let end = maybe_eval(env, args.front().unwrap(), eval_args)?
        .as_num("range")?
        .floor();
      Ok(QuootValue::from(
        (0..end).map(QuootValue::from).collect::<QuootStrictList>(),
      ))
    }
    2 => {
      let start = maybe_eval(env, args.front().unwrap(), eval_args)?
        .as_num("range")?
        .floor();
      let end = maybe_eval(env, args.get(1).unwrap(), eval_args)?
        .as_num("range")?
        .floor();
      Ok(QuootValue::from(
        (start..end)
          .map(QuootValue::from)
          .collect::<QuootStrictList>(),
      ))
    }
    n => Err(QuootEvalError::OperatorError(format!(
      "range: need 0, 1, or 2 arguments, got {}",
      n
    ))),
  }
}

pub fn quoot_apply(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Err(QuootEvalError::OperatorError(
      "apply: need 1 or 2 arguments, got 0".to_string(),
    )),
    1 => {
      let op =
        maybe_eval(env, args.front().unwrap(), eval_args)?.as_op("apply")?;
      let env_clone = env.clone();
      Ok(QuootValue::Op(QuootOp::new(Box::leak(Box::new(
        move |_op_self: &QuootOp,
              _inner_env: &Env,
              inner_args: &QuootStrictList,
              _inner_eval_args: bool| {
          if inner_args.len() == 1 {
            (op.f)(
              &op,
              &env_clone,
              &maybe_eval(
                &env_clone,
                inner_args.front().unwrap(),
                _inner_eval_args,
              )?
              .as_list("apply")?
              .as_strict()?,
              _inner_eval_args,
            )
          } else {
            Err(QuootEvalError::OperatorError(format!(
              "apply: function constructed with 1-argument apply call \
              needs 1 argument, got {}",
              inner_args.len(),
            )))
          }
        },
      )))))
    }
    2 => match maybe_eval(env, args.front().unwrap(), eval_args)? {
      QuootValue::Op(op) => (op.f)(
        &op,
        env,
        &maybe_eval(env, args.get(1).unwrap(), eval_args)?
          .as_list("apply")?
          .as_strict()?,
        eval_args,
      ),
      other => Err(QuootEvalError::OperatorError(format!(
        "apply: cannot invoke type <{}>",
        other.type_string()
      ))),
    },
    n => Err(QuootEvalError::OperatorError(format!(
      "apply: need 1 or 2 arguments, got {}",
      n
    ))),
  }
}

pub fn quoot_identity(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 1 {
    Ok(maybe_eval(env, &args.front().unwrap(), eval_args)?)
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "identity: need 1 argument, got {}",
      args.len()
    )))
  }
}

pub fn quoot_compose(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Ok(QuootValue::Op(QuootOp::new(&quoot_identity))),
    1 => Ok(QuootValue::Op(
      maybe_eval(env, args.front().unwrap(), eval_args)?.as_op("compose")?,
    )),
    _ => {
      let ops = args
        .iter()
        .rev()
        .map(|arg| maybe_eval(env, arg, eval_args)?.as_op("compose"))
        .collect::<Vec<Result<QuootOp, QuootEvalError>>>()
        .into_iter()
        .collect::<Result<Vec<QuootOp>, QuootEvalError>>()?;
      Ok(QuootValue::Op(QuootOp::new(Box::leak(Box::new(
        move |_op_self: &QuootOp,
              inner_env: &Env,
              inner_args: &QuootStrictList,
              inner_eval_args: bool| {
          let op = &ops[0];
          let mut value = (op.f)(
            op,
            inner_env,
            &maybe_eval_all(
              inner_env,
              inner_args,
              eval_args && inner_eval_args,
            )?,
            false,
          )?;
          for op in &ops[1..] {
            value =
              (op.f)(&op, inner_env, &QuootStrictList::unit(value), false)?;
          }
          Ok(value)
        },
      )))))
    }
  }
}

pub fn quoot_partial(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Err(QuootEvalError::OperatorError(
      "partial: need at least 1 argument, got 0".to_owned(),
    )),
    1 => Ok(QuootValue::Op(
      maybe_eval(env, args.front().unwrap(), eval_args)?.as_op("partial")?,
    )),
    _ => {
      let mut values = maybe_eval_all(env, args, eval_args)?;
      let f = values.pop_front().unwrap().as_op("partial")?;
      Ok(QuootValue::Op(partial(f, values)))
    }
  }
}

pub fn quoot_is_nil(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      match maybe_eval(env, args.front().unwrap(), eval_args)? {
        QuootValue::Nil => true,
        _ => false,
      },
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "nil?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_bool(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      match maybe_eval(env, args.front().unwrap(), eval_args)? {
        QuootValue::Bool(_) => true,
        _ => false,
      },
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "bool?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_list(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      match maybe_eval(env, args.front().unwrap(), eval_args)? {
        QuootValue::List(_) => true,
        _ => false,
      },
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "list?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_num(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      match maybe_eval(env, args.front().unwrap(), eval_args)? {
        QuootValue::Num(_) => true,
        _ => false,
      },
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "num?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_nan(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      match maybe_eval(env, args.front().unwrap(), eval_args)? {
        QuootValue::Num(Num::Float(f)) => f.is_nan(),
        _ => false,
      },
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "nan?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_inf(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      match maybe_eval(env, args.front().unwrap(), eval_args)? {
        QuootValue::Num(Num::Float(f)) => f.is_infinite(),
        _ => false,
      },
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "inf?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_zero(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      match maybe_eval(env, args.front().unwrap(), eval_args)? {
        QuootValue::Num(num) => match num {
          Num::Int(i) => i == 0,
          Num::Float(f) => f == 0.,
        },
        _ => false,
      },
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "zero?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_neg(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      match maybe_eval(env, args.front().unwrap(), eval_args)? {
        QuootValue::Num(num) => match num {
          Num::Int(i) => i < 0,
          Num::Float(f) => f < 0.,
        },
        _ => false,
      },
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "neg?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_pos(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      match maybe_eval(env, args.front().unwrap(), eval_args)? {
        QuootValue::Num(num) => match num {
          Num::Int(i) => i > 0,
          Num::Float(f) => f > 0.,
        },
        _ => false,
      },
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "pos?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_string(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      match maybe_eval(env, args.front().unwrap(), eval_args)? {
        QuootValue::String(_) => true,
        _ => false,
      },
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "str?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_symbol(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      match maybe_eval(env, args.front().unwrap(), eval_args)? {
        QuootValue::Symbol(_) => true,
        _ => false,
      },
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "symbol?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_op(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      match maybe_eval(env, args.front().unwrap(), eval_args)? {
        QuootValue::Op(_) => true,
        _ => false,
      },
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "op?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_empty(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      match maybe_eval(env, args.front().unwrap(), eval_args)? {
        QuootValue::Nil => true,
        QuootValue::List(list) => list.is_empty()?,
        other => {
          return Err(QuootEvalError::OperatorError(format!(
            "empty?: cannot check whether type {} is empty",
            other.type_string()
          )))
        }
      },
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "empty?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_is_even(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      match maybe_eval(env, args.front().unwrap(), eval_args)?
        .as_num("even?")?
      {
        Num::Int(i) => i % 2 == 0,
        Num::Float(f) => f % 2.0 == 0.0,
      },
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "even?: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_first(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => match maybe_eval(env, args.front().unwrap(), eval_args)? {
      QuootValue::Nil => Ok(QuootValue::Nil),
      QuootValue::List(QuootList::Strict(list)) => Ok({
        match list.front() {
          None => QuootValue::Nil,
          Some(value) => value.to_owned(),
        }
      }),
      QuootValue::List(QuootList::Lazy(list)) => Ok({
        list.realize_to(1)?;
        match list.get(0)? {
          None => QuootValue::Nil,
          Some(value) => value,
        }
      }),
      other => {
        return Err(QuootEvalError::OperatorError(format!(
          "first: cannot get the first element of type <{}>",
          other.type_string()
        )))
      }
    },
    n => Err(QuootEvalError::OperatorError(format!(
      "first: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_last(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => match maybe_eval(env, args.front().unwrap(), eval_args)? {
      QuootValue::Nil => Ok(QuootValue::Nil),
      QuootValue::List(QuootList::Strict(list)) => Ok(match list.last() {
        None => QuootValue::Nil,
        Some(value) => value.to_owned(),
      }),
      QuootValue::List(QuootList::Lazy(list)) => Ok({
        list.fully_realize()?;
        match list.get(list.realized_len() - 1)? {
          None => QuootValue::Nil,
          Some(value) => value.to_owned(),
        }
      }),
      other => {
        return Err(QuootEvalError::OperatorError(format!(
          "last: cannot get the last element of type <{}>",
          other.type_string()
        )))
      }
    },
    n => Err(QuootEvalError::OperatorError(format!(
      "last: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_rest(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::List(
      maybe_eval(env, args.front().unwrap(), eval_args)?
        .as_list("rest")?
        .rest()?,
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "first: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_reverse(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => {
      let mut list = maybe_eval(env, args.front().unwrap(), eval_args)?
        .as_list("reverse")?
        .as_strict()?;
      let mut new_list = QuootStrictList::new();
      while let Some(value) = list.pop_front() {
        new_list.push_front(value);
      }
      Ok(QuootValue::from(new_list))
    }
    n => Err(QuootEvalError::OperatorError(format!(
      "reverse: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_bool(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      maybe_eval(env, args.front().unwrap(), eval_args)?.as_bool(),
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "bool: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_not(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Bool(
      !maybe_eval(env, args.front().unwrap(), eval_args)?.as_bool(),
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "bool: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_and(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Ok(QuootValue::Bool(true)),
    n => {
      for i in 0..n - 1 {
        let value = maybe_eval(env, &args[i], eval_args)?;
        if !value.as_bool() {
          return Ok(value);
        }
      }
      maybe_eval(env, &args[n - 1], eval_args)
    }
  }
}

pub fn quoot_or(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    0 => Ok(QuootValue::Bool(true)),
    n => {
      for i in 0..n - 1 {
        let value = maybe_eval(env, &args[i], eval_args)?;
        if value.as_bool() {
          return Ok(value);
        }
      }
      maybe_eval(env, &args[n - 1], eval_args)
    }
  }
}

pub fn quoot_int(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::from(
      maybe_eval(env, args.front().unwrap(), eval_args)?
        .as_num("int")?
        .floor(),
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "int: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_abs(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Num(
      match maybe_eval(env, args.front().unwrap(), eval_args)?.as_num("abs")? {
        Num::Int(i) => Num::Int(i.abs()),
        Num::Float(f) => Num::Float(f.abs()),
      },
    )),
    n => Err(QuootEvalError::OperatorError(format!(
      "abs: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_sqrt(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => Ok(QuootValue::Num(Num::Float(
      maybe_eval(env, args.front().unwrap(), eval_args)?
        .as_num("sqrt")?
        .as_float()
        .sqrt(),
    ))),
    n => Err(QuootEvalError::OperatorError(format!(
      "sqrt: need 1 argument, got {}",
      n
    ))),
  }
}

pub fn quoot_filter(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let predicate =
      maybe_eval(env, args.front().unwrap(), eval_args)?.as_op("filter")?;
    let list =
      maybe_eval(env, args.get(1).unwrap(), eval_args)?.as_list("filter")?;
    let mut initial_builder_values = QuootStrictList::new();
    initial_builder_values.push_back(QuootValue::from(0i64));
    initial_builder_values.push_back(QuootValue::List(list));
    initial_builder_values.push_back(QuootValue::Op(predicate));
    Ok(QuootValue::from(QuootLazyList::new(
      &|state| {
        let builder_values =
          state.builder_values.as_ref().unwrap().as_strict()?;
        let mut index =
          builder_values.front().unwrap().as_num("filter")?.floor();
        let original_list = builder_values.get(1).unwrap().as_list("filter")?;
        let predicate = builder_values.get(2).unwrap().as_op("filter")?;
        while let Some(value) = original_list.get(index)? {
          if (predicate.f)(
            &predicate,
            &state.captured_environment.as_ref().unwrap(),
            &QuootStrictList::unit(value.clone()),
            false,
          )?
          .as_bool()
          {
            let mut new_builder_values = QuootStrictList::new();
            new_builder_values.push_back(QuootValue::from(index + 1));
            new_builder_values.push_back(QuootValue::List(original_list));
            new_builder_values.push_back(QuootValue::Op(predicate));
            state.builder_values = Some(QuootList::Strict(new_builder_values));
            state.realized_values.push_back(value);
            return Ok(());
          }
          index += 1;
        }
        state.is_finished = true;
        Ok(())
      },
      QuootLazyState::new(
        QuootStrictList::new(),
        Some(QuootList::Strict(initial_builder_values)),
        Some(env.clone()),
      ),
    )))
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "filter: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_map(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() > 1 {
    let mut map_values = vec![maybe_eval(env, &args[0], eval_args)?];
    for i in 1..args.len() {
      map_values.push(QuootValue::List(
        maybe_eval(env, &args[i], eval_args)?.as_list("map")?,
      ));
    }
    Ok(QuootValue::from(QuootLazyList::new(
      &|state| {
        let index = state.realized_values.len() as i64;
        let builder_values =
          state.builder_values.as_ref().unwrap().as_strict()?;
        let op = builder_values.get(0).unwrap().as_op("map")?;
        let mut op_args: Vec<QuootValue> = vec![];
        for i in 1..builder_values.len() {
          match builder_values.get(i).unwrap().as_list("map")?.get(index)? {
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
          state.realized_values.push_back((op.f)(
            &op,
            &state.captured_environment.as_ref().unwrap(),
            &QuootStrictList::from(op_args),
            false,
          )?);
        }
        Ok(())
      },
      QuootLazyState::new(
        QuootStrictList::new(),
        Some(QuootList::Strict(QuootStrictList::from(map_values))),
        Some(env.clone()),
      ),
    )))
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "map: need at least 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_greater(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let a = maybe_eval(env, args.front().unwrap(), eval_args)?.as_num(">")?;
    let b = maybe_eval(env, args.get(1).unwrap(), eval_args)?.as_num(">")?;
    Ok(QuootValue::Bool(a > b))
  } else {
    Err(QuootEvalError::OperatorError(format!(
      ">: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_greater_or_equal(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let a = maybe_eval(env, args.front().unwrap(), eval_args)?.as_num(">")?;
    let b = maybe_eval(env, args.get(1).unwrap(), eval_args)?.as_num(">")?;
    Ok(QuootValue::Bool(a >= b))
  } else {
    Err(QuootEvalError::OperatorError(format!(
      ">=: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_less(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let a = maybe_eval(env, args.front().unwrap(), eval_args)?.as_num(">")?;
    let b = maybe_eval(env, args.get(1).unwrap(), eval_args)?.as_num(">")?;
    Ok(QuootValue::Bool(a < b))
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "<: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_less_or_equal(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 2 {
    let a = maybe_eval(env, args.front().unwrap(), eval_args)?.as_num(">")?;
    let b = maybe_eval(env, args.get(1).unwrap(), eval_args)?.as_num(">")?;
    Ok(QuootValue::Bool(a <= b))
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "<=: need 2 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_if(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  if args.len() == 3 {
    let condition =
      maybe_eval(env, args.front().unwrap(), eval_args)?.as_bool();
    Ok(maybe_eval(
      env,
      args.get(if condition { 1 } else { 2 }).unwrap(),
      eval_args,
    )?)
  } else {
    Err(QuootEvalError::OperatorError(format!(
      "if: need 3 arguments, got {}",
      args.len()
    )))
  }
}

pub fn quoot_rand(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  Ok(QuootValue::Num(Num::Float(rand::random::<f64>())))
}

pub fn quoot_repeat(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => {
      let value = maybe_eval(env, args.front().unwrap(), eval_args)?;
      Ok(QuootValue::from(QuootLazyList::new(
        &|lazy_state| {
          let value =
            lazy_state.builder_values.as_ref().unwrap().as_strict()?[0]
              .to_owned();
          lazy_state.realized_values.push_back(value);
          Ok(())
        },
        QuootLazyState::new(
          QuootStrictList::new(),
          Some(QuootList::Strict(QuootStrictList::unit(value))),
          Some(env.to_owned()),
        ),
      )))
    }
    2 => {
      let value = maybe_eval(env, &args[0], eval_args)?;
      let count = maybe_eval(env, &args[1], eval_args)?
        .as_num("repeat")?
        .floor();
      let mut values: Vec<QuootValue> = vec![];
      for _ in 0..count {
        values.push(value.to_owned());
      }
      Ok(QuootValue::from(QuootStrictList::from(values)))
    }
    n => Err(QuootEvalError::OperatorError(format!(
      "repeatedly: need 1 or 2 arguments, got {}",
      n
    ))),
  }
}

pub fn quoot_repeatedly(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => {
      let generator_op = maybe_eval(env, args.front().unwrap(), eval_args)?
        .as_op("repeatedly")?;
      Ok(QuootValue::from(QuootLazyList::new(
        &|lazy_state| {
          let generator_op =
            lazy_state.builder_values.as_ref().unwrap().as_strict()?[0]
              .as_op("repeatedly")?;
          let value = (generator_op.f)(
            &generator_op,
            &lazy_state.captured_environment.as_ref().unwrap(),
            &QuootStrictList::new(),
            false,
          )?;
          lazy_state.realized_values.push_back(value);
          Ok(())
        },
        QuootLazyState::new(
          QuootStrictList::new(),
          Some(QuootList::Strict(QuootStrictList::unit(QuootValue::Op(
            generator_op,
          )))),
          Some(env.to_owned()),
        ),
      )))
    }
    2 => {
      let generator_op =
        maybe_eval(env, &args[0], eval_args)?.as_op("repeatedly")?;
      let count = maybe_eval(env, &args[1], eval_args)?
        .as_num("repeatedly")?
        .floor();
      let mut values: Vec<QuootValue> = vec![];
      for _ in 0..count {
        values.push((generator_op.f)(
          &generator_op,
          &env,
          &QuootStrictList::new(),
          false,
        )?)
      }
      Ok(QuootValue::from(QuootStrictList::from(values)))
    }
    n => Err(QuootEvalError::OperatorError(format!(
      "repeatedly: need 1 or 2 arguments, got {}",
      n
    ))),
  }
}

pub fn quoot_iterate(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    2 => {
      let op = maybe_eval(env, &args[0], eval_args)?.as_op("iterate")?;
      let initial_value = maybe_eval(env, &args[1], eval_args)?;
      Ok(QuootValue::from(QuootLazyList::new(
        &|lazy_state| {
          let generator_op =
            lazy_state.builder_values.as_ref().unwrap().as_strict()?[0]
              .as_op("iterate")?;
          let value = (generator_op.f)(
            &generator_op,
            &lazy_state.captured_environment.as_ref().unwrap(),
            &QuootStrictList::unit(
              lazy_state.realized_values.last().unwrap().to_owned(),
            ),
            false,
          )?;
          lazy_state.realized_values.push_back(value);
          Ok(())
        },
        QuootLazyState::new(
          QuootStrictList::unit(initial_value),
          Some(QuootList::Strict(QuootStrictList::unit(QuootValue::Op(op)))),
          Some(env.to_owned()),
        ),
      )))
    }
    3 => {
      let op = maybe_eval(env, &args[0], eval_args)?.as_op("iterate")?;
      let mut value = maybe_eval(env, &args[1], eval_args)?;
      let count = maybe_eval(env, &args[2], eval_args)?
        .as_num("iterate")?
        .floor();
      for _ in 0..count as usize {
        value = (op.f)(&op, env, &QuootStrictList::unit(value), false)?;
      }
      Ok(value)
    }
    n => Err(QuootEvalError::OperatorError(format!(
      "repeatedly: need 2 or 3 arguments, got {}",
      n
    ))),
  }
}

pub fn quoot_time(
  _op_self: &QuootOp,
  env: &Env,
  args: &QuootStrictList,
  eval_args: bool,
) -> Result<QuootValue, QuootEvalError> {
  match args.len() {
    1 => {
      let timer = Instant::now();
      let value = maybe_eval(env, &args[0], eval_args)?;
      println!("{:.2?}", timer.elapsed());
      Ok(value)
    }
    n => Err(QuootEvalError::OperatorError(format!(
      "time: need 1 argument, got {}",
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
    bindings.insert(name.to_string(), QuootValue::from(*value as f64));
  });
  let operator_bindings: &[(
    &str,
    fn(
      &QuootOp,
      &Env,
      &QuootStrictList,
      bool,
    ) -> Result<QuootValue, QuootEvalError>,
  )] = &[
    ("time", quoot_time),
    ("let", quoot_let),
    ("eval", quoot_eval),
    ("quote", quoot_quote),
    ("operator", quoot_operator),
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
    bindings.insert(name.to_string(), QuootValue::Op(QuootOp::new(op)));
  });
  bindings
}
