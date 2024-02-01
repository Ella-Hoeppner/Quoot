use crate::library::quoot_add;
use crate::library::quoot_apply;
use crate::library::quoot_compose;
use crate::library::quoot_concat;
use crate::library::quoot_cons;
use crate::library::quoot_count;
use crate::library::quoot_dec;
use crate::library::quoot_divide;
use crate::library::quoot_drop;
use crate::library::quoot_equal;
use crate::library::quoot_get;
use crate::library::quoot_identity;
use crate::library::quoot_inc;
use crate::library::quoot_is_bool;
use crate::library::quoot_is_empty;
use crate::library::quoot_is_fn;
use crate::library::quoot_is_list;
use crate::library::quoot_is_nil;
use crate::library::quoot_is_num;
use crate::library::quoot_is_string;
use crate::library::quoot_is_symbol;
use crate::library::quoot_list_constructor;
use crate::library::quoot_map;
use crate::library::quoot_modulo;
use crate::library::quoot_multiply;
use crate::library::quoot_partial;
use crate::library::quoot_quotient;
use crate::library::quoot_range;
use crate::library::quoot_subtract;
use crate::library::quoot_take;
use crate::model::Num;
use crate::model::QuootEvalError;
use crate::model::QuootFn;
use crate::model::QuootValue;
use crate::model::QuootValueList;
use crate::parse::parse;
use std::collections::HashMap;
use std::io;
use std::io::Write;

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
  pub fn add_standard_bindings(&mut self) {
    self.bind("TAU", QuootValue::Num(Num::Float(6.283185307179586)));
    self.bind("=", QuootValue::Fn(&quoot_equal));
    self.bind("inc", QuootValue::Fn(&quoot_inc));
    self.bind("dec", QuootValue::Fn(&quoot_dec));
    self.bind("+", QuootValue::Fn(&quoot_add));
    self.bind("-", QuootValue::Fn(&quoot_subtract));
    self.bind("*", QuootValue::Fn(&quoot_multiply));
    self.bind("/", QuootValue::Fn(&quoot_divide));
    self.bind("mod", QuootValue::Fn(&quoot_modulo));
    self.bind("quot", QuootValue::Fn(&quoot_quotient));
    self.bind("list", QuootValue::Fn(&quoot_list_constructor));
    self.bind("count", QuootValue::Fn(&quoot_count));
    self.bind("cons", QuootValue::Fn(&quoot_cons));
    self.bind("concat", QuootValue::Fn(&quoot_concat));
    self.bind("get", QuootValue::Fn(&quoot_get));
    self.bind("take", QuootValue::Fn(&quoot_take));
    self.bind("drop", QuootValue::Fn(&quoot_drop));
    self.bind("range", QuootValue::Fn(&quoot_range));
    self.bind("identity", QuootValue::Fn(&quoot_identity));
    self.bind("apply", QuootValue::Fn(&quoot_apply));
    self.bind("partial", QuootValue::Fn(&quoot_partial));
    self.bind("|", QuootValue::Fn(&quoot_partial));
    self.bind("compose", QuootValue::Fn(&quoot_compose));
    self.bind(".", QuootValue::Fn(&quoot_compose));
    self.bind("map", QuootValue::Fn(&quoot_map));
    self.bind("nil?", QuootValue::Fn(&quoot_is_nil));
    self.bind("bool?", QuootValue::Fn(&quoot_is_bool));
    self.bind("list?", QuootValue::Fn(&quoot_is_list));
    self.bind("num?", QuootValue::Fn(&quoot_is_num));
    self.bind("str?", QuootValue::Fn(&quoot_is_string));
    self.bind("symbol?", QuootValue::Fn(&quoot_is_symbol));
    self.bind("fn?", QuootValue::Fn(&quoot_is_fn));
    self.bind("empty?", QuootValue::Fn(&quoot_is_empty));
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
    args: QuootValueList,
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
        let evaluated_values: &mut QuootValueList = &mut values
          .iter()
          .map(|v| self.eval(v.to_owned()))
          .into_iter()
          .collect::<Result<QuootValueList, QuootEvalError>>()?;
        match evaluated_values.pop_front() {
          None => Ok(QuootValue::List(QuootValueList::new())),
          Some(function) => match function {
            QuootValue::Fn(f) => {
              self.apply(f.clone(), evaluated_values.to_owned())
            }
            _ => todo!(),
          },
        }
      }
      other => Ok(other),
    }
  }
  pub fn new_with_standard_env() -> Interpreter {
    let mut interpreter = Interpreter::default();
    interpreter.env.add_standard_bindings();
    interpreter
  }
}

fn print_prompt() {
  print!("> ");
  io::stdout().flush().unwrap();
}

pub fn repl() {
  println!("\nQuoot repl started :D\n");
  let interpreter = &mut Interpreter::new_with_standard_env();
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
