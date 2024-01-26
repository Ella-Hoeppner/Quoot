use crate::parse::parse;
use crate::parse::QuootParseError;
use crate::parse::Sexp;
use std::fmt;
use std::io;
use std::io::Write;

pub enum Qexp {
  List(Vec<Qexp>),
  Int(i64),
  Float(f64),
  String(String),
  Symbol(String),
}

impl Qexp {
  pub fn from_token(token: &String) -> Qexp {
    let chars: Vec<char> = token.chars().collect();
    if chars[0] == '"' {
      return Qexp::String(chars[1..chars.len() - 1].iter().collect());
    }
    match token.parse::<i64>() {
      Ok(int) => return Qexp::Int(int),
      Err(_) => match token.parse::<f64>() {
        Ok(float) => return Qexp::Float(float),
        Err(_) => (),
      },
    }
    Qexp::Symbol(token.clone())
  }
  pub fn from_sexp(sexp: &Sexp) -> Qexp {
    match sexp {
      Sexp::List(sub_sexps) => Qexp::List(
        sub_sexps
          .iter()
          .map(|sub_sexp| Qexp::from_sexp(sub_sexp))
          .collect(),
      ),
      Sexp::Leaf(token) => Qexp::from_token(token),
    }
  }
}

impl fmt::Display for Qexp {
  fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      Qexp::Symbol(token) => fmt.write_str(token)?,
      Qexp::String(token) => {
        fmt.write_str("\"")?;
        fmt.write_str(token)?;
        fmt.write_str("\"")?;
      }
      Qexp::Int(int) => fmt.write_str(&int.to_string())?,
      Qexp::Float(float) => fmt.write_str(&float.to_string())?,
      Qexp::List(sub_expressions) => {
        fmt.write_str("(")?;
        let mut separator = "";
        for sexp in sub_expressions {
          fmt.write_str(separator)?;
          fmt.write_str(&sexp.to_string())?;
          separator = " ";
        }
        fmt.write_str(")")?;
      }
    }
    Ok(())
  }
}

fn print_prompt() {
  print!("> ");
  io::stdout().flush().unwrap();
}

fn eval(form: &str) -> Result<Sexp, QuootParseError> {
  parse(form)
}

pub fn repl() {
  println!("Quoot repl started!\n");
  let mut input_buffer = String::new();
  let stdin = io::stdin();
  print_prompt();
  while stdin.read_line(&mut input_buffer).is_ok() {
    let trimmed_input = input_buffer.trim_end();
    if trimmed_input.eq("#EXIT") {
      println!("\nExiting Quoot repl\n");
      break;
    }
    match eval(trimmed_input) {
      Err(e) => println!("{:?}", e),
      Ok(sexp) => {
        println!(
          "{:?}\n{:?}",
          sexp.to_string(),
          Qexp::from_sexp(&sexp).to_string()
        )
      }
    }
    input_buffer.clear();
    print_prompt();
  }
}
