#[derive(Debug)]
pub enum QuootParseError {
  UnmatchedCloser,
  UnclosedOpener,
  UnclosedString,
}

fn is_whitespace(c: char) -> bool {
  c == ' ' || c == '\t' || c == '\n'
}

#[derive(Debug, Clone)]
pub enum Sexp {
  List(Vec<Sexp>),
  Leaf(String),
}

fn sexp_insert(sexp: &mut Sexp, value: Sexp, depth: usize) {
  if depth == 0 {
    match sexp {
      Sexp::List(values) => values.push(value),
      Sexp::Leaf(_) => panic!("sexp_insert trying to insert into a leaf"),
    }
  } else {
    match sexp {
      Sexp::List(values) => {
        let index = values.len() - 1;
        sexp_insert(&mut values[index], value, depth - 1)
      }
      Sexp::Leaf(_) => panic!("sexp_insert trying to descend into a leaf"),
    }
  }
}

pub fn parse_chars(chars: Vec<char>) -> Result<Sexp, QuootParseError> {
  let root: &mut Sexp = &mut Sexp::List(vec![]);
  let mut char_index: usize = 0;
  let mut consumed_index: usize = 0;
  let mut opener_indeces: Vec<usize> = vec![];
  loop {
    if char_index >= chars.len() {
      break;
    }
    let char = chars[char_index];
    let string_opener = char == '"';
    let whitespace = is_whitespace(char);
    let opener = char == '(';
    let closer = char == ')';
    if string_opener || opener || closer || whitespace {
      if consumed_index != char_index {
        sexp_insert(
          root,
          Sexp::Leaf(chars[consumed_index..char_index].iter().collect()),
          opener_indeces.len(),
        );
      }
      consumed_index = char_index + 1;
    }
    if string_opener {
      loop {
        char_index += 1;
        if char_index >= chars.len() {
          return Err(QuootParseError::UnclosedString);
        }
        let string_char = chars[char_index];
        if string_char == '\\' {
          char_index += 1
        } else if string_char == '"' {
          sexp_insert(
            root,
            Sexp::Leaf(chars[consumed_index..char_index].iter().collect()),
            opener_indeces.len(),
          );
          consumed_index = char_index + 1;
          break;
        }
      }
    }
    if opener {
      sexp_insert(root, Sexp::List(vec![]), opener_indeces.len());
      opener_indeces.push(char_index);
    }
    if closer {
      if opener_indeces.len() == 0 {
        return Err(QuootParseError::UnmatchedCloser);
      }
      opener_indeces.pop();
    }
    char_index += 1;
  }
  if opener_indeces.len() != 0 {
    return Err(QuootParseError::UnclosedOpener);
  }
  if consumed_index < chars.len() - 1 {
    sexp_insert(
      root,
      Sexp::Leaf(chars[consumed_index..chars.len()].iter().collect()),
      opener_indeces.len(),
    );
  }
  return Ok(root.to_owned());
}

pub fn parse(s: &str) -> Result<Sexp, QuootParseError> {
  parse_chars(s.chars().collect())
}
