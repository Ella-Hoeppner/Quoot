#[derive(Debug)]
pub enum QuootParseError {
  UnmatchedCloser,
  MismatchedCloser(String, String),
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

pub fn chars_match(pattern: &Vec<char>, chars: &[char]) -> bool {
  chars.len() >= pattern.len()
    && match (0..pattern.len())
      .map(|i| pattern[i] == chars[i])
      .reduce(|acc, e| acc && e)
    {
      None => true,
      Some(b) => b,
    }
}

pub fn parse_chars(chars: Vec<char>) -> Result<Sexp, QuootParseError> {
  let root: &mut Sexp = &mut Sexp::List(vec![]);
  let mut char_index: usize = 0;
  let mut consumed_index: usize = 0;
  let delimiters: Vec<(Vec<char>, Vec<char>, Option<String>)> = vec![
    (vec!['('], vec![')'], None),
    (vec!['['], vec![']'], Some("vector".to_string())),
    (vec!['{'], vec!['}'], Some("hashmap".to_string())),
  ];
  let mut active_openers: Vec<(usize, usize)> = vec![];
  loop {
    if char_index >= chars.len() {
      break;
    }
    let char = chars[char_index];
    let string_opener = char == '"';
    let whitespace = is_whitespace(char);
    let opener_index = (0..delimiters.len())
      .rev()
      .find(|delimiter_index| chars_match(&delimiters[*delimiter_index].0, &chars[char_index..]));
    let closer_index = (0..delimiters.len())
      .rev()
      .find(|delimiter_index| chars_match(&delimiters[*delimiter_index].1, &chars[char_index..]));
    if string_opener || opener_index != None || closer_index != None || whitespace {
      if consumed_index != char_index {
        sexp_insert(
          root,
          Sexp::Leaf(chars[consumed_index..char_index].iter().collect()),
          active_openers.len(),
        );
      }
      consumed_index = char_index + 1;
    };
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
            active_openers.len(),
          );
          consumed_index = char_index + 1;
          break;
        }
      }
    }
    char_index += match opener_index {
      Some(i) => {
        sexp_insert(root, Sexp::List(vec![]), active_openers.len());
        active_openers.push((char_index, i));
        let delimiter = &delimiters[i];
        match delimiter.2.clone() {
          Some(delimiter_tag) => sexp_insert(root, Sexp::Leaf(delimiter_tag), active_openers.len()),
          None => (),
        }
        delimiter.0.len()
      }
      None => match closer_index {
        Some(i) => match active_openers.pop() {
          None => return Err(QuootParseError::UnmatchedCloser),
          Some((_, opener_index)) => {
            if i != opener_index {
              return Err(QuootParseError::MismatchedCloser(
                delimiters[opener_index].0.iter().collect(),
                delimiters[i].1.iter().collect(),
              ));
            }
            delimiters[i].1.len()
          }
        },
        None => 1,
      },
    };
  }
  if active_openers.len() != 0 {
    return Err(QuootParseError::UnclosedOpener);
  }
  if consumed_index < chars.len() - 1 {
    sexp_insert(
      root,
      Sexp::Leaf(chars[consumed_index..chars.len()].iter().collect()),
      active_openers.len(),
    );
  }
  return Ok(root.to_owned());
}

pub fn parse(s: &str) -> Result<Sexp, QuootParseError> {
  parse_chars(s.chars().collect())
}
