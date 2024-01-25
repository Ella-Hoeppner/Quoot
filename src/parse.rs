#[derive(Debug)]
pub enum QuootParseError {
  UnmatchedCloser(String),
  MismatchedCloser(String, String),
  UnclosedOpener(String),
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
  let prefixes: Vec<(Vec<char>, String)> = vec![
    (vec!['\''], "quote".to_string()),
    (vec!['~'], "unquote".to_string()),
  ];
  struct Opening {
    char_index: usize,
    is_prefix: bool,
    delimiter_or_prefix_index: usize,
  }
  let mut active_openings: Vec<Opening> = vec![];
  loop {
    if char_index >= chars.len() {
      break;
    }
    let char = chars[char_index];
    let string_opener = char == '"';
    let whitespace = is_whitespace(char);
    let prefix_index = if consumed_index == char_index {
      (0..prefixes.len()).rev().find(|prefix_index| {
        chars_match(&prefixes[*prefix_index].0, &chars[char_index..])
      })
    } else {
      None
    };
    let opener_index = (0..delimiters.len()).rev().find(|delimiter_index| {
      chars_match(&delimiters[*delimiter_index].0, &chars[char_index..])
    });
    let closer_index = (0..delimiters.len()).rev().find(|delimiter_index| {
      chars_match(&delimiters[*delimiter_index].1, &chars[char_index..])
    });
    if consumed_index != char_index
      && (string_opener
        || opener_index.is_some()
        || closer_index.is_some()
        || whitespace)
    {
      sexp_insert(
        root,
        Sexp::Leaf(chars[consumed_index..char_index].iter().collect()),
        active_openings.len(),
      );
      if whitespace {
        match active_openings.last() {
          Some(next_opening) => {
            if next_opening.is_prefix {
              active_openings.pop();
            }
          }
          None => (),
        }
      }
    };
    if string_opener {
      let string_start_index = char_index;
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
            Sexp::Leaf(
              chars[string_start_index..char_index + 1].iter().collect(),
            ),
            active_openings.len(),
          );
          break;
        }
      }
    }
    char_index += match prefix_index {
      Some(i) => {
        let prefix = &prefixes[i];
        sexp_insert(root, Sexp::List(vec![]), active_openings.len());
        active_openings.push(Opening {
          char_index: char_index,
          is_prefix: true,
          delimiter_or_prefix_index: i,
        });
        sexp_insert(root, Sexp::Leaf(prefix.1.clone()), active_openings.len());
        prefix.0.len()
      }
      None => match opener_index {
        Some(i) => {
          sexp_insert(root, Sexp::List(vec![]), active_openings.len());
          active_openings.push(Opening {
            char_index: char_index,
            is_prefix: false,
            delimiter_or_prefix_index: i,
          });
          let delimiter = &delimiters[i];
          match delimiter.2.clone() {
            Some(delimiter_tag) => sexp_insert(
              root,
              Sexp::Leaf(delimiter_tag),
              active_openings.len(),
            ),
            None => (),
          }
          delimiter.0.len()
        }
        None => match closer_index {
          Some(i) => match active_openings.pop() {
            None => {
              return Err(QuootParseError::UnmatchedCloser(
                delimiters[i].1.iter().collect(),
              ))
            }
            Some(opening) => {
              if i != opening.delimiter_or_prefix_index {
                return Err(QuootParseError::MismatchedCloser(
                  delimiters[opening.delimiter_or_prefix_index]
                    .0
                    .iter()
                    .collect(),
                  delimiters[i].1.iter().collect(),
                ));
              }
              match active_openings.last() {
                Some(next_opening) => {
                  if next_opening.is_prefix {
                    active_openings.pop();
                  }
                }
                None => (),
              }
              delimiters[i].1.len()
            }
          },
          None => 1,
        },
      },
    };
    if string_opener
      || prefix_index.is_some()
      || opener_index.is_some()
      || closer_index.is_some()
      || whitespace
    {
      consumed_index = char_index;
    }
  }
  loop {
    match active_openings.pop() {
      Some(opening) => {
        if !opening.is_prefix {
          return Err(QuootParseError::UnclosedOpener(
            delimiters[opening.delimiter_or_prefix_index]
              .0
              .iter()
              .collect(),
          ));
        }
      }
      None => break,
    }
  }
  if consumed_index < chars.len() - 1 {
    sexp_insert(
      root,
      Sexp::Leaf(chars[consumed_index..chars.len()].iter().collect()),
      active_openings.len(),
    );
  }
  return Ok(root.to_owned());
}

pub fn parse(s: &str) -> Result<Sexp, QuootParseError> {
  parse_chars(s.chars().collect())
}
