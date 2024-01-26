use std::fmt;

#[derive(Debug)]
pub enum QuootParseError {
  UnmatchedCloser(String),
  MismatchedCloser(String, String),
  UnclosedOpener(String),
  UnclosedString,
}

fn is_whitespace(c: char) -> bool {
  c == ' ' || c == ',' || c == '\t' || c == '\n'
}

#[derive(Clone)]
pub enum Sexp {
  List(Vec<Sexp>),
  Leaf(String),
}

impl fmt::Display for Sexp {
  fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      Sexp::Leaf(token) => fmt.write_str(token)?,
      Sexp::List(sub_expressions) => {
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

fn sexp_insert(sexp: &mut Sexp, value: Sexp, depth: usize) {
  match sexp {
    Sexp::Leaf(_) => panic!("trying to insert into a leaf"),
    Sexp::List(values) => {
      if depth == 0 {
        values.push(value)
      } else {
        let index = values.len() - 1;
        sexp_insert(&mut values[index], value, depth - 1)
      }
    }
  }
}

pub fn chars_match(pattern: &[char], chars: &[char]) -> bool {
  chars.len() >= pattern.len()
    && match (0..pattern.len())
      .map(|i| pattern[i] == chars[i])
      .reduce(|acc, e| acc && e)
    {
      None => true,
      Some(b) => b,
    }
}

struct Opening {
  char_index: usize,
  is_prefix: bool,
  delimiter_or_prefix_index: usize,
}
struct ParserState {
  root: Sexp,
  opening_stack: Vec<Opening>,
}
impl ParserState {
  pub fn new() -> ParserState {
    ParserState {
      root: Sexp::List(vec![]),
      opening_stack: vec![],
    }
  }
  fn insert_sexp(&mut self, sexp: Sexp) {
    sexp_insert(&mut self.root, sexp, self.opening_stack.len());
  }
  fn peek_opening(&mut self) -> Option<&Opening> {
    self.opening_stack.last()
  }
  pub fn insert_token(&mut self, token: String) {
    self.insert_sexp(Sexp::Leaf(token));
  }
  pub fn open_prefix(
    &mut self,
    char_index: usize,
    prefix_index: usize,
    tag: &String,
  ) {
    self.insert_sexp(Sexp::List(vec![]));
    self.opening_stack.push(Opening {
      char_index,
      is_prefix: true,
      delimiter_or_prefix_index: prefix_index,
    });
    self.insert_token(tag.clone());
  }
  pub fn close_prefixes(&mut self) {
    loop {
      match self.peek_opening() {
        None => break,
        Some(next_opening) => {
          if next_opening.is_prefix {
            self.opening_stack.pop();
          } else {
            break;
          }
        }
      }
    }
  }
  pub fn open_list(
    &mut self,
    char_index: usize,
    delimiter_index: usize,
    tag: &Option<String>,
  ) {
    self.insert_sexp(Sexp::List(vec![]));
    self.opening_stack.push(Opening {
      char_index,
      is_prefix: false,
      delimiter_or_prefix_index: delimiter_index,
    });
    match tag.clone() {
      Some(delimiter_tag) => self.insert_token(delimiter_tag),
      None => (),
    }
  }
  pub fn close_list(&mut self) -> Option<Opening> {
    self.close_prefixes();
    self.opening_stack.pop()
  }
}

pub fn parse_chars(chars: Vec<char>) -> Result<Sexp, QuootParseError> {
  if chars.is_empty() {
    return Ok(Sexp::List(vec![]));
  }

  let delimiters: Vec<(&[char], &[char], Option<String>)> = vec![
    (&['('], &[')'], None),
    (&['['], &[']'], Some("vector".to_string())),
    (&['{'], &['}'], Some("hashmap".to_string())),
    (&['#', '['], &[']'], Some("set".to_string())),
    (&['#', '{'], &['}'], Some("ordered-hashmap".to_string())),
  ];
  let prefixes: Vec<(&[char], String)> = vec![
    (&['\''], "quote".to_string()),
    (&['~'], "unquote".to_string()),
  ];

  let parser_state: &mut ParserState = &mut ParserState::new();
  let mut char_index: usize = 0;
  let mut consumed_index: usize = 0;

  loop {
    if char_index >= chars.len() {
      break;
    }
    let char = chars[char_index];
    let string_opener = char == '"';
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
    let whitespace = prefix_index.is_none()
      && opener_index.is_none()
      && closer_index.is_none()
      && is_whitespace(char);
    if consumed_index != char_index
      && (string_opener
        || opener_index.is_some()
        || closer_index.is_some()
        || whitespace)
    {
      // If consumption isn't up to the current index, and this character
      // indicates the previous token should end, add previous token to AST.
      parser_state
        .insert_token(chars[consumed_index..char_index].iter().collect());
      if whitespace {
        // If this character is whitespace, indicating that this was the end of
        // a terminal token rather than a closing delimiter, check if innermost
        // opener is a prefix (as opposed to a delimiter), and if so close it.
        parser_state.close_prefixes();
      }
    };
    if string_opener {
      // If this character is a ", indicating the start of a string, consume
      // the entire string.
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
          parser_state.insert_token(
            chars[string_start_index..char_index + 1].iter().collect(),
          );
          break;
        }
      }
    }

    // Handle the case where this character is the start of a prefix, opener,
    // or closer, and adjust the character index accordingly.
    char_index += match prefix_index {
      Some(i) => {
        // If this is a prefix, add a list, with the tag of the prefix as the
        // first element of the list, to the AST.
        let prefix = &prefixes[i];
        parser_state.open_prefix(char_index, i, &prefix.1);
        prefix.0.len()
      }
      None => match opener_index {
        Some(i) => {
          // If this is an opener, add a list, with the tag of the delimiter
          // pair (if it has one) as the first element of the list, to the AST.
          let delimiter = &delimiters[i];
          parser_state.open_list(char_index, i, &delimiter.2);
          delimiter.0.len()
        }
        None => match closer_index {
          Some(i) => {
            // The closer identified at this point may be inaccurate if it is
            // the prefix of (or identical to) another closer. Therefore, to
            // properly update the character index, the below expression
            // ensures that the correct closer is used.
            delimiters[match parser_state.close_list() {
              Some(opening) => {
                parser_state.close_prefixes();
                if i == opening.delimiter_or_prefix_index {
                  // If the closer index matches what is expected by the
                  // opener, just use that index.
                  i
                } else {
                  // If the closer type does not match the one expected by the
                  // opener, check whether the expected closer is also matched
                  // by the current string. If so, accept the closer and
                  // return the expected length rather than the length of the
                  // original matched closer, otherwise return an error.
                  if chars_match(
                    &delimiters[opening.delimiter_or_prefix_index].1,
                    &chars[char_index..],
                  ) {
                    opening.delimiter_or_prefix_index
                  } else {
                    return Err(QuootParseError::MismatchedCloser(
                      delimiters[opening.delimiter_or_prefix_index]
                        .0
                        .iter()
                        .collect(),
                      delimiters[i].1.iter().collect(),
                    ));
                  }
                }
              }
              None => {
                return Err(QuootParseError::UnmatchedCloser(
                  delimiters[i].1.iter().collect(),
                ))
              }
            }]
            .1
            .len()
          }
          None => 1,
        },
      },
    };
    // Catch the consumption up to the current position, if the current
    // character matched with something for which that should be done.
    if string_opener
      || prefix_index.is_some()
      || opener_index.is_some()
      || closer_index.is_some()
      || whitespace
    {
      consumed_index = char_index;
    }
  }
  // Throw an error if there are any open lists at the end of the string.
  match parser_state.close_list() {
    Some(opening) => {
      return Err(QuootParseError::UnclosedOpener(
        delimiters[opening.delimiter_or_prefix_index]
          .0
          .iter()
          .collect(),
      ))
    }
    None => (),
  }

  // If consumption isn't caught up to the end of the string, that means the
  // string must end with a token not followed by whitespace, so add one
  // final leaf to the AST.
  if consumed_index < char_index {
    parser_state
      .insert_token(chars[consumed_index..chars.len()].iter().collect());
  }
  return Ok(parser_state.root.to_owned());
}

pub fn parse(s: &str) -> Result<Sexp, QuootParseError> {
  parse_chars(s.chars().collect())
}
