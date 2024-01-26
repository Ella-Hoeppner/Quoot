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
enum Opening {
  Prefix,
  List(&'static [char], &'static [char]),
}
struct ParserState {
  root: Sexp,
  opening_stack: Vec<(usize, Opening)>,
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
    match self.opening_stack.last() {
      Some(index_opening_pair) => Some(&index_opening_pair.1),
      None => None,
    }
  }
  pub fn insert_token(&mut self, token: String) {
    self.insert_sexp(Sexp::Leaf(token));
  }
  pub fn open_prefix(&mut self, char_index: usize, tag: &String) {
    self.insert_sexp(Sexp::List(vec![]));
    self.opening_stack.push((char_index, Opening::Prefix));
    self.insert_token(tag.clone());
  }
  pub fn close_prefixes(&mut self) {
    loop {
      match self.peek_opening() {
        None => break,
        Some(next_opening) => match next_opening {
          Opening::Prefix => {
            self.opening_stack.pop();
          }
          Opening::List(_, _) => break,
        },
      }
    }
  }
  pub fn open_list(
    &mut self,
    char_index: usize,
    opener: &'static [char],
    closer: &'static [char],
    tag: &Option<String>,
  ) {
    self.insert_sexp(Sexp::List(vec![]));
    self
      .opening_stack
      .push((char_index, Opening::List(opener, closer)));
    match tag.clone() {
      Some(delimiter_tag) => self.insert_token(delimiter_tag),
      None => (),
    }
  }
  pub fn close_list(&mut self) -> Option<&[char]> {
    self.close_prefixes();
    let closer = match self.opening_stack.pop() {
      Some(index_opening_pair) => match index_opening_pair.1 {
        Opening::List(_, closer) => Some(closer),
        Opening::Prefix => unreachable!(),
      },
      None => None,
    };
    self.close_prefixes();
    closer
  }
  pub fn has_open_list(&mut self) -> bool {
    self
      .opening_stack
      .iter()
      .find(|index_opening_pair| match index_opening_pair.1 {
        Opening::Prefix => false,
        Opening::List(_, _) => true,
      })
      .is_some()
  }
  pub fn get_open_list(&mut self) -> Option<(&[char], &[char])> {
    self
      .opening_stack
      .iter()
      .rev()
      .find_map(|index_opening_pair| match index_opening_pair.1 {
        Opening::Prefix => None,
        Opening::List(opener, closer) => Some((opener, closer)),
      })
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
    let matched_closer = delimiters
      .iter()
      .find(|(_, closer, _)| chars_match(closer, &chars[char_index..]));
    let open_list = parser_state.get_open_list();
    let was_expected_closer_matched = match open_list {
      Some((_, closer)) => chars_match(closer, &chars[char_index..]),
      None => false,
    };
    match matched_closer {
      None => (),
      Some(closer) => match open_list {
        None => {
          return Err(QuootParseError::UnmatchedCloser(
            closer.1.iter().collect(),
          ))
        }
        Some((opener, _)) => {
          if !was_expected_closer_matched {
            return Err(QuootParseError::MismatchedCloser(
              opener.iter().collect(),
              closer.1.iter().collect(),
            ));
          }
        }
      },
    }
    let matched_opening = delimiters
      .iter()
      .rev()
      .find(|delimiter| chars_match(&delimiter.0, &chars[char_index..]));
    if !was_expected_closer_matched && matched_closer.is_some() {}
    let whitespace = prefix_index.is_none()
      && matched_opening.is_none()
      && !was_expected_closer_matched
      && is_whitespace(char);
    if consumed_index != char_index
      && (string_opener
        || matched_opening.is_some()
        || was_expected_closer_matched
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
        parser_state.open_prefix(char_index, &prefix.1);
        prefix.0.len()
      }
      None => match matched_opening {
        Some(delimiter) => {
          // If this is an opener, add a list, with the tag of the delimiter
          // pair (if it has one) as the first element of the list, to the AST.
          parser_state.open_list(
            char_index,
            delimiter.0,
            delimiter.1,
            &delimiter.2,
          );
          delimiter.0.len()
        }
        None => {
          if was_expected_closer_matched {
            // This unwrap is safe, because expected_closer_matched will only
            // be true if there is a list to close.
            parser_state.close_list().unwrap().len()
          } else {
            1
          }
        }
      },
    };
    // Catch the consumption up to the current position, if the current
    // character matched with something for which that should be done.
    if string_opener
      || prefix_index.is_some()
      || matched_opening.is_some()
      || was_expected_closer_matched
      || whitespace
    {
      consumed_index = char_index;
    }
  }
  // Throw an error if there are any open lists at the end of the string.
  match parser_state.get_open_list() {
    Some((opener, _)) => {
      return Err(QuootParseError::UnclosedOpener(opener.iter().collect()))
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
