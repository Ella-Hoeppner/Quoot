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
  if chars.is_empty() {
    return Ok(root.to_owned());
  }
  let mut char_index: usize = 0;
  let mut consumed_index: usize = 0;
  let delimiters: Vec<(Vec<char>, Vec<char>, Option<String>)> = vec![
    (vec!['('], vec![')'], None),
    (vec!['['], vec![']'], Some("vector".to_string())),
    (vec!['{'], vec!['}'], Some("hashmap".to_string())),
    /*(vec!['#', '['], vec![']'], Some("set".to_string())),
    (
      vec!['#', '{'],
      vec!['}'],
      Some("ordered-hashmap".to_string()),
    ),*/
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
      sexp_insert(
        root,
        Sexp::Leaf(chars[consumed_index..char_index].iter().collect()),
        active_openings.len(),
      );
      if whitespace {
        // If this character is whitespace, indicating that this was the end of
        // a terminal token rather than a closing delimiter, check if innermost
        // opener is a prefix (as opposed to a delimiter), and if so close it.
        loop {
          match active_openings.last() {
            Some(next_opening) => {
              if next_opening.is_prefix {
                active_openings.pop();
              } else {
                break;
              }
            }
            None => break,
          }
        }
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

    // Handle the case where this character is the start of a prefix, opener,
    // or closer, and adjust the character index accordingly.
    char_index += match prefix_index {
      Some(i) => {
        // If this is a prefix, add a list, with the tag of the prefix as the
        // first element of the list, to the AST.
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
          // If this is an opener, add a list, with the tag of the delimiter
          // pair (if it has one) as the first element of the list, to the AST.
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
          // If this is a closer, start popping off active openings until we
          // find a non-prefix (as any open prefixes should close with this
          // closer). Check that this non-prefix opening corresponds to this
          // closer, returning an error otherwise.
          Some(i) => loop {
            match active_openings.pop() {
              Some(opening) => {
                if !opening.is_prefix {
                  if i != opening.delimiter_or_prefix_index {
                    return Err(QuootParseError::MismatchedCloser(
                      delimiters[opening.delimiter_or_prefix_index]
                        .0
                        .iter()
                        .collect(),
                      delimiters[i].1.iter().collect(),
                    ));
                  }
                  // Check whether the next belongs to a prefix, and if so close
                  // that opener. Repeat this until there are no openings left,
                  // or we encounter a non-prefix opening.
                  loop {
                    match active_openings.last() {
                      Some(next_opening) => {
                        if next_opening.is_prefix {
                          active_openings.pop();
                        } else {
                          break;
                        }
                      }
                      None => break,
                    }
                  }
                  break delimiters[i].1.len();
                }
              }
              None => {
                return Err(QuootParseError::UnmatchedCloser(
                  delimiters[i].1.iter().collect(),
                ))
              }
            }
          },
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
  // Check all remaining active openings, and throw an error if any of them
  // are unclosed delimiters (as opposed to prefixes).
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

  // If consumption isn't caught up to the end of the string, that means the
  // string must end with a terminal not followed by whitespace, so add one
  // final leaf to the AST.
  if consumed_index < char_index {
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
