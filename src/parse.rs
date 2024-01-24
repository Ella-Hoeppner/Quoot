#[derive(Debug)]
pub enum QuootType {
  List(Vec<QuootType>),
  Symbol(String),
}

#[derive(Debug)]
pub enum QuootParseError {
  UnmatchedCloser,
  UnclosedOpener,
}

pub fn parse_chars(chars: &[char]) -> Result<QuootType, QuootParseError> {
  let mut char_index: usize = 0;
  let mut opener_index: usize = usize::MAX;
  let mut closer_index: usize = usize::MAX;
  let mut tree_depth: usize = 0;
  loop {
    if char_index >= chars.len() {
      break;
    }
    /*println!(
      "parsing: {:?},{:?},{:?},{:?}",
      opener_index, closer_index, char_index, chars[char_index]
    );*/
    let char = chars[char_index];
    if char == '(' {
      if tree_depth == 0 {
        opener_index = char_index;
      }
      tree_depth += 1;
    }
    if char == ')' {
      if tree_depth == 0 {
        return Err(QuootParseError::UnmatchedCloser);
      }
      if tree_depth == 1 {
        closer_index = char_index;
        break;
      }
      tree_depth -= 1;
    }
    char_index += 1;
  }
  if opener_index == usize::MAX || closer_index == usize::MAX {
    return Err(QuootParseError::UnclosedOpener);
  }
  Ok(QuootType::List(vec![QuootType::Symbol(
    chars.iter().collect(),
  )]))
}

pub fn parse(s: &str) -> Result<QuootType, QuootParseError> {
  let chars: Vec<char> = s.chars().collect();
  parse_chars(&chars)
}

pub fn test_parser() {
  println!("\n{:?}", parse("(hello!)"));
}
