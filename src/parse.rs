#[derive(Debug)]
pub enum QuootExpression {
  List(Vec<QuootExpression>),
  Symbol(String),
}

#[derive(Debug)]
pub enum QuootParseError {
  UnmatchedCloser,
  UnclosedOpener,
}

fn is_whitespace(c: char) -> bool {
  c == ' ' || c == '\t' || c == '\n'
}

pub fn parse_chars(chars: &[char]) -> Result<Vec<QuootExpression>, QuootParseError> {
  let mut char_index: usize = 0;
  let mut consumption_index: usize = 0;
  let mut opener_index: usize = usize::MAX;
  let mut tree_depth: usize = 0;
  let mut elements: Vec<QuootExpression> = vec![];
  loop {
    if char_index >= chars.len() {
      break;
    }
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
        let internal_chars = &chars[opener_index + 1..char_index];
        let result = parse_chars(internal_chars);
        match result {
          Ok(expressions) => elements.push(QuootExpression::List(expressions)),
          Err(error) => return Err(error),
        };
        opener_index = usize::MAX;
        consumption_index = char_index + 1;
      }
      tree_depth -= 1;
    }
    if tree_depth == 0 && is_whitespace(char) {
      if char_index != consumption_index {
        elements.push(QuootExpression::Symbol(
          chars[consumption_index..char_index].iter().collect(),
        ));
      }
      consumption_index = char_index + 1;
    }
    char_index += 1;
  }
  if opener_index != usize::MAX {
    return Err(QuootParseError::UnclosedOpener);
  }
  if consumption_index < chars.len() {
    elements.push(QuootExpression::Symbol(
      chars[consumption_index..chars.len()].iter().collect(),
    ));
  }
  Ok(elements)
}

pub fn parse(s: &str) -> Result<Vec<QuootExpression>, QuootParseError> {
  let chars: Vec<char> = s.chars().collect();
  parse_chars(&chars)
}

pub fn test_parser() {
  println!("\n{:?}", parse("(hello!)"));
}
