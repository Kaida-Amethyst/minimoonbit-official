pub struct Lexer {
  context : Context
  mut idx : Int
  mut line : Int
  mut column : Int
  mut error_msg : String
}

pub fn Lexer::new(context : Context) -> Lexer {
  Lexer::{ context, idx: 0, line: 0, column: 0, error_msg: "" }
}

fn write_error(self : Lexer, msg : String) -> Unit {
  let mut end_idx = self.get_current_loc()
  while end_idx < self.context.code.length() {
    let c = self.context.code[end_idx]
    if c == '\n' {
      break
    }
    end_idx += 1
  }
  guard let Some(start_idx) = self.context.line_start_index[self.line]
  let err_code = self.sub_string(start_idx, end_idx)
  let err_point = repeat(" ", self.column) + "^\n"
  let err_point2 = repeat(" ", self.column) + "|\n"
  let err_desc = repeat(" ", self.column) + msg + "\n"
  self.error_msg = err_code + err_point + err_point2 + err_desc
}

fn sub_string(self : Lexer, start : Int, end : Int) -> String {
  if start < 0 || end < 0 {
    return ""
  }
  if end > self.context.code.length() {
    return ""
  }
  if start >= end {
    return ""
  }
  self.context.code.substring(~start, ~end)
}

fn get_tokens(self : Lexer) -> Array[Token] {
  self.context.tokens
}

fn get_current_loc(self : Lexer) -> Int {
  self.idx
}

fn get_current_line(self : Lexer) -> Int {
  self.line
}

fn forward(self : Lexer) -> Unit {
  self.idx += 1
  self.column += 1
}

fn forward_twice(self : Lexer) -> Unit {
  self.idx += 2
  self.column += 2
}

fn forward_n(self : Lexer, n : Int) -> Unit {
  self.idx += n
  self.column += n
}

fn forward_with(self : Lexer, c : Char) -> Bool {
  if self.current_is(c) {
    self.forward()
    true
  } else {
    false
  }
}

fn forward_with_str(self : Lexer, s : String) -> Bool {
  if self.current_is_followed_by(s) {
    self.forward_n(s.length())
    true
  } else {
    false
  }
}

fn forward_with_alpha(self : Lexer) -> Bool {
  if self.current_is_alpha() {
    self.forward()
    true
  } else {
    false
  }
}

fn forward_with_digit(self : Lexer) -> Bool {
  if self.current_is_digit() {
    self.forward()
    true
  } else {
    false
  }
}

fn forward_with_hex_digit(self : Lexer) -> Bool {
  if self.current_is_hex_digit() {
    self.forward()
    true
  } else {
    false
  }
}

fn forward_with_oct_digit(self : Lexer) -> Bool {
  if self.current_is_oct_digit() {
    self.forward()
    true
  } else {
    false
  }
}

fn is_end(self : Lexer) -> Bool {
  self.idx >= self.context.code.length()
}

fn is_not_end(self : Lexer) -> Bool {
  self.idx < self.context.code.length()
}

fn is_last(self : Lexer) -> Bool {
  self.idx == self.context.code.length() - 1
}

fn has_next(self : Lexer) -> Bool {
  self.idx < self.context.code.length() - 1
}

fn get(self : Lexer) -> Char? {
  if self.is_end() {
    None
  } else {
    Some(self.context.code[self.idx])
  }
}

fn get_and_forward(self : Lexer) -> Char? {
  if self.is_end() {
    None
  } else {
    let c = self.context.code[self.idx]
    self.forward()
    Some(c)
  }
}

fn get_n(self : Lexer, n : Int) -> String? {
  if self.idx + n <= self.context.code.length() {
    Some(self.sub_string(self.idx, self.idx + n))
  } else {
    None
  }
}

fn get_and_forward_n(self : Lexer, n : Int) -> String? {
  if self.idx + n <= self.context.code.length() {
    let sub_str = self.sub_string(self.idx, self.idx + n)
    self.forward_n(n)
    Some(sub_str)
  } else {
    None
  }
}

fn get_next(self : Lexer) -> Char? {
  if self.has_next() {
    Some(self.context.code[self.idx + 1])
  } else {
    None
  }
}

fn nth(self : Lexer, n : Int) -> Char? {
  if self.idx + n < self.context.code.length() {
    Some(self.context.code[self.idx + n])
  } else {
    None
  }
}

fn line_feed(self : Lexer) -> Unit {
  self.context.line_end_index[self.line] = self.idx
  self.context.line_count += 1
  self.line += 1
  self.column = 0
  self.idx += 1
  self.context.line_start_index[self.line] = self.idx
}

fn current_is(self : Lexer, c : Char) -> Bool {
  match self.get() {
    Some(a) => c == a
    None => false
  }
}

fn current_is_followed_by(self : Lexer, s : String) -> Bool {
  let s_len = s.length()
  match self.get_n(s_len) {
    Some(sub_str) => s == sub_str
    None => false
  }
}

fn current_is_not(self : Lexer, c : Char) -> Bool {
  match self.get() {
    Some(a) => c != a
    None => true
  }
}

fn next_is(self : Lexer, c : Char) -> Bool {
  match self.get_next() {
    Some(a) => c == a
    None => false
  }
}

fn next_is_whitespace(self : Lexer) -> Bool {
  match self.get_next() {
    Some(c) => is_whitespace(c)
    None => false
  }
}

fn next_is_not(self : Lexer, c : Char) -> Bool {
  match self.get_next() {
    Some(a) => c != a
    None => true
  }
}

fn current_is_digit(self : Lexer) -> Bool {
  match self.get() {
    Some(c) => is_digit(c)
    None => false
  }
}

fn current_is_not_digit(self : Lexer) -> Bool {
  match self.get() {
    Some(c) => not(is_digit(c))
    None => true
  }
}

fn next_is_digit(self : Lexer) -> Bool {
  match self.get_next() {
    Some(c) => is_digit(c)
    None => false
  }
}

fn next_is_not_digit(self : Lexer) -> Bool {
  match self.get_next() {
    Some(c) => not(is_digit(c))
    None => true
  }
}

fn current_is_hex_digit(self : Lexer) -> Bool {
  match self.get() {
    Some(c) => is_hex_digit(c)
    None => false
  }
}

fn current_is_not_hex_digit(self : Lexer) -> Bool {
  match self.get() {
    Some(c) => not(is_hex_digit(c))
    None => true
  }
}

fn next_is_hex_digit(self : Lexer) -> Bool {
  match self.get_next() {
    Some(c) => is_hex_digit(c)
    None => false
  }
}

fn next_is_not_hex_digit(self : Lexer) -> Bool {
  match self.get_next() {
    Some(c) => not(is_hex_digit(c))
    None => true
  }
}

fn current_is_oct_digit(self : Lexer) -> Bool {
  match self.get() {
    Some(c) => is_oct_digit(c)
    None => false
  }
}

fn current_is_not_oct_digit(self : Lexer) -> Bool {
  match self.get() {
    Some(c) => not(is_oct_digit(c))
    None => true
  }
}

fn current_is_uppercase(self : Lexer) -> Bool {
  match self.get() {
    Some(c) => is_uppercase(c)
    None => false
  }
}

fn current_is_not_uppercase(self : Lexer) -> Bool {
  match self.get() {
    Some(c) => not(is_uppercase(c))
    None => true
  }
}

fn current_is_lowercase(self : Lexer) -> Bool {
  match self.get() {
    Some(c) => is_lowercase(c)
    None => false
  }
}

fn current_is_not_lowercase(self : Lexer) -> Bool {
  match self.get() {
    Some(c) => not(is_lowercase(c))
    None => true
  }
}

fn next_is_lowercase(self : Lexer) -> Bool {
  match self.get_next() {
    Some(c) => is_lowercase(c)
    None => false
  }
}

fn next_is_not_lowercase(self : Lexer) -> Bool {
  match self.get_next() {
    Some(c) => not(is_lowercase(c))
    None => true
  }
}

fn current_is_alpha(self : Lexer) -> Bool {
  match self.get() {
    Some(c) => is_alpha(c)
    None => false
  }
}

fn current_is_not_alpha(self : Lexer) -> Bool {
  match self.get() {
    Some(c) => not(is_alpha(c))
    None => true
  }
}

fn next_is_alpha(self : Lexer) -> Bool {
  match self.get_next() {
    Some(c) => is_alpha(c)
    None => false
  }
}

fn next_is_not_alpha(self : Lexer) -> Bool {
  match self.get_next() {
    Some(c) => not(is_alpha(c))
    None => true
  }
}

fn current_is_symbol(self : Lexer) -> Bool {
  match self.get() {
    Some(c) => is_symbol(c)
    None => false
  }
}

fn current_is_not_symbol(self : Lexer) -> Bool {
  match self.get() {
    Some(c) => not(is_symbol(c))
    None => true
  }
}

fn next_is_symbol(self : Lexer) -> Bool {
  match self.get_next() {
    Some(c) => is_symbol(c)
    None => false
  }
}

fn next_is_not_symbol(self : Lexer) -> Bool {
  match self.get_next() {
    Some(c) => not(is_symbol(c))
    None => true
  }
}

fn push_token(self : Lexer, tok : Token) -> Unit {
  self.context.push_token(tok)
}

fn create_eof(self : Lexer) -> Token {
  let tok = Token::new(
    self.context,
    TokenType::EOF,
    self.get_current_loc(),
    self.get_current_loc(),
    self.get_current_line(),
    self.column,
  )
  self.push_token(tok)
  tok
}

fn create_terminator(self : Lexer) -> Token {
  let tok = Token::new(
    self.context,
    TokenType::Terminator(';'),
    self.get_current_loc(),
    self.get_current_loc() + 1,
    self.get_current_line(),
    self.column,
  )
  self.push_token(tok)
  self.forward()
  tok
}

fn create_comma(self : Lexer) -> Token {
  let tok = Token::new(
    self.context,
    TokenType::Comma(','),
    self.get_current_loc(),
    self.get_current_loc() + 1,
    self.get_current_line(),
    self.column,
  )
  self.push_token(tok)
  self.forward()
  tok
}

fn create_parenthesis(self : Lexer) -> Token {
  guard let Some(c) = self.get()
  let tok = Token::new(
    self.context,
    TokenType::Bracket(c),
    self.get_current_loc(),
    self.get_current_loc() + 1,
    self.get_current_line(),
    self.column,
  )
  self.push_token(tok)
  self.forward()
  tok
}

fn create_wildcard(self : Lexer) -> Token {
  let tok = Token::new(
    self.context,
    TokenType::WildCard,
    self.get_current_loc(),
    self.get_current_loc() + 1,
    self.get_current_line(),
    self.column,
  )
  self.push_token(tok)
  self.forward()
  tok
}

fn create_floating(self : Lexer, sub_str : String) -> Token {
  let v = stod(sub_str)
  let tok = Token::new(
    self.context,
    TokenType::Double(v),
    self.get_current_loc() - sub_str.length(),
    self.get_current_loc(),
    self.get_current_line(),
    self.column - sub_str.length(),
  )
  self.push_token(tok)
  tok
}

fn create_integer(self : Lexer, sub_str : String) -> Token {
  let v = stoi(sub_str)
  let tok = Token::new(
    self.context,
    TokenType::Integer(v),
    self.get_current_loc() - sub_str.length(),
    self.get_current_loc(),
    self.get_current_line(),
    self.column - sub_str.length(),
  )
  self.push_token(tok)
  tok
}

fn create_long(self : Lexer, sub_str : String) -> Token {
  let v = stol(sub_str)
  let tok = Token::new(
    self.context,
    TokenType::Long(v),
    self.get_current_loc() - sub_str.length(),
    self.get_current_loc(),
    self.get_current_line(),
    self.column - sub_str.length(),
  )
  self.push_token(tok)
  tok
}

fn create_unsigned(self : Lexer, sub_str : String) -> Token {
  let v = stou(sub_str)
  let tok = Token::new(
    self.context,
    TokenType::Unsigned(v),
    self.get_current_loc() - sub_str.length(),
    self.get_current_loc(),
    self.get_current_line(),
    self.column - sub_str.length(),
  )
  self.push_token(tok)
  tok
}

fn create_unsigned_long(self : Lexer, sub_str : String) -> Token {
  let v = stoul(sub_str)
  let tok = Token::new(
    self.context,
    TokenType::UnsignedLong(v),
    self.get_current_loc() - sub_str.length(),
    self.get_current_loc(),
    self.get_current_line(),
    self.column - sub_str.length(),
  )
  self.push_token(tok)
  tok
}

fn create_true(self : Lexer) -> Token {
  let tok = Token::new(
    self.context,
    TokenType::Boolean(true),
    self.get_current_loc() - 4,
    self.get_current_loc(),
    self.get_current_line(),
    self.column - 4,
  )
  self.push_token(tok)
  tok
}

fn create_false(self : Lexer) -> Token {
  let tok = Token::new(
    self.context,
    TokenType::Boolean(false),
    self.get_current_loc() - 5,
    self.get_current_loc(),
    self.get_current_line(),
    self.column - 5,
  )
  self.push_token(tok)
  tok
}

fn create_keyword(self : Lexer, sub_str : String) -> Token {
  let tok = Token::new(
    self.context,
    TokenType::KeyWord(sub_str),
    self.get_current_loc() - sub_str.length(),
    self.get_current_loc(),
    self.get_current_line(),
    self.column - sub_str.length(),
  )
  self.push_token(tok)
  tok
}

fn create_uppercase(self : Lexer, sub_str : String) -> Token {
  let tok = Token::new(
    self.context,
    TokenType::UpperCase(sub_str),
    self.get_current_loc() - sub_str.length(),
    self.get_current_loc(),
    self.get_current_line(),
    self.column - sub_str.length(),
  )
  self.push_token(tok)
  tok
}

fn create_lowercase(self : Lexer, sub_str : String) -> Token {
  let tok = Token::new(
    self.context,
    TokenType::LowerCase(sub_str),
    self.get_current_loc() - sub_str.length(),
    self.get_current_loc(),
    self.get_current_line(),
    self.column - sub_str.length(),
  )
  self.push_token(tok)
  tok
}

fn create_operator(self : Lexer, sub_str : String) -> Token {
  let tok = Token::new(
    self.context,
    TokenType::Operator(sub_str),
    self.get_current_loc() - sub_str.length(),
    self.get_current_loc(),
    self.get_current_line(),
    self.column - sub_str.length(),
  )
  self.push_token(tok)
  tok
}

let keywords = [
  "if", "else", "while", "for", "break", "continue", "as", "pub", "fn", "struct",
  "enum", "impl", "trait", "type", "let", "mut", "use", "mod", "return", "match",
  "loop", "in",
]

fn create_word(self : Lexer) -> Token {
  if self.current_is('_') && self.next_is_whitespace() {
    self.forward()
    return self.create_wildcard()
  }
  let start_loc = self.get_current_loc()
  guard let Some(first_char) = self.get()
  while self.forward_with_alpha() ||
        self.forward_with('_') ||
        self.forward_with_digit() {

  }
  let sub_str = self.sub_string(start_loc, self.get_current_loc())
  match sub_str {
    "true" => return self.create_true()
    "false" => return self.create_false()
    _ => ()
  }
  if keywords.iter().any(fn(s) { s == sub_str }) {
    return self.create_keyword(sub_str)
  }
  if is_uppercase(first_char) {
    return self.create_uppercase(sub_str)
  }
  // if sub_str == "_" {
  //   return self.create_wildcard()
  // }
  self.create_lowercase(sub_str)
}

fn create_normal_symbol(self : Lexer, sub_str : String) -> Token {
  guard let Some(c) = self.get()
  let tok = Token::new(
    self.context,
    TokenType::Symbol(sub_str),
    self.get_current_loc() - sub_str.length(),
    self.get_current_loc(),
    self.get_current_line(),
    self.column - sub_str.length(),
  )
  self.push_token(tok)
  tok
}

let op_chars = ['+', '-', '*', '/', '%', '&', '|', '^', '>', '<', '=']

let ops = [
  "+=", "-=", "*=", "/=", "%=", ">=", "<=", "==", "!=", "&&", "||", ">>", "<<",
]

let symbols = ["..", "=>", "::", "->"]

fn create_symbol(self : Lexer) -> Token {
  if self.next_is_not_symbol() {
    guard let Some(c) = self.get_and_forward()
    if op_chars.contains(c) {
      return self.create_operator(c.to_string())
    } else {
      return self.create_normal_symbol(c.to_string())
    }
  }
  guard let Some(op) = self.get_and_forward_n(2)
  if ops.contains(op) {
    return self.create_operator(op)
  }
  if symbols.contains(op) {
    return self.create_normal_symbol(op)
  }
  self.create_eof()
}

fn create_string_literal(self : Lexer) -> Token!Error {
  let start_idx = self.get_current_loc()
  guard let Some(quote) = self.get_and_forward()
  while self.current_is_not(quote) {
    guard let Some(c) = self.get()
    match c {
      '\n' => {
        self.write_error("Unexpected newline in string literal")
        raise LexicalError(self)
      }
      _ => ()
    }
    self.forward()
  }
  if self.is_end() {
    self.write_error("Not found closing quote for string literal")
    raise LexicalError(self)
  }
  self.forward()
  let sub_str = self.sub_string(start_idx, self.get_current_loc())
  let tok = Token::new_string_literal(
    self.context,
    sub_str,
    start_idx,
    self.get_current_loc(),
    self.get_current_line(),
    self.column,
  )
  tok
}

// TODO: Lacking escape character handling
fn create_char_literal(self : Lexer) -> Token {
  let start_idx = self.get_current_loc()
  let Some(c) = self.nth(1)
  self.forward_twice()
  let tok = Token::new_char_literal(
    self.context,
    c,
    start_idx,
    self.get_current_loc(),
    self.get_current_line(),
    self.column,
  )
  self.push_token(tok)
  tok
}

fn create_hexical_numeric(self : Lexer) -> Token {
  let start_loc = self.get_current_loc()
  let _ = self.forward_with_str("0x") || self.forward_with_str("0X")
  while self.forward_with_hex_digit() {

  }
  let sub_str = self.sub_string(start_loc, self.get_current_loc())

  // TODO: use match if when moonbit supports it
  if self.forward_with_str("UL") {
    return self.create_unsigned_long(sub_str)
  }
  if self.forward_with('L') {
    return self.create_long(sub_str)
  }
  if self.forward_with('U') {
    return self.create_unsigned(sub_str)
  }
  self.create_integer(sub_str)
}

fn create_octal_numeric(self : Lexer) -> Token {
  let start_loc = self.get_current_loc()
  let _ = self.forward_with_str("0o") || self.forward_with_str("0O")
  while self.forward_with_oct_digit() {

  }
  let sub_str = self.sub_string(start_loc, self.get_current_loc())

  // TODO: use match if when moonbit supports it
  if self.forward_with_str("UL") {
    return self.create_unsigned_long(sub_str)
  }
  if self.forward_with('L') {
    return self.create_long(sub_str)
  }
  if self.forward_with('U') {
    return self.create_unsigned(sub_str)
  }
  self.create_integer(sub_str)
}

fn create_numeric(self : Lexer) -> Token {
  if self.is_last() {
    let Some(c) = self.get()
    self.forward()
    return self.create_integer(c.to_string())
  }
  if self.current_is_followed_by("0x") || self.current_is_followed_by("0X") {
    return self.create_hexical_numeric()
  }
  if self.current_is_followed_by("0o") || self.current_is_followed_by("0O") {
    return self.create_octal_numeric()
  }
  let mut has_dot = false
  let mut has_exp = false
  let start_loc = self.get_current_loc()
  while self.is_not_end() {
    if has_dot && self.current_is('.') {
      break
    }
    if has_exp && (self.current_is('e') || self.current_is('E')) {
      break
    }
    if self.forward_with_digit() {
      continue
    }
    if self.forward_with('.') {
      has_dot = true
      continue
    }
    if self.forward_with('e') || self.forward_with('E') {
      has_exp = true
      let _ = self.forward_with('+') || self.forward_with('-')
      continue
    }

    // Not a digit, dot, or exp, break
    break
  }
  let sub_str = self.sub_string(start_loc, self.get_current_loc())
  if has_dot || has_exp {
    return self.create_floating(sub_str)
  }

  // TODO: use match if when moonbit supports it
  if self.forward_with_str("UL") {
    return self.create_unsigned_long(sub_str)
  }
  if self.forward_with('L') {
    return self.create_long(sub_str)
  }
  if self.forward_with('U') {
    return self.create_unsigned(sub_str)
  }
  self.create_integer(sub_str)
}

pub fn advance(self : Lexer) -> Token!Error {
  if self.is_end() {
    return self.create_eof()
  }
  while self.is_not_end() {
    guard let Some(c) = self.get() else { None => break }
    match c {
      ';' => return self.create_terminator()
      ',' => return self.create_comma()
      '_' => return self.create_word()
      '\"' => return self.create_string_literal!()
      '\'' => return self.create_char_literal()
      _ => ()
    }
    if c == '\n' {
      self.line_feed()
      continue
    }
    if is_whitespace(c) {
      self.forward()
      continue
    }
    if is_digit(c) {
      return self.create_numeric()
    }
    if is_alpha(c) {
      return self.create_word()
    }
    if is_parenthesis(c) {
      return self.create_parenthesis()
    }
    if is_symbol(c) {
      return self.create_symbol()
    }
    break
  }
  if self.is_not_end() {
    guard let Some(c) = self.get()
    self.write_error("Lexcial Error, Unknown character: \'\{c}\'")
    raise LexicalError(self)
  }
  self.create_eof()
}
