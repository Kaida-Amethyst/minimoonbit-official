pub enum TokenType {
  Integer(Int)
  Long(Int64)
  Unsigned(UInt)
  UnsignedLong(UInt64)
  Float(Float)
  Double(Double)
  Boolean(Bool)
  Operator(String) // *, /, %, +, -, ^, &, |, ~, <<, >>, . etc.
  Symbol(String) // .., :, ::, 
  Bracket(Char) // (, ), [, ], {, }
  StringLiteral(String)
  CharLiteral(Char)
  LowerCase(String)
  UpperCase(String)
  KeyWord(String) // if, while, for, loop, etc.
  Comma(Char) // ,
  Terminator(Char) // ;, \n, \0
  WildCard
  EOF
}

pub fn to_string(self : TokenType) -> String {
  match self {
    TokenType::Integer(v) => "Integer(\{v.to_string()})"
    TokenType::Long(v) => "Long(\{v.to_string()})"
    TokenType::Unsigned(v) => "Unsigned(\{v.to_string()})"
    TokenType::UnsignedLong(v) => "UnsignedLong(\{v.to_string()})"
    TokenType::Double(v) => "Double(\{v.to_string()})"
    TokenType::Boolean(v) => "Boolean(\{v.to_string()})"
    TokenType::Operator(v) => "Operator(\{v})"
    TokenType::Symbol(v) => "Symbol(\{v})"
    TokenType::Bracket(v) => "Bracket(\{v})"
    TokenType::StringLiteral(v) => "StringLiteral(\{v})"
    TokenType::CharLiteral(v) => "CharLiteral(\{v})"
    TokenType::LowerCase(v) => "LowerCase(\{v})"
    TokenType::UpperCase(v) => "UpperCase(\{v})"
    TokenType::KeyWord(v) => "KeyWord(\{v})"
    TokenType::Comma(v) => "Comma(\{v})"
    TokenType::Terminator(v) => "Terminator(\{v})"
    TokenType::WildCard => "WildCard(_)"
    TokenType::EOF => "EOF".to_string()
  }
}

pub fn to_string_tag_only(self : TokenType) -> String {
  match self {
    TokenType::Integer(_) => "Integer"
    TokenType::Long(_) => "Long"
    TokenType::Unsigned(_) => "Unsigned"
    TokenType::UnsignedLong(_) => "UnsignedLong"
    TokenType::Double(_) => "Double"
    TokenType::Boolean(_) => "Boolean"
    TokenType::Operator(_) => "Operator"
    TokenType::Symbol(_) => "Symbol"
    TokenType::Bracket(_) => "Bracket"
    TokenType::StringLiteral(_) => "StringLiteral"
    TokenType::CharLiteral(_) => "CharLiteral"
    TokenType::LowerCase(_) => "LowerCase"
    TokenType::UpperCase(_) => "UpperCase"
    TokenType::KeyWord(_) => "KeyWord"
    TokenType::Comma(_) => "Comma"
    TokenType::Terminator(_) => "Terminator"
    TokenType::WildCard => "WildCard"
    TokenType::EOF => "EOF"
  }
}

pub struct Token {
  context : Context
  token_type : TokenType
  line : Int
  column : Int
  start_loc : Int
  end_loc : Int
  mut err_msg : String
}

pub fn write_error(self : Token, errmsg : String) -> Unit {
  if self.err_msg == "" {
    self.err_msg = errmsg
  }
}

pub fn get_token_content(self : Token) -> String {
  match self.token_type {
    TokenType::EOF => return "EOF".to_string()
    _ => ()
  }
  self.context.code.substring(start=self.start_loc, end=self.end_loc)
}

pub fn Token::new(
  context : Context,
  token_type : TokenType,
  start_loc : Int,
  end_loc : Int,
  line : Int,
  column : Int
) -> Token {
  Token::{
    context,
    token_type,
    line,
    column,
    start_loc,
    end_loc,
    err_msg: String::default(),
  }
}

pub fn Token::new_string_literal(
  context : Context,
  str : String,
  start_loc : Int,
  end_loc : Int,
  line : Int,
  column : Int
) -> Token {
  Token::new(
    context,
    TokenType::StringLiteral(str),
    start_loc,
    end_loc,
    line,
    column,
  )
}

pub fn Token::new_char_literal(
  context : Context,
  ch : Char,
  start_loc : Int,
  end_loc : Int,
  line : Int,
  column : Int
) -> Token {
  Token::new(
    context,
    TokenType::CharLiteral(ch),
    start_loc,
    end_loc,
    line,
    column,
  )
}

pub fn output(self : Token, logger : Logger) -> Unit {
  logger.write_string(self.to_string())
}

pub fn to_string(self : Token) -> String {
  let mut token_type_str = self.token_type.to_string_tag_only() + ","
  while token_type_str.length() < 14 {
    token_type_str += " "
  }
  let mut line_str = "line: " + self.line.to_string() + ","
  while line_str.length() < 10 {
    line_str = line_str + " "
  }
  let mut column_str = "column: " + self.column.to_string() + ","
  while column_str.length() < 12 {
    column_str += " "
  }

  // let mut loc_str = "loc: (" + self.start_loc.to_string() + ", " + self.end_loc.to_string() + ")"
  // while loc_str.length() < 16 {
  //   loc_str += " "
  // }

  let mut token_content = self.get_token_content()
  if token_content.length() > 10 {
    token_content = token_content.substring(start=0, end=10) + "..."
  }
  token_content = "content: " + token_content

  // "Token { " + token_type_str + line_str + column_str + loc_str + token_content + "}"
  "Token { " + token_type_str + line_str + column_str + token_content + " }"
}

pub fn get_errmsg(self : Token, errmsg : String) -> String {
  match self.token_type {
    TokenType::EOF => return ""
    _ => ()
  }
  let Some(line_start) = self.context.line_start_index[self.line]
  let Some(line_end) = self.context.line_end_index[self.line]
  let line_hint = "\{self.line + 1}"
  let line_hint_len = line_hint.length()
  let line_hint1 = line_hint + " |"
  let line_hint2 = repeat(" ", line_hint_len) + " |"
  let err_code = self.context.get_line(self.line)
  let err_code = "\n" + line_hint1 + err_code + "\n"
  let err_point = line_hint2 + repeat(" ", self.column) + "^\n"
  let err_point2 = line_hint2 + repeat(" ", self.column) + "|\n"
  let err_desc = line_hint2 + repeat(" ", self.column) + errmsg
  err_code + err_point + err_point2 + err_desc
}

pub fn to_string_detailed(self : Token) -> String {
  match self.token_type {
    TokenType::EOF => return ""
    _ => ()
  }
  // let Some(line_start) = self.context.line_start_index[self.line]
  // let Some(line_end) = self.context.line_end_index[self.line]
  let line_hint = "\{self.line + 1}"
  let line_hint_len = line_hint.length()
  let line_hint1 = line_hint + " |"
  let line_hint2 = repeat(" ", line_hint_len) + " |"
  let code = self.context.get_line(self.line)
  let code = "\n" + line_hint1 + code + "\n"
  let point = line_hint2 + repeat(" ", self.column) + "^\n"
  let point2 = line_hint2 + repeat(" ", self.column) + "|\n"
  let desc = line_hint2 +
    repeat(" ", self.column) +
    self.token_type.to_string_tag_only()
  code + point + point2 + desc
}

pub fn throw_err(self : Token) -> Unit {
  if self.err_msg == "" {
    return
  }
  let errmsg = @Color.red_font(self.err_msg)
  // let Some(line_start) = self.context.line_start_index[self.line]
  // let Some(line_end) = self.context.line_end_index[self.line]
  let line_hint = "\{self.line + 1}"
  let line_hint_len = line_hint.length()
  let line_hint1 = line_hint + " |"
  let line_hint2 = repeat(" ", line_hint_len) + " |"
  let err_code = self.context.get_line(self.line)
  let err_code = "\n" + line_hint1 + err_code + "\n"
  let err_point = line_hint2 + repeat(" ", self.column) + "^\n"
  let err_desc = line_hint2 + repeat(" ", self.column) + errmsg
  // let errmsg = err_code + err_point + err_point2 + err_desc
  let errmsg = err_code + err_point + err_desc
  println(errmsg)
  self.err_msg = ""
}

// Must Public
pub fn is_operator(self : Token) -> Bool {
  match self.token_type {
    TokenType::Operator(_) => true
    _ => false
  }
}
