pub struct Parser {
  context : Context
  tokens : Array[Token]
  mut idx : Int
  mut archive_idx : Int
  mut extra_err_msg : String
}

pub fn Parser::new(context : Context) -> Parser {
  Parser::{
    context,
    tokens: context.tokens,
    idx: 0,
    archive_idx: 0,
    extra_err_msg: String::default(),
  }
}

fn create_location(self : Parser, head_idx : Int, last_idx : Int) -> Location {
  let head_token = self.tokens[head_idx]
  let last_token = self.tokens[last_idx]
  Location::{ head_idx, last_idx, head_token, last_token }
}

fn create_meta(self : Parser, head_idx : Int, last_idx : Int) -> Meta {
  let location = self.create_location(head_idx, last_idx)
  Meta::new(location)
}

fn write_extra_err_msg(self : Parser, msg : String) -> Unit {
  self.extra_err_msg = msg
}

fn get(self : Parser) -> Token!Error {
  if self.idx >= self.tokens.length() {
    raise ParseError(self)
  }
  self.tokens[self.idx]
}

fn index(self : Parser) -> Int {
  self.idx
}

fn get_current_tok_type(self : Parser) -> TokenType!Error {
  let token = self.get!()
  token.token_type
}

fn look_back(self : Parser) -> Token!Error {
  if self.idx == 0 {
    raise ParseError(self)
  }
  self.tokens[self.idx - 1]
}

fn look_back_n(self : Parser, n : Int) -> Token!Error {
  if self.idx - n < 0 {
    raise ParseError(self)
  }
  self.tokens[self.idx - n]
}

fn look_ahead(self : Parser) -> Token!Error {
  if self.idx + 1 >= self.tokens.length() {
    raise ParseError(self)
  }
  self.tokens[self.idx + 1]
}

fn look_ahead_n(self : Parser, n : Int) -> Token!Error {
  if self.idx + n >= self.tokens.length() {
    raise ParseError(self)
  }
  self.tokens[self.idx + n]
}

fn look_ahead_is(self : Parser, s : String) -> Bool {
  if self.idx + 1 >= self.tokens.length() {
    return false
  }
  let token = self.tokens[self.idx + 1]
  token.get_token_content() == s
}

fn look_ahead_n_is_uppercase(self : Parser, n : Int) -> Bool {
  if self.idx + n >= self.tokens.length() {
    return false
  }
  let token = self.tokens[self.idx + n]
  match token.token_type {
    TokenType::UpperCase(_) => true
    _ => false
  }
}

fn look_ahead_n_is_lowercase(self : Parser, n : Int) -> Bool {
  if self.idx + n >= self.tokens.length() {
    return false
  }
  let token = self.tokens[self.idx + n]
  match token.token_type {
    TokenType::LowerCase(_) => true
    _ => false
  }
}

fn throw_msg(self : Parser, msg : String) -> Unit {
  let Ok(token) = self.get?()
  let msg = token.get_errmsg(msg)
  println(msg)
}

fn forward(self : Parser) -> Unit {
  self.idx += 1
}

fn forward_and_get(self : Parser) -> Token!Error {
  if self.idx >= self.tokens.length() {
    raise ParseError(self)
  }
  let token = self.tokens[self.idx]
  self.forward()
  return token
}

fn forward_with(self : Parser, s : String) -> Bool {
  if self.idx >= self.tokens.length() {
    return false
  }
  let token = self.tokens[self.idx]
  if token.get_token_content() != s {
    return false
  }
  self.forward()
  return true
}

fn forward_with_keyword(self : Parser, kw : String) -> Bool {
  if self.idx >= self.tokens.length() {
    return false
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::KeyWord(k) => if k != kw { return false }
    _ => return false
  }
  self.forward()
  return true
}

fn forward_with_bracket(self : Parser, p : Char) -> Bool {
  if self.idx >= self.tokens.length() {
    return false
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::Bracket(c) => if c != p { return false }
    _ => return false
  }
  self.forward()
  return true
}

fn forward_with_terminator(self : Parser) -> Bool {
  if self.idx >= self.tokens.length() {
    return false
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::Terminator(_) => {
      self.forward()
      return true
    }
    _ => return false
  }
}

fn forward_with_lowercase(self : Parser) -> Bool {
  if self.idx >= self.tokens.length() {
    return false
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::LowerCase(_) => {
      self.forward()
      return true
    }
    _ => return false
  }
}

fn forward_with_uppercase(self : Parser) -> Bool {
  if self.idx >= self.tokens.length() {
    return false
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::UpperCase(_) => {
      self.forward()
      return true
    }
    _ => return false
  }
}

fn forward_with_operator(self : Parser, op : String) -> Bool {
  if self.idx >= self.tokens.length() {
    return false
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::Operator(o) => if o != op { return false }
    _ => return false
  }
  self.forward()
  return true
}

fn forward_with_symbol(self : Parser, s : String) -> Bool {
  if self.idx >= self.tokens.length() {
    return false
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::Symbol(sym) => if sym != s { return false }
    _ => return false
  }
  self.forward()
  return true
}

fn forward_with_comma(self : Parser) -> Bool {
  if self.idx >= self.tokens.length() {
    return false
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::Comma(_) => {
      self.forward()
      return true
    }
    _ => return false
  }
}

fn skip(self : Parser, s : String) -> Unit!Error {
  if self.idx >= self.tokens.length() {
    self.write_extra_err_msg("Unexpected end of file")
    raise ParseError(self)
  }
  let token = self.tokens[self.idx]
  if token.get_token_content() != s {
    let msg = "Unexpected token: " + token.get_token_content()
    let msg = msg + ", Expected: `" + s + "`"
    self.write_extra_err_msg(msg)
    raise ParseError(self)
  }
  self.forward()
}

fn skip_bracket(self : Parser, p : Char) -> Unit!Error {
  if self.idx >= self.tokens.length() {
    self.write_extra_err_msg("Unexpected end of file")
    raise ParseError(self)
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::Bracket(c) =>
      if c != p {
        let msg = "Unexpected token: " + token.get_token_content()
        let msg = msg + ", Expected: `" + p.to_string() + "`"
        self.write_extra_err_msg(msg)
        raise ParseError(self)
      }
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected: `" + p.to_string() + "`"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
  self.forward()
}

fn skip_keyword(self : Parser, kw : String) -> Unit!Error {
  if self.idx >= self.tokens.length() {
    self.write_extra_err_msg("Unexpected end of file")
    raise ParseError(self)
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::KeyWord(k) =>
      if k != kw {
        let msg = "Unexpected token: " + token.get_token_content()
        let msg = msg + ", Expected: `" + kw + "`"
        self.write_extra_err_msg(msg)
        raise ParseError(self)
      }
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected: `" + kw + "`"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
  self.forward()
}

fn skip_operator(self : Parser, op : String) -> Unit!Error {
  if self.idx >= self.tokens.length() {
    self.write_extra_err_msg("Unexpected end of file")
    raise ParseError(self)
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::Operator(o) =>
      if o != op {
        let msg = "Unexpected token: " + token.get_token_content()
        let msg = msg + ", Expected: `" + op + "`"
        self.write_extra_err_msg(msg)
        raise ParseError(self)
      }
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected: `" + op + "`"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
  self.forward()
}

fn skip_symbol(self : Parser, s : String) -> Unit!Error {
  if self.idx >= self.tokens.length() {
    self.write_extra_err_msg("Unexpected end of file")
    raise ParseError(self)
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::Symbol(sym) =>
      if sym != s {
        let msg = "Unexpected token: " + token.get_token_content()
        let msg = msg + ", Expected: `" + s + "`"
        self.write_extra_err_msg(msg)
        raise ParseError(self)
      }
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected: `" + s + "`"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
  self.forward()
}

fn skip_comma(self : Parser) -> Unit!Error {
  if self.idx >= self.tokens.length() {
    self.write_extra_err_msg("Unexpected end of file")
    raise ParseError(self)
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::Comma(_) => self.forward()
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected: `,`"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
}

fn skip_and_get(self : Parser, s : String) -> Token!Error {
  if self.idx >= self.tokens.length() {
    raise ParseError(self)
  }
  let token = self.tokens[self.idx]
  if token.get_token_content() != s {
    let msg = "Unexpected token: " + token.get_token_content()
    let msg = msg + ", Expected: `" + s + "`"
    self.write_extra_err_msg(msg)
    raise ParseError(self)
  }
  self.forward()
  token
}

fn skip_and_index(self : Parser, s : String) -> Int!Error {
  if self.idx >= self.tokens.length() {
    raise ParseError(self)
  }
  let idx = self.idx
  let token = self.tokens[idx]
  if token.get_token_content() != s {
    let msg = "Unexpected token: " + token.get_token_content()
    let msg = msg + ", Expected: `" + s + "`"
    self.write_extra_err_msg(msg)
    raise ParseError(self)
  }
  self.forward()
  idx
}

fn skip_and_get_lowercase(
  self : Parser,
  ~scope : UpperCase? = None
) -> LowerCase!Error {
  let Ok(token) = self.get?()
  let lowercase = match token.token_type {
    TokenType::LowerCase(_) => {
      let meta = self.create_meta(self.idx, self.idx)
      self.forward()
      LowerCase::new(meta, token, ~scope)
    }
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected a lower case ident"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
  lowercase
}

fn skip_and_index_lowercase(self : Parser) -> Int!Error {
  if self.idx >= self.tokens.length() {
    raise ParseError(self)
  }
  let idx = self.idx
  let token = self.tokens[idx]
  match token.token_type {
    TokenType::LowerCase(_) => self.forward()
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected a lower case ident"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
  idx
}

fn skip_and_get_uppercase(self : Parser) -> UpperCase!Error {
  let Ok(token) = self.get?()
  let uppercase = match token.token_type {
    TokenType::UpperCase(_) => {
      let meta = self.create_meta(self.idx, self.idx)
      self.forward()
      UpperCase::new(meta, token)
    }
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected an upper case ident"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
  uppercase
}

fn skip_and_index_uppercase(self : Parser) -> Int!Error {
  if self.idx >= self.tokens.length() {
    raise ParseError(self)
  }
  let idx = self.idx
  let token = self.tokens[idx]
  match token.token_type {
    TokenType::UpperCase(_) => self.forward()
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected: an upper case ident"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
  idx
}

fn skip_and_get_assign_op(self : Parser) -> Operator!Error {
  let meta = self.create_meta(self.idx, self.idx)
  let Ok(token) = self.get?()
  match token.token_type {
    TokenType::Operator("=")
    | TokenType::Operator("+=")
    | TokenType::Operator("-=")
    | TokenType::Operator("*=")
    | TokenType::Operator("/=") | TokenType::Operator("%=") => {
      self.forward()
      Operator::new(meta, token)
    }
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected: an assign operator"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
}

fn skip_and_get_symbol(self : Parser, s : String) -> Token!Error {
  if self.idx >= self.tokens.length() {
    raise ParseError(self)
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::Symbol(sym) =>
      if sym != s {
        let msg = "Unexpected token: " + token.get_token_content()
        let msg = msg + ", Expected: `" + s + "`"
        self.write_extra_err_msg(msg)
        raise ParseError(self)
      }
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected: `" + s + "`"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
  self.forward()
  token
}

fn skip_and_index_assign_op(self : Parser) -> Int!Error {
  if self.idx >= self.tokens.length() {
    raise ParseError(self)
  }
  let idx = self.idx
  let token = self.tokens[idx]
  match token.token_type {
    TokenType::Operator("=")
    | TokenType::Operator("+=")
    | TokenType::Operator("-=")
    | TokenType::Operator("*=")
    | TokenType::Operator("/=") | TokenType::Operator("%=") => self.forward()
    _ => raise ParseError(self)
  }
  idx
}

fn skip_and_get_bracket(self : Parser, p : Char) -> Token!Error {
  if self.idx >= self.tokens.length() {
    raise ParseError(self)
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::Bracket(c) => if c != p { raise ParseError(self) }
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected: `" + p.to_string() + "`"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
  self.forward()
  token
}

fn skip_and_index_bracket(self : Parser, p : Char) -> Int!Error {
  if self.idx >= self.tokens.length() {
    raise ParseError(self)
  }
  let idx = self.idx
  let token = self.tokens[idx]
  match token.token_type {
    TokenType::Bracket(c) => if c != p { raise ParseError(self) }
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected: `" + p.to_string() + "`"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
  self.forward()
  idx
}

fn skip_and_get_keyword(self : Parser, kw : String) -> Token!Error {
  if self.idx >= self.tokens.length() {
    raise ParseError(self)
  }
  let token = self.tokens[self.idx]
  match token.token_type {
    TokenType::KeyWord(k) => if k != kw { raise ParseError(self) }
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected: `" + kw + "`"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
  self.forward()
  token
}

fn skip_and_index_keyword(self : Parser, kw : String) -> Int!Error {
  if self.idx >= self.tokens.length() {
    raise ParseError(self)
  }
  let idx = self.idx
  let token = self.tokens[idx]
  match token.token_type {
    TokenType::KeyWord(k) => if k != kw { raise ParseError(self) }
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected: `" + kw + "`"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
  self.forward()
  idx
}

fn skip_and_index_terminator(self : Parser) -> Int!Error {
  if self.idx >= self.tokens.length() {
    raise ParseError(self)
  }
  let idx = self.idx
  let token = self.tokens[idx]
  match token.token_type {
    TokenType::Terminator(_) => self.forward()
    _ => {
      let msg = "Unexpected token: " + token.get_token_content()
      let msg = msg + ", Expected: `;`"
      self.write_extra_err_msg(msg)
      raise ParseError(self)
    }
  }
  idx
}

fn archive(self : Parser) -> Unit {
  self.archive_idx = self.idx
}

fn restore(self : Parser) -> Unit {
  self.idx = self.archive_idx
  self.archive_idx = 0
}

fn current_is(self : Parser, tok : String) -> Bool {
  match self.get?() {
    Ok(token) => token.get_token_content() == tok
    Err(_) => false
  }
}

fn current_is_bracket(self : Parser, p : Char) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::Bracket(c) => c == p
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_dot(self : Parser) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::Symbol(".") => true
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_comma(self : Parser) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::Comma(_) => true
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_wildcard(self : Parser) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::WildCard => true
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_integer(self : Parser) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::Integer(_) => true
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_double(self : Parser) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::Double(_) => true
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_long(self : Parser) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::Long(_) => true
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_unsigned(self : Parser) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::Unsigned(_) => true
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_unsigned_long(self : Parser) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::UnsignedLong(_) => true
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_char(self : Parser) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::CharLiteral(_) => true
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_string(self : Parser) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::StringLiteral(_) => true
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_literal(self : Parser) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::Boolean(_) => true
        TokenType::Integer(_) => true
        TokenType::Double(_) => true
        TokenType::Long(_) => true
        TokenType::Unsigned(_) => true
        TokenType::UnsignedLong(_) => true
        TokenType::CharLiteral(_) => true
        TokenType::StringLiteral(_) => true
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_lower_case(self : Parser) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::LowerCase(_) => true
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_upper_case(self : Parser) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::UpperCase(_) => true
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_keyword(self : Parser, kw : String) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::KeyWord(k) => k == kw
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_terminator(self : Parser) -> Bool {
  match self.get?() {
    Ok(token) =>
      match token.token_type {
        TokenType::Terminator(_) => true
        _ => false
      }
    Err(_) => false
  }
}

fn current_is_infix_op(self : Parser) -> Bool {
  guard let Ok(token) = self.get?() else {
    Err(_) => {
      println("Compiler Parsing Error. Loc: Parser:: current_is_infix_op")
      panic()
    }
  }
  match token.token_type {
    TokenType::Operator("+") => true
    TokenType::Operator("-") => true
    TokenType::Operator("*") => true
    TokenType::Operator("/") => true
    TokenType::Operator("%") => true
    TokenType::Operator(">") => true
    TokenType::Operator("<") => true
    TokenType::Operator("==") => true
    TokenType::Operator("!=") => true
    TokenType::Operator(">=") => true
    TokenType::Operator("<=") => true
    TokenType::Operator("&&") => true
    TokenType::Operator("||") => true
    TokenType::Operator("<<") => true
    TokenType::Operator(">>") => true
    _ => false
  }
}

fn current_is_assign_op(self : Parser) -> Bool {
  guard let Ok(token) = self.get?() else {
    Err(_) => {
      println("Compiler Parsing Error. Loc: Parser:: current_is_assign_op")
      panic()
    }
  }
  match token.token_type {
    TokenType::Operator("=") => true
    TokenType::Operator("+=") => true
    TokenType::Operator("-=") => true
    TokenType::Operator("*=") => true
    TokenType::Operator("/=") => true
    TokenType::Operator("%=") => true
    _ => false
  }
}

fn current_is_eof(self : Parser) -> Bool {
  let token = match self.get?() {
    Ok(token) => token
    Err(_) => return true
  }
  match token.token_type {
    TokenType::EOF => true
    _ => false
  }
}

fn build_literal(self : Parser) -> Literal!Error {
  let meta = self.create_meta(self.idx, self.idx)
  let tok = self.forward_and_get!()
  let can_be_literal = match tok.token_type {
    TokenType::Boolean(b) => {
      b as CanBeLiteral
    }
    TokenType::Integer(i) => {
      i as CanBeLiteral
    }
    TokenType::Double(d) => {
      d as CanBeLiteral
    }
    TokenType::Long(l) => {
      l as CanBeLiteral
    }
    TokenType::Unsigned(u) => {
      u as CanBeLiteral
    }
    TokenType::UnsignedLong(ul) => {
      ul as CanBeLiteral
    }
    TokenType::CharLiteral(c) => {
      c as CanBeLiteral
    }
    TokenType::StringLiteral(s) => {
      s as CanBeLiteral
    }
    _ => raise ParseError(self)
  }
  can_be_literal.to_literal(meta)
}

fn build_lowercase(
  self : Parser,
  ~scope : UpperCase? = None
) -> LowerCase!Error {
  let meta = self.create_meta(self.idx, self.idx)
  let token = self.forward_and_get!()
  LowerCase::new(meta, token, ~scope)
}

fn build_uppercase(self : Parser) -> UpperCase!Error {
  let meta = self.create_meta(self.idx, self.idx)
  let token = self.forward_and_get!()
  UpperCase::new(meta, token)
}

fn build_infix_op(self : Parser) -> Operator!Error {
  let meta = self.create_meta(self.idx, self.idx)
  let token = self.forward_and_get!()
  Operator::new(meta, token)
}

fn build_unary_op(self : Parser) -> Operator!Error {
  let meta = self.create_meta(self.idx, self.idx)
  let token = self.forward_and_get!()
  Operator::new(meta, token)
}

fn build_access(
  self : Parser,
  apply_expr : ApplyExpr,
  accessor : Accessor
) -> AccessExpr {
  let head_idx = apply_expr.meta().get_head_idx()
  let last_idx = accessor.meta().get_last_idx()
  let meta = self.create_meta(head_idx, last_idx)
  AccessExpr::new(meta, apply_expr, accessor)
}

fn build_call(
  self : Parser,
  apply_expr : ApplyExpr,
  args_tuple : TupleExpr
) -> CallExpr {
  let args = args_tuple.exprs
  let head_idx = apply_expr.meta().get_head_idx()
  let last_idx = args_tuple.meta().get_last_idx()
  let meta = self.create_meta(head_idx, last_idx)
  CallExpr::new(meta, apply_expr, args)
}

fn build_binary_expr(
  self : Parser,
  op : Operator,
  left : Expr,
  right : Expr
) -> BinaryExpr {
  let head_idx = left.meta().get_head_idx()
  let last_idx = right.meta().get_last_idx()
  let meta = self.create_meta(head_idx, last_idx)
  BinaryExpr::new(meta, op, left, right)
}

fn build_unary_expr(self : Parser, op : Operator, expr : Expr) -> UnaryExpr {
  let head_idx = op.meta.get_head_idx()
  let last_idx = expr.meta().get_last_idx()
  let meta = self.create_meta(head_idx, last_idx)
  UnaryExpr::new(meta, op, expr)
}

fn build_tuple_expr(self : Parser, exprs : Array[Expr]) -> TupleExpr {
  let (head_idx, last_idx) = match exprs.length() {
    0 => (self.idx - 2, self.idx - 1)
    _ =>
      (
        exprs[0].meta().get_head_idx(),
        exprs.last().unwrap().meta().get_last_idx(),
      )
  }
  let meta = self.create_meta(head_idx, last_idx)
  TupleExpr::new(meta, exprs)
}

fn build_array_expr(self : Parser, exprs : Array[Expr]) -> ArrayExpr {
  let (head_idx, last_idx) = match exprs.length() {
    0 => (self.idx - 2, self.idx - 1)
    _ =>
      (
        exprs[0].meta().get_head_idx(),
        exprs.last().unwrap().meta().get_last_idx(),
      )
  }
  let meta = self.create_meta(head_idx, last_idx)
  ArrayExpr::new(meta, exprs)
}

fn build_if_expr(
  self : Parser,
  cond_expr : Expr,
  then_expr : BlockExpr,
  else_clause : ElseClause?
) -> IfExpr {
  let head_idx = cond_expr.meta().get_head_idx() - 1
  let last_idx = match else_clause {
    Some(else_clause) => else_clause.meta().get_last_idx()
    None => then_expr.meta().get_last_idx()
  }
  let meta = self.create_meta(head_idx, last_idx)
  IfExpr::new(meta, cond_expr, then_expr, else_clause)
}

fn build_while_expr(
  self : Parser,
  cond_expr : Expr,
  body : BlockExpr
) -> WhileExpr {
  let head_idx = cond_expr.meta().get_head_idx() - 1
  let last_idx = body.meta().get_last_idx()
  let meta = self.create_meta(head_idx, last_idx)
  WhileExpr::new(meta, cond_expr, body)
}

fn build_wildcard_pat(self : Parser) -> WildCard {
  let meta = self.create_meta(self.idx, self.idx)
  self.forward()
  WildCard::new(meta)
}

fn build_as_pat(
  self : Parser,
  atom_pat : AtomPat,
  as_ident : LowerCase?
) -> AsPat {
  let head_idx = atom_pat.meta().get_head_idx()
  let last_idx = match as_ident {
    Some(as_ident) => as_ident.meta().get_last_idx()
    None => atom_pat.meta().get_last_idx()
  }
  let meta = self.create_meta(head_idx, last_idx)
  AsPat::new(meta, atom_pat, as_ident)
}

fn build_multi_pat(self : Parser, atom_pats : Array[AtomPat]) -> MultiPat {
  let head_idx = atom_pats[0].meta().get_head_idx()
  let last_idx = atom_pats.last().unwrap().meta().get_last_idx()
  let meta = self.create_meta(head_idx, last_idx)
  MultiPat::new(meta, atom_pats)
}

fn build_match_arm(
  self : Parser,
  pat : Pattern,
  match_guard : Expr?,
  expr : Expr
) -> MatchArm {
  let head_idx = pat.meta().get_head_idx()
  let last_idx = expr.meta().get_last_idx()
  let meta = self.create_meta(head_idx, last_idx)
  MatchArm::new(meta, pat, match_guard, expr)
}

fn build_match_expr(
  self : Parser,
  match_expr : Expr,
  arms : Array[MatchArm]
) -> MatchExpr {
  let (head_idx, last_idx) = match arms.length() {
    0 => (self.idx - 2, self.idx - 1)
    _ =>
      (
        match_expr.meta().get_head_idx() - 1,
        arms.last().unwrap().meta().get_last_idx(),
      )
  }
  let meta = self.create_meta(head_idx, last_idx)
  MatchExpr::new(meta, match_expr, arms)
}

fn build_func_type(
  self : Parser,
  arg_type : TypeExpr,
  ret_type : TypeExpr
) -> FuncType {
  let head_idx = arg_type.meta().get_head_idx()
  let last_idx = ret_type.meta().get_last_idx()
  let meta = self.create_meta(head_idx, last_idx)
  FuncType::new(meta, arg_type, ret_type)
}

fn build_apply_type(
  self : Parser,
  type_name : UpperCase,
  type_args : Array[TypeExpr]
) -> ApplyType {
  let head_idx = type_name.meta().get_head_idx()
  let last_idx = type_args.last().unwrap().meta().get_last_idx() + 1
  let meta = self.create_meta(head_idx, last_idx)
  ApplyType::new(meta, type_name, type_args)
}

fn build_let_expr(
  self : Parser,
  pat : AsPat,
  expr : Expr,
  type_annote : TypeExpr?
) -> LetExpr {
  let head_idx = pat.meta().get_head_idx() - 1
  let last_idx = expr.meta().get_last_idx()
  let meta = self.create_meta(head_idx, last_idx)
  LetExpr::new(meta, pat, type_annote, expr)
}

fn build_assign_expr(
  self : Parser,
  left : ApplyExpr,
  assign_op : Operator,
  right : Expr
) -> AssignExpr {
  let head_idx = left.meta().get_head_idx()
  let last_idx = right.meta().get_last_idx()
  let meta = self.create_meta(head_idx, last_idx)
  AssignExpr::new(meta, left, assign_op, right)
}

fn build_break_expr(self : Parser) -> BreakExpr!Error {
  let head_idx = self.index()
  self.skip_keyword!("break")
  let last_idx = self.skip_and_index_terminator!()
  let meta = self.create_meta(head_idx, last_idx)
  BreakExpr::new(meta)
}

fn build_continue_expr(self : Parser) -> ContinueExpr!Error {
  let head_idx = self.index()
  self.skip_keyword!("continue")
  let last_idx = self.skip_and_index_terminator!()
  let meta = self.create_meta(head_idx, last_idx)
  ContinueExpr::new(meta)
}

fn build_return_expr(self : Parser) -> ReturnExpr!Error {
  let head_idx = self.index()
  self.skip_keyword!("return")
  if self.current_is_terminator() {
    let last_idx = self.skip_and_index_terminator!()
    let meta = self.create_meta(head_idx, last_idx)
    return ReturnExpr::new(meta, None)
  }
  let expr = self.parse_expr!()
  let last_idx = self.skip_and_index_terminator!()
  let meta = self.create_meta(head_idx, last_idx)
  ReturnExpr::new(meta, Some(expr))
}

fn build_null_state_expr(self : Parser) -> StateExpr {
  let meta = self.create_meta(self.idx, self.idx)
  let null_expr = NullExpr::new(meta)
  StateExpr::Null(null_expr)
}

fn build_block_expr(self : Parser, state_exprs : Array[StateExpr]) -> BlockExpr {
  let (head_idx, last_idx) = match state_exprs.length() {
    0 => (self.idx - 2, self.idx - 1)
    _ =>
      (
        state_exprs[0].meta().get_head_idx(),
        state_exprs.last().unwrap().meta().get_last_idx(),
      )
  }
  let meta = self.create_meta(head_idx, last_idx)
  // eliminate null state expression
  let eliminated_null : Array[StateExpr] = []
  for i = 0; i < state_exprs.length() - 1; i = i + 1 {
    let state = state_exprs[i]
    match state {
      StateExpr::Null(_) => continue
      _ => eliminated_null.push(state)
    }
  }
  eliminated_null.push(state_exprs.last().unwrap())
  BlockExpr::new(meta, state_exprs)
}

fn build_closure_param(
  self : Parser,
  ident : LowerCase,
  type_annote : TypeExpr?
) -> ClosureParam {
  let head_idx = ident.meta().get_head_idx()
  let last_idx = match type_annote {
    Some(type_annote) => type_annote.meta().get_last_idx()
    None => ident.meta().get_last_idx()
  }
  let meta = self.create_meta(head_idx, last_idx)
  ClosureParam::new(meta, ident, type_annote)
}

fn build_closure(
  self : Parser,
  meta : Meta,
  name : LowerCase?,
  params : Array[ClosureParam],
  ret_type : TypeExpr?,
  body : BlockExpr
) -> Closure {
  Closure::new(meta, name, params, ret_type, body)
}

fn build_func_param(
  self : Parser,
  ident : LowerCase,
  type_annote : TypeExpr
) -> FuncParam {
  let meta = self.create_meta(
    ident.meta().get_head_idx(),
    type_annote.meta().get_last_idx(),
  )
  FuncParam::new(meta, ident, type_annote)
}

fn build_generic_type(
  self : Parser,
  ident : UpperCase,
  constraints : Array[UpperCase]
) -> GenericType {
  let head_idx = ident.meta().get_head_idx()
  let last_idx = if constraints.is_empty() {
    ident.meta().get_last_idx()
  } else {
    constraints.last().unwrap().meta().get_last_idx()
  }
  let meta = self.create_meta(head_idx, last_idx)
  GenericType::new(meta, ident, constraints)
}

fn build_func_def(
  self : Parser,
  name : LowerCase,
  generic : Array[GenericType],
  params : Array[FuncParam],
  ret_type : TypeExpr,
  body : BlockExpr
) -> FuncDef {
  let head_idx = match name.scope {
    Some(scope) => scope.meta().get_head_idx()
    None => name.meta().get_head_idx()
  }
  let last_idx = body.meta().get_last_idx()
  let meta = self.create_meta(head_idx, last_idx)
  FuncDef::new(meta, name, generic, params, ret_type, body)
}

fn build_main_func(self : Parser, body : BlockExpr) -> MainFunc {
  let head_idx = body.meta().get_head_idx() - 2
  let last_idx = body.meta().get_last_idx()
  let meta = self.create_meta(head_idx, last_idx)
  MainFunc::new(meta, body)
}

fn build_struct_def(
  self : Parser,
  name : UpperCase,
  generic : Array[GenericType],
  fields : Array[StructField]
) -> StructDef {
  let head_idx = name.meta().get_head_idx()
  let last_idx = match fields.length() {
    0 => name.meta().get_last_idx()
    _ => fields.last().unwrap().meta().get_last_idx()
  }
  let meta = self.create_meta(head_idx, last_idx)
  StructDef::new(meta, name, generic, fields)
}

fn build_struct_field(
  self : Parser,
  name : LowerCase,
  type_annote : TypeExpr
) -> StructField {
  let head_idx = name.meta().get_head_idx()
  let last_idx = type_annote.meta().get_last_idx()
  let meta = self.create_meta(head_idx, last_idx)
  StructField::new(meta, name, type_annote)
}

/// -----------------------------------------------------
/// ### Parse - Method Expression
///
/// ### BNF:
///
/// method_expr ::= uppercase "::" lowercase ;
///
/// #### Example:
///
/// - `Array::new`
/// - `Map::default`
///------------------------------------------------------
fn parse_method_expr(self : Parser) -> MethodExpr!Error {
  let head_idx = self.index()
  let scope = self.build_uppercase!()
  self.skip_symbol!("::")
  let ident = self.build_lowercase!(scope=Some(scope))
  let meta = self.create_meta(head_idx, ident.meta().get_last_idx())
  MethodExpr::new(meta, scope, ident)
}

/// -----------------------------------------------------
/// ## Parse - Atom Expression
///
/// ### BNF:
///
/// atom_expr ::=
///    literal     |
///    ident       |
///    method_expr |
///    closure     |
///    tuple_expr  |
///    array_expr  ;
///
/// ### Note:
///
/// Atom expression is the most basic expression in the language.
/// It define the basic building blocks of the language.
///
/// ### Example:
/// 
/// - `42`
/// - `x`
/// - `Array::new`
/// - `fn inc(x) { x + 1 }`
/// - `(1, 2)`
/// - `[1, 2, 3]`
///
/// ## TODO
///
/// - ConstructExpr, like `Color::Red(255, 0, 0)`
/// -----------------------------------------------------
fn parse_atomic_expr(self : Parser) -> AtomExpr!Error {
  if self.current_is_literal() {
    let literal = self.build_literal!()
    return AtomExpr::Literal(literal)
  }
  if self.current_is_upper_case() {
    if self.look_ahead_is("::") && self.look_ahead_n_is_lowercase(2) {
      return AtomExpr::Method(self.parse_method_expr!())
    }
    // TODO: ConstructExpr
    self.write_extra_err_msg("Unimplemented, Source Loc: parse_atomic_expr")
    raise ParseError(self)
  }
  match self.get!().token_type {
    TokenType::LowerCase(_) => AtomExpr::Ident(self.build_lowercase!())
    TokenType::KeyWord("fn") => AtomExpr::Closure(self.parse_closure!())
    TokenType::Bracket('[') => AtomExpr::ArrayExpr(self.parse_array_expr!())
    TokenType::Operator("-") => AtomExpr::UnaryExpr(self.parse_unary_expr!())
    TokenType::Bracket('(') => {
      let tuple_expr = self.parse_tuple_expr!()
      match tuple_expr.exprs.length() {
        0 => AtomExpr::Unit(UnitExpr::new(tuple_expr.meta()))
        1 => AtomExpr::Paren(tuple_expr.exprs[0])
        _ => AtomExpr::TupleExpr(tuple_expr)
      }
    }
    _ => raise ParseError(self)
  }
}

/// -----------------------------------------------------
/// ## Parse - Apply Expression
///
/// ### BNF:
///
/// apply_expr ::= atom_expr | access_expr | call_expr ;
///
/// ### Example:
///
/// - `42`
/// - `point.x`
/// - `arr.get(0)`
/// -----------------------------------------------------
fn parse_apply_expr(self : Parser) -> ApplyExpr!Error {
  let mut apply_expr = ApplyExpr::Atomic(self.parse_atomic_expr!())
  for {
    if self.forward_with_bracket('[') {
      let accessor = Accessor::ArrayAcc(self.parse_expr!())
      let access = self.build_access(apply_expr, accessor)
      apply_expr = ApplyExpr::Access(access)
      self.skip_bracket!(']')
      continue
    }
    if self.forward_with_symbol(".") {
      if self.current_is_lower_case() {
        let accessor = Accessor::FieldAcc(self.build_lowercase!())
        let access = self.build_access(apply_expr, accessor)
        apply_expr = ApplyExpr::Access(access)
        continue
      }
      if self.current_is_integer() {
        let accessor = Accessor::TupleAcc(self.build_literal!())
        let access = self.build_access(apply_expr, accessor)
        apply_expr = ApplyExpr::Access(access)
        continue
      }
      raise ParseError(self)
    }
    if self.current_is_bracket('(') {
      let tuple_expr = self.parse_tuple_expr!()
      let call_expr = self.build_call(apply_expr, tuple_expr)
      apply_expr = ApplyExpr::CallExpr(call_expr)
    }
    break
  }
  apply_expr
}

/// -----------------------------------------------------
/// ## Parse - Expression
///
/// ### BNF:
///
/// expr ::=
///    if_expr |
///    match_expr |
///    block_expr |
///    infix_expr ;
///
/// ### Example:
///
/// - `if true { 1 } else { 2 }`
/// - `match 1 { 1 => 2, _ => 3 }`
/// - `{ 1 + 2 }`
/// - `1 + 2`
/// -----------------------------------------------------
fn parse_expr(self : Parser) -> Expr!Error {
  let expr = match self.get!().token_type {
    TokenType::KeyWord("if") => Expr::IfExpr(self.parse_if_expr!())
    TokenType::KeyWord("match") => Expr::Match(self.parse_match_expr!())
    TokenType::Bracket('{') => Expr::Block(self.parse_block_expr!())
    _ => Expr::Apply(self.parse_apply_expr!())
  }
  if self.current_is_infix_op() {
    self.parse_binary_expr!(expr)
  } else {
    expr
  }
}

/// -----------------------------------------------------
/// ## Parse - Tuple Expression
///
/// ### BNF:
///
/// - `tuple_expr ::= "(" expr ("," expr)* ")" ;`
///
/// ### Example:
///
/// - `(1, 2)`
/// - `(1, max(1, 2), "abc")`
/// -----------------------------------------------------
fn parse_tuple_expr(self : Parser) -> TupleExpr!Error {
  self.skip_bracket!('(')
  let exprs : Array[Expr] = []
  while not(self.current_is_bracket(')')) {
    exprs.push(self.parse_expr!())
    if self.forward_with_comma() {
      continue
    }
    break
  }
  self.skip_bracket!(')')
  self.build_tuple_expr(exprs)
}

/// -----------------------------------------------------
/// ## Parse - Array Expression
///
/// ### BNF:
///
/// array_expr ::= "[" expr ("," expr)* "]" ;
///
/// ### Note
///
/// Every element in the array must have the same type.
/// While it is only checked by the type checker.
///
/// ### Example:
///
/// - `[1, 2, 3]`
/// - `[1.0, 2.0, 3.0]`
/// - `['a', 'b', 'c']`
/// -----------------------------------------------------
fn parse_array_expr(self : Parser) -> ArrayExpr!Error {
  self.skip_bracket!('[')
  let exprs : Array[Expr] = []
  while not(self.current_is_bracket(']')) {
    exprs.push(self.parse_expr!())
    if self.forward_with_comma() {
      continue
    }
    break
  }
  self.skip_bracket!(']')
  self.build_array_expr(exprs)
}

/// ------------------------------------------------------
/// ## Parse - Type Expression
///
/// ### BNF:
///
/// - `type_expr ::= UpperCase | tuple_type | FuncType`
///
/// ### Note
///
/// Type expression is a type that can be used in type annotation.
///
/// ### Example:
///
/// - `Int`
/// - `(Int, Int)`
/// - `Int -> Int`
///
/// ## TODO
///
/// - Construct Type, like `Array[Int]`
/// - Function Type, like `(Int) -> Int`, `(Array[Int]) -> Int`
/// --------------------------------------------------
// TODO: more consice implementation
fn parse_type_expr(self : Parser) -> TypeExpr!Error {
  let type_expr = if self.current_is_bracket('(') {
    let tuple_type = TypeExpr::TupleType(self.parse_tuple_type!())
    if self.forward_with_symbol("->") {
      let ret_type = self.parse_type_expr!()
      let func_type = self.build_func_type(tuple_type, ret_type)
      TypeExpr::FuncType(func_type)
    } else {
      tuple_type
    }
  } else {
    let type_ident = self.skip_and_get_uppercase!()
    if self.forward_with_bracket('[') {
      let type_arg = self.parse_type_expr!()
      let type_args : Array[TypeExpr] = [type_arg]
      while self.forward_with_comma() {
        let type_arg = self.parse_type_expr!()
        type_args.push(type_arg)
      }
      let apply_type = self.build_apply_type(type_ident, type_args)
      self.skip_bracket!(']')
      TypeExpr::ApplyType(apply_type)
    } else {
      TypeExpr::IdentType(type_ident)
    }
  }
  type_expr
}

/// -----------------------------------------------------
/// ## Parse - Tuple Type
///
/// ### BNF:
///
/// - `tuple_type ::= "(" type_expr ("," type_expr)* ")"`
/// -----------------------------------------------------
fn parse_tuple_type(self : Parser) -> TupleType!Error {
  let head_idx = self.skip_and_index_bracket!('(')
  let types : Array[TypeExpr] = []
  while not(self.current_is_bracket(')')) {
    let type_expr = self.parse_type_expr!()
    types.push(type_expr)
    if self.forward_with_comma() {
      continue
    }
    break
  }
  let last_idx = self.skip_and_index_bracket!(')')
  let meta = self.create_meta(head_idx, last_idx)
  TupleType::new(meta, types)
}

/// -----------------------------------------------------
/// ## Parse - Pattern
///
/// ### BNF:
///
/// - `pattern ::= as_pat | multi_pat ;`
/// ----------------------------------------------------
fn parse_pattern(self : Parser) -> Pattern!Error {
  let atom_pat = self.parse_atom_pat!()
  if self.current_is("|") {
    return Pattern::MultiPat(self.parse_multi_pat!(atom_pat))
  }
  if self.forward_with_keyword("as") {
    let ident = self.skip_and_get_lowercase!()
    return Pattern::AsPat(self.build_as_pat(atom_pat, Some(ident)))
  }
  Pattern::AsPat(self.build_as_pat(atom_pat, None))
}

/// -----------------------------------------------------
/// ## Parse - Multi Pattern
///
/// ### BNF:
///
/// - `multi_pat ::= atom_pat ("|" atom_pat)* ;`
/// ----------------------------------------------------
fn parse_multi_pat(self : Parser, head_atom_pat : AtomPat) -> MultiPat!Error {
  let atom_pats : Array[AtomPat] = [head_atom_pat]
  while self.forward_with_symbol("|") {
    let atom_pat = self.parse_atom_pat!()
    atom_pats.push(atom_pat)
  }
  self.build_multi_pat(atom_pats)
}

/// -----------------------------------------------------
/// ## Parse - As Pattern
///
/// ### BNF:
///
/// as_pat ::= atom_pat ("as" lower_case)? ;
///
/// ### Note
///
/// As pattern is a pattern that can bind a value to a variable.
///
/// ### Example:
///
/// - `let (a, b) as c = (1, 2)` In this case, a = 1, b = 2, c = (1, 2)
/// - `let x as y = 42`, In this case, x = 42, y = 42, but it's not recommended
/// ----------------------------------------------------- 
fn parse_as_pat(self : Parser) -> AsPat!Error {
  let atom_pat = self.parse_atom_pat!()
  let as_ident = match self.forward_with_keyword("as") {
    true => Some(self.skip_and_get_lowercase!())
    false => None
  }
  self.build_as_pat(atom_pat, as_ident)
}

/// -----------------------------------------------------
/// ## Parse - Atom Pattern
///
/// ### BNF:
///
/// atom_pat ::= tuple_pat | array_pat | literal | lower_case | wildcard ;
///
/// ### Example:
///
/// - `let (a, b) = (1, 2)`
/// - `match x { 1 => 1, _ => 2 }`
///
/// ## TODO
///
/// - Construct Pattern, like `Color::Red(v)`
/// - Array Pattern, like `[a, b, c]`
///
/// ## Outlook
///
/// - Support complex array pattern, like: `let [x, ..xs] = [1, 2, 3]`
/// in this case, x = 1, xs = [2, 3]
/// -----------------------------------------------------
fn parse_atom_pat(self : Parser) -> AtomPat!Error {
  if self.current_is_literal() {
    let literal = self.build_literal!()
    return AtomPat::Literal(literal)
  }
  match self.get!().token_type {
    TokenType::Bracket('(') => return AtomPat::Tuple(self.parse_tuple_pat!())
    TokenType::LowerCase(_) => return AtomPat::Ident(self.build_lowercase!())
    TokenType::WildCard(_) =>
      return AtomPat::WildCard(self.build_wildcard_pat())
    TokenType::Bracket('[') => {
      self.write_extra_err_msg("Unimplemented, Source Loc: parse_atom_pat")
      raise ParseError(self)
    }
    TokenType::UpperCase(_) => {
      self.write_extra_err_msg("Unimplemented, Source Loc: parse_atom_pat")
      raise ParseError(self)
    }
    _ => {
      self.write_extra_err_msg("Unimplemented, Source Loc: parse_atom_pat")
      raise ParseError(self)
    }
  }
  raise ParseError(self)
}

/// -----------------------------------------------------
/// ## Parse - Tuple Pattern
///
/// ### BNF:
///
/// - `tuple_pat ::= "(" as_pat ("," as_pat)* ")" ;`
/// -----------------------------------------------------
fn parse_tuple_pat(self : Parser) -> TuplePat!Error {
  let head_idx = self.skip_and_index_bracket!('(')
  let as_pats : Array[AsPat] = []
  let mut as_pat = self.parse_as_pat!()
  as_pats.push(as_pat)
  while not(self.current_is_bracket(')')) {
    self.skip_comma!()
    as_pat = self.parse_as_pat!()
    as_pats.push(as_pat)
  }
  let last_idx = self.skip_and_index_bracket!(')')
  let meta = self.create_meta(head_idx, last_idx)
  TuplePat::new(meta, as_pats)
}

/// -----------------------------------------------------
/// ## Parse - Let Expression
///
/// ### BNF:
///
/// - `let_expr ::= "let" as_pat [":" type_annote] "=" expr ";" ;`
///
/// ### Note
///
/// Usually it should have `mut` keyword.
/// But currently, we do not implement it.
/// Besides, let expression support shadowing mechanism.
///
/// ### Example:
///
/// - `let x = 42`
/// - `let (x, y) as z = (1, 2)` In this case, `x = 1, y = 2, z = (1, 2)`
/// -----------------------------------------------------
fn parse_let_expr(self : Parser) -> LetExpr!Error {
  self.skip_keyword!("let")
  let pat = self.parse_as_pat!()
  let type_expr = if self.forward_with_symbol(":") {
    Some(self.parse_type_expr!())
  } else {
    None
  }
  self.skip_operator!("=")
  let expr = self.parse_expr!()
  self.build_let_expr(pat, expr, type_expr)
}

/// -----------------------------------------------------
/// ## Parse - Infix Expression
///
/// ### BNF:
///
/// `binary_expr ::= expr op expr ;`
///
/// ### Note
///
/// Different with other function, the ast generated possibly contains
/// multiple nodes, because the infix expression may contains multiple
/// expressions.
///
/// It needs one apply_expr as head, because during parsing,
/// AssignExpr and InfixExpr's the leftmost must be an apply_expr.
/// Parser must parse an apply expr first, then it can know this statement
/// is an assign expr or infix expr.
/// 
/// ### Example:
///
/// - `x + 42` It's a binary expr
/// - `x` It's an apply expr
/// - `x + y * z`, generate a binary ast with two layers.
/// -----------------------------------------------------
fn parse_binary_expr(self : Parser, leftmost : Expr) -> Expr!Error {
  let oper_stack : Array[Operator] = [self.build_infix_op!()]
  let expr_stack : Array[Expr] = [
    leftmost,
    Expr::Apply(self.parse_apply_expr!()),
  ]
  while self.current_is_infix_op() {
    let op_right = self.build_infix_op!()
    let op_left = oper_stack.last().unwrap()
    if op_right.prec >= op_left.prec {
      oper_stack.push(op_right)
      expr_stack.push(Expr::Apply(self.parse_apply_expr!()))
      continue
    } else {
      while not(oper_stack.is_empty()) {
        let right_expr = expr_stack.pop().unwrap()
        let left_expr = expr_stack.pop().unwrap()
        let op = oper_stack.pop().unwrap()
        let binary_expr = self.build_binary_expr(op, left_expr, right_expr)
        let binary_expr = Expr::Binary(binary_expr)
        expr_stack.push(binary_expr)
      }
      oper_stack.push(op_right)
      expr_stack.push(Expr::Apply(self.parse_apply_expr!()))
    }
  }
  while not(oper_stack.is_empty()) {
    let right_expr = expr_stack.pop().unwrap()
    let left_expr = expr_stack.pop().unwrap()
    let op = oper_stack.pop().unwrap()
    let binary_expr = self.build_binary_expr(op, left_expr, right_expr)
    let binary_expr = Expr::Binary(binary_expr)
    expr_stack.push(binary_expr)
  }
  expr_stack.pop().unwrap()
}

fn parse_unary_expr(self : Parser) -> UnaryExpr!Error {
  let op = self.build_unary_op!()
  let expr = self.parse_expr!()
  self.build_unary_expr(op, expr)
}

/// -----------------------------------------------------
/// ## Parse - Assign Expression
///
/// ### BNF:
///
/// - `assign_expr ::= apply_expr assign_op expr ;`
///
/// ### Note
///
/// It needs an apply expr as input argument, because during parsing,
/// AssignExpr and InfixExpr's the leftmost must be an apply_expr.
/// Parser must parse an apply expr first, then it can know this statement
/// is an assign expr or infix expr.
/// 
/// It's not an parse error, it's a semantic error.
///
/// Notice that the left value if an apply expr, which means the following
/// expression:
///
/// ```
/// 1 = 42;
/// ```
/// 
/// ### Example:
///
/// - `x = 42`
/// - `(x, y) = (1, 2)`
/// - `(a, b) = (b, a)`
/// - `a[b+42] = 42`
/// -----------------------------------------------------
fn parse_assign_expr(self : Parser, left : ApplyExpr) -> AssignExpr!Error {
  let aop = self.skip_and_get_assign_op!()
  let right = self.parse_expr!()
  self.build_assign_expr(left, aop, right)
}

/// -----------------------------------------------------
/// ## Parse - State Expression
///
/// ### BNF:
///
/// state_expr ::=
///    let_expr      |
///    assign_expr   |
///    while_expr    |
///    break_expr    |
///    continue_expr |
///    expr          ;
///
/// ### Note
///
/// Each state expression have type, if it's expr,
/// its type is same as expr, if not, its type is `Unit`
/// -----------------------------------------------------
fn parse_state_expr(self : Parser) -> StateExpr!Error {
  match self.get_current_tok_type!() {
    TokenType::KeyWord("let") => StateExpr::Let(self.parse_let_expr!())
    TokenType::KeyWord("while") => StateExpr::While(self.parse_while_expr!())
    TokenType::KeyWord("break") => StateExpr::Break(self.build_break_expr!())
    TokenType::KeyWord("continue") =>
      StateExpr::Continue(self.build_continue_expr!())
    TokenType::KeyWord("return") => StateExpr::Return(self.build_return_expr!())
    TokenType::Terminator(_) => {
      self.forward()
      self.build_null_state_expr()
    }
    TokenType::Bracket('}') => return self.build_null_state_expr()
    TokenType::KeyWord("if")
    | TokenType::KeyWord("fn") | TokenType::KeyWord("match") =>
      StateExpr::Expr(self.parse_expr!())
    _ => {
      let apply_expr = self.parse_apply_expr!()
      if self.current_is_assign_op() {
        StateExpr::Assign(self.parse_assign_expr!(apply_expr))
      } else if self.current_is_infix_op() {
        let leftmost_expr = Expr::Apply(apply_expr)
        StateExpr::Expr(self.parse_binary_expr!(leftmost_expr))
      } else {
        StateExpr::Expr(Expr::Apply(apply_expr))
      }
    }
  }
}

/// -----------------------------------------------------
/// ## Parse - If Expression
///
/// ### BNF:
///
/// `if_expr ::= 'if' expr block_expr ('else' if_expr | block_expr) ;`
///
/// ### Note
///
/// Unlike C-like language, `if` is expression, not a statement
/// which means `if` could return a value
///
/// ### Example:
///
/// - `let x = if a > b { a } else { b }`
/// -----------------------------------------------------
fn parse_if_expr(self : Parser) -> IfExpr!Error {
  self.skip_keyword!("if")
  let cond_expr = self.parse_expr!()
  let then = self.parse_block_expr!()
  let else_clause = if self.forward_with_keyword("else") {
    Some(self.parse_else_clause!())
  } else {
    None
  }
  self.build_if_expr(cond_expr, then, else_clause)
}

fn parse_else_clause(self : Parser) -> ElseClause!Error {
  match self.get!().token_type {
    TokenType::KeyWord("if") => ElseClause::IfExpr(self.parse_if_expr!())
    TokenType::Bracket('{') => ElseClause::Block(self.parse_block_expr!())
    _ => raise ParseError(self)
  }
}

/// -----------------------------------------------------
/// ## Parse - Match Expr
///
/// ### BNF:
///
/// - `match_expr ::= "match" expr "{" match_arm* "}" ;`
/// - `match_arm ::= pattern guard? "=>" Expr ;`
///
/// ## TODO
///
/// - need test
/// -----------------------------------------------------
fn parse_match_expr(self : Parser) -> MatchExpr!Error {
  self.skip_keyword!("match")
  let expr = self.parse_expr!()
  let arms : Array[MatchArm] = []
  self.skip_bracket!('{')
  while not(self.current_is_bracket('}')) {
    arms.push(self.parse_match_arm!())
    if self.current_is_bracket('}') {
      break
    }
    self.skip_comma!()
  }
  self.skip_bracket!('}')
  self.build_match_expr(expr, arms)
}

fn parse_match_arm(self : Parser) -> MatchArm!Error {
  let pat = self.parse_pattern!()
  let match_guard = self.parse_match_guard!()
  self.skip_symbol!("=>")
  let expr = self.parse_expr!()
  self.build_match_arm(pat, match_guard, expr)
}

fn parse_match_guard(self : Parser) -> Expr?!Error {
  if self.forward_with_keyword("if") {
    Some(self.parse_expr!())
  } else {
    None
  }
}

/// -----------------------------------------------------
/// ## Parse - Block Expression
///
/// ### BNF:
///
/// - `block_expr ::= '{' state_expr* '}' ;`
///
/// ### Note
///
/// Block is an expression, it could have a vlue.
/// Besides, it is unnecessary that each statement have a terminator.
/// The last statement could be a expression without terminator,
/// and the value and type of block is the value and type of last
///
/// ### Example:
///
/// - `let t = { let x = 1; x + 1 }`
/// -----------------------------------------------------
fn parse_block_expr(self : Parser) -> BlockExpr!Error {
  self.skip_bracket!('{')
  let state_exprs : Array[StateExpr] = []
  while not(self.current_is_bracket('}')) {
    state_exprs.push(self.parse_state_expr!())
  }
  self.skip_bracket!('}')
  self.build_block_expr(state_exprs)
}

/// -----------------------------------------------------
/// ## Parse - While Expression
///
/// ### BNF:
///
/// - `while_expr ::= 'while' expr block_expr ;`
/// -----------------------------------------------------
fn parse_while_expr(self : Parser) -> WhileExpr!Error {
  self.skip_keyword!("while")
  let cond_expr = self.parse_expr!()
  let body = self.parse_block_expr!()
  self.build_while_expr(cond_expr, body)
}

/// -----------------------------------------------------
/// ## Parse - Closure Parameter
///
/// ### BNF:
///
/// - `closure_param ::= LowerCase (':' type_expr)? ;`
/// -----------------------------------------------------
fn parse_closure_param(self : Parser) -> ClosureParam!Error {
  let ident = self.skip_and_get_lowercase!()
  let type_annote = if self.forward_with_symbol(":") {
    Some(self.parse_type_expr!())
  } else {
    None
  }
  self.build_closure_param(ident, type_annote)
}

/// -----------------------------------------------------
/// ## Parse - Closure
///
/// ### Note
///
/// Closure is a special function, it is defined inside a
/// function. Different with normal function, we allow closure
/// definition lack type annotation for parameters and return type
/// Bisides, closure could be anonymous function
///
/// ### BNF:
///
/// - `closure ::= 'fn' LowerCase? closure_params ret_type block_expr ;`
/// - `closure_params ::= '(' closure_param* ')' ;`
/// - `closure_param ::= LowerCase (':' type_expr)? ;`
/// - `ret_type ::= '->' type_expr ;`
///
/// ### Example:
///
/// - `fn max(a, b) { if a > b { a } else { b } }`
/// - `fn fact(n) { if n == 0 { 1 } else { n * fact(n - 1) } }`
/// - `fn (t) { t + 1 }`
/// -----------------------------------------------------
fn parse_closure(self : Parser) -> Closure!Error {
  let head_idx = self.skip_and_index_keyword!("fn")
  let name = if self.current_is_lower_case() {
    Some(self.build_lowercase!())
  } else {
    None
  }
  self.skip_bracket!('(')
  let params : Array[ClosureParam] = []
  if not(self.current_is_bracket(')')) {
    params.push(self.parse_closure_param!())
    while self.forward_with_comma() {
      params.push(self.parse_closure_param!())
    }
  }
  self.skip_bracket!(')')
  let ret_type = if self.forward_with_symbol("->") {
    Some(self.parse_type_expr!())
  } else {
    None
  }
  let body = self.parse_block_expr!()
  let last_idx = body.meta().get_last_idx()
  let meta = self.create_meta(head_idx, last_idx)
  self.build_closure(meta, name, params, ret_type, body)
}

/// -----------------------------------------------------
/// ### Parse - Function Generic Type
///
/// ### BNF:
///
/// - `generic_type ::= UpperCase (':' constraints)? ;`
/// - `constraints ::= UpperCase ('+' UpperCase)* ;`
///
/// ### Example:
///
/// - `T`
/// - `T: Ord`
/// - `T: Ord + Show`
/// -----------------------------------------------------
fn parse_generic_type(self : Parser) -> GenericType!Error {
  let type_ident = self.skip_and_get_uppercase!()
  let constraints : Array[UpperCase] = []
  if self.forward_with_symbol(":") {
    constraints.push(self.skip_and_get_uppercase!())
    while self.forward_with_operator("+") {
      constraints.push(self.skip_and_get_uppercase!())
    }
  }
  self.build_generic_type(type_ident, constraints)
}

/// -----------------------------------------------------
/// ## Parse - Function Parameter
///
/// ### BNF:
///
/// - `func_param ::= LowerCase ':' type_expr ;`
///
/// ### Example:
///
/// - `x: Int`
/// - `y: Double`
/// - `p: Point[Double, Double]`
/// -----------------------------------------------------
fn parse_func_param(self : Parser) -> FuncParam!Error {
  let param_ident = self.skip_and_get_lowercase!()
  self.skip_symbol!(":")
  let type_expr = self.parse_type_expr!()
  self.build_func_param(param_ident, type_expr)
}

/// -----------------------------------------------------
/// ## Parse - Main Function
///
/// ### Note
///
/// Main function is a special function, there is only one
/// main function in a program
///
/// ### BNF
///
/// - `main_func ::= 'fn' 'main' func_body ;`
/// -----------------------------------------------------
fn parse_main_func(self : Parser) -> MainFunc!Error {
  self.skip_keyword!("fn")
  self.skip!("main")
  let func_body = self.parse_block_expr!()
  self.build_main_func(func_body)
}

/// -----------------------------------------------------
/// ## Parse - Function Definition
///
/// ### BNF:
///
/// - `func_def ::= 'fn' func_name generic_types? parameter ret_type func_body ;`
/// - `func_name ::= (UpperCase '::')? LowerCase ;`
/// - `generic_types ::= '[' generic_types (',' generic_types)* ']' ;`
/// - `generic_type ::= UpperCase (':' constraint)? ;`
/// - `constraint ::= UpperCase ('+' UpperCase)* ;`
/// -----------------------------------------------------
fn parse_func_def(self : Parser) -> FuncDef!Error {
  self.skip_keyword!("fn")
  if self.forward_with("main") {
    self.write_extra_err_msg("Could not define main function here")
    raise ParseError(self)
  }
  let scope = if self.current_is_upper_case() {
    let r = Some(self.build_uppercase!())
    self.skip_symbol!("::")
    r
  } else {
    None
  }
  let func_ident = self.skip_and_get_lowercase!(~scope)
  let generic_types = []
  if self.forward_with_bracket('[') {
    generic_types.push(self.parse_generic_type!())
    while self.forward_with_comma() {
      generic_types.push(self.parse_generic_type!())
    }
    self.skip_bracket!(']')
  }
  self.skip_bracket!('(')
  let params : Array[FuncParam] = []
  if not(self.current_is_bracket(')')) {
    params.push(self.parse_func_param!())
    while self.forward_with_comma() {
      params.push(self.parse_func_param!())
    }
  }
  self.skip_bracket!(')')
  self.skip_symbol!("->")
  let ret_type = self.parse_type_expr!()
  let func_body = self.parse_block_expr!()
  self.build_func_def(func_ident, generic_types, params, ret_type, func_body)
}

/// -----------------------------------------------------
/// ## Parse - Struct Definition
///
/// ### BNF:
///
/// - `struct_def ::= 'struct' UpperCase generic_types '{' struct_field* '}' ;`
/// - `struct_field ::= LowerCase ':' type_expr ;`
/// - `generic_types ::= '[' generic_type (',' generic_type)* ']' ;`
/// - `generic_type ::= UpperCase (':' constraint)? ;`
/// - `constraint ::= UpperCase ('+' UpperCase)* ;`
/// -----------------------------------------------------

fn parse_struct_def(self : Parser) -> StructDef!Error {
  self.skip_keyword!("struct")
  let name = self.skip_and_get_uppercase!()
  let generic_types = []
  if self.forward_with_bracket('[') {
    generic_types.push(self.parse_generic_type!())
    while self.forward_with_comma() {
      generic_types.push(self.parse_generic_type!())
    }
    self.skip_bracket!(']')
  }
  self.skip_bracket!('{')
  let fields : Array[StructField] = []
  while not(self.current_is_bracket('}')) {
    fields.push(self.parse_struct_field!())
    if self.current_is_bracket('}') {
      break
    }
    self.skip_comma!()
  }
  self.skip_bracket!('}')
  self.build_struct_def(name, generic_types, fields)
}

fn parse_struct_field(self : Parser) -> StructField!Error {
  let name = self.skip_and_get_lowercase!()
  self.skip_symbol!(":")
  let type_annote = self.parse_type_expr!()
  self.build_struct_field(name, type_annote)
}

/// -----------------------------------------------------
/// ## Parse - Top Declaration
///
/// ### BNF:
///
/// ```
/// top_decl ::=
///     func_def   | 
///     main_func  |
///     struct_def |
///     enum_def   |   (Unimplemented)
///     type_def   |   (Unimplemented)
///     impl_def   |   (Unimplemented)
///     trait_def  |   (Unimplemented)
///     let_expr       (Unimplemented)
/// ```
/// -----------------------------------------------------
fn parse_top_decl(self : Parser) -> TopDecl!Error {
  let top_decl = match self.get_current_tok_type!() {
    TokenType::KeyWord("fn") =>
      if self.look_ahead_is("main") {
        TopDecl::MainFunc(self.parse_main_func!())
      } else {
        TopDecl::FuncDef(self.parse_func_def!())
      }
    TokenType::KeyWord("struct") => TopDecl::StructDef(self.parse_struct_def!())
    TokenType::KeyWord("enum") => {
      self.write_extra_err_msg("Unimplemented, Source Loc: parse_top_decl")
      raise ParseError(self)
    }
    TokenType::KeyWord("type") => {
      self.write_extra_err_msg("Unimplemented, Source Loc: parse_top_decl")
      raise ParseError(self)
    }
    TokenType::KeyWord("impl") => {
      self.write_extra_err_msg("Unimplemented, Source Loc: parse_top_decl")
      raise ParseError(self)
    }
    TokenType::KeyWord("trait") => {
      self.write_extra_err_msg("Unimplemented, Source Loc: parse_top_decl")
      raise ParseError(self)
    }
    TokenType::KeyWord("let") => {
      let let_expr = self.parse_let_expr!()
      self.context.push_let_expr(let_expr)
      TopDecl::LetExpr(let_expr)
    }
    _ => {
      self.write_extra_err_msg("Unknown top declaration")
      raise ParseError(self)
    }
  }
  top_decl
}
