/// -----------------------------------------------------------------------
/// ## Lexer Tests
/// -----------------------------------------------------------------------
test "utils test" {
  if stod("1.23") != 1.23 {
    fail!("stod stod(\"1.23\") != 1.23")
  }
  let test_list = ["1.23", "2.34", "3.45", "0.1", "14", "0.0", "0.0001"]
  let test_real = [1.23, 2.34, 3.45, 0.1, 14, 0.0, 0.0001]
  let mut idx = 0
  if test_list.length() != test_real.length() {
    fail!("Fix test case! stod test_list.len() != test_real.len()")
  }
  while idx < test_list.length() {
    if stod(test_list[idx]) != test_real[idx] {
      fail!(
        "stod test failed, str = " +
        test_list[idx] +
        ", real = " +
        test_real[idx].to_string(),
      )
    }
    idx += 1
  }
}

fn fast_get_tok(code : String) -> Token!Error {
  let context = Context::new(code)
  let lexer = Lexer::new(context)
  lexer.advance!()
}

fn fast_lex(code : String) -> Array[Token]!Error {
  let context = Context::new(code)
  let lexer = Lexer::new(context)
  lexer.run!()
  context.tokens
}

// Test Lexer
test "run single Token Non failure" {
  let code_snippets = [
    "1234", "345", "789", "1234", "18902", "33667", "0", "1", "2", "3", "4", "5",
    "6", "7", "8", "9", "0x1234", "0x345", "0x789", "0x1234", "0x18902", "0x33667",
    "0X87ab", "0X1750", "0Xaabb", "0Xabcd", "0xAAFF", "0xbcdE", "0o1234", "0o345",
    "0o710", "0o1234", "0o10102", "0o33667", "1234L", "345L", "789L", "1234L", "18902L",
    "33667L", "0x123U", "0x345U", "0x789U", "0x1234U", "0x18902U", "0x33667U", "0X87abUL",
    "0X1750UL", "0XaabbUL", "0XabcdUL", "0xAAFFUL", "0xbcdEUL", "0.01", "12.34",
    "0.123", "1.113", "0.0001", "0.00001", "1e-1", "1e-2", "1e-3", "1e-4", "1e-5",
    "1e-6", "1.1e01", "1.2e02", "1.3e03", "1.4e04", "1.5e05", "1.6e06", "1.1e-1",
    "1.2e-2", "1.3e-3", "1.4e-4", "1.5e-5", "1.6e-6", "0.1e-1", "0.2e-2", "0.3e-3",
    "0.4e-4", "0.5e-5", "0.6e-6", "12.34e-1", "12.34e1", "12.34e2", "12.34e3", "12.34e4",
    "12.34e5", "true", "false", "fn", "let", "if", "else", "return", "while", "for",
    "break", "continue", "match", "loop", "in", "as", "enum", "+", "-", "*", "/",
    "%", ">", "<", ">=", "<=", "==", "!=",
  ]
  for code in code_snippets {
    match fast_get_tok?(code) {
      Ok(_) => ()
      Err(_) => fail!("Not Pass run Single Token Non-Failure Test")
    }
  }
}

test "run multi tokens, one line, non failure" {
  let code_snippets = [
    "let x = 1;", "x = 33;", "let (a, b) = (1, 2);", "fn plus1(a) { return a + 1; }",
    "fn foo[T, U](a: T, b: U) { return a; }", "let [a,..] = [1, 2, 3];", "if a > b { a } else {b};",
    "while a < b { a += 1} else {a * 2}", "match n {1 => 2; 2 => 4; _ => 0}", "struct Point { x: Int, y: Int }",
    "enum Color { Red, Green, Blue }", "Array::new()", "HashMap::new()", "let m : HashMap[Int, Int] = HashMap::new();",
  ]
  for code in code_snippets {
    let tokens = match fast_lex?(code) {
      Ok(t) => t
      Err(_) => {
        let mut errmsg =
          #| Not Pass run Multi Tokens One Line Non-Failure Test
        errmsg += "\nError happened when lexing:\n"
        errmsg += code
        fail!(errmsg)
      }
    }

  }
}

test "run multi line, non failure" {
  let code1 =
    #| fn gcd (a: Int, b: Int) -> Int {
    #|   if b == 0 {
    #|     return a;
    #|   }
    #|   return gcd(b, a % b);
    #| }
  let code2 =
    #| fn fact(n: Int) -> Int {
    #|   if n == 0 {
    #|     return 1;
    #|   }
    #|   return n * fact(n - 1);
    #| }
  let code3 =
    #| fn fib(n: Int) -> Int {
    #|   if n == 0 {
    #|    return 0;
    #|   }
    #|   if n == 1 || n == 2 {
    #|     return 1;
    #|   }
    #|   return fib(n - 1) + fib(n - 2);
    #| }
  let code4 =
    #| fn is_prime(n: Int) -> Bool {
    #|   if n < 2 {
    #|     return false;
    #|   }
    #|   let i = 2;
    #|   while i < n  {
    #|     if n % i == 0 {
    #|       return false;
    #|     }
    #|     i += 1;
    #|   }
    #|   return true;
    #| }
  let code5 =
    #| fn head(l : Array[Int]) -> Int {
    #|   let [a, ..] = l;
    #|   a
    #| }
  let code6 =
    #| fn tail(l : Array[Int]) -> Array[Int] {
    #|   let [_, b] = l;
    #|   b
    #| }
  let code7 =
    #| fn last(l : Array[Int]) -> Int {
    #|   let [.., a] = l;
    #|   a
    #| }
  let code8 =
    #| fn assert_eq[T: eq](
    #|   a: T, b: T, msg: String
    #| ) -> Result[Unit, String] {
    #|   if a != b {
    #|     Err(msg);
    #|   }
    #|   Ok(())
    #| }
  let codes = [code1, code2, code3, code4, code5, code6, code7, code8]
  for code in codes {
    match fast_lex?(code) {
      Ok(_) => ()
      Err(_) => {
        let mut errmsg =
          #| Not Pass run Multi Line Non-Failure Test
        errmsg += "\nError happened when lexing:\n"
        errmsg += code
        fail!(errmsg)
      }
    }
  }
}

test "lexer int correctness" {
  let test_set : Array[(String, Int)] = [
    ("123", 123),
    ("1234", 1234),
    ("0x123", 0x123),
    ("0x345", 0x345),
    ("0xaabb", 0xaabb),
    ("0XaaCC", 0XaaCC),
    ("0o123", 0o123),
    ("0o345", 0o345),
    ("0o710", 0o710),
    ("0o1234", 0o1234),
    ("0o10102", 0o10102),
    ("0o33667", 0o33667),
  ]
  for pair in test_set {
    let (code, ans) = pair
    let tok = match fast_get_tok?(code) {
      Ok(t) => t
      Err(_) => fail!("Parse Integer failed in Lexer")
    }
    let tok_type = tok.token_type
    match tok_type {
      TokenType::Integer(i) =>
        if i != ans {
          let mut errmsg =
            #| Parse Integer failed in Lexer,
          errmsg += code + " should be Integer, while get token : \{i}"
          fail!(errmsg)
        }
      _ => {
        let mut errmsg =
          #| Parse Integer failed in Lexer,
        errmsg += code + " should be Integer, while get token : \{tok_type}"
        fail!(errmsg)
      }
    }
  }
}

test "lexer unsigned int correctness" {
  let test_set : Array[(String, UInt)] = [
    ("123U", 123U),
    ("1234U", 1234U),
    ("0x123U", 0x123U),
    ("0x345U", 0x345U),
    ("0xaabbU", 0xaabbU),
    ("0XaaCCU", 0XaaCCU),
    ("0o123U", 0o123U),
    ("0o345U", 0o345U),
    ("0o710U", 0o710U),
    ("0o1234U", 0o1234U),
    ("0o10102U", 0o10102U),
    ("0o33667U", 0o33667U),
  ]
  for pair in test_set {
    let (code, ans) = pair
    let tok = match fast_get_tok?(code) {
      Ok(t) => t
      Err(_) => fail!("Parse Unsigned Integer failed in Lexer")
    }
    let tok_type = tok.token_type
    match tok_type {
      TokenType::Unsigned(i) =>
        if i != ans {
          let mut errmsg =
            #| Parse Unsigned Integer failed in Lexer,
          errmsg += code + " should be Unsigned Integer, while get token : \{i}"
          fail!(errmsg)
        }
      _ => {
        let mut errmsg =
          #| Parse Unsigned Integer failed in Lexer,
        errmsg += code +
          " should be Unsigned Integer, while get token : \{tok_type}"
        fail!(errmsg)
      }
    }
  }
}

test "lexer long correctness" {
  let test_set : Array[(String, Int64)] = [
    ("123L", 123L),
    ("1234L", 1234L),
    ("0x123L", 0x123L),
    ("0x345L", 0x345L),
    ("0xaabbL", 0xaabbL),
    ("0XaaCCL", 0XaaCCL),
    ("0o123L", 0o123L),
    ("0o345L", 0o345L),
    ("0o710L", 0o710L),
    ("0o1234L", 0o1234L),
    ("0o10102L", 0o10102L),
    ("0o33667L", 0o33667L),
  ]
  for pair in test_set {
    let (code, ans) = pair
    let token = match fast_get_tok?(code) {
      Ok(t) => t
      Err(_) => fail!("Parse Long failed in Lexer")
    }
    let tok_type = token.token_type
    match tok_type {
      TokenType::Long(i) =>
        if i != ans {
          let mut errmsg =
            #| Parse Long failed in Lexer,
          errmsg += code + " should be Long, while get token : \{i}"
          fail!(errmsg)
        }
      _ => fail!("Parse Long failed in Lexer, \{code} is not Long")
    }
  }
}

test "lexer unsigned long correctness" {
  let test_set : Array[(String, UInt64)] = [
    ("123UL", 123UL),
    ("1234UL", 1234UL),
    ("0x123UL", 0x123UL),
    ("0x345UL", 0x345UL),
    ("0xaabbUL", 0xaabbUL),
    ("0XaaCCUL", 0XaaCCUL),
    ("0o123UL", 0o123UL),
    ("0o345UL", 0o345UL),
    ("0o710UL", 0o710UL),
    ("0o1234UL", 0o1234UL),
    ("0o10102UL", 0o10102UL),
    ("0o33667UL", 0o33667UL),
  ]
  for pair in test_set {
    let (code, ans) = pair
    let token = match fast_get_tok?(code) {
      Ok(t) => t
      Err(_) => fail!("Parse Unsigned Long failed in Lexer")
    }
    let tok_type = token.token_type
    match tok_type {
      TokenType::UnsignedLong(i) =>
        if i != ans {
          let mut errmsg =
            #| Parse Unsigned Long failed in Lexer,
          errmsg += code + " should be Unsigned Long, while get token : \{i}"
          fail!(errmsg)
        }
      _ => {
        let mut errmsg =
          #| Parse Unsigned Long failed in Lexer,
        errmsg += code +
          " should be Unsigned Long, while get token : \{tok_type}"
        fail!(errmsg)
      }
    }
  }
}

test "lexer double correctness" {
  let test_set : Array[(String, Double)] = [
    ("0.01", 0.01),
    ("12.35", 12.35),
    ("0.125", 0.125),
    ("1.115", 1.115),
    ("0.0001", 0.0001),
    ("0.00001", 0.00001),
    ("1.0e-1", 1.0e-1),
    ("1.0e-2", 1.0e-2),
    ("1.0e-3", 1.0e-3),
    ("1.0e-4", 1.0e-4),
    ("1.0e-5", 1.0e-5),
    ("1.0e-6", 1.0e-6),
    ("1.1e01", 1.1e01),
    ("1.2e02", 1.2e02),
    ("1.3e03", 1.3e03),
    ("1.4e04", 1.4e04),
    ("1.5e05", 1.5e05),
    ("1.6e06", 1.6e06),
    // ("1.5e-1", 1.5e-1),
    // ("1.2e-2", 1.2e-2),
    // ("1.3e-3", 1.3e-3),
    // ("1.4e-4", 1.4e-4),
    // ("1.5e-5", 1.5e-5),
    // ("1.6e-6", 1.6e-6),
    // ("0.1e-1", 0.1e-1),
    // ("0.2e-2", 0.2e-2),
    // ("0.3e-3", 0.3e-3),
    // ("0.4e-4", 0.4e-4),
    // ("0.5e-5", 0.5e-5),
    // ("0.6e-6", 0.6e-6),
    ("12.34e-1", 12.34e-1),
    ("12.34e1", 12.34e1),
    ("12.34e2", 12.34e2),
    ("12.34e3", 12.34e3),
    ("12.34e4", 12.34e4),
    ("12.34e5", 12.34e5),
  ]
  for pair in test_set {
    let (code, ans) = pair
    let token = match fast_get_tok?(code) {
      Ok(t) => t
      Err(_) => fail!("Parse Double failed in Lexer")
    }
    let tok_type = token.token_type
    match tok_type {
      TokenType::Double(i) =>
        if i != ans {
          let mut errmsg =
            #| Parse Double failed in Lexer,
          errmsg += code + " should be Double, while get token : \{i}"
          fail!(errmsg)
        }
      _ => {
        let mut errmsg =
          #| Parse Double failed in Lexer,
        errmsg += code + " should be Double, while get token : \{tok_type}"
        fail!(errmsg)
      }
    }
  }
}

test "lexer boolean correctness" {
  match fast_get_tok!("true").token_type {
    TokenType::Boolean(true) => ()
    _ => fail!("Parse Boolean true failed in Lexer")
  }
  match fast_get_tok!("false").token_type {
    TokenType::Boolean(false) => ()
    _ => fail!("Parse Boolean false failed in Lexer")
  }
}

test "lexer keyword correctness" {
  let test_cases = [
    "fn", "let", "if", "else", "return", "while", "for", "break", "continue",
  ]
  for word in test_cases {
    match fast_get_tok!(word).token_type {
      TokenType::KeyWord(_) => ()
      _ => fail!("Parse Keyword failed in Lexer")
    }
  }
}

test "lexer operator correctness" {
  let text_cases : Array[String] = [
    "+", "-", "*", "/", "%", ">", "<", ">=", "<=", "==", "!=", "+=", "-=", "*=",
    "/=", "%=",
  ]
  for op in text_cases {
    let tok = match fast_get_tok?(op) {
      Ok(t) => t
      Err(_) => fail!("Parse Operator \"{op}\" failed in Lexer")
    }
    match tok.token_type {
      TokenType::Operator(_) => ()
      _ => {
        let mut errmsg =
          #| Parse Operator failed in Lexer, 
        errmsg += op +
          " should be Operator, while get token : \{tok.token_type}"
        fail!(errmsg)
      }
    }
  }
}

/// ---------------------------------------------------------------------------
/// ## Parser Tests
/// ---------------------------------------------------------------------------

/// ------------------------------------------------------
/// ## Basic Ast Node Parsing
/// ------------------------------------------------------
/// ------------------------------------------------------
/// ## Test Atom Expression Parsing
///
/// ### TODO
///
/// - Support Construct Parsing, eg: `Color::Red(255, 0, 0)`
/// - Support Struct Parsing, eg: `Point::{x: 1, y: 2}`
/// ------------------------------------------------------
test "parse atom expr" {
  let codes = [
    "123", "456", // Literal
     "22.0", "33.45", // Literal
     "true", "false", // Literal 
     "xyz", "abc", // Ident
     "(1, 2, 3)", "(z, y, 33)", // Tuple
     "[1, a, b]", // Array
     "fn inc(x) {x + 1}", // closure
     "(if a > b {a} else {b})", // Paren Expr
  ]
  fn fast_parse_atomic(code : String) -> AtomExpr!Error {
    let context = Context::new(code)
    let lexer = Lexer::new(context)
    let _ = lexer.run!()
    let parser = Parser::new(context)
    parser.parse_atomic_expr!()
  }

  for code in codes {
    let _ = fast_parse_atomic!(code)

  }
}

/// ------------------------------------------------------
/// ## Test Apply Expression Parsing
/// ------------------------------------------------------
test "parse apply expr" {
  let codes = [
    "123", "abc", "(1, 2, 3)", "[1, 42]", // Atomic
     "p.x", "person.name", // struct access
     "(1, 2).0", "(a, b).1", // tuple access
     "tup.3", "tup.4", // tuple access
     "arr[3]", "arr[(1+3)]", // array access
     "map.get(1)", // call expr
     "map.get(2).x", "arr[3].y", "func_map.make_points(10)[3].z", // complex
  ]
  fn fast_parse_apply(code : String) -> ApplyExpr!Error {
    let context = Context::new(code)
    let lexer = Lexer::new(context)
    let _ = lexer.run!()
    let parser = Parser::new(context)
    parser.parse_apply_expr!()
  }

  for code in codes {
    let _ = fast_parse_apply!(code)

  }
}

/// ------------------------------------------------------
/// ## Test Binary Expression Parsing
/// ------------------------------------------------------
test "parse binary expr" {
  let codes_left2_right1 = ["a * b + 1", "a/b + 1"]
  let codes_left1_right2 = ["1 + a * b", "1 + a/b"]
  let codes_left2_right2 = ["a * b + c * d", "a/b + c/d"]
  fn fast_parse_binary(code : String) -> BinaryExpr!Error {
    let context = Context::new(code)
    let lexer = Lexer::new(context)
    let _ = lexer.run!()
    let parser = Parser::new(context)
    let expr = parser.parse_expr!()
    match expr {
      Binary(e) => e
      _ => fail!("Parse binary failed!")
    }
  }

  for code in codes_left2_right1 {
    let bin = fast_parse_binary!(code)
    match (bin.left, bin.right) {
      (Binary(_), Apply(_)) => ()
      _ => fail!("Parse binary failed! Maybe operator precedence is wrong!")
    }
  }
  for code in codes_left1_right2 {
    let bin = fast_parse_binary!(code)
    match (bin.left, bin.right) {
      (Apply(_), Binary(_)) => ()
      _ => fail!("Parse binary failed! Maybe operator precedence is wrong!")
    }
  }
  for code in codes_left2_right2 {
    let bin = fast_parse_binary!(code)
    match (bin.left, bin.right) {
      (Binary(_), Binary(_)) => ()
      _ => fail!("Parse binary failed! Maybe operator precedence is wrong!")
    }
  }
}

test "parse atom pattern" {
  let codes = ["a", "abc", "12", "(a, b)"]
  fn fast_parse_atom_pattern(code : String) -> AtomPat!Error {
    let context = Context::new(code)
    let lexer = Lexer::new(context)
    let _ = lexer.run!()
    let parser = Parser::new(context)
    parser.parse_atom_pat!()
  }

  for code in codes {
    let _ = fast_parse_atom_pattern!(code)

  }
}

test "parse as pattern" {
  let codes = ["42 as b", "(a, b) as t"]
  fn fast_parse_as_pattern(code : String) -> AsPat!Error {
    let context = Context::new(code)
    let lexer = Lexer::new(context)
    let _ = lexer.run!()
    let parser = Parser::new(context)
    parser.parse_as_pat!()
  }

  for code in codes {
    let _ = fast_parse_as_pattern!(code)

  }
}

test "parse assign expr" {
  let codes = [
    "a = 42", "a = b", "(a, b) = (1, 2)", "(a, (p.x, p[y])) = (1, (3, 4))",
  ]
  fn fast_parse_assign(code : String) -> AssignExpr!Error {
    let context = Context::new(code)
    let lexer = Lexer::new(context)
    let _ = lexer.run!()
    let parser = Parser::new(context)
    let expr = parser.parse_state_expr!()
    match expr {
      Assign(e) => e
      _ => fail!("Parse assign failed! expr is \{expr.to_json().stringify()}")
    }
  }

  for code in codes {
    let _ = fast_parse_assign!(code)

  }
}

test "parse if expr" {
  let codes = [
    "if a > b {a} else {b}", "if true {42} else {33}", "if isinf(n) { 1.0 } else { 0.0 }",
  ]
  fn fast_parse_if(code : String) -> IfExpr!Error {
    let context = Context::new(code)
    let lexer = Lexer::new(context)
    let _ = lexer.run!()
    let parser = Parser::new(context)
    parser.parse_if_expr!()
  }

  for code in codes {
    let _ = fast_parse_if!(code)

  }
}

test "parse while expr" {
  let codes = [
    "while a > b { a = a + 1; b = b - 1; }", "while true { work(); }", "while not(isinf(n)) { n * 2; }",
  ]
  fn fast_parse_while(code : String) -> WhileExpr!Error {
    let context = Context::new(code)
    let lexer = Lexer::new(context)
    let _ = lexer.run!()
    let parser = Parser::new(context)
    parser.parse_while_expr!()
  }

  for code in codes {
    let _ = fast_parse_while!(code)

  }
}

/// ------------------------------------------------------
/// ## Test StateExpr Parsing
///
/// ## TODO
///
/// - Support `for` loop parsing
/// - Support `match` statement parsing
/// - Support `try` statement parsing
/// - Support `raise` statement parsing`
/// - Support `loop` statement parsing
/// ------------------------------------------------------
test "parse state expr" {
  let codes = [
    "a = 42", "a + 42", "break", "continue", "return", "return (a, b)", "if a > b {a} else {b}",
    "while a > b { a = a + 1; b = b - 1; }", "{ let a = 42; let b = 33; a + b; }",
  ]
  fn fast_parse_state(code : String) -> StateExpr!Error {
    let context = Context::new(code)
    let lexer = Lexer::new(context)
    let _ = lexer.run!()
    let parser = Parser::new(context)
    parser.parse_state_expr!()
  }

  for code in codes {
    let _ = fast_parse_state!(code)

  }
}

test "parse block expr" {
  let codes = [
    "{ let a = 42; let b = 33; a + b; }", "{ let n = 1; while n < 10 { n = n + 1; } n; }",
    "{ let (a, b) = 1; while a < 10 {a = a + b; b = a; } a }",
  ]
  fn fast_parse_block(code : String) -> BlockExpr!Error {
    let context = Context::new(code)
    let lexer = Lexer::new(context)
    let _ = lexer.run!()
    let parser = Parser::new(context)
    parser.parse_block_expr!()
  }

  for code in codes {
    let _ = fast_parse_block!(code)

  }
}

/// ------------------------------------------------------
/// ## Test Closure Parsing
///
/// ## TODO
///
/// - one paring error: `fn mul2(a: Int) {a * 2}`
/// hint: possibly lexer error
/// ------------------------------------------------------
test "parse closure" {
  let codes = [
    "fn inc(x) {x + 1}", "fn add(a, b) {a + b}", "fn max(a, b) {if a > b {a} else {b}}",
    "fn min(a, b) {if a < b {a} else {b}}",
    // "fn mul2(a: Int) {a * 2}",
     "fn plus(a) -> Int {a + 2}",
  ]
  fn fast_parse_closure(code : String) -> Closure!Error {
    let context = Context::new(code)
    let lexer = Lexer::new(context)
    let _ = lexer.run!()
    let parser = Parser::new(context)
    parser.parse_closure!()
  }

  for code in codes {
    let _ = fast_parse_closure!(code)

  }
}

/// ------------------------------------------------------
/// ## Test Function Definition Parsing
/// ------------------------------------------------------
fn read_file(file_path : String) -> String {
  if @fs.exists(file_path) {
    @fs.read_to_string(file_path)
  } else {
    ""
  }
}

test "parse function definition" {
  let test_files = [
    "unit_test/test_files/fact.mbt", "unit_test/test_files/max.mbt", "unit_test/test_files/fib.mbt",
    "unit_test/test_files/is_prime.mbt",
  ]
  guard let Some(pwd) = @env.get_env_var("PWD")
  let codes = test_files.map(fn(file) { read_file(pwd + "/" + file) })
  fn fast_parse_func_def(code : String, path : String) -> FuncDef!Error {
    if code == "" {
      let err = @Color.red_font("File not found: \{path}")
      fail!(err)
    }
    let context = Context::new(code)
    let lexer = Lexer::new(context)
    let _ = lexer.run!()
    let parser = Parser::new(context)
    parser.parse_func_def!()
  }

  for i, code in codes {
    let _ = fast_parse_func_def!(code, test_files[i])

  }
}
