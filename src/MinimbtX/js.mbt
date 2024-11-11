/// ----------------------------------------------------
/// Ast to javascript
/// ----------------------------------------------------

pub fn pre_indent(indent: Int) -> String {
  repeat("  ", indent)
}

pub fn to_js(self: Program) -> String {
  let mut js = "".to_string()
  for decl in self.top_decls {
    js = js + decl.to_js() + "\n"
  }
  js
}

/// ----------------------------------------------------
/// TopDecl to JS
/// ----------------------------------------------------
pub fn to_js(self: TopDecl) -> String {
  let mut wild_card_count = 0
  fn wild_card_counter() -> Int {
    wild_card_count += 1
    wild_card_count
  }
  match self {
    FuncDef(f) => f.to_js()
    MainFunc(m) => m.to_js()
    LetExpr(l) => l.to_js(0, wild_card_counter)
    _ => {
      "Unimplemented top decl to_js"
    }
  }
}
/// ----------------------------------------------------
/// Main Func
/// ----------------------------------------------------
pub fn to_js(self: MainFunc) -> String {
  let decl = "function main() {\n"
  let body = self.body.to_js(1)
  decl + body + "}\n"
}
/// ----------------------------------------------------
/// FuncDef to JS
/// ----------------------------------------------------
pub fn to_js(self: FuncDef) -> String {
  let is_ret = match self.meta().query_type() {
    Unit => false
    Undefine => false
    Type::FuncTy(fty) => match fty.ret_type {
      Unit => false
      Undefine => false
      _ => true
    }
    _ => {
      println("function type should not have other possibilities")
      println("func is \{self.name.id} ,type is \{self.meta().query_type()}")
      panic()
    }
  }
  let decl = "function " + self.name.id + "("
  let params = self.params.map(fn(p) { p.ident.id }).join(", ")
  let decl = decl + params + ") {\n"
  let body = self.body.to_js(1, ~is_ret)
  decl + body + "}\n"
}

/// ----------------------------------------------------
/// Closure to JS
/// ----------------------------------------------------
pub fn to_js(self: Closure, indent: Int) -> String {
  let prefix = pre_indent(indent)
  let func_name = match self.name {
    Some(n) => n.id
    None => ""
  }
  let is_ret = match self.meta().query_type() {
    Unit => false
    Undefine => false
    Type::FuncTy(fty) => match fty.ret_type {
      Unit => false
      Undefine => false
      _ => true
    }
    _ => {
      println("function type should not have other possibilities")
      match self.name {
        Some(n) => println("func is \{n.id} ,type is \{self.meta().query_type()}")
        None => println("func type is \{self.meta().query_type()}")
      }
      panic()
    }
  }
  let decl = "function " + func_name + "("
  let params = self.params.map(fn(p) { p.ident.id }).join(", ")
  let decl = decl + params + ") {\n"
  let body = self.body.to_js(indent+1, ~is_ret)
  prefix + decl + body + prefix + "}"
}

/// ----------------------------------------------------
/// BlockExpr to JS
/// ----------------------------------------------------
pub fn to_js(self: BlockExpr, indent: Int, ~is_ret: Bool=false) -> String {
  let mut wild_card_count = 0
  fn wild_card_counter() {
    wild_card_count += 1
    wild_card_count
  }
  let mut js = ""
  let len = self.state_exprs.length()
  for i, state in self.state_exprs {
    if i < len - 1 {
      match state {
        Null(_) => continue
        _ => js = js + state.to_js(indent, wild_card_counter) + "\n"
      }
    } else {
      js = js + state.to_js(indent, wild_card_counter, ~is_ret) + "\n"
    }
  }
  js
}


/// ----------------------------------------------------
/// StateExpr to JS
/// ----------------------------------------------------
pub fn to_js(self: StateExpr, indent : Int, wild_card_counter:()-> Int, ~is_ret: Bool = false) -> String {
  match self {
    Let(l) => l.to_js(indent, wild_card_counter)
    Assign(a) => a.to_js(indent)
    While(w) => w.to_js(indent)
    // Break(b) => b.to_js(indent)
    // Continue(c) => c.to_js(indent)
    // Return(r) => r.to_js(indent)
    Expr(e) => e.to_js(indent, ~is_ret)
    Null(_) => if is_ret { "return ;" } else { "" }
    _ => {
      "Unimplemented state expr to_js"
    }
  }
}


/// ----------------------------------------------------
/// AssignExpr to JS
/// ----------------------------------------------------
pub fn to_js(self: AssignExpr, indent: Int) -> String {
  let prefix = pre_indent(indent)
  let left = self.left.to_js(0)
  let right = self.right.to_js(0)
  prefix + left + " = " + right + ";"
}

/// ----------------------------------------------------
/// LetExpr to JS
/// ----------------------------------------------------
pub fn to_js(self: LetExpr, indent: Int, wild_card_counter:()-> Int) -> String {
  let prefix = pre_indent(indent)
  let expr = self.expr.to_js(0)
  match self.pat.atom_pat {
    WildCard(_) => prefix + expr + ";"
    _ => {
      let pat = self.pat.to_js(0, wild_card_counter)
      prefix + "let " + pat + " = " + expr + ";"
    }
  }
}

/// ----------------------------------------------------
/// Pattern to JS
/// ----------------------------------------------------
pub fn to_js(
  self: AsPat, indent: Int, wild_card_counter: ()-> Int
) -> String {
  self.atom_pat.to_js(indent, wild_card_counter)
}

pub fn to_js(
  self: AtomPat, indent: Int,
  wild_card_counter: ()-> Int
) -> String {
  match self {
    WildCard(w) => w.to_js(0, wild_card_counter)
    Ident(i) => i.to_js(indent)
    Tuple(t) => {
      let tuple_str = t.as_pats.map(fn(pat) {
        pat.to_js(indent, wild_card_counter) 
      }).join(", ")
      "[" + tuple_str + "]"
    }
    _ => {
      "Unimplemented atom pat to_js"
    }
  }
}

pub fn to_js(
  self: WildCard, indent : Int, wild_card_counter: ()-> Int
) -> String {
  let count = wild_card_counter()
  pre_indent(indent) + "_" + count.to_string()
}

/// ----------------------------------------------------
/// WhileExpr to JS
/// ----------------------------------------------------
pub fn to_js(self: WhileExpr, indent: Int) -> String {
  let prefix = pre_indent(indent)
  let cond = self.cond_expr.to_js(0)
  let body = self.body.to_js(indent+1)
  prefix + "while (" + cond + ") {\n" + body + "\n" + prefix + "}"
}

/// ----------------------------------------------------
/// IfExpr to JS
/// ----------------------------------------------------

fn promote_if_stock_maker() -> () -> Map[String, String] {
  let stock : Map[String, String] = Map::new()
  fn() -> Map[String, String] {
    stock
  }
}

let promote_if_stock : () -> Map[String, String] = promote_if_stock_maker()

fn release_promoted() -> String {
  let mut res = "".to_string()
  promote_if_stock().each(fn(k, v) {
    res = res + v + "\n"
  })
  promote_if_stock().clear()
  res
}

pub fn to_js(
  self: IfExpr, indent: Int, ~is_ret: Bool = false, ~promote: Bool = false
) -> String {
  if promote {
    let closure_name = "__promote" + promote_counter().to_string()
    let prefix = pre_indent(indent)
    let mut closure_str = prefix + "function " + closure_name + "() {\n"
    let then_str = self.then_expr.to_js(indent + 1, is_ret=true)
    let else_str = match self.else_clause {
      Some(e) => prefix + "} else {\n" + e.to_js(indent + 1, is_ret=true) + prefix + "}"
      None => "".to_string()
    }
    closure_str = prefix + closure_str + "if (" + self.cond_expr.to_js(0) + ") {\n" + then_str + else_str + prefix + "}"
    promote_if_stock().set(closure_name, closure_str)
    closure_name + "()"
  } else {
    let prefix = pre_indent(indent)
    let cond = self.cond_expr.to_js(0)
    let then = self.then_expr.to_js(indent + 1, ~is_ret)
    let else_clause = match self.else_clause {
      Some(e) => prefix + "} else {\n" + e.to_js(indent + 1, ~is_ret) + prefix + "}"
      None => "".to_string()
    }
    prefix + "if (" + cond + ") {\n" + then + else_clause
  }
}

pub fn to_js(self: ElseClause, indent: Int, ~is_ret: Bool = false) -> String {
  match self {
    Block(b) => b.to_js(indent, ~is_ret)
    IfExpr(i) => i.to_js(indent, ~is_ret)
  }
}

/// ----------------------------------------------------
/// Expr to JS
/// ----------------------------------------------------
pub fn to_js(self: Expr, indent: Int, ~is_ret: Bool=false, ~promote: Bool = false) -> String {
  match self {
    Binary(b) => b.to_js(indent, ~is_ret)
    Apply(a) => a.to_js(indent, ~is_ret)
    Block(b) => b.to_js(indent, ~is_ret)
    IfExpr(i) => i.to_js(indent, ~is_ret, ~promote)
    _ => {
      "Unimplemented expr to_js"
    }
  }
}

/// ----------------------------------------------------
/// BinaryExpr to JS
/// ----------------------------------------------------
pub fn to_js(self: BinaryExpr, indent : Int, ~is_ret: Bool = false) -> String {
  let left = self.left.to_js(0, promote = true)
  let right = self.right.to_js(0, promote = true)
  let op = self.op.sym
  let s = ""
  let m = if is_ret {
    release_promoted() +
    pre_indent(indent) + "return " + left + " " + op + " " + right + ";"
  } else {
    pre_indent(indent) + left + " " + op + " " + right
  }
  s + m
}

/// ----------------------------------------------------
/// UnaryExpr to JS
/// ----------------------------------------------------
pub fn to_js(self: UnaryExpr, indent : Int, ~is_ret: Bool = false) -> String {
  let expr = self.expr.to_js(0, promote=true)
  let op = self.uop.sym
  if is_ret {
    release_promoted() +
    pre_indent(indent) + "return " + op + expr + ";"
  } else {
    pre_indent(indent) + op + expr
  }
}

/// ----------------------------------------------------
/// ArrayExpr to JS
/// ----------------------------------------------------
pub fn to_js(self: ArrayExpr, indent: Int, ~is_ret: Bool = false) -> String {
  let mut js = "[".to_string()
  for expr in self.exprs {
    js = js + expr.to_js(0, promote=true) + ", "
  }
  if is_ret {
    release_promoted() +
    pre_indent(indent) + "return " + js + "]"
  } else {
    pre_indent(indent) + js + "]"
  }
}

/// ----------------------------------------------------
/// TupleExpr to JS
/// ----------------------------------------------------
pub fn to_js(self: TupleExpr, indent: Int, ~is_ret: Bool = false) -> String {
  let js = "[".to_string()
  let expr_str = self.exprs.map(fn(expr) { expr.to_js(0, promote = true) }).join(", ")
  let js = js + expr_str
  if is_ret {
    release_promoted() +
    pre_indent(indent) + "return " + js + "]"
  } else {
    pre_indent(indent) + js + "]"
  }
}

/// ----------------------------------------------------
/// CallExpr to JS
/// ----------------------------------------------------
pub fn to_js(self: CallExpr, indent: Int, ~is_ret: Bool = false) -> String {
  let mut res = ""
  let callee = self.apply_expr.to_js(0)
  let callee = rename_callee(callee, self.args)
  let args_str = self.args.map(fn(arg) { arg.to_js(0, promote=true) }).join(", ")
  res += release_promoted()
  if is_ret {
    res + pre_indent(indent) + "return " + callee + "(" + args_str + ")"
  } else {
    res + pre_indent(indent) + callee + "(" + args_str + ")"
  }
}
// for complex expr, may be need to promote to closure
fn make_promote_counter() -> () -> Int {
  let mut count = 0
  fn() -> Int {
    count += 1
    count
  }
}

let promote_counter : () -> Int = make_promote_counter()

fn promote_to_closure(self: Expr, indent: Int) -> (String, String) {
  let prefix = pre_indent(indent)
  let fname = "__promote" + promote_counter().to_string()
  let mut s = prefix + "function " + fname + "() {\n"
  s += self.to_js(indent + 1, is_ret=true) + "\n"
  (s + prefix + "}", fname)
}

// for the following function, need to rename:
// 1. need to add `minimbt_` before it:
//   - print_int
//   - print_endline
//   - print_newline
//   - int_of_float
//   - float_of_int
//   - floor
//   - abs_float
//   - truncate
//   - sqrt
//   - sin
//   - cos
//   - atan
// 2. for `Array::make`, check the second arg's type
// if it is int: rename it to `minimbt_create_array`
// if it is double: rename it to `minimbt_create_float_array`
// if others, rename it to `minimbt_create_ptr_array`
// 3. for `not`, rename it to `!`
// 4. if not on the list, return the original name
fn rename_callee(fname: String, args: Array[Expr]) -> String {
  match fname {
    "not" => "!"
    "print_int" => "minimbt_print_int"
    "print_endline" => "minimbt_print_endline"
    "print_newline" => "minimbt_print_newline"
    "int_of_float" => "minimbt_int_of_float"
    "float_of_int" => "minimbt_float_of_int"
    "floor" => "minimbt_floor"
    "abs_float" => "minimbt_abs_float"
    "truncate" => "minimbt_truncate"
    "sqrt" => "minimbt_sqrt"
    "sin" => "minimbt_sin"
    "cos" => "minimbt_cos"
    "atan" => "minimbt_atan"
    "Array::make" => {
      match args[1].meta().query_type() {
        Type::Int32 => "minimbt_create_array"
        Type::Double => "minimbt_create_float_array"
        _ => "minimbt_create_ptr_array"
      }
    }
    _ => fname
  }
}
/// ----------------------------------------------------
/// AccessExpr to JS
/// ----------------------------------------------------
pub fn to_js(self: AccessExpr, indent: Int, ~is_ret: Bool = false) -> String {
  match self.accessor {
    ArrayAcc(a) => {
      let apply_expr = self.apply_expr.to_js(0)
      let accessor = a.to_js(0)
      if is_ret {
        pre_indent(indent) + "return " + apply_expr + "[" + accessor + "]"
      } else {
        pre_indent(indent) + apply_expr + "[" + accessor + "]"
      }
    }
    _ => {
      "Unimplemented access expr to_js"
    }
  }
}

/// ----------------------------------------------------
/// ApplyExpr to JS
/// ----------------------------------------------------
pub fn to_js(self: ApplyExpr, indent: Int, ~is_ret: Bool = false) -> String {
  match self {
    Atomic(a) => a.to_js(indent, ~is_ret)
    Access(a) => a.to_js(indent, ~is_ret)
    CallExpr(c) => c.to_js(indent, ~is_ret)
  }
}

/// ----------------------------------------------------
/// AtomExpr to JS
/// ----------------------------------------------------
pub fn to_js(self: AtomExpr, indent: Int, ~is_ret: Bool = false) -> String {
  match self {
    Literal(l) => l.to_js(indent, ~is_ret)
    Ident(i) => i.to_js(indent, ~is_ret)
    Paren(e) => e.to_js(indent, ~is_ret, promote=true)
    Method(m) => m.to_js(indent)
    Closure(c) => c.to_js(indent)
    Unit(_) => if is_ret { pre_indent(indent) + "return ;" } else { "" }
    UnaryExpr(u) => u.to_js(indent, ~is_ret)
    ArrayExpr(a) => a.to_js(indent, ~is_ret)
    TupleExpr(t) => t.to_js(indent, ~is_ret)
    _ => {
      "Unimplemented atom expr to_js"
    }
  }
}

pub fn to_js(self: MethodExpr, indent: Int, ~is_ret: Bool = false) -> String {
  if is_ret {
    pre_indent(indent) + "return " + self.ident.id
  } else {
    pre_indent(indent) + self.ident.id
  }
}

/// ----------------------------------------------------
/// LowerCase, UpperCase, Literal to JS
/// ----------------------------------------------------
pub fn to_js(self: UpperCase, indent: Int, ~is_ret: Bool = false) -> String {
  if is_ret {
    pre_indent(indent) + "return " + self.id
  } else {
    pre_indent(indent) + self.id
  }
}

pub fn to_js(self: LowerCase, indent: Int, ~is_ret: Bool = false) -> String {
  if is_ret {
    pre_indent(indent) + "return " + self.id
  } else {
    pre_indent(indent) + self.id
  }
}

pub fn to_js(self: Literal, indent: Int, ~is_ret: Bool = false) -> String {
  let prefix = pre_indent(indent)
  let s = match self.category {
    Boolean(b) => b.to_string()
    Double(d) => d.to_string()
    Integer(i) => i.to_string()
    Long(l) => l.to_string()
    Unsigned(u) => u.to_string()
    UnsignedLong(ul) => ul.to_string()
    Char(c) => c.to_string()
    String(s) => s
  }
  if is_ret {
    prefix + "return " + s
  } else {
    prefix + s
  }
}
