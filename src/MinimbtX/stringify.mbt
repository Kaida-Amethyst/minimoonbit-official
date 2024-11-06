/// -------------------------------------------------
/// # Stringify Type
/// -------------------------------------------------
pub fn stringify(self : Type) -> String {
  if self.is_undefine() {
    @Color.bold_red(self.to_string())
  } else {
    @Color.bold_green(self.to_string())
  }
}

/// -------------------------------------------------
/// # Stringify AST
/// -------------------------------------------------
fn pre_space(indent : Int) -> String {
  repeat("   | ", indent)
}

fn ensure_line_feed(s : String) -> String {
  s.trim_end("\n") + "\n"
}

/// -------------------------------------------------
/// ## Whole Program
/// -------------------------------------------------
pub fn stringify(self : Program, indent : Int) -> String {
  let mut whole = "".to_string()
  for decl in self.top_decls {
    let decl_str = decl.stringify(indent)
    whole = whole + decl_str + "\n"
  }
  whole
}

/// -------------------------------------------------
/// ## Top Declaration
/// -------------------------------------------------
pub fn stringify(self : TopDecl, indent : Int) -> String {
  match self {
    FuncDef(f) => f.stringify(indent)
    MainFunc(m) => m.stringify(indent)
    LetExpr(l) => l.stringify(indent)
    // StructDef(s) => s.stringify(indent)
    // EnumDef(e) => e.stringify(indent)
    _ => "Unimplemented TopDecl Stringify"
  }
}

/// -------------------------------------------------
/// ## Function Definition
/// -------------------------------------------------
pub fn stringify(self : FuncDef, indent : Int) -> String {
  let head_line = pre_space(indent) + "FuncDef --- "
  let func_name = self.name.id +
    ": " +
    self.meta().analysis.ast_type.stringify()
  let head_line = head_line + func_name + "\n"
  let params_str = if self.params.length() > 0 {
    let arg_line = pre_space(indent + 1) + "Parameters:\n"
    self.params.fold(
      fn(acc, param) { acc + param.stringify(indent + 2) + "\n" },
      init=arg_line,
    )
  } else {
    ""
  }
  let body_head = pre_space(indent + 1) +
    "Body:" +
    self.body.meta().analysis.ast_type.stringify() +
    "\n"
  let body_str = self.body.stringify(indent + 1)
  let body_str = ensure_line_feed(body_str)
  head_line + params_str + body_head + body_str
}

pub fn stringify(self : MainFunc, indent : Int) -> String {
  let head_line = pre_space(indent) + "MainFunc:\n"
  let body_str = self.body.stringify(indent + 1)
  head_line + body_str
}

pub fn stringify(self : FuncParam, indent : Int) -> String {
  let prefix = pre_space(indent)
  let whole = prefix + self.ident.id + ":" + self.type_annote.stringify(0)
  whole
}

/// -------------------------------------------------
/// ## Closure Definition
/// -------------------------------------------------
pub fn stringify(self : Closure, indent : Int) -> String {
  let head_line = "Closure Def --- "
  let name_str = match self.name {
    Some(name) => name.stringify(0) + "\n"
    None => "(Anonymous)\n"
  }
  let head_line = pre_space(indent) + head_line + name_str
  let params_str = if self.params.length() > 0 {
    let arg_line = pre_space(indent + 1) + "Parameters:\n"
    self.params.fold(
      fn(acc, param) { acc + param.stringify(indent + 2) + "\n" },
      init=arg_line,
    )
  } else {
    ""
  }
  let ret_type_str = match self.ret_type {
    Some(t) => {
      let prefix = pre_space(indent + 1)
      prefix + "Return Type Annote: " + t.stringify(0) + "\n"
    }
    None => ""
  }
  let body_head = pre_space(indent + 1) + "Body:\n"
  let body_str = self.body.stringify(indent + 1)
  let body_str = ensure_line_feed(body_str)
  head_line + params_str + ret_type_str + body_head + body_str
}

pub fn stringify(self : ClosureParam, indent : Int) -> String {
  let prefix = pre_space(indent)
  let ty_str = match self.type_annote {
    Some(t) => t.stringify(0)
    None => self.meta().analysis.ast_type.stringify()
  }
  let whole = prefix + self.ident.id + ":" + ty_str
  whole
}

/// -------------------------------------------------
/// ## TypeExpr
/// -------------------------------------------------
pub fn stringify(self : TypeExpr, indent : Int) -> String {
  let s = match self {
    IdentType(i) => i.stringify(indent)
    TupleType(t) => t.stringify(indent)
    ApplyType(a) => a.stringify(indent)
    FuncType(f) => f.stringify(indent)
  }
  @Color.bold_magenta(s)
}

pub fn stringify(self : TupleType, indent : Int) -> String {
  let prefix = pre_space(indent)
  let mut type_str = prefix + "("
  let sub_tys = self.types
  match sub_tys.length() {
    0 => type_str += ")"
    _ => {
      type_str += sub_tys[0].stringify(0)
      for ty in sub_tys[1:] {
        type_str += ", " + ty.stringify(0)
      }
      type_str += ")"
    }
  }
  type_str
}

pub fn stringify(self : ApplyType, indent : Int) -> String {
  let prefix = pre_space(indent)
  let main_type_str = self.type_name.stringify(0)
  let mut sub_type_str = self.type_args[0].stringify(0)
  for i = 1; i < self.type_args.length(); i = i + 1 {
    sub_type_str += self.type_args[i].stringify(0)
  }
  let type_str = prefix + main_type_str + "[" + sub_type_str + "]"
  type_str
}

pub fn stringify(self : FuncType, indent : Int) -> String {
  let prefix = pre_space(indent)
  let arg_type_str = self.arg_type.stringify(indent + 1)
  let ret_type_str = self.ret_type.stringify(indent + 1)
  prefix + arg_type_str + " -> " + ret_type_str
}

/// -------------------------------------------------
/// ## State Expression
/// -------------------------------------------------
pub fn stringify(self : StateExpr, indent : Int) -> String {
  match self {
    Let(l) => l.stringify(indent)
    Assign(a) => a.stringify(indent)
    While(w) => w.stringify(indent)
    Break(b) => b.stringify(indent)
    Continue(c) => c.stringify(indent)
    Return(r) => r.stringify(indent)
    Expr(e) => e.stringify(indent)
    Null(_) => ""
  }
}

/// -------------------------------------------------
/// Let Expression
/// -------------------------------------------------
pub fn stringify(self : LetExpr, indent : Int) -> String {
  let prefix = pre_space(indent)
  let head_line = prefix + "LetExpr:\n"
  let as_pat = self.pat
  let pat_str = match as_pat.as_ident {
    Some(as_ident) => {
      let head_line = pre_space(indent) + "AsPattern:\n"
      let atom_pat_str = as_pat.atom_pat.stringify(indent + 1) + "\n"
      let as_ident_str = pre_space(indent) +
        "As:\n" +
        as_ident.stringify(indent + 1)
      ensure_line_feed(head_line + atom_pat_str + as_ident_str)
    }
    None => {
      let head_line = pre_space(indent) + "Atom Pattern:\n"
      let atom_pat_str = as_pat.atom_pat.stringify(indent + 1)
      ensure_line_feed(head_line + atom_pat_str)
    }
  }
  let type_annote = match self.type_annote {
    Some(t) => {
      let prefix = pre_space(indent + 1)
      prefix + "Type: " + t.stringify(0) + "\n"
    }
    None => ""
  }
  let expr = self.expr.stringify(indent)
  let expr_str = ensure_line_feed(expr)
  head_line + pat_str + type_annote + expr_str
}

/// -------------------------------------------------
/// Assign Expression
/// -------------------------------------------------
pub fn stringify(self : AssignExpr, indent : Int) -> String {
  let prefix = pre_space(indent)
  let head_line = prefix + "AssignExpr: " + self.assign_op.sym + "\n"
  let left = pre_space(indent + 1) + "Left: \n"
  let left = left + self.left.stringify(indent + 2) + "\n"
  let right = pre_space(indent + 1) + "Right: \n"
  let right = right + self.right.stringify(indent + 2)
  head_line + left + right
}

pub fn stringify(self : BreakExpr, indent : Int) -> String {
  pre_space(indent) + "Break"
}

pub fn stringify(self : ContinueExpr, indent : Int) -> String {
  pre_space(indent) + "Continue"
}

pub fn stringify(self : ReturnExpr, indent : Int) -> String {
  let prefix = pre_space(indent)
  let head_line = prefix + "ReturnExpr:\n"
  let expr = match self.expr {
    Some(e) => e.stringify(indent + 1)
    None => ""
  }
  head_line + expr
}

/// -------------------------------------------------
/// While Expression
/// -------------------------------------------------
pub fn stringify(self : WhileExpr, indent : Int) -> String {
  let head_line = pre_space(indent) + "WhileExpr:\n"
  let cond_head = pre_space(indent + 1) + "Condition:\n"
  let cond_str = self.cond_expr.stringify(indent + 2)
  let cond_str = ensure_line_feed(cond_str)
  let body_head = pre_space(indent + 1) + "Body:\n"
  let body_str = self.body.stringify(indent + 2)
  let body_str = ensure_line_feed(body_str)
  head_line + cond_head + cond_str + body_head + body_str
}

/// -------------------------------------------------
/// Pattern
/// -------------------------------------------------
pub fn stringify(self : AsPat, indent : Int) -> String {
  match self.as_ident {
    Some(as_ident) => {
      let atom_pat_str = self.atom_pat.stringify(indent + 1) + "\n"
      let as_ident_str = pre_space(indent) +
        "As:\n" +
        as_ident.stringify(indent + 1) +
        "\n"
      atom_pat_str + as_ident_str
    }
    None => {
      let atom_pat_str = self.atom_pat.stringify(indent + 1)
      atom_pat_str
    }
  }
}

pub fn stringify(self : AtomPat, indent : Int) -> String {
  match self {
    WildCard(_) => pre_space(indent) + "wildcard(_)"
    Ident(i) => i.stringify(indent)
    Tuple(t) => {
      let head_line = pre_space(indent) + "Tuple Pattern:\n"
      let mut tuple_str = "".to_string()
      for pat in t {
        let this_pat_str = pat.stringify(indent)
        tuple_str = tuple_str + this_pat_str + "\n"
      }
      head_line + tuple_str
    }
    // ArrayPat(a) => a.stringify(indent)
    // StructPat(s) => s.stringify(indent)
    // UnitPat(u) => u.stringify(indent)
    _ => "Unimplemented Atom Pattern Stringify"
  }
}

/// -------------------------------------------------
/// Expression
/// -------------------------------------------------
pub fn stringify(self : Expr, indent : Int) -> String {
  match self {
    Binary(b) => b.stringify(indent)
    Apply(a) => a.stringify(indent)
    Block(b) => b.stringify(indent)
    // Match(m) => m.stringify(indent)
    IfExpr(i) => i.stringify(indent)
    _ => "Unimplemented Expr Stringify"
  }
}

/// -------------------------------------------------
/// If Expression
/// -------------------------------------------------
pub fn stringify(self : IfExpr, indent : Int) -> String {
  let head_line = pre_space(indent) +
    "IfExpr:" +
    self.meta().analysis.ast_type.stringify() +
    "\n"
  let cond_head = pre_space(indent + 1) + "Condition:\n"
  let cond_str = self.cond_expr.stringify(indent + 2)
  let cond_str = ensure_line_feed(cond_str)
  let then_head = pre_space(indent + 1) +
    "Then:" +
    self.then_expr.meta().analysis.ast_type.stringify() +
    "\n"
  let then_str = self.then_expr.stringify(indent + 2)
  let then_str = ensure_line_feed(then_str)
  match self.else_clause {
    Some(else_clause) => {
      let else_head = pre_space(indent + 1) +
        "Else:" +
        else_clause.meta().analysis.ast_type.stringify() +
        "\n"
      let else_str = else_clause.stringify(indent + 2) + "\n"
      head_line +
      cond_head +
      cond_str +
      then_head +
      then_str +
      else_head +
      else_str
    }
    None => head_line + cond_head + cond_str + then_head + then_str
  }
}

pub fn stringify(self : ElseClause, indent : Int) -> String {
  match self {
    Block(b) => b.stringify(indent)
    IfExpr(i) => i.stringify(indent)
  }
}

/// -------------------------------------------------
/// Block Expression
/// -------------------------------------------------
pub fn stringify(self : BlockExpr, indent : Int) -> String {
  let len = self.state_exprs.length()
  match len {
    0 => "Empty Block"
    1 => self.state_exprs[0].stringify(indent)
    _ => {
      let prefix = pre_space(indent)
      let head_line = prefix +
        "BlockExpr: " +
        self.meta().analysis.ast_type.stringify() +
        "\n"
      let mut state_exprs_str = "".to_string()
      for state in self.state_exprs {
        match state {
          Expr(_) => {
            let expr_ty_str = state.meta().analysis.ast_type.stringify()
            let expr_head = pre_space(indent + 1) +
              "Expr: " +
              expr_ty_str +
              "\n"
            let this_state_str = state.stringify(indent + 2)
            state_exprs_str = state_exprs_str + expr_head + this_state_str
            state_exprs_str = ensure_line_feed(state_exprs_str)
          }
          _ => {
            let this_state_str = state.stringify(indent + 1)
            state_exprs_str = state_exprs_str + this_state_str
            state_exprs_str = ensure_line_feed(state_exprs_str)
          }
        }
      }
      head_line + state_exprs_str
    }
  }
}

/// -------------------------------------------------
/// Binary Expression
/// -------------------------------------------------
pub fn stringify(self : BinaryExpr, indent : Int) -> String {
  let prefix = pre_space(indent)
  let head_line = prefix +
    (match self.op.sym {
      "+" => "Add(Binary):"
      "-" => "Sub(Binary):"
      "*" => "Mul(Binary):"
      "/" => "Div(Binary):"
      ">" => "Gt(Binary):"
      "<" => "Lt(Binary):"
      ">=" => "Ge(Binary):"
      "<=" => "Le(Binary):"
      "==" => "Eq(Binary):"
      "!=" => "Ne(Binary):"
      _ => "Unimplemented Binary Operator"
    }) +
    " " +
    self.meta().analysis.ast_type.stringify() +
    "\n"
  let left = self.left.stringify(indent + 1) + "\n"
  let right = self.right.stringify(indent + 1)
  ensure_line_feed(head_line + left + right)
}

/// -------------------------------------------------
/// Unary Expression
/// -------------------------------------------------
pub fn stringify(self : UnaryExpr, indent : Int) -> String {
  let prefix = pre_space(indent)
  let head_line = prefix +
    (match self.uop.sym {
      "-" => "Neg(Unary):" + self.meta().analysis.ast_type.stringify()
      _ => "Unimplemented Unary Operator"
    }) +
    "\n"
  let expr = self.expr.stringify(indent + 1) + "\n"
  head_line + expr
}

fn is_simple_apply(self : ApplyExpr) -> Bool {
  match self {
    Atomic(a) =>
      match a {
        Ident(_) => true
        Literal(_) => true
        _ => false
      }
    _ => false
  }
}

/// -------------------------------------------------
/// ## Apply Expression
/// -------------------------------------------------
pub fn stringify(self : ApplyExpr, indent : Int) -> String {
  match self {
    Atomic(a) => a.stringify(indent)
    Access(a) => a.stringify(indent)
    CallExpr(c) => c.stringify(indent)
  }
}

/// -------------------------------------------------
/// ### Call Expression
/// -------------------------------------------------
pub fn stringify(self : CallExpr, indent : Int) -> String {
  let prefix = pre_space(indent)
  let head_line = prefix +
    "CallExpr:" +
    self.meta().analysis.ast_type.stringify() +
    "\n"
  let apply_expr = pre_space(indent + 1) +
    @Color.bold_blue(self.apply_expr.stringify(0))
  let apply_expr = ensure_line_feed(apply_expr)
  let args_head = pre_space(indent + 1) + "Arguments:\n"
  let mut args_str = "".to_string()
  for arg in self.args {
    let this_arg_str = arg.stringify(indent + 1)
    args_str = ensure_line_feed(args_str + this_arg_str)
  }
  head_line + apply_expr + args_head + args_str
}

/// -------------------------------------------------
/// ### Access Expression
/// -------------------------------------------------
pub fn stringify(self : AccessExpr, indent : Int) -> String {
  let prefix = pre_space(indent)
  if is_simple_apply(self.apply_expr) {
    match self.accessor {
      ArrayAcc(e) => {
        let head_line = prefix +
          "Access Array: " +
          self.meta().analysis.ast_type.stringify() +
          "\n"
        let from_head = pre_space(indent + 1) + "From:\n"
        let apply_expr = self.apply_expr.stringify(indent + 2) + "\n"
        let accessor_head = pre_space(indent + 1) + "Index:\n"
        let expr_str = e.stringify(indent + 2)
        head_line + from_head + apply_expr + accessor_head + expr_str
      }
      _ => "Unimplemented Accessor"
    }
  } else {
    let head_line = prefix +
      "AccessExpr: " +
      self.meta().analysis.ast_type.stringify() +
      "\n"
    let apply_expr = self.apply_expr.stringify(indent + 1) + "\n"
    match self.accessor {
      ArrayAcc(e) => {
        let accessor_head = pre_space(indent + 1) + "Index:\n"
        let expr_str = e.stringify(indent + 2)
        let expr_str = ensure_line_feed(expr_str)
        head_line + apply_expr + accessor_head + expr_str
      }
      _ => "Unimplemented Accessor"
    }
  }
}

/// -------------------------------------------------
/// ## Atomic Expression
/// -------------------------------------------------
pub fn stringify(self : AtomExpr, indent : Int) -> String {
  match self {
    Literal(l) => l.stringify(indent)
    Ident(i) => i.stringify(indent)
    Paren(e) => e.stringify(indent)
    Method(m) => m.stringify(indent)
    Closure(c) => c.stringify(indent)
    Construct(c) => "AtomExpr Construct Stringify Unimplemented"
    StructExpr(s) => "AtomExpr StructExpr Stringify Unimplemented"
    UnaryExpr(u) => u.stringify(indent)
    TupleExpr(t) => t.stringify(indent)
    ArrayExpr(a) => a.stringify(indent)
    Unit(u) => u.stringify(indent)
  }
}

/// -------------------------------------------------
/// ### Tuple Expression
/// -------------------------------------------------
pub fn stringify(self : TupleExpr, indent : Int) -> String {
  let prefix = pre_space(indent)
  let head_line = prefix +
    "TupleExpr: " +
    self.meta().analysis.ast_type.stringify() +
    "\n"
  let mut exprs_str = "".to_string()
  for expr in self.exprs {
    let this_expr_str = expr.stringify(indent + 1)
    exprs_str = exprs_str + this_expr_str
    exprs_str = ensure_line_feed(exprs_str)
  }
  head_line + exprs_str
}

/// -------------------------------------------------
/// ### Array Expression
/// -------------------------------------------------
pub fn stringify(self : ArrayExpr, indent : Int) -> String {
  let prefix = pre_space(indent)
  let head_line = prefix +
    "ArrayExpr: " +
    self.meta().analysis.ast_type.stringify() +
    "\n"
  let mut exprs_str = "".to_string()
  for expr in self.exprs {
    let this_expr_str = expr.stringify(indent + 1)
    exprs_str = exprs_str + this_expr_str
    exprs_str = ensure_line_feed(exprs_str)
  }
  head_line + exprs_str
}

/// -------------------------------------------------
/// ### Method Expression
/// -------------------------------------------------
pub fn stringify(self : MethodExpr, indent : Int) -> String {
  pre_space(indent) +
  "Method: " +
  self.ident.id +
  " " +
  self.meta().analysis.ast_type.stringify()
}

/// -------------------------------------------------
/// ### Unit, Ident, Literal
/// -------------------------------------------------
pub fn stringify(self : UnitExpr, indent : Int) -> String {
  pre_space(indent) + "Unit():" + self.meta().analysis.ast_type.stringify()
}

pub fn stringify(self : LowerCase, indent : Int) -> String {
  pre_space(indent) + self.id + ": " + self.meta().analysis.ast_type.stringify()
}

pub fn stringify(self : UpperCase, indent : Int) -> String {
  pre_space(indent) + self.id
}

pub fn stringify(self : Literal, indent : Int) -> String {
  let prefix = pre_space(indent)
  let prefix = prefix +
    (match self.category {
      Boolean(b) => b.to_string()
      Double(d) => d.to_string()
      Integer(i) => i.to_string()
      Long(l) => l.to_string()
      Unsigned(u) => u.to_string()
      UnsignedLong(ul) => ul.to_string()
      Char(c) => "Char(" + c.to_string() + ")"
      String(s) => "String(" + s + ")"
    }) +
    ": " +
    self.meta().analysis.ast_type.stringify()
  prefix
}

/// -------------------------------------------------
/// Knf Stringify
/// -------------------------------------------------
fn prespace(indent : Int) -> String {
  repeat("  ", indent)
}

pub fn stringify(self : Knf, indent : Int) -> String {
  let prespace = prespace(indent)
  match self {
    Unit => prespace + "Unit"
    Int(i) => prespace + i.to_string()
    Int64(i) => prespace + i.to_string() + "L"
    UInt(u) => prespace + u.to_string() + "U"
    UInt64(u) => prespace + u.to_string() + "UL"
    Double(d) => prespace + d.to_string()
    Boolean(b) => prespace + b.to_string()
    Var(name) => prespace + name.to_string()
    Neg(name) => prespace + "-" + name.to_string()
    Not(name) => prespace + "not(" + name.to_string() + ")"
    Add(l, r) => prespace + l.to_string() + " + " + r.to_string()
    Sub(l, r) => prespace + l.to_string() + " - " + r.to_string()
    Mul(l, r) => prespace + l.to_string() + " * " + r.to_string()
    Div(l, r) => prespace + l.to_string() + " / " + r.to_string()
    Rem(l, r) => prespace + l.to_string() + " % " + r.to_string()
    And(l, r) => prespace + l.to_string() + " && " + r.to_string()
    Or(l, r) => prespace + l.to_string() + " || " + r.to_string()
    Eq(l, r) => prespace + l.to_string() + " == " + r.to_string()
    Ne(l, r) => prespace + l.to_string() + " != " + r.to_string()
    Gt(l, r) => prespace + l.to_string() + " > " + r.to_string()
    Lt(l, r) => prespace + l.to_string() + " < " + r.to_string()
    Ge(l, r) => prespace + l.to_string() + " >= " + r.to_string()
    Le(l, r) => prespace + l.to_string() + " <= " + r.to_string()
    Tuple(_) => self.stringify_tuple(indent)
    Array(_) => self.stringify_array(indent)
    Let(_) => self.stringify_let(indent)
    LetTuple(_) => self.stringify_let_tuple(indent)
    LetRec(_) => self.stringify_let_rec(indent)
    Apply(_, _) => self.stringify_apply(indent)
    ExternApply(_) => self.stringify_extern_apply(indent)
    Beq(_) => self.stringify_beq(indent)
    Bne(_) => self.stringify_bne(indent)
    Bgt(_) => self.stringify_bgt(indent)
    Blt(_) => self.stringify_blt(indent)
    Bge(_) => self.stringify_bge(indent)
    Ble(_, _, _, _) => self.stringify_ble(indent)
    Br(_, _, _) => self.stringify_br(indent)
    Get(l, r) => "Get(" + l.to_string() + ", " + r.to_string() + ")"
    Put(l, i, r) => prespace + "Put(\{l}[\{i}]) = \{r}"
    Error(msg) => msg
  }
}

fn stringify_tuple(self : Knf, indent : Int) -> String {
  guard let Knf::Tuple(names) = self
  prespace(indent) +
  "(" +
  names.map(fn(name) { name.to_string() }).join(", ") +
  ")"
}

fn stringify_array(self : Knf, indent : Int) -> String {
  guard let Knf::Array(names) = self
  prespace(indent) +
  "[" +
  names.map(fn(name) { name.to_string() }).join(", ") +
  "]"
}

fn stringify_let(self : Knf, indent : Int) -> String {
  guard let Knf::Let((name, ty), knf, next) = self
  prespace(indent) +
  "let " +
  name.to_string() + " : " + ty.to_string() +
  " = " +
  knf.stringify(0) +
  "\n" +
  next.stringify(indent)
}

fn stringify_let_tuple(self : Knf, indent : Int) -> String {
  guard let Knf::LetTuple(names, expr, next) = self
  prespace(indent) +
  "let tuple (" +
  names.map(fn(name) { name.0.to_string() }).join(", ") +
  ") = " +
  expr.to_string() + "\n" +
  next.stringify(indent)
}

fn stringify_let_rec(self : Knf, indent : Int) -> String {
  guard let Knf::LetRec(knf_func_def, knf, next) = self
  prespace(indent) + "def " + knf_func_def.name.to_string() + "(" +
  knf_func_def.params.map(fn(param) { let (param, ty) = param; "\{param} : \{ty}"}).join(", ") +
  ") ->" + knf_func_def.ret_type.to_string() + "{\n" +
  knf.stringify(indent + 1) + "\n" +
  prespace(indent) + "}\n\n" +
  next.stringify(indent)
}

fn stringify_apply(self : Knf, indent : Int) -> String {
  guard let Knf::Apply(name, args) = self
  prespace(indent) + "call " + name.to_string() + "(" +
  args.map(fn(arg) { arg.to_string() }).join(", ") + ")"
}

fn stringify_extern_apply(self : Knf, indent : Int) -> String {
  guard let Knf::ExternApply(name, args) = self
  let args_str = args.map(fn(arg) { arg.to_string() }).join(", ")
  prespace(indent) + "call extern \{name} (\{args_str})"
}

fn stringify_beq(self : Knf, indent : Int) -> String {
  guard let Knf::Beq(l, r, then_knf, else_knf) = self
  prespace(indent) +
  "if \{l} == \{r}\n" +
  prespace(indent) +
  "then\n" +
  then_knf.stringify(indent + 1) +
  "\n" +
  prespace(indent) +
  "else\n" +
  else_knf.stringify(indent + 1)
}

fn stringify_bne(self : Knf, indent : Int) -> String {
  guard let Knf::Bne(l, r, then_knf, else_knf) = self
  prespace(indent) +
  "if \{l} != \{r}\n" +
  prespace(indent) +
  "then\n" +
  then_knf.stringify(indent + 1) +
  "\n" +
  prespace(indent) +
  "else\n" +
  else_knf.stringify(indent + 1)
}

fn stringify_bgt(self : Knf, indent : Int) -> String {
  guard let Knf::Bgt(l, r, then_knf, else_knf) = self
  prespace(indent) +
  "if \{l} > \{r}\n" +
  prespace(indent) +
  "then\n" +
  then_knf.stringify(indent + 1) +
  "\n" +
  prespace(indent) +
  "else\n" +
  else_knf.stringify(indent + 1)
}

fn stringify_blt(self : Knf, indent : Int) -> String {
  guard let Knf::Blt(l, r, then_knf, else_knf) = self
  prespace(indent) +
  "if \{l} < \{r}\n" +
  prespace(indent) +
  "then\n" +
  then_knf.stringify(indent + 1) +
  "\n" +
  prespace(indent) +
  "else\n" +
  else_knf.stringify(indent + 1)
}

fn stringify_bge(self : Knf, indent : Int) -> String {
  guard let Knf::Bge(l, r, then_knf, else_knf) = self
  prespace(indent) +
  "if \{l} >= \{r}\n" +
  prespace(indent) +
  "then\n" +
  then_knf.stringify(indent + 1) +
  "\n" +
  prespace(indent) +
  "else\n" +
  else_knf.stringify(indent + 1)
}

fn stringify_ble(self : Knf, indent : Int) -> String {
  guard let Knf::Ble(l, r, then_knf, else_knf) = self
  prespace(indent) +
  "if \{l} <= \{r}\n" +
  prespace(indent) +
  "then\n" +
  then_knf.stringify(indent + 1) +
  "\n" +
  prespace(indent) +
  "else\n" +
  else_knf.stringify(indent + 1)
}

fn stringify_br(self : Knf, indent : Int) -> String {
  guard let Knf::Br(cond, then_knf, else_knf) = self
  prespace(indent) +
  "if \{cond}" +
  prespace(indent) +
  "then\n" +
  then_knf.stringify(indent + 1) +
  "\n" +
  prespace(indent) +
  "else\n" +
  else_knf.stringify(indent + 1)
}

/// --------------------------------------------------
/// Closure Stringify
/// --------------------------------------------------
fn label_maker() -> () -> String {
  let mut count = 0
  fn() -> String {
    count += 1
    "L" + count.to_string()
  }
}

let make_label : () -> String = label_maker()

pub fn stringify(self : ClosureIR, indent : Int) -> String {
  let pre_str = repeat("  ", indent)

  let body_str = match self {
    Unit => "Unit\n"
    Int(i) => "\{i}\n"
    Int64(i) => "\{i}\n"
    UInt(i) => "\{i}\n"
    UInt64(i) => "\{i}\n"
    Double(f) => "\{f}\n"
    Boolean(b) => "\{b}\n"
    Var(name) => "\{name}\n"
    Neg(name) => "neg \{name}\n"
    Add(lhs, rhs) => "add \{lhs}, \{rhs}\n"
    Sub(lhs, rhs) => "sub \{lhs}, \{rhs}\n"
    Mul(lhs, rhs) => "mul \{lhs}, \{rhs}\n"
    Div(lhs, rhs) => "div \{lhs}, \{rhs}\n"
    Rem(lhs, rhs) => "rem \{lhs}, \{rhs}\n"
    Eq(lhs, rhs) =>  "eq \{lhs}, \{rhs}\n"
    Ne(lhs, rhs) =>  "ne \{lhs}, \{rhs}\n"
    Gt(lhs, rhs) =>  "gt \{lhs}, \{rhs}\n"
    Lt(lhs, rhs) =>  "lt \{lhs}, \{rhs}\n"
    Ge(lhs, rhs) =>  "ge \{lhs}, \{rhs}\n"
    Le(lhs, rhs) =>  "le \{lhs}, \{rhs}\n"
    Not(name) => "not \{name}\n"
    And(lhs, rhs) => "and \{lhs}, \{rhs}\n"
    Or(lhs, rhs) => "or \{lhs}, \{rhs}\n"
    Beq(lhs, rhs, then, elsc) => {
      let label1 = make_label()
      let label2 = make_label()
      "beq \{lhs}, \{rhs}, then \{label1}, else \{label2}\n" +
      pre_str + "\{label1}:\n" + then.stringify(indent + 1) +
      pre_str + "\{label2}:\n" + elsc.stringify(indent + 1)
    }
    Bne(lhs, rhs, then, elsc) => {
      let label1 = make_label()
      let label2 = make_label()
      "bne \{lhs}, \{rhs}, then \{label1}, else \{label2}\n" +
      pre_str + "\{label1}:\n" + then.stringify(indent + 1) +
      pre_str + "\{label2}:\n" + elsc.stringify(indent + 1)
    }
    Bgt(lhs, rhs, then, elsc) => {
      let label1 = make_label()
      let label2 = make_label()
      "bgt \{lhs}, \{rhs}, then \{label1}, else \{label2}\n" +
      pre_str + "\{label1}:\n" + then.stringify(indent + 1) +
      pre_str + "\{label2}:\n" + elsc.stringify(indent + 1)
    }
    Bge(lhs, rhs, then, elsc) => {
      let label1 = make_label()
      let label2 = make_label()
      "bge \{lhs}, \{rhs}, then \{label1}, else \{label2}\n" +
      pre_str + "\{label1}:\n" + then.stringify(indent + 1) +
      pre_str + "\{label2}:\n" + elsc.stringify(indent + 1)
    }
    Blt(lhs, rhs, then, elsc) => {
      let label1 = make_label()
      let label2 = make_label()
      "blt \{lhs}, \{rhs}, then \{label1}, else \{label2}\n" +
      pre_str + "\{label1}:\n" + then.stringify(indent + 1) +
      pre_str + "\{label2}:\n" + elsc.stringify(indent + 1)
    }
    Ble(lhs, rhs, then, elsc) => {
      let label1 = make_label()
      let label2 = make_label()
      "ble \{lhs}, \{rhs}, then \{label1}, else \{label2}\n" +
      pre_str + "\{label1}:\n" + then.stringify(indent + 1) +
      pre_str + "\{label2}:\n" + elsc.stringify(indent + 1)
    }
    Br(cond, then, elsc) => {
      let label1 = make_label()
      let label2 = make_label()
      "br \{cond}, then \{label1}, else \{label2}\n" +
      pre_str + "\{label1}:\n" + then.stringify(indent + 1) +
      pre_str + "\{label2}:\n" + elsc.stringify(indent + 1)
    }
    Let((name, ty), value, next) =>
      "let \{name} : \{ty} = \{value}" + next.stringify(indent)
    LetTuple(names, value, next) => {
      let names_str = names
        .map(fn(pair) { pair.0.to_string() + " : " + pair.1.to_string() })
        .join(", ")
      "let (\{names_str}) = \{value}\n" + next.stringify(indent)
    }
    MakeClosure((name, ty), closure_def, next) => {
      "make_closure \{name} - \{closure_def} : \{ty}\n" + next.stringify(indent)
    }
    CallClosure(name, args) => {
      let args_str = args.map(fn(arg) { arg.to_string() }).join(", ")
      "call_closure \{name}(\{args_str})\n"
    }
    CallDirect(name, args) => {
      let args_str = args.map(fn(arg) { arg.to_string() }).join(", ")
      "call_direct \{name}(\{args_str})\n"
    }
    MakeTuple(names) => {
      let names_str = names.map(fn(name) { name.to_string() }).join(" ")
      "make_tuple (\{names_str})\n"
    }
    ArrayGet(arr, idx) => "array_get \{arr} \{idx}\n"
    ArrayPut(arr, idx, value) => "array_put \{arr} \{idx} \{value}\n"
  }
  pre_str + body_str
}
