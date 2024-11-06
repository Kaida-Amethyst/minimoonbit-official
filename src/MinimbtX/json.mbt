pub fn to_json(self : TempTy) -> Json {
  { "id": self.id.to_json(), "constraints": self.constraints.to_json() }
}

pub fn to_json(self : ReasoningTy) -> Json {
  "Reasoning".to_json()
}

pub fn to_json(self : Constraint) -> Json {
  self.name.to_json()
}

pub fn to_json(self : Type) -> Json {
  match self {
    Undefine => "Undefine".to_json()
    Unit => "Unit".to_json()
    UInt32 => "UInt32".to_json()
    UInt64 => "UInt64".to_json()
    Int32 => "Int32".to_json()
    Int64 => "Int64".to_json()
    Float32 => "Float32".to_json()
    Double => "Double".to_json()
    Boolean => "Boolean".to_json()
    String => "String".to_json()
    Char => "Char".to_json()
    // TupleTy(tupty) => tupty.to_json()
    TempTy(temp_ty) => temp_ty.to_json()
    _ => "Unimplemented".to_json()
  }
}

// pub struct Program {
//   top_decls : Array[TopDecl]
// }
pub fn to_json(self: Program) -> Json {
   self.top_decls.to_json()
}

pub fn to_json(self : TopDecl) -> Json {
  match self {
    FuncDef(f) => f.to_json()
    MainFunc(m) => m.to_json()
    LetExpr(l) => l.to_json()
    StructDef(s) => s.to_json()
    _ => "Unimplemented".to_json()
  }
}

pub fn to_json(self : Expr) -> Json {
  match self {
    Apply(apply) => { "apply_expression": apply.to_json() }
    Binary(binary) => { "binary_expression": binary.to_json() }
    Match(m) => { "match_expression": m.to_json() }
    IfExpr(i) => { "if_expression": i.to_json() }
    Block(b) => { "block_expression": b.to_json() }
    _ => "Unimplemented".to_json()
  }
}

pub fn to_json(self : BinaryExpr) -> Json {
  {
    "operator": self.op.to_json(),
    "left": self.left.to_json(),
    "right": self.right.to_json(),
    "type": self.meta().analysis.ast_type.to_json(),
  }
}

pub fn to_json(self : ApplyExpr) -> Json {
  match self {
    Atomic(a) => a.to_json()
    Access(a) => { "access": a.to_json() }
    CallExpr(a) => { "func_call": a.to_json() }
    _ => "Unimplemented".to_json()
  }
}

pub fn to_json(self : AccessExpr) -> Json {
  {
    "apply_expr": self.apply_expr.to_json(),
    "accessor": self.accessor.to_json(),
  }
}

pub fn to_json(self : Accessor) -> Json {
  match self {
    TupleAcc(l) => { "tuple_accessor": l.to_json() }
    FieldAcc(l) => { "field_accessor": l.to_json() }
    ArrayAcc(i) => { "index_accessor": i.to_json() }
  }
}

pub fn to_json(self : CallExpr) -> Json {
  { "func": self.apply_expr.to_json(), "args": self.args.to_json() }
}

pub fn to_json(self : AtomExpr) -> Json {
  match self {
    Literal(l) => l.to_json()
    Ident(id) => id.to_json()
    TupleExpr(t) => { "tuple": t.to_json() }
    ArrayExpr(a) => { "array": a.to_json() }
    Method(m) => m.to_json()
    Closure(c) => { "closure": c.to_json() }
    Paren(e) => e.to_json()
    _ => "Unimplemented".to_json()
  }
}

pub fn to_json(self : TupleExpr) -> Json {
  { "tuple_expr": self.exprs.to_json() }
}

pub fn to_json(self : ArrayExpr) -> Json {
  { "array_expr": self.exprs.to_json() }
}

pub fn to_json(self : LowerCase) -> Json {
  self.tok.get_token_content().to_json()
}

pub fn to_json(self : MethodExpr) -> Json {
  { "scope": self.scope.to_json(), "ident": self.ident.to_json() }
}

pub fn to_json(self : UpperCase) -> Json {
  self.tok.get_token_content().to_json()
}

pub fn to_json(self : Operator) -> Json {
  let tok = self.tok
  tok.get_token_content().to_json()
}

pub fn to_json(self : Literal) -> Json {
  let str = match self.category {
    Boolean(b) => "Boolean(" + b.to_string() + ")"
    Double(d) => "Double(" + d.to_string() + ")"
    Integer(i) => "Integer(" + i.to_string() + ")"
    Char(c) => "Char(" + c.to_string() + ")"
    String(s) => "String(" + s + ")"
  }
  str.to_json()
}

// Pattern
pub fn to_json(self : Pattern) -> Json {
  match self {
    AsPat(as_pat) => as_pat.to_json()
    _ => "Unimplemented".to_json()
  }
}

pub fn to_json(self : AsPat) -> Json {
  match self.as_ident {
    Some(as_id) => { "pat": self.atom_pat.to_json(), "as": as_id.to_json() }
    None => self.atom_pat.to_json()
  }
}

pub fn to_json(self : AtomPat) -> Json {
  match self {
    WildCard(_) => "WildCard(_)".to_json()
    Ident(id) => id.to_json()
    Literal(l) => l.to_json()
    Tuple(t) => t.to_json()
    _ => "Unimplemented".to_json()
  }
}

pub fn to_json(self : TuplePat) -> Json {
  { "pats": self.as_pats.to_json() }
}

// TypeExpr
pub fn to_json(self : TypeExpr) -> Json {
  match self {
    IdentType(i) => i.to_json()
    ApplyType(a) => a.to_json()
    TupleType(t) => t.to_json()
    FuncType(f) => f.to_json()
    _ => "Unimplemented".to_json()
  }
}

pub fn to_json(self : TupleType) -> Json {
  { "types": self.types.to_json() }
}

pub fn to_json(self : ApplyType) -> Json {
  {
    "type_name": self.type_name.to_json(),
    "type_arg": self.type_args.to_json(),
  }
}

pub fn to_json(self : FuncType) -> Json {
  { "arg_type": self.arg_type.to_json(), "ret_type": self.ret_type.to_json() }
}

// Statement
pub fn to_json(self : LetExpr) -> Json {
  match self.type_annote {
    Some(t) =>
      {
        "pat": self.pat.to_json(),
        "expr": self.expr.to_json(),
        "type": t.to_json(),
      }
    None => { "pat": self.pat.to_json(), "expr": self.expr.to_json() }
  }
}

pub fn to_json(self : AssignExpr) -> Json {
  {
    "left": self.left.to_json(),
    "assign_op": self.assign_op.to_json(),
    "right": self.right.to_json(),
  }
}

pub fn to_json(self : StateExpr) -> Json {
  match self {
    Let(l) => { "let_expression": l.to_json() }
    Assign(assign) =>
      {
        "left": assign.left.to_json(),
        "assign_op": assign.assign_op.to_json(),
        "right": assign.right.to_json(),
      }
    While(w) => w.to_json()
    Break(_) => "break_statement".to_json()
    Continue(_) => "continue_statement".to_json()
    Return(r) =>
      match r.expr {
        Some(expr) => { "return_statement": expr.to_json() }
        None => "return_statement".to_json()
      }
    Expr(e) => e.to_json()
    Null(_) => "null_statement".to_json()
    _ => "Unimplemented".to_json()
  }
}

pub fn to_json(self : BlockExpr) -> Json {
  {
    "statements": self.state_exprs.to_json(),
    "type": self.meta().analysis.ast_type.to_json(),
  }
}

pub fn to_json(self : MatchExpr) -> Json {
  { "expr": self.expr.to_json(), "arms": self.arms.to_json() }
}

pub fn to_json(self : MatchArm) -> Json {
  match self.match_guard {
    Some(_) =>
      {
        "pat": self.pat.to_json(),
        "guard": self.match_guard.to_json(),
        "expr": self.expr.to_json(),
      }
    None => { "pat": self.pat.to_json(), "expr": self.expr.to_json() }
  }
}

pub fn to_json(self : IfExpr) -> Json {
  match self.else_clause {
    Some(else_clause) =>
      {
        "cond_expr": self.cond_expr.to_json(),
        "then_expr": self.then_expr.to_json(),
        "else_clause": else_clause.to_json(),
      }
    None =>
      {
        "cond_expr": self.cond_expr.to_json(),
        "then_expr": self.then_expr.to_json(),
      }
  }
}

pub fn to_json(self : ElseClause) -> Json {
  match self {
    Block(b) => b.to_json()
    IfExpr(i) => i.to_json()
  }
}

pub fn to_json(self : WhileExpr) -> Json {
  { "cond": self.cond_expr.to_json(), "body": self.body.to_json() }
}

pub fn to_json(self : ClosureParam) -> Json {
  {
    "ident": self.ident.to_json(),
    "type": match self.type_annote {
      Some(t) => t.to_json()
      None => "Not marked".to_json()
    },
  }
}

pub fn to_json(self : Closure) -> Json {
  {
    "name": match self.name {
      Some(name) => name.to_json()
      None => "anonymous".to_json()
    },
    "params": self.params.to_json(),
    "ret_type": match self.ret_type {
      Some(t) => t.to_json()
      None => "Not marked".to_json()
    },
    "body": self.body.to_json(),
  }
}

pub fn to_json(self : FuncParam) -> Json {
  { "ident": self.ident.to_json(), "type": self.type_annote.to_json() }
}

pub fn to_json(self : FuncDef) -> Json {
  {
    "name": self.name.to_json(),
    "params": self.params.to_json(),
    "ret_type": self.ret_type.to_json(),
    "body": self.body.to_json(),
  }
}

pub fn to_json(self : StructDef) -> Json {
  { "name": self.name.to_json(), "fields": self.fields.to_json() }
}

pub fn to_json(self : StructField) -> Json {
  { "name": self.name.to_json(), "type": self.type_annote.to_json() }
}

pub fn to_json(self : MainFunc) -> Json {
  { "body": self.body.to_json() }
}
