/// ------------------------------------------------------------
/// Union Syntax
/// ------------------------------------------------------------
pub fn union_syntax(syntaxs: Array[@types.Syntax]) -> @types.Syntax {
  let mut res = @types.Syntax::Unit
  for syn in syntaxs {
    fn concat(init, next) {
      match (init, next) {
        (@types.Syntax::Unit, _) => next
        (@types.Syntax::Let(item, syn, next_syn), _) => {
          @types.Syntax::Let(item, syn, concat(next_syn, next))
        }
        (@types.Syntax::LetRec(fundef, next_syn), _) => {
          @types.Syntax::LetRec(fundef, concat(next_syn, next))
        }
        (@types.Syntax::LetTuple(items, syn, next_syn), _) => {
          @types.Syntax::LetTuple(items, syn, concat(next_syn, next))
        }
        (@types.Syntax::Put(syn1, syn2, next_syn), _) => {
          @types.Syntax::Put(syn1, syn2, concat(next_syn, next))
        }
        _ => {
          println("Should not have other union syntax")
          panic()
        }
      }
    }
    res = concat(res, syn)
  }
  res
}


/// ------------------------------------------------------------
/// TopDecl to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : TopDecl) -> @types.Syntax {
  match self {
    FuncDef(func_def) => func_def.to_syntax()
    MainFunc(main_func) => main_func.to_syntax()
    _ => {
      println("Unimplemented top_decl to syntax")
      panic()
    }
  }
}

/// ------------------------------------------------------------
/// Function Definition to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : FuncDef) -> @types.Syntax {
  let name = self.name.id
  let args = self.params.map(
    fn(param) {
      let id = param.ident.id
      let ty = param.type_annote.to_syntax_type()
      return (id, ty)
    },
  )
  let ret_ty = self.ret_type.to_syntax_type()
  let body = self.body.to_syntax()
  let syntax_fundef = @types.Fundef::{ name: (name, ret_ty), args, body }
  return @types.LetRec(syntax_fundef, @types.Syntax::Unit)
}

/// ------------------------------------------------------------
/// Main Func to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self: MainFunc) -> @types.Syntax {
  let name = "main"
  let args = []
  let ret_ty = @types.Type::Unit
  let body = self.body.to_syntax()
  let syntax_fundef = @types.Fundef::{ name: (name, ret_ty), args, body }
  return @types.LetRec(syntax_fundef, @types.Syntax::Unit)
}

/// ------------------------------------------------------------
/// Closure to Syntax
///
/// ## Note
///
/// Same as Function Definition, closure also convert to `LetRec`
/// if name is None, name will be `__$anonymous`
/// For all type expr, if it's None, convert to `Var(Ref[None])`
/// ------------------------------------------------------------
pub fn to_syntax(self : Closure) -> @types.Syntax {
  let name = match self.name {
    Some(name) => name.id
    None => "__$anonymous"
  }
  let args = self.params.map(
    fn(param) {
      let id = param.ident.id
      let ty = match param.type_annote {
        Some(type_annote) => type_annote.to_syntax_type()
        None => @types.Type::Var(Ref::new(None))
      }
      return (id, ty)
    },
  )
  let ret_ty = match self.ret_type {
    Some(ret_type) => ret_type.to_syntax_type()
    None => @types.Type::Var(Ref::new(None))
  }
  let body = self.body.to_syntax()
  let syntax_fundef = @types.Fundef::{ name: (name, ret_ty), args, body }
  return @types.LetRec(syntax_fundef, @types.Syntax::Unit)
}

/// ------------------------------------------------------------
/// Block Expr to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : BlockExpr) -> @types.Syntax {
  if self.is_empty() {
    return @types.Syntax::Unit
  }
  let mut syntax = self.state_exprs[0].to_syntax()
  let state_exprs = self.state_exprs[1:]
  loop syntax, state_exprs {
    _, [] => break
    @types.Syntax::Let(item, syn, _), state_exprs => {
      let state_syntax = state_exprs[0].to_syntax()
      syntax = @types.Syntax::Let(item, syn, state_syntax)
      continue syntax, state_exprs[1:]
    }
    @types.Syntax::LetRec(fundef, _), state_exprs => {
      let state_syntax = state_exprs[0].to_syntax()
      syntax = @types.Syntax::LetRec(fundef, state_syntax)
      continue syntax, state_exprs[1:]
    }
    @types.Syntax::LetTuple(items, syn, _), state_exprs => {
      let state_syntax = state_exprs[0].to_syntax()
      syntax = @types.Syntax::LetTuple(items, syn, state_syntax)
      continue syntax, state_exprs[1:]
    }
    @types.Syntax::Put(syn1, syn2, _), state_exprs => {
      let state_syntax = state_exprs[0].to_syntax()
      syntax = @types.Syntax::Put(syn1, syn2, state_syntax)
      continue syntax, state_exprs[1:]
    }
    _, _ => {
      println("Unimplemented block expr to syntax")
      panic()
    }
  }
  return syntax
}

/// ------------------------------------------------------------
/// State Expr to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : StateExpr) -> @types.Syntax {
  match self {
    Let(let_expr) => let_expr.to_syntax()
    Assign(assign_expr) => assign_expr.to_syntax()
    Expr(expr) => expr.to_syntax()
    Null(_) => @types.Syntax::Unit
    _ => {
      println("Unimplemented state expr to syntax")
      panic()
    }
  }
}

/// ------------------------------------------------------------
/// If Expr to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : IfExpr) -> @types.Syntax {
  let cond_syntax = self.cond_expr.to_syntax()
  let then_syntax = self.then_expr.to_syntax()
  let else_syntax = match self.else_clause {
    Some(else_clause) => else_clause.to_syntax()
    None => @types.Syntax::Unit
  }
  return @types.If(cond_syntax, then_syntax, else_syntax)
}

pub fn to_syntax(self : ElseClause) -> @types.Syntax {
  match self {
    Block(block_expr) => block_expr.to_syntax()
    IfExpr(if_expr) => if_expr.to_syntax()
  }
}

/// ------------------------------------------------------------
/// Assign Expr to Syntax
///
/// ## Note
///
/// Notice that for this syntax, assign's left value must be
/// a `Get`, not a `Var`
///
/// For example:
///
/// - `a = 1` is not allowed
/// - `a[0] = 1` ok
///
/// ## TODO
///
/// - Support +=, -=, *=, /=
///
/// ------------------------------------------------------------
pub fn to_syntax(self : AssignExpr) -> @types.Syntax {
  let access_expr = match self.left {
    Access(access_expr) => access_expr
    _ => {
      println("Unimplemented assign left value")
      panic()
    }
  }
  let left_syntax = access_expr.to_syntax()
  let right_syntax = self.right.to_syntax()
  match self.assign_op.sym {
    "=" => @types.Put(left_syntax, right_syntax, @types.Syntax::Unit)
    _ => {
      println("Unimplemented assign operator")
      panic()
    }
  }
}

/// ------------------------------------------------------------
/// Let Expr to Syntax
///
/// ## Note
///
/// - Case 1: No type annotation
///   ```
///   let x = 1;
///   let t = 1.0;
///   ```
///
/// - Case 2: With type annotation
///   ```
///   let x: Int = 1;
///   let (x, y) : (Int, Int) = (1, 2);
///   ```
/// ------------------------------------------------------------
pub fn to_syntax(self : LetExpr) -> @types.Syntax {
  let type_checked = match self.expr.meta().query_type() {
    Undefine => false
    _ => true
  }
  if type_checked {
    return let_expr_to_syntax_with_type(self)
  }
  match self.type_annote {
    Some(_) => let_expr_to_syntax_with_type(self)
    None => let_expr_to_syntax_without_type(self)
  }
}

pub fn simple_syntax_type_from_atom_pat(
  self : AtomPat
) -> Array[(String, @types.Type)] {
  match self {
    Ident(ident) => {
      let id = ident.id
      let ty : Ref[@types.Type?] = Ref::new(None)
      let ty = @types.Type::Var(ty)
      return [(id, ty)]
    }
    Tuple(tuple_pat) => {
      let types : Array[(String, @types.Type)] = []
      for as_pat in tuple_pat {
        let atom_pat = as_pat.atom_pat
        let new_types = simple_syntax_type_from_atom_pat(atom_pat)
        new_types.each(fn(elem) { types.push(elem) })
      }
      return types
    }
    _ => {
      println("Unimplemented atom pat to syntax type")
      panic()
    }
  }
}

pub fn let_expr_to_syntax_without_type(self : LetExpr) -> @types.Syntax {
  let atom_pat = self.pat.atom_pat
  let pat_type_pairs = simple_syntax_type_from_atom_pat(atom_pat)
  if pat_type_pairs.length() == 1 {
    let (id, ty) = pat_type_pairs[0]
    let expr_syntax = self.expr.to_syntax()
    return @types.Let((id, ty), expr_syntax, @types.Syntax::Unit)
  } else {
    let expr_syntax = self.expr.to_syntax()
    return @types.LetTuple(pat_type_pairs, expr_syntax, @types.Syntax::Unit)
  }
}

pub fn ident_type_to_syntax_type(id : String) -> @types.Type {
  match id {
    "Int" => @types.Type::Int
    "Double" => @types.Type::Double
    "Bool" => @types.Type::Bool
    _ => {
      println("Unimplemented ident type to syntax type")
      panic()
    }
  }
}

pub fn to_syntax_type(self : TypeExpr) -> @types.Type {
  match self {
    IdentType(id) => ident_type_to_syntax_type(id.id)
    TupleType(tuple_types) => {
      let types : Array[@types.Type] = []
      for type_expr in tuple_types.types {
        types.push(type_expr.to_syntax_type())
      }
      return @types.Type::Tuple(types)
    }
    _ => {
      println("Unimplemented type expr to syntax type")
      panic()
    }
  }
}

pub fn combine_atom_pat_type_expr(
  atom_pat : AtomPat,
  ty : @types.Type
) -> Array[(String, @types.Type)] {
  match (atom_pat, ty) {
    (Ident(ident), _) => {
      let id = ident.id
      return [(id, ty)]
    }
    (Tuple(tuple_pat), Tuple(tuple_types)) => {
      let types : Array[(String, @types.Type)] = []
      loop tuple_pat.as_pats[0:], tuple_types[0:] {
        [], [] => break
        pats, tys => {
          let pat = pats[0].atom_pat
          let ty = tys[0]
          let new_types = combine_atom_pat_type_expr(pat, ty)
          new_types.each(fn(elem) { types.push(elem) })
          continue pats[1:], tys[1:]
        }
      }
      return types
    }
    _ => {
      println("Unimplemented combine atom pat type expr")
      panic()
    }
  }
}

pub fn let_expr_to_syntax_with_type(self : LetExpr) -> @types.Syntax {
  // guard let Some(type_annote) = self.type_annote
  let ty = match self.expr.meta().query_type() {
    Undefine => {
      guard let Some(type_annote) = self.type_annote;
      type_annote.to_syntax_type()
    }
    _ => self.expr.meta().query_type().to_syntax_type()
  }
  let atom_pat = self.pat.atom_pat
  let pat_type_pairs = combine_atom_pat_type_expr(atom_pat, ty)
  if pat_type_pairs.length() == 1 {
    let (id, ty) = pat_type_pairs[0]
    let expr_syntax = self.expr.to_syntax()
    return @types.Let((id, ty), expr_syntax, @types.Syntax::Unit)
  } else {
    let expr_syntax = self.expr.to_syntax()
    return @types.LetTuple(pat_type_pairs, expr_syntax, @types.Syntax::Unit)
  }
}

pub fn to_syntax(self : BinaryExpr) -> @types.Syntax {
  let left_syntax = self.left.to_syntax()
  let right_syntax = self.right.to_syntax()
  // TODO: kind
  let ty = self.meta().query_type()
  let kind = match ty {
    Undefine => None
    Int32 => Some(@types.Kind::Int)
    Double => Some(@types.Kind::Double)
    Boolean => Some(@types.Kind::Int)  // it will not be used
    _ => {
      println("Unsupported type: loc: BinaryExpr, type is \{ty}")
      panic()
    }
  }
  match self.op.sym {
    "+" => @types.Prim(left_syntax, right_syntax, @types.Add, ~kind)
    "*" => @types.Prim(left_syntax, right_syntax, @types.Mul, ~kind)
    "-" => @types.Prim(left_syntax, right_syntax, @types.Sub, ~kind)
    "/" => @types.Prim(left_syntax, right_syntax, @types.Div, ~kind)
    "==" => @types.Eq(left_syntax, right_syntax)
    "<=" => @types.LE(left_syntax, right_syntax)
    _ => {
      println("Unimplemented binary operator")
      panic()
    }
  }
}

pub fn to_syntax(self : UnaryExpr) -> @types.Syntax {
  let expr_syntax = self.expr.to_syntax()
  let kind = match self.meta().query_type() {
    Undefine => None
    Int32 => Some(@types.Kind::Int)
    Double => Some(@types.Kind::Double)
    _ => {
      println("Unsupported type: loc: UnaryExpr")
      panic()
    }
  }
  @types.Neg(expr_syntax, ~kind)
}

pub fn to_syntax(self : Expr) -> @types.Syntax {
  match self {
    Apply(apply_expr) => apply_expr.to_syntax()
    Binary(binary_expr) => binary_expr.to_syntax()
    Block(block_expr) => block_expr.to_syntax()
    IfExpr(if_expr) => if_expr.to_syntax()
    _ => {
      println("Unimplemented expr to syntax")
      panic()
    }
  }
}

pub fn to_syntax(self : TupleExpr) -> @types.Syntax{
  let exprs_syntax : Array[@types.Syntax] = []
  for expr in self.exprs {
    exprs_syntax.push(expr.to_syntax())
  }
  return @types.Syntax::Tuple(exprs_syntax)
}

/// ------------------------------------------------------------
/// AccessExpr to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : AccessExpr) -> @types.Syntax {
  match self.accessor {
    TupleAcc(_) | FieldAcc(_) => {
      println("Unsupport tuple or field accessor")
      panic()
    }
    ArrayAcc(expr) => {
      let apply_syntax = self.apply_expr.to_syntax()
      let accessor_syntax = expr.to_syntax()
      return @types.Get(apply_syntax, accessor_syntax)
    }
  }
}

/// ------------------------------------------------------------
/// ApplyExpr to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : ApplyExpr) -> @types.Syntax {
  match self {
    Atomic(atom_expr) => atom_expr.to_syntax()
    Access(access_expr) => access_expr.to_syntax()
    CallExpr(call_expr) => call_expr.to_syntax()
  }
}

/// ------------------------------------------------------------
/// CallExpr to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : CallExpr) -> @types.Syntax {
  let apply_expr = self.apply_expr
  let args_syntax : Array[@types.Syntax] = []
  for arg in self.args {
    args_syntax.push(arg.to_syntax())
  }
  if is_apply_expr_not(apply_expr) {
    if args_syntax.length() != 1 {
      println("Not should have only one argument")
      panic()
    }
    return @types.Not(args_syntax[0])
  }
  if is_apply_expr_array_make(apply_expr) {
    if args_syntax.length() != 2 {
      println("Array::make should have two arguments")
      panic()
    }
    return Array(args_syntax[0], args_syntax[1])
  }
  let apply_syntax = apply_expr.to_syntax()
  return @types.App(apply_syntax, args_syntax)
}

pub fn is_apply_expr_not(expr : ApplyExpr) -> Bool {
  match expr {
    Atomic(atom_expr) =>
      match atom_expr {
        Ident(ident) => ident.id == "not"
        _ => false
      }
    _ => false
  }
}

pub fn is_apply_expr_array_make(expr : ApplyExpr) -> Bool {
  match expr {
    Atomic(atom_expr) =>
      match atom_expr {
        Method(method_expr) =>
          method_expr.scope.id == "Array" && method_expr.ident.id == "make"
        _ => false
      }
    _ => false
  }
}

/// ------------------------------------------------------------
/// AtomExpr to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : AtomExpr) -> @types.Syntax {
  match self {
    Literal(literal) => literal.to_syntax()
    Ident(ident) => Var(ident.id)
    TupleExpr(tuple_expr) => tuple_expr.to_syntax()
    UnaryExpr(unary_expr) => unary_expr.to_syntax()
    Paren(expr) => expr.to_syntax()
    Closure(closure) => closure.to_syntax()
    _ => {
      println("Unimplemented atom_expr to syntax")
      panic()
    }
  }
}

/// ------------------------------------------------------------
/// Literal to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : Literal) -> @types.Syntax {
  match self.category {
    Boolean(b) => Bool(b)
    Integer(i) => Int(i)
    Double(d) => Double(d)
    _ => {
      println("Unimplemented literal to syntax")
      panic()
    }
  }
}

/// ------------------------------------------------------------
/// Type to Syntax Type
/// ------------------------------------------------------------
// pub enum Type {
//   Unit
//   Bool
//   Int
//   Double
//   Fun(Array[Type], Type) // (_, _, _) -> _
//   Tuple(Array[Type]) // (_, _, _)
//   Array(Type) // Array[_]
//   Var(Ref[Type?])
//   Ptr
// } derive(Show)
// pub enum Type {
//   Undefine
//   Unit
//   Int32
//   Int64
//   UInt32
//   UInt64
//   Float32
//   Double
//   Boolean
//   String
//   Char
//   TupleTy(TupleTy)
//   CompTy(CompTy)
//   EnumTy(EnumTy)
//   StruTy(StruTy)
//   FuncTy(FuncTy)
//   ReasoningTy(ReasoningTy)
//   TempTy(TempTy)
// }
pub fn to_syntax_type(self: Type) -> @types.Type {
  match self {
    Unit => @types.Type::Unit
    Int32 => @types.Type::Int
    UInt32 => @types.Type::Int
    Double => @types.Type::Double
    Boolean => @types.Type::Bool
    TupleTy(tup) => tup.to_syntax_type()
    _ => {
      println("Unsupported type convert to syntax type")
      panic()
    }
  }
}

pub fn to_syntax_type(self: TupleTy) -> @types.Type {
  let types : Array[@types.Type] = []
  for ty in self.types {
    types.push(ty.to_syntax_type())
  }
  return @types.Type::Tuple(types)
}
