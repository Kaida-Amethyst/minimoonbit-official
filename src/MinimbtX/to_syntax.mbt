type! UnimplementedSyntax String

// pub enum Syntax {
//   Unit
//   Bool(Bool) // true false
//   Int(Int) // int
//   Double(Double) // double
//   Var(String) // _
//   Tuple(Array[Syntax]) // (_, _, _)
//   Not(Syntax) // not(_)
//   Array(Syntax, Syntax) // Array::make(_, _)
//   Neg(Syntax, mut ~kind : Kind?) // -_
//   App(Syntax, Array[Syntax]) // _(_, _, _) or _()
//   Get(Syntax, Syntax) // _[_]
//   If(Syntax, Syntax, Syntax) // if _ { _ } else { _ }
//   Prim(Syntax, Syntax, Op, mut ~kind : Kind?) // _+ - * / _
//   Eq(Syntax, Syntax) // _==_
//   LE(Syntax, Syntax) // _<=_
//   Let((String, Type), Syntax, Syntax) // let _: _ = _; _
//   LetRec(Fundef, Syntax) // fn f() {} ; _
//   LetTuple(Array[(String, Type)], Syntax, Syntax) // let (_ , _) : (_, _)= _; _
//   Put(Syntax, Syntax, Syntax) // _[_] = _
// } derive(Show)
// pub struct Fundef {
//   name : (String, Type)
//   args : Array[(String, Type)]
//   body : Syntax
// } derive(Show)
/// ------------------------------------------------------------
/// TopDecl to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : TopDecl) -> @types.Syntax!Error {
  match self {
    FuncDef(func_def) => func_def.to_syntax!()
    _ => raise UnimplementedSyntax("Unimplemented top_decl to syntax")
  }
}

/// ------------------------------------------------------------
/// Function Definition to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : FuncDef) -> @types.Syntax!Error {
  let name = self.name.id
  let args = self.params.map(
    fn(param) {
      let id = param.ident.id
      let ty = match param.type_annote.to_syntax_type?() {
        Ok(ty) => ty
        Err(_) => {
          println("Unknown Error happened in `top_decl_func_def_to_syntax`")
          @types.Type::Unit
        }
      }
      return (id, ty)
    },
  )
  let ret_ty = self.ret_type.to_syntax_type!()
  let body = self.body.to_syntax!()
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
pub fn to_syntax(self : Closure) -> @types.Syntax!Error {
  let name = match self.name {
    Some(name) => name.id
    None => "__$anonymous"
  }
  let args = self.params.map(
    fn(param) {
      let id = param.ident.id
      let ty = match param.type_annote {
        Some(type_annote) =>
          match type_annote.to_syntax_type?() {
            Ok(ty) => ty
            Err(_) => {
              println("Unknown Error happened in `closure_to_syntax`")
              @types.Type::Unit
            }
          }
        None => @types.Type::Var(Ref::new(None))
      }
      return (id, ty)
    },
  )
  let ret_ty = match self.ret_type {
    Some(ret_type) =>
      match ret_type.to_syntax_type?() {
        Ok(ty) => ty
        Err(_) => {
          println("Unknown Error happened in `closure_to_syntax`")
          @types.Type::Unit
        }
      }
    None => @types.Type::Var(Ref::new(None))
  }
  let body = self.body.to_syntax!()
  let syntax_fundef = @types.Fundef::{ name: (name, ret_ty), args, body }
  return @types.LetRec(syntax_fundef, @types.Syntax::Unit)
}

/// ------------------------------------------------------------
/// Block Expr to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : BlockExpr) -> @types.Syntax!Error {
  if self.is_empty() {
    return @types.Syntax::Unit
  }
  let mut syntax = self.state_exprs[0].to_syntax!()
  let state_exprs = self.state_exprs[1:]
  loop syntax, state_exprs {
    _, [] => break
    @types.Syntax::Let(item, syn, _), state_exprs => {
      let state_syntax = state_exprs[0].to_syntax!()
      syntax = @types.Syntax::Let(item, syn, state_syntax)
      continue syntax, state_exprs[1:]
    }
    @types.Syntax::LetRec(fundef, _), state_exprs => {
      let state_syntax = state_exprs[0].to_syntax!()
      syntax = @types.Syntax::LetRec(fundef, state_syntax)
      continue syntax, state_exprs[1:]
    }
    @types.Syntax::LetTuple(items, syn, _), state_exprs => {
      let state_syntax = state_exprs[0].to_syntax!()
      syntax = @types.Syntax::LetTuple(items, syn, state_syntax)
      continue syntax, state_exprs[1:]
    }
    @types.Syntax::Put(syn1, syn2, _), state_exprs => {
      let state_syntax = state_exprs[0].to_syntax!()
      syntax = @types.Syntax::Put(syn1, syn2, state_syntax)
      continue syntax, state_exprs[1:]
    }
    _, _ => raise UnimplementedSyntax("Unimplemented block expr to syntax")
  }
  return syntax
}

/// ------------------------------------------------------------
/// State Expr to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : StateExpr) -> @types.Syntax!Error {
  match self {
    Let(let_expr) => let_expr.to_syntax!()
    Assign(assign_expr) => assign_expr.to_syntax!()
    Expr(expr) => expr.to_syntax!()
    Null(_) => @types.Syntax::Unit
    _ => raise UnimplementedSyntax("Unimplemented state_expr to syntax")
  }
}

/// ------------------------------------------------------------
/// If Expr to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : IfExpr) -> @types.Syntax!Error {
  let cond_syntax = self.cond_expr.to_syntax!()
  let then_syntax = self.then_expr.to_syntax!()
  let else_syntax = match self.else_clause {
    Some(else_clause) => else_clause.to_syntax!()
    None => @types.Syntax::Unit
  }
  return @types.If(cond_syntax, then_syntax, else_syntax)
}

pub fn to_syntax(self : ElseClause) -> @types.Syntax!Error {
  match self {
    Block(block_expr) => block_expr.to_syntax!()
    IfExpr(if_expr) => if_expr.to_syntax!()
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
pub fn to_syntax(self : AssignExpr) -> @types.Syntax!Error {
  let access_expr = match self.left {
    Access(access_expr) => access_expr
    _ => raise UnimplementedSyntax("Unimplemented assign left value")
  }
  let left_syntax = access_expr.to_syntax!()
  let right_syntax = self.right.to_syntax!()
  match self.assign_op.sym {
    "=" => @types.Put(left_syntax, right_syntax, @types.Syntax::Unit)
    _ => raise UnimplementedSyntax("Unimplemented assign operator")
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
pub fn to_syntax(self : LetExpr) -> @types.Syntax!Error {
  match self.type_annote {
    Some(_) => let_expr_to_syntax_with_type!(self)
    None => let_expr_to_syntax_without_type!(self)
  }
}

pub fn simple_syntax_type_from_atom_pat(
  self : AtomPat
) -> Array[(String, @types.Type)]!Error {
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
        let new_types = simple_syntax_type_from_atom_pat!(atom_pat)
        new_types.each(fn(elem) { types.push(elem) })
      }
      return types
    }
    _ => raise UnimplementedSyntax("Unimplemented atom pat to syntax type")
  }
}

pub fn let_expr_to_syntax_without_type(self : LetExpr) -> @types.Syntax!Error {
  let atom_pat = self.pat.atom_pat
  let pat_type_pairs = simple_syntax_type_from_atom_pat!(atom_pat)
  if pat_type_pairs.length() == 1 {
    let (id, ty) = pat_type_pairs[0]
    let expr_syntax = self.expr.to_syntax!()
    return @types.Let((id, ty), expr_syntax, @types.Syntax::Unit)
  } else {
    let expr_syntax = self.expr.to_syntax!()
    return @types.LetTuple(pat_type_pairs, expr_syntax, @types.Syntax::Unit)
  }
}

pub fn ident_type_to_syntax_type(id : String) -> @types.Type!Error {
  match id {
    "Int" => @types.Type::Int
    "Double" => @types.Type::Double
    "Bool" => @types.Type::Bool
    _ => raise UnimplementedSyntax("Unimplemented ident type")
  }
}

pub fn to_syntax_type(self : TypeExpr) -> @types.Type!Error {
  match self {
    IdentType(id) => ident_type_to_syntax_type!(id.id)
    TupleType(tuple_types) => {
      let types : Array[@types.Type] = []
      for type_expr in tuple_types.types {
        types.push(type_expr.to_syntax_type!())
      }
      return @types.Type::Tuple(types)
    }
  }
}

pub fn combine_atom_pat_type_expr(
  atom_pat : AtomPat,
  type_expr : TypeExpr
) -> Array[(String, @types.Type)]!Error {
  match (atom_pat, type_expr) {
    (Ident(ident), _) => {
      let id = ident.id
      let ty = type_expr.to_syntax_type!()
      return [(id, ty)]
    }
    (Tuple(tuple_pat), TupleType(tuple_types)) => {
      let types : Array[(String, @types.Type)] = []
      loop tuple_pat.as_pats[0:], tuple_types.types[0:] {
        [], [] => break
        pats, tys => {
          let pat = pats[0].atom_pat
          let ty = tys[0]
          let new_types = combine_atom_pat_type_expr!(pat, ty)
          new_types.each(fn(elem) { types.push(elem) })
          continue pats[1:], tys[1:]
        }
      }
      return types
    }
  }
}

pub fn let_expr_to_syntax_with_type(self : LetExpr) -> @types.Syntax!Error {
  let Some(type_annote) = self.type_annote
  let atom_pat = self.pat.atom_pat
  let pat_type_pairs = combine_atom_pat_type_expr!(atom_pat, type_annote)
  if pat_type_pairs.length() == 1 {
    let (id, ty) = pat_type_pairs[0]
    let expr_syntax = self.expr.to_syntax!()
    return @types.Let((id, ty), expr_syntax, @types.Syntax::Unit)
  } else {
    let expr_syntax = self.expr.to_syntax!()
    return @types.LetTuple(pat_type_pairs, expr_syntax, @types.Syntax::Unit)
  }
}

pub fn to_syntax(self : BinaryExpr) -> @types.Syntax!Error {
  let left_syntax = self.left.to_syntax!()
  let right_syntax = self.right.to_syntax!()
  // TODO: kind
  match self.op.sym {
    "+" => @types.Prim(left_syntax, right_syntax, @types.Add, kind=None)
    "*" => @types.Prim(left_syntax, right_syntax, @types.Mul, kind=None)
    "-" => @types.Prim(left_syntax, right_syntax, @types.Sub, kind=None)
    "/" => @types.Prim(left_syntax, right_syntax, @types.Div, kind=None)
    "==" => @types.Eq(left_syntax, right_syntax)
    "<=" => @types.LE(left_syntax, right_syntax)
    _ => raise UnimplementedSyntax("Unimplemented binary operator")
  }
}

pub fn to_syntax(self : UnaryExpr) -> @types.Syntax!Error {
  let expr_syntax = self.expr.to_syntax!()
  @types.Neg(expr_syntax, kind=None)
}

pub fn to_syntax(self : Expr) -> @types.Syntax!Error {
  match self {
    Apply(apply_expr) => apply_expr.to_syntax!()
    Binary(binary_expr) => binary_expr.to_syntax!()
    Block(block_expr) => block_expr.to_syntax!()
    // Match(match_expr) => match_expr.to_syntax()
    IfExpr(if_expr) => if_expr.to_syntax!()
    _ => raise UnimplementedSyntax("Unimplemented expr to syntax")
  }
}

pub fn to_syntax(self : TupleExpr) -> @types.Syntax!Error {
  let exprs_syntax : Array[@types.Syntax] = []
  for expr in self.exprs {
    exprs_syntax.push(expr.to_syntax!())
  }
  return @types.Syntax::Tuple(exprs_syntax)
}

/// ------------------------------------------------------------
/// AccessExpr to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : AccessExpr) -> @types.Syntax!Error {
  match self.accessor {
    TupleAcc(_) => raise UnimplementedSyntax("Tuple access")
    FieldAcc(_) => raise UnimplementedSyntax("Field access")
    ArrayAcc(expr) => {
      let apply_syntax = self.apply_expr.to_syntax!()
      let accessor_syntax = expr.to_syntax!()
      return @types.Get(apply_syntax, accessor_syntax)
    }
  }
}

/// ------------------------------------------------------------
/// ApplyExpr to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : ApplyExpr) -> @types.Syntax!Error {
  match self {
    Atomic(atom_expr) => atom_expr.to_syntax!()
    Access(access_expr) => access_expr.to_syntax!()
    CallExpr(call_expr) => call_expr.to_syntax!()
  }
}

/// ------------------------------------------------------------
/// CallExpr to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : CallExpr) -> @types.Syntax!Error {
  let apply_expr = self.apply_expr
  let args_syntax : Array[@types.Syntax] = []
  for arg in self.args {
    args_syntax.push(arg.to_syntax!())
  }
  if is_apply_expr_not(apply_expr) {
    if args_syntax.length() != 1 {
      raise UnimplementedSyntax("Not should have only one argument")
    }
    return @types.Not(args_syntax[0])
  }
  if is_apply_expr_array_make(apply_expr) {
    if args_syntax.length() != 2 {
      raise UnimplementedSyntax("Array::make should have two arguments")
    }
    return Array(args_syntax[0], args_syntax[1])
  }
  let apply_syntax = apply_expr.to_syntax!()
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
pub fn to_syntax(self : AtomExpr) -> @types.Syntax!Error {
  match self {
    Literal(literal) => literal.to_syntax!()
    Ident(ident) => Var(ident.id)
    TupleExpr(tuple_expr) => tuple_expr.to_syntax!()
    UnaryExpr(unary_expr) => unary_expr.to_syntax!()
    Paren(expr) => expr.to_syntax!()
    Closure(closure) => closure.to_syntax!()
    _ => raise UnimplementedSyntax("Unimplemented atom_expr to syntax")
  }
}

/// ------------------------------------------------------------
/// Literal to Syntax
/// ------------------------------------------------------------
pub fn to_syntax(self : Literal) -> @types.Syntax!Error {
  match self.category {
    Boolean(b) => Bool(b)
    Integer(i) => Int(i)
    Double(d) => Double(d)
    _ => raise UnimplementedSyntax("Unimplemented literal to syntax")
  }
}
