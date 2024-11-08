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
    @types.Syntax::Put(_, _, _), state_exprs => {
      let state_syntax = state_exprs[0].to_syntax()
      syntax = @types.Syntax::Let(("_", @types.Type::Unit), syntax, state_syntax)
      continue syntax, state_exprs[1:]
    }
    _, _ => {
      println("Unimplemented block expr to syntax \{syntax}")
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
    WildCard(_) => {
      Array::new()
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
    "Unit" => @types.Type::Unit
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
      @types.Type::Tuple(types)
    }
    ApplyType(appty) => {
      let type_arg = appty.type_args[0].to_syntax_type()
      @types.Type::Array(type_arg) // no other possibility for now
    }
    FuncType(fty) => {
      let arg_ty = fty.arg_type.to_syntax_type()
      let ret_ty = fty.ret_type.to_syntax_type()
      match arg_ty {
        Tuple(tup) => @types.Type::Fun(tup, ret_ty)
        _ => @types.Type::Fun([arg_ty], ret_ty)
      }
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
    (WildCard(_), _) => {
      Array::new()
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
    return @types.App(apply_expr.to_syntax(), args_syntax)
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
  let re = match expr {
    Atomic(atom_expr) =>
      match atom_expr {
        Method(method_expr) => {
          method_expr.ident.id == "Array::make"
        }
        _ => false
      }
    _ => false
  }
  re
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
    Method(m) => @types.Syntax::Var(m.ident.id)
    Unit(_) => @types.Syntax::Unit
    _ => {
      println("Unimplemented atom_expr to syntax\{self.stringify(0)}")
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
    CompTy(comp) => {
      let subTy = comp.subs[0].to_syntax_type()
      // no other possibility for now
      @types.Type::Array(subTy)
    }
    FuncTy(fty) => {
      let args = fty.param_types.iter().map(fn(arg) { arg.to_syntax_type() }).to_array()
      let ret = fty.ret_type.to_syntax_type()
      @types.Type::Fun(args, ret)
    }
    _ => {
      println("Unsupported type convert to syntax type \{self}")
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

/// ----------------------------------------------------
/// MibitX.knf to Syntax.knf
/// ----------------------------------------------------
fn to_syntax_name(self: Name) -> @types.Name {
  let name = match self.mark {
    "" => None
    _ => Some(self.mark)
  }
  let slot = match self.slot {
    None => 0
    Some(i) => i
  }
  @types.Name::{ name, slot }
}

pub fn knf_conversion(knf: Knf) -> @knf.Knf {
  let type_recorder : Map[@types.Name, @types.Type] = Map::new()
  fn to_syntax_knf(knf: Knf) -> @knf.Knf {
    match knf {
      Unit => Unit
      Int(i) => Int(i)
      Double(d) => Double(d)
      Var(name) => Var(to_syntax_name(name))
      Neg(operand) => {
        let operand = to_syntax_name(operand)
        let ty = type_recorder.get(operand).unwrap()
        match ty {
          @types.Type::Int => Neg(operand)
          @types.Type::Double => FNeg(operand)
          _ => {
            println("Unsupported type \{ty} for Neg")
            panic()
          }
        }
      }
      Add(lhs, rhs) => {
        let lhs = to_syntax_name(lhs)
        let rhs = to_syntax_name(rhs)
        let ty = type_recorder.get(lhs).unwrap()
        match ty {
          @types.Type::Int => Add(lhs, rhs)
          @types.Type::Double => FAdd(lhs, rhs)
          _ => {
            println("Unsupported type \{ty} for Add")
            panic()
          }
        }
      }
      Sub(lhs, rhs) => {
        let lhs = to_syntax_name(lhs)
        let rhs = to_syntax_name(rhs)
        let ty = type_recorder.get(lhs).unwrap()
        match ty {
          @types.Type::Int => Sub(lhs, rhs)
          @types.Type::Double => FSub(lhs, rhs)
          _ => {
            println("Unsupported type \{ty} for Sub")
            panic()
          }
        }
      }
      Mul(lhs, rhs) => {
        let lhs = to_syntax_name(lhs)
        let rhs = to_syntax_name(rhs)
        let ty = type_recorder.get(lhs).unwrap()
        match ty {
          @types.Type::Int => Mul(lhs, rhs)
          @types.Type::Double => FMul(lhs, rhs)
          _ => {
            println("Unsupported type \{ty} for Mul")
            panic()
          }
        }
      }
      Div(lhs, rhs) => {
        let lhs = to_syntax_name(lhs)
        let rhs = to_syntax_name(rhs)
        let ty = type_recorder.get(lhs).unwrap()
        match ty {
          @types.Type::Int => Div(lhs, rhs)
          @types.Type::Double => FDiv(lhs, rhs)
          _ => {
            println("Unsupported type \{ty} for Div")
            panic()
          }
        }
      }
      Beq(lhs, rhs, then, elec) => {
        let lhs = to_syntax_name(lhs)
        let rhs = to_syntax_name(rhs)
        let then = to_syntax_knf(then)
        let elec = to_syntax_knf(elec)
        IfEq(lhs, rhs, then, elec)
      }
      Bne(lhs, rhs, then, elec) => {
        let lhs = to_syntax_name(lhs)
        let rhs = to_syntax_name(rhs)
        let then = to_syntax_knf(then)
        let elec = to_syntax_knf(elec)
        IfEq(lhs, rhs, elec, then)
      }
      Ble(lhs, rhs, then, elec) => {
        let lhs = to_syntax_name(lhs)
        let rhs = to_syntax_name(rhs)
        let then = to_syntax_knf(then)
        let elec = to_syntax_knf(elec)
        IfLe(lhs, rhs, then, elec)
      }
      Bge(lhs, rhs, then, elec) => {
        let lhs = to_syntax_name(lhs)
        let rhs = to_syntax_name(rhs)
        let then = to_syntax_knf(then)
        let elec = to_syntax_knf(elec)
        IfLe(rhs, lhs, then, elec)
      }
      Bgt(lhs, rhs, then, elec) => {
        let lhs = to_syntax_name(lhs)
        let rhs = to_syntax_name(rhs)
        let then = to_syntax_knf(then)
        let elec = to_syntax_knf(elec)
        IfLe(lhs, rhs, elec, then)
      }
      Blt(lhs, rhs, then, elec) => {
        let lhs = to_syntax_name(lhs)
        let rhs = to_syntax_name(rhs)
        let then = to_syntax_knf(then)
        let elec = to_syntax_knf(elec)
        IfLe(rhs, lhs, elec, then)
      }
      Let((name, ty), body, next) => {
        let name = to_syntax_name(name)
        let ty = ty.to_syntax_type()
        type_recorder.set(name, ty)
        let body = to_syntax_knf(body)
        let next = to_syntax_knf(next)
        Let((name, ty), body, next)
      }
      Tuple(names) => {
        let names = names.map(fn(name) { to_syntax_name(name) })
        Tuple(names)
      }
      LetTuple(items, name, body) => {
        let items = items.map(fn(pair) {
          let (name, ty) = pair
          let name = to_syntax_name(name)
          let ty = ty.to_syntax_type()
          type_recorder.set(name, ty)
          (name, ty)
        })
        let name = to_syntax_name(name)
        let body = to_syntax_knf(body)
        LetTuple(items, name, body)
      }
      Apply(name, args) => {
        let fname = name.to_string()
        let callee = if fname.starts_with("__builtin_array_make") {
          let ty = type_recorder.get(args[1].to_syntax_name())
          let callee = match ty {
            Some(@types.Type::Int) => "minimbt_" + "create_array"
            Some(@types.Type::Double) => "minimbt_" + "create_float_array"
            Some(_) => "minimbt_" + "create_ptr_array"
            None => {
              println("knf 2 knf Apply array make error")
              panic()
            }
          }
          @types.Name::{ name: Some(callee), slot: 0 }
        } else {
          to_syntax_name(name)
        }
        let args = args.map(fn(arg) { to_syntax_name(arg) })
        Apply(callee, args)
      }
      ExternApply(name, args) => {
        let name = "minimbt_" + name.to_string()
        let args = args.map(fn(arg) { to_syntax_name(arg) })
        ExternalFunctionApplication(name, args)
      }
      Get(name, idx) => {
        let name = to_syntax_name(name)
        let idx = to_syntax_name(idx)
        Get(name, idx)
      }
      Put(name, idx, value) => {
        let name = to_syntax_name(name)
        let idx = to_syntax_name(idx)
        let value = to_syntax_name(value)
        Put(name, idx, value)
      }
      LetRec(knf_func_def, body, next) => {
        let func_name = knf_func_def.name.to_syntax_name()
        let ty = knf_func_def.func_type.to_syntax_type()
        let args = knf_func_def.params.map(fn(pair) {
          let (name, ty) = pair
          let name = to_syntax_name(name)
          let ty = ty.to_syntax_type()
          type_recorder.set(name, ty)
          (name, ty)
        })
        type_recorder.set(func_name, ty)
        let body = to_syntax_knf(body)
        let syntax_func_def = @knf.FuncDef::{ name: func_name, ty, args, body }
        LetRec(syntax_func_def, to_syntax_knf(next))
      }
      _ => {
        println("Unsupported knf conversion: \{knf.stringify(0)}")
        panic()
      }
    }
  }
  fn add_apply_main(knf: @knf.Knf) -> @knf.Knf {
    let name_main = @types.Name::{ name: Some("main"), slot: 0 }
    let app_main = @knf.Knf::Apply(name_main, [])
    match knf {
      Let(n, m, @knf.Knf::Unit) => Let(n, m, app_main)
      LetRec(fundef, @knf.Knf::Unit) => LetRec(fundef, app_main)
      LetTuple(items, n, @knf.Knf::Unit) => LetTuple(items, n, app_main)
      Let(n, m, next) => Let(n, m, add_apply_main(next))
      LetRec(fundef, next) => LetRec(fundef, add_apply_main(next))
      LetTuple(items, n, next) => LetTuple(items, n, add_apply_main(next))
      _ => {
        println("Meet problem during add apply main")
        panic()
      }
    }

  }
  let knf = to_syntax_knf(knf)
  add_apply_main(knf)
}

/// ------------------------------------------------------------
/// MibitX.ClosureIR to Syntax.Closure
/// ------------------------------------------------------------
fn lowtype_conversion(low_ty: LowType) -> @closure.LowType {
  match low_ty {
    Unit => @closure.LowType::Unit
    Int => @closure.LowType::Int
    Double => @closure.LowType::Double
    Boolean => @closure.LowType::Bool
    Ptr => @closure.LowType::Ptr
    Tuple(tys) => {
      let tys = tys.map(fn(ty) { lowtype_conversion(ty) })
      @closure.LowType::Tuple(tys)
    }
    Array(ty) => {
      @closure.LowType::Array(lowtype_conversion(ty))
    }
    ClosureFn(params, ret) => {
      let params = params.map(fn(ty) { lowtype_conversion(ty) })
      let ret = lowtype_conversion(ret)
      @closure.LowType::ClosureFn(params, ret)
    }
    DirectFn(params, ret) => {
      let params = params.map(fn(ty) { lowtype_conversion(ty) })
      let ret = lowtype_conversion(ret)
      @closure.LowType::DirectFn(params, ret)
    }
    _ => {
      println("Unsupported lowtype conversion")
      panic()
    }
  }
}

fn to_syntax_closure(self: ClosureDef) -> @closure.Closure {
  let name = self.fname.to_string()
  let actual_free_vars = self.free_vars.map(fn (pair) {
    let (name, ty) = pair
    name.to_syntax_name()
  })
  @closure.Closure::{ name, actual_free_vars }
}

fn to_syntax_closure(
  self: ClosureIR,
  type_recorder: Map[@types.Name, @closure.LowType]
) -> @closure.Expr {
  let need_rename_funcs = [
    "print_int", "print_endline", "truncate",
    "sin", "cos", "sqrt", "abs_float", "int_of_float",
    "float_of_int", "read_int", "read_char",
  ]

  match self {
    Unit => Unit
    Int(i) => Int(i)
    Double(d) => Double(d)
    Neg(name) => Neg(to_syntax_name(name))
    Var(name) => Var(to_syntax_name(name))
    Add(lhs, rhs) => {
      let lhs = to_syntax_name(lhs)
      let rhs = to_syntax_name(rhs)
      let ty = type_recorder.get(lhs).unwrap()
      match ty {
        Int => Add(lhs, rhs)
        Double => FAdd(lhs, rhs)
        _ => {
          println("Unsupported type \{ty} for Add")
          panic()
        }
      }
    }
    Sub(lhs, rhs) => {
      let lhs = to_syntax_name(lhs)
      let rhs = to_syntax_name(rhs)
      let ty = type_recorder.get(lhs).unwrap()
      match ty {
        Int => Sub(lhs, rhs)
        Double => FSub(lhs, rhs)
        _ => {
          println("Unsupported type \{ty} for Sub")
          panic()
        }
      }
    }
    Mul(lhs, rhs) => {
      let lhs = to_syntax_name(lhs)
      let rhs = to_syntax_name(rhs)
      let ty = type_recorder.get(lhs).unwrap()
      match ty {
        Int => Mul(lhs, rhs)
        Double => FMul(lhs, rhs)
        _ => {
          println("Unsupported type \{ty} for Mul")
          panic()
        }
      }
    }
    Div(lhs, rhs) => {
      let lhs = to_syntax_name(lhs)
      let rhs = to_syntax_name(rhs)
      let ty = type_recorder.get(lhs).unwrap()
      match ty {
        Int => Div(lhs, rhs)
        Double => FDiv(lhs, rhs)
        _ => {
          println("Unsupported type \{ty} for Div")
          panic()
        }
      }
    }
    Beq(lhs, rhs, then, elec) => {
      let lhs = to_syntax_name(lhs)
      let rhs = to_syntax_name(rhs)
      let then = then.to_syntax_closure(type_recorder)
      let elec = elec.to_syntax_closure(type_recorder)
      IfEq(lhs, rhs, then, elec)
    }
    Bne(lhs, rhs, then, elec) => {
      let lhs = to_syntax_name(lhs)
      let rhs = to_syntax_name(rhs)
      let then = then.to_syntax_closure(type_recorder)
      let elec = elec.to_syntax_closure(type_recorder)
      IfEq(lhs, rhs, elec, then)
    }
    Ble(lhs, rhs, then, elec) => {
      let lhs = to_syntax_name(lhs)
      let rhs = to_syntax_name(rhs)
      let then = then.to_syntax_closure(type_recorder)
      let elec = elec.to_syntax_closure(type_recorder)
      IfLe(lhs, rhs, then, elec)
    }
    Bge(lhs, rhs, then, elec) => {
      let lhs = to_syntax_name(lhs)
      let rhs = to_syntax_name(rhs)
      let then = then.to_syntax_closure(type_recorder)
      let elec = elec.to_syntax_closure(type_recorder)
      IfLe(rhs, lhs, then, elec)
    }
    Bgt(lhs, rhs, then, elec) => {
      let lhs = to_syntax_name(lhs)
      let rhs = to_syntax_name(rhs)
      let then = then.to_syntax_closure(type_recorder)
      let elec = elec.to_syntax_closure(type_recorder)
      IfLe(lhs, rhs, elec, then)
    }
    Blt(lhs, rhs, then, elec) => {
      let lhs = to_syntax_name(lhs)
      let rhs = to_syntax_name(rhs)
      let then = then.to_syntax_closure(type_recorder)
      let elec = elec.to_syntax_closure(type_recorder)
      IfLe(rhs, lhs, elec, then)
    }
    Let((name, ty), body, next) => {
      let name = to_syntax_name(name)
      let ty = lowtype_conversion(ty)
      type_recorder.set(name, ty)
      let body = body.to_syntax_closure(type_recorder)
      let next = next.to_syntax_closure(type_recorder)
      Let((name, ty), body, next)
    }
    LetTuple(items, name, body) => {
      let items = items.map(fn(pair) {
        let (name, ty) = pair
        let name = to_syntax_name(name)
        let ty = lowtype_conversion(ty)
        type_recorder.set(name, ty)
        (name, ty)
      })
      let name = to_syntax_name(name)
      let body = body.to_syntax_closure(type_recorder)
      LetTuple(items, name, body)
    }
    CallClosure(name, args) => {
      let name = to_syntax_name(name)
      let args = args.map(fn(arg) { to_syntax_name(arg) })
      CallClosure(name, args)
    }
    CallDirect(name, args) => {
      let label = name.to_string()
      let label = if need_rename_funcs.contains(label) {
        "minimbt_" + label
      } else {
        label
      }
      let label = if label.starts_with("__builtin_array_make") {
        match type_recorder.get(args[1].to_syntax_name()) {
          Some(@closure.LowType::Int) => "minimbt_" + "create_array"
          Some(@closure.LowType::Double) => "minimbt_" + "create_float_array"
          Some(_) => "minimbt_" + "create_ptr_array"
          None => {
            println("closure 2 closure Call array make error")
            panic()
          }
        }
      } else {
        label
      }
      let args = args.map(fn(arg) { to_syntax_name(arg) })
      CallDirect(label, args)
    }
    MakeTuple(names) => {
      let names = names.map(fn(name) { to_syntax_name(name) })
      MakeTuple(names)
    }
    ArrayGet(name, idx) => {
      let name = to_syntax_name(name)
      let idx = to_syntax_name(idx)
      ArrayGet(name, idx)
    }
    ArrayPut(name, idx, value) => {
      let name = to_syntax_name(name)
      let idx = to_syntax_name(idx)
      let value = to_syntax_name(value)
      ArrayPut(name, idx, value)
    }
    MakeClosure((name, ty), closuredef, next) => {
      let name = to_syntax_name(name)
      let ty = lowtype_conversion(ty)
      let closure = closuredef.to_syntax_closure()
      let next = next.to_syntax_closure(type_recorder)
      MakeClosure((name, ty), closure, next)
    }
    _ => {
      println("Unsupported closure conversion \{self.stringify(0)}")
      panic()
    }
  }
}

fn concat_closure_ir(prev: @closure.Expr, next: @closure.Expr) -> @closure.Expr {
  match prev {
    Let((name, ty), body, @closure.Expr::Unit) => {
      Let((name, ty), body, next)
    }
    LetTuple(items, name, @closure.Expr::Unit) => {
      LetTuple(items, name, next)
    }
    Let((name, ty), body, other_next) => {
      Let((name, ty), body, concat_closure_ir(other_next, next))
    }
    LetTuple(items, name, other_next) => {
      LetTuple(items, name, concat_closure_ir(other_next, next))
    }
    @closure.Expr::Unit => next
    _ => {
      println("Meet problem during concat closure ir, prev is \{prev}")
      panic()
    }
  }
}

fn concat_closure_arr(arr: ArrayView[@closure.Expr]) -> @closure.Expr {
  let len = arr.length()
  if len == 0 {
    return @closure.Expr::Unit
  }
  let last = arr[len - 1]
  let rest = arr[0: len - 1]
  concat_closure_ir(concat_closure_arr(rest), last)
}

pub fn closure_func_def_conversion(
  self: ClosureIRFuncDef
) -> @closure.FuncDef {
  let old_name = self.fname.to_syntax_name()
  let name = old_name.to_string()
  let is_closure = match self.free_vars.length() {
    0 => false
    _ => true
  }
  let type_recorder : Map[@types.Name, @closure.LowType] = Map::new()
  let ty = lowtype_conversion(self.func_type)
  let args = self.params.map(fn(pair) {
    let (name, ty) = pair
    let name = name.to_syntax_name()
    let ty = lowtype_conversion(ty)
    type_recorder.set(name, ty)
    (name, ty)
  })
  match args.last() {
    Some(pair) => {
      let (name, _) = pair
      if name.to_string() == "__capture__" {
        let _ = args.pop()
      }
    }
    None => ()
  }
  let formal_free_vars = self.free_vars.map(fn(pair) {
    let (name, ty) = pair
    let name = name.to_syntax_name()
    let ty = lowtype_conversion(ty)
    type_recorder.set(name, ty)
    (name, ty)
  })
  let whole_body : Array[@closure.Expr] = Array::new()
  self.prog.global_vars.each(fn (name, pair) {
    let (ty, sub_ir) = pair
    let name = to_syntax_name(name)
    let ty = lowtype_conversion(ty)
    let sub_ir = sub_ir.to_syntax_closure(type_recorder)
    type_recorder.set(name, ty)
    whole_body.push(Let((name, ty), sub_ir, Unit))
  })
  let body = self.body.to_syntax_closure(type_recorder)
  whole_body.push(body)
  let body = concat_closure_arr(whole_body[0:])
  @closure.FuncDef::{
    name, old_name,
    is_closure, ty,
    args, formal_free_vars,
    body
  }
}

pub fn closure_program_conversion(self: ClosureProgram) -> @closure.Program {
  let fundefs = self.func_defs.map(fn(func_def) {
    closure_func_def_conversion(func_def)
  })
  let type_recorder : Map[@types.Name, @closure.LowType] = Map::new()
  let body =self.body.to_syntax_closure(type_recorder)
  @closure.Program::{ fundefs, body }
}
