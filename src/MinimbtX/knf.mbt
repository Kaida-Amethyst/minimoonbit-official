
/// ---------------------------------------------------
/// ## Knf IR
/// ---------------------------------------------------
pub enum Knf {
  Unit
  Int(Int)
  Int64(Int64)
  UInt(UInt)
  UInt64(UInt64)
  Double(Double)
  Boolean(Bool)
  Var(Name)
  Neg(Name)
  Not(Name)
  Add(Name, Name)
  Sub(Name, Name)
  Mul(Name, Name)
  Div(Name, Name)
  Rem(Name, Name)
  And(Name, Name)
  Or(Name, Name)
  Eq(Name, Name)
  Ne(Name, Name)
  Gt(Name, Name)
  Lt(Name, Name)
  Ge(Name, Name)
  Le(Name, Name)
  Beq(Name, Name, Knf, Knf)
  Bne(Name, Name, Knf, Knf)
  Bgt(Name, Name, Knf, Knf)
  Blt(Name, Name, Knf, Knf)
  Bge(Name, Name, Knf, Knf)
  Br(Name, Knf, Knf)
  Ble(Name, Name, Knf, Knf)
  Tuple(Array[Name])
  Array(Array[Name])
  Let((Name, Type), Knf, Knf)
  LetTuple(Array[(Name, Type)], Name, Knf)
  Apply(Name, Array[Name])
  ExternApply(Name, Array[Name])
  Get(Name, Name)
  Put(Name, Name, Name)
  LetRec(KnfFuncDef, Knf, Knf)
  Error(String)
}

struct KnfFuncDef {
  name : Name
  params : Array[(Name, Type)]
  ret_type : Type
  func_type: Type
}

fn KnfFuncDef::new(
  name : Name, params : Array[(Name, Type)], ret_type: Type, func_type: Type
) -> KnfFuncDef {
  KnfFuncDef::{ name, params, ret_type, func_type }
}

pub fn to_string(self : KnfFuncDef) -> String {
  let params = self.params.map(fn(param) { let (param, ty) = param; param.to_string() + ty.to_string() }).join(", ")
  self.name.to_string() + "(" + params + ") ->" + self.ret_type.to_string()
}

// Note: to_name will execute add_knf
pub fn to_name(
  self : Knf,
  builder : KnfBuilder,
  ty : Type,
  ~mark : String = "tmp"
) -> Name {
  match self {
    Knf::Var(n) => n
    Knf::Let(_) | Knf::LetRec(_) => {
      println("Could not execute to name for let and let rec knf")
      panic()
    }
    _ => {
      let name = Name::new(mark, slot=Some(builder.context.tmp_count))
      builder.add_knf(Knf::Let((name, ty), self, Knf::Unit))
      builder.context.tmp_count += 1
      builder.set(mark, name, ty)
      name
    }
  }
}

pub fn to_let(
  self : Knf,
  builder : KnfBuilder,
  ty : Type,
  ~mark : String = "tmp"
) -> Knf {
  match self {
    Knf::Let(_) | Knf::LetRec(_) | Knf::LetTuple(_) => {
      println("Could not execute to let for let, let tuple and let rec knf")
      panic()
    }
    _ => {
      match mark {
        "tmp" => {
          let name = builder.new_tmp_name()
          builder.set(mark, name, ty)
          let knf = Knf::Let((name, ty), self, Knf::Unit)
          knf
        }
        "_" => {
          let name = builder.new_wildcard_name()
          Knf::Let((name, ty), self, Knf::Unit)
        }
        _ => {
          let name = builder.new_ident_name(mark, ty)
          Knf::Let((name, ty), self, Knf::Unit)
        }
      }
    }
  }
}

pub struct KnfContext {
  context : Context
  mut knf : Knf
  mut main_knf : Knf
  mut var_count : Int
  mut tmp_count : Int
  extern_table : Map[String, (Name, Type)]
  builtin_table : Map[String, (Name, Type)]
  abstract_table : Map[String, (Name, Type)]
}

pub fn KnfContext::new(context : Context) -> KnfContext {
  let c = KnfContext::{
    context,
    knf: Knf::Unit,
    main_knf: Knf::Unit,
    var_count: 0,
    tmp_count: 0,
    extern_table: Map::new(),
    builtin_table: Map::new(),
    abstract_table: Map::new(),
  }
  for n, ety in context.extern_funcs {
    c.extern_table.set(n, (Name::new(n), ety))
  }
  for n, bty in context.builtin_types {
    match bty.is_abstract() {
      false => c.builtin_table.set(n, (Name::new(n), bty))
      true => {
        c.abstract_table.set(n, (Name::new(n), bty))
        match bty {
          Type::FuncTy(fty) => {
            for instance in fty.instance.types {
              guard let Type::FuncTy(fty) = instance
              let fname = match n.to_string() {
                "Array::make" => "__builtin_array_make" + fty.mangling()
                _ => fty.mangling_with_func_name(n.to_string())
              }
              c.builtin_table.set(fname, (Name::new(fname), instance))
            }
          }
          _ => ()
        }
      }
    }
  }
  c
}

pub fn KnfContext::create_builder(self : KnfContext) -> KnfBuilder {
  KnfBuilder::default(self)
}

pub fn find_name(self : KnfContext, name : String) -> (Name, Type)? {
  match self.extern_table.get(name) {
    Some(n) => Some(n)
    None => match self.builtin_table.get(name) {
      Some(n) => Some(n)
      None => match self.abstract_table.get(name) {
        Some(n) => Some(n)
        None => None
      }
    }
  }
}

pub struct KnfBuilder {
  context : KnfContext
  mut knf : Knf
  name_table : Map[String, (Name, Type)]
  parent : KnfBuilder?
}

pub fn KnfBuilder::default(context : KnfContext) -> KnfBuilder {
  KnfBuilder::{ context, knf: Knf::Unit, name_table: Map::new(), parent: None }
}

pub fn KnfBuilder::new(parent : KnfBuilder) -> KnfBuilder {
  KnfBuilder::{
    context: parent.context,
    knf: Knf::Unit,
    name_table: Map::new(),
    parent: Some(parent),
  }
}

pub fn get(self : KnfBuilder, name : String) -> (Name, Type)? {
  match self.name_table.get(name) {
    Some(n) => Some(n)
    None =>
      match self.parent {
        Some(p) => p.get(name)
        None => self.context.find_name(name)
      }
  }
}

pub fn set(self : KnfBuilder, name : String, n : Name, ty: Type) -> Unit {
  self.name_table.set(name, (n, ty))
}

pub fn new_ident_name(self : KnfBuilder, name : String, ty : Type) -> Name {
  let n = match self.get(name) {
    Some((n, _)) => {
      let slot = match n.slot {
        Some(s) => s + 1
        None => 0
      }
      let slot = Some(slot)
      Name::new(name, ~slot)
    }
    None => Name::new(name)
  }
  self.set(name, n, ty)
  self.context.var_count += 1
  n
}

pub fn new_wildcard_name(self : KnfBuilder) -> Name {
  let n = Name::new("_")
  self.context.var_count += 1
  n
}

pub fn new_tmp_name(self: KnfBuilder) -> Name {
  let n = Name::new("tmp", slot=Some(self.context.tmp_count))
  self.context.tmp_count += 1
  n
}

pub fn add_knf(self : KnfBuilder, knf : Knf) -> Unit {
  fn knf_append(head : Knf, ele : Knf) -> Knf {
    match head {
      Knf::Unit => ele
      Knf::Let(prev, inner, Knf::Unit) => Knf::Let(prev, inner, ele)
      Knf::Let(prev, inner, next) => Knf::Let(prev, inner, knf_append(next, ele))
      Knf::LetRec(prev, inner, Knf::Unit) => Knf::LetRec(prev, inner, ele)
      Knf::LetRec(prev, inner, next) => Knf::LetRec(prev, inner, knf_append(next, ele))
      Knf::LetTuple(prev, inner, Knf::Unit) => Knf::LetTuple(prev, inner, ele)
      Knf::LetTuple(prev, inner, next) => Knf::LetTuple(prev, inner, knf_append(next, ele))
      _ => {
        println("Could not append knf")
        panic()
      }
    }
  }

  self.knf = knf_append(self.knf, knf)
}

fn to_knf(self : MainFunc, builder : KnfBuilder) -> Knf {
  match builder.context.main_knf {
    Knf::Unit => self.body.to_knf(builder)
    _ => {
      println("Main function is already defined")
      panic()
    }
  }
}

fn to_knf(self : FuncDef, builder : KnfBuilder) -> Knf {
  let func_name = self.name.id
  let func_type = self.meta().query_type()
  let ret_type = self.ret_type.meta().query_type()
  let knf_func_name = builder.new_ident_name(func_name, func_type)
  let arg_names : Array[(Name, Type)] = Array::new()
  let builder = KnfBuilder::new(builder)
  self.params.each(
    fn(param) {
      let param_name = param.ident.id
      let param_type = param.meta().query_type()
      // if param type is unit, we do not need to create a new name
      match param_type {
        Type::Unit => ()
        _ => {
          let name = builder.new_ident_name(param_name, param_type)
          arg_names.push((name, param_type))
          builder.set(param_name, name, param_type)
        }
      }
    },
  )
  let knf_func_def = KnfFuncDef::new(knf_func_name, arg_names, ret_type, func_type)
  let func_body = self.body.to_knf(builder)
  Knf::LetRec(knf_func_def, func_body, Knf::Unit)
}

fn to_knf(self : Closure, builder : KnfBuilder) -> Knf {
  let closure_name = match self.name {
    Some(n) => n.id
    None => "_$anonymous_closure"
  }
  let closure_type = self.meta().query_type()
  let knf_closure_name = builder.new_ident_name(closure_name, closure_type)
  let builder = KnfBuilder::new(builder)
  let arg_names : Array[(Name, Type)] = Array::new()
  self.params.each(
    fn(param) {
      let param_name = param.ident.id
      let param_type = param.meta().query_type()
      match param_type {
        Type::Unit => ()
        _ => {
          let name = builder.new_ident_name(param_name, param_type)
          arg_names.push((name, param_type))
          builder.set(param_name, name, param_type)
        }
      }
    },
  )
  guard let Type::FuncTy(fty) = closure_type
  let knf_func_def = KnfFuncDef::new(knf_closure_name, arg_names, fty.ret_type, closure_type)
  let closure_body = self.body.to_knf(builder)
  Knf::LetRec(knf_func_def, closure_body, Knf::Unit)
}

fn to_knf(self : BlockExpr, builder : KnfBuilder) -> Knf {
  let new_builder = KnfBuilder::new(builder)
  for i, state in self.state_exprs {
    let knf = state.to_knf(new_builder)
    let knf = match knf {
      Knf::Put(_) =>
        if i < self.state_exprs.length() - 1 {
          Knf::Let((builder.new_wildcard_name(), Type::Unit), knf, Knf::Unit)
        } else {
          knf
        }
      _ => knf
    }
    new_builder.add_knf(knf)
  }
  new_builder.knf
}

fn to_knf(self : StateExpr, builder : KnfBuilder) -> Knf {
  match self {
    Let(l) => l.to_knf(builder)
    Expr(e) => e.to_knf(builder)
    Assign(a) => a.to_knf(builder)
    // LetRec(l) => l.to_knf(builder)
    Null(_) => Knf::Unit
    _ => {
      println("Not implemented yet, source: fn compile_state_expr")
      println(self.stringify(0))
      panic()
    }
  }
}

fn to_knf(self : IfExpr, builder : KnfBuilder) -> Knf {
  let cond_knf = self.cond_expr.to_knf(builder)
  let then_knf = self.then_expr.to_knf(builder)
  let else_knf = match self.else_clause {
    Some(e) => e.to_knf(builder)
    None => Knf::Unit
  }
  match cond_knf {
    Knf::Eq(n1, n2) => Knf::Beq(n1, n2, then_knf, else_knf)
    Knf::Ne(n1, n2) => Knf::Bne(n1, n2, then_knf, else_knf)
    Knf::Gt(n1, n2) => Knf::Bgt(n1, n2, then_knf, else_knf)
    Knf::Lt(n1, n2) => Knf::Blt(n1, n2, then_knf, else_knf)
    Knf::Ge(n1, n2) => Knf::Bge(n1, n2, then_knf, else_knf)
    Knf::Le(n1, n2) => Knf::Ble(n1, n2, then_knf, else_knf)
    _ => {
      let cond_ty = self.cond_expr.meta().query_type()
      let cond_name = cond_knf.to_name(builder, cond_ty)
      Knf::Br(cond_name, then_knf, else_knf)
    }
  }
}

fn to_knf(self : ElseClause, builder : KnfBuilder) -> Knf {
  match self {
    Block(b) => b.to_knf(builder)
    IfExpr(i) => i.to_knf(builder)
  }
}

fn to_knf(self : LetExpr, builder : KnfBuilder) -> Knf {
  let expr = self.expr
  let expr_ty = self.meta().query_type()
  match self.pat.atom_pat {
    WildCard(_) => expr.to_knf(builder).to_let(builder, expr_ty, mark="_")
    Ident(id) =>
      expr
      .to_knf(builder)
      .to_let(builder, id.meta().query_type(), mark=id.id)
    Tuple(tup) => {
      let new_pat : Array[(Name, Type)] = Array::new()
      for p in tup.as_pats {
        let ty = p.meta().query_type()
        let name = match p.atom_pat {
          WildCard(_) => builder.new_wildcard_name()
          Ident(id) => builder.new_ident_name(id.id, ty)
          _ => {
            println("Too complex let pattern, it is unsppoted yet")
            println(self.stringify(0))
            panic()
          }
        }
        new_pat.push((name, ty))
      }
      let expr_name = expr.to_knf(builder).to_name(builder, expr_ty)
      Knf::LetTuple(new_pat, expr_name, Knf::Unit)
    }
    _ => {
      println("Too complex let pattern, it is unsppoted yet")
      println(self.stringify(0))
      panic()
    }
  }
}

fn to_knf(self : AssignExpr, builder : KnfBuilder) -> Knf {
  let right = self.right
  let right_ty = right.meta().query_type()
  let right_name = right.to_knf(builder).to_name(builder, right_ty)
  let left = self.left
  let left_knf = left.to_knf(builder)
  let knf = match left_knf {
    Get(name, index) => {
      let right_name = match self.assign_op.sym {
        "=" => right_name
        "+=" => Knf::Add(name, right_name).to_name(builder, right_ty)
        "-=" => Knf::Sub(name, right_name).to_name(builder, right_ty)
        "*=" => Knf::Mul(name, right_name).to_name(builder, right_ty)
        "/=" => Knf::Div(name, right_name).to_name(builder, right_ty)
        _ =>
          Knf::Error("Not implemented yet, unsupported assign op").to_name(
            builder, right_ty,
          )
      }
      Put(name, index, right_name)
    }
    _ => Knf::Error("Not implemented yet, ilegale left value")
  }
  match knf {
    Knf::Error(msg) => {
      println(msg)
      panic()
    }
    _ => knf
  }
}

fn to_knf(self : Expr, builder : KnfBuilder) -> Knf {
  match self {
    Apply(a) => a.to_knf(builder)
    Binary(b) => b.to_knf(builder)
    Block(b) => b.to_knf(builder)
    IfExpr(i) => i.to_knf(builder)
    // MatchExpr(m) => m.to_knf(builder)
    _ => {
      println("Not implemented yet, source: fn compile_expr")
      println(self.stringify(0))
      panic()
    }
  }
}

fn to_knf(self : BinaryExpr, builder : KnfBuilder) -> Knf {
  let left_expr = self.left
  let left_ty = left_expr.meta().query_type()
  let left_name = left_expr.to_knf(builder).to_name(builder, left_ty)
  let right_expr = self.right
  let right_ty = right_expr.meta().query_type()
  let right_name = right_expr.to_knf(builder).to_name(builder, right_ty)
  match self.op.sym {
    "+" => Knf::Add(left_name, right_name)
    "-" => Knf::Sub(left_name, right_name)
    "*" => Knf::Mul(left_name, right_name)
    "/" => Knf::Div(left_name, right_name)
    "%" => Knf::Rem(left_name, right_name)
    "&&" => Knf::And(left_name, right_name)
    "||" => Knf::Or(left_name, right_name)
    "==" => Knf::Eq(left_name, right_name)
    "!=" => Knf::Ne(left_name, right_name)
    ">" => Knf::Gt(left_name, right_name)
    "<" => Knf::Lt(left_name, right_name)
    ">=" => Knf::Ge(left_name, right_name)
    "<=" => Knf::Le(left_name, right_name)
    _ => {
      println("Not implemented yet, source: fn compile_binary_expr")
      println("binary expr op: \{self.op.sym}")
      panic()
    }
  }
}

fn to_knf(self : UnaryExpr, builder : KnfBuilder) -> Knf {
  let expr = self.expr
  if expr.is_literal() {
    guard let Some(lit) = expr.get_literal()
    match lit.category {
      Integer(i) => return Knf::Int(-i)
      Long(i) => return Knf::Int64(-i)
      Double(d) => return Knf::Double(-d)
      _ => ()
    }
  }
  let ty = expr.meta().query_type()
  let name = expr.to_knf(builder).to_name(builder, ty)
  match self.uop.sym {
    "-" => Knf::Neg(name)
    _ => {
      println("Not implemented yet, source: fn compile_unary_expr")
      println("unary expr op: \{self.uop.sym}")
      panic()
    }
  }
}

fn to_knf(self : AccessExpr, builder : KnfBuilder) -> Knf {
  let apply_knf = self.apply_expr
    .to_knf(builder)
    .to_name(builder, self.apply_expr.meta().query_type())
  let accessor = self.accessor
  match accessor {
    ArrayAcc(expr) => {
      let ty = expr.meta().query_type()
      let index = expr.to_knf(builder).to_name(builder, ty)
      Knf::Get(apply_knf, index)
    }
    _ => {
      println("Not implemented yet, source: fn compile_access_expr")
      println(self.stringify(0))
      panic()
    }
  }
}

fn abstract_func_rename(
  fname: Name, fty: FuncTy , builder: KnfBuilder
) -> Name {
  // check if it is abstract
  match fname.mark {
    "Array::make" => {
      let name_mangling = "__builtin_array_make" + fty.mangling()
      return Name::new(name_mangling)
    }
    _ => ()
  }
  match builder.context.abstract_table.get(fname.mark) {
    Some(_) => {
      let name_mangling = Name::new(fty.mangling_with_func_name(fname.mark))
      name_mangling
    }
    None => fname
  }
}


fn optimize_not(arg: Expr, builder: KnfBuilder) -> Knf? {
  return match arg {
    Binary(bin) => {
      let left_expr = bin.left
      let left_ty = left_expr.meta().query_type()
      let left_name = left_expr.to_knf(builder).to_name(builder, left_ty)
      let right_expr = bin.right
      let right_ty = right_expr.meta().query_type()
      let right_name = right_expr.to_knf(builder).to_name(builder, right_ty)
      match bin.op.sym {
        ">=" => Some(Knf::Lt(left_name, right_name))
        "<=" => Some(Knf::Gt(left_name, right_name))
        ">"  => Some(Knf::Le(left_name, right_name))
        "<"  => Some(Knf::Ge(left_name, right_name))
        "==" => Some(Knf::Ne(left_name, right_name))
        "!=" => Some(Knf::Eq(left_name, right_name))
        _ => None
      }
    }
    _ => None
  }
}

fn to_knf(self : CallExpr, builder : KnfBuilder) -> Knf {
  let apply_name = self.apply_expr
    .to_knf(builder)
    .to_name(builder, self.apply_expr.meta().query_type())
  // for `not(a > b)` or something like that, make it to a <= b
  if apply_name.mark == "not" {
    let arg = self.args[0]
    match optimize_not(arg, builder) {
      Some(knf) => return knf
      None => ()
    }
  }

  guard let Type::FuncTy(fty) = self.apply_expr.meta().query_type()
  let real_name = abstract_func_rename(apply_name, fty, builder)
  let apply_name = real_name
  // println("apply name: \{apply_name}, its type is \{self.apply_expr.meta().query_type()}")
  let args : Array[Name] = Array::new()
  self.args.each(
    fn(arg) {
      let arg_ty = arg.meta().query_type()
      match arg_ty {
        Type::Unit => ()
        _ => {
          let n = arg.to_knf(builder).to_name(builder, arg_ty)
          args.push(n)
        }
      }
    },
  )
  match builder.context.extern_table.get(apply_name.mark) {
    Some(_) => Knf::ExternApply(apply_name, args)
    None => {
      match apply_name.mark {
        "not" => Knf::Not(args[0])
        _ => Knf::Apply(apply_name, args)
      }
    }
  }
}

fn to_knf(self : ApplyExpr, builder : KnfBuilder) -> Knf {
  match self {
    Atomic(atom) => atom.to_knf(builder)
    Access(acce) => acce.to_knf(builder)
    CallExpr(call) => call.to_knf(builder)
  }
}

fn to_knf(self : ArrayExpr, builder : KnfBuilder) -> Knf {
  let exprs = self.exprs
  let names = exprs.map(
    fn(expr) {
      expr.to_knf(builder).to_name(builder, expr.meta().query_type())
    },
  )
  Knf::Array(names)
}

fn to_knf(self : TupleExpr, builder : KnfBuilder) -> Knf {
  let exprs = self.exprs
  let names = exprs.map(
    fn(expr) {
      expr.to_knf(builder).to_name(builder, expr.meta().query_type())
    },
  )
  Knf::Tuple(names)
}

fn to_knf(self : AtomExpr, builder : KnfBuilder) -> Knf {
  match self {
    Literal(l) => l.to_knf(builder)
    Ident(id) => id.to_knf(builder)
    Paren(e) => e.to_knf(builder)
    Method(m) => m.to_knf(builder)
    UnaryExpr(u) => u.to_knf(builder)
    TupleExpr(t) => t.to_knf(builder)
    ArrayExpr(a) => a.to_knf(builder)
    Closure(c) => c.to_knf(builder)
    Unit(_) => Knf::Unit
    _ => {
      println("Not implemented yet, source: fn compile_atom_expr")
      println(self.stringify(0))
      panic()
    }
  }
}

fn to_knf(self : MethodExpr, builder : KnfBuilder) -> Knf {
  let (name, _) = builder
    .get(self.ident.id)
    .or_else(
      fn() {
        println("Could not find name")
        panic()
      },
    )
  Knf::Var(name)
}

fn to_knf(self : LowerCase, builder : KnfBuilder) -> Knf {
  let id = self.id
  let (name, _) = builder.get(id).or_else(fn () {
    println("Could not find name, loc: lower case to knf, id is \{id}")
    panic()

  })
  Knf::Var(name)
}

fn to_knf(self : Literal, builder : KnfBuilder) -> Knf {
  match self.category {
    Boolean(b) => Knf::Boolean(b)
    Double(d) => Knf::Double(d)
    Integer(i) => Knf::Int(i)
    Long(l) => Knf::Int64(l)
    Unsigned(u) => Knf::UInt(u)
    UnsignedLong(ul) => Knf::UInt64(ul)
    _ => {
      println("Not implemented yet, source: fn compile_literal")
      panic()
    }
  }
}
