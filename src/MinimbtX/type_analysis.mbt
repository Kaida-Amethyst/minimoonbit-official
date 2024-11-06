/// ---------------------------------------------------
/// # Type System and Type Analysis
/// ---------------------------------------------------

/// ---------------------------------------------------
/// ## Type Definition
/// ---------------------------------------------------
pub enum Type {
  Undefine
  Unit
  Int32
  Int64
  UInt32
  UInt64
  Float32
  Double
  Boolean
  String
  Char
  TupleTy(TupleTy)
  CompTy(CompTy)
  EnumTy(EnumTy)
  StruTy(StruTy)
  FuncTy(FuncTy)
  ReasoningTy(ReasoningTy)
  TempTy(TempTy)
}

pub fn to_string(self : Type) -> String {
  match self {
    Undefine => @Color.bold_red("Undefine")
    Unit => "Unit"
    Int32 => "Int32"
    Int64 => "Int64"
    UInt32 => "UInt32"
    UInt64 => "UInt64"
    Float32 => "Float32"
    Double => "Double"
    Boolean => "Boolean"
    String => "String"
    Char => "Char"
    TupleTy(t) => t.to_string()
    CompTy(s) => s.to_string()
    EnumTy(e) => e.to_string()
    StruTy(s) => s.to_string()
    FuncTy(f) => f.to_string()
    ReasoningTy(i) => i.to_string()
    TempTy(t) => t.to_string()
  }
}

// Mainly used in function or struct name mangling
pub fn mangling(self: Type) -> String {
  match self {
    Unit => "v"
    Int32 => "i"
    Int64 => "l"
    UInt32 => "u"
    UInt64 => "ul"
    Boolean => "b"
    Float32 => "f"
    Double => "d"
    String => "s"
    Char => "c"
    TupleTy(tup) => tup.mangling()
    CompTy(comp) => comp.mangling()
    EnumTy(e) => e.mangling()
    StruTy(stru) => stru.mangling()
    FuncTy(func) => func.mangling()
    _ => {
      println("Cannot mangling type: \{self.to_string()}")
      panic()
    }
  }

}

pub fn op_equal(self : Type, other : Type) -> Bool {
  match (self, other) {
    (Unit, Unit) => true
    (Int32, Int32) => true
    (Int64, Int64) => true
    (UInt32, UInt32) => true
    (UInt64, UInt64) => true
    (Float32, Float32) => true
    (Double, Double) => true
    (Boolean, Boolean) => true
    (Char, Char) => true
    (String, String) => true
    (CompTy(s1), CompTy(s2)) => s1 == s2
    (TupleTy(t1), TupleTy(t2)) => t1 == t2
    (FuncTy(f1), FuncTy(f2)) => f1 == f2
    (ReasoningTy(i1), ReasoningTy(i2)) => i1 == i2
    (TempTy(t1), TempTy(t2)) => t1 == t2
    _ => false
  }
}

pub fn is_abstract(self : Type) -> Bool {
  match self {
    TempTy(_) => true
    ReasoningTy(_) => true // TODO: question?
    TupleTy(t) => t.is_abstract()
    CompTy(s) => s.is_abstract()
    StruTy(s) => s.is_abstract()
    FuncTy(f) => f.is_abstract()
    _ => false
  }
}

pub fn is_bool(self : Type) -> Bool {
  match self {
    Boolean => true
    _ => false
  }
}

pub fn is_tuple(self : Type) -> Bool {
  match self {
    TupleTy(_) => true
    _ => false
  }
}

pub fn is_comp(self : Type) -> Bool {
  match self {
    CompTy(_) => true
    _ => false
  }
}

pub fn is_struct(self : Type) -> Bool {
  match self {
    StruTy(_) => true
    _ => false
  }
}

pub fn is_function(self : Type) -> Bool {
  match self {
    FuncTy(_) => true
    _ => false
  }
}

pub fn is_temp(self : Type) -> Bool {
  match self {
    TempTy(_) => true
    _ => false
  }
}

pub fn is_array(self : Type) -> Bool {
  match self {
    CompTy(s) => s.master == "Array"
    _ => false
  }
}

pub fn is_undefine(self : Type) -> Bool {
  match self {
    Undefine => true
    TupleTy(t) => t.types.iter().any(fn(t) { t.is_undefine() })
    CompTy(c) => c.subs.iter().any(fn(t) { t.is_undefine() })
    StruTy(s) => s.subs.iter().any(fn(t) { t.is_undefine() })
    EnumTy(e) => e.subs.iter().any(fn(t) { t.is_undefine() })
    FuncTy(f) =>
      f.param_types.iter().any(fn(t) { t.is_undefine() }) ||
      f.ret_type.is_undefine()
    _ => false
  }
}

fn Type::create_reasoning(id : Int) -> Type {
  let indef = ReasoningTy::new(id)
  Type::ReasoningTy(indef)
}

fn Type::create_temp_ty(
  id : Int,
  ~constraints : Array[Constraint] = []
) -> Type {
  let tempty = TempTy::new(id, constraints)
  Type::TempTy(tempty)
}

fn Type::create_tuple_ty(tys : TypeArr) -> Type {
  Type::TupleTy(TupleTy::new(tys))
}

fn Type::create_comp_ty(master : String, subs : TypeArr) -> Type {
  Type::CompTy(CompTy::new(master, subs))
}

fn Type::create_array_ty(ele_ty : Type) -> Type {
  Type::CompTy(CompTy::array_ty(ele_ty))
}

fn Type::create_struct_ty(
  master : String,
  subs : TypeArr,
  ~source : UpperCase? = None
) -> Type {
  Type::StruTy(StruTy::new(master, subs, ~source))
}

fn Type::create_func_ty(
  arg_types : TypeArr,
  ret_type : Type,
  ~source : LowerCase? = None
) -> Type {
  Type::FuncTy(FuncTy::new(arg_types, ret_type, ~source))
}

fn Type::builtin_not() -> Type {
  Type::FuncTy(FuncTy::builtin_not())
}

fn Type::builtin_array_make() -> Type {
  let ele_ty = Type::create_temp_ty(0)
  let arg_types = TypeArr::new([Type::Int32, ele_ty])
  let ret_type = Type::create_array_ty(ele_ty)
  Type::FuncTy(FuncTy::new(arg_types, ret_type))
}

fn Type::builtin_array_ty() -> Type {
  let ele_ty = Type::create_temp_ty(0)
  Type::CompTy(CompTy::array_ty(ele_ty))
}

pub fn template_infer(self : Type, temp_map : Map[Int, Type]) -> Type {
  match self {
    TempTy(temp_arg) => temp_arg.template_infer(temp_map)
    TupleTy(temp_tuple) => temp_tuple.template_infer(temp_map)
    CompTy(temp_struct) => temp_struct.template_infer(temp_map)
    StruTy(temp_struct) => temp_struct.template_infer(temp_map)
    EnumTy(temp_enum) => temp_enum.template_infer(temp_map)
    FuncTy(temp_func) => temp_func.template_infer(temp_map)
    _ => self
  }
}

pub struct TypeArr {
  types : Array[Type]
}

pub fn TypeArr::default() -> TypeArr {
  TypeArr::{ types: Array::new() }
}

pub fn TypeArr::new(types : Array[Type]) -> TypeArr {
  TypeArr::{ types, }
}

pub fn iter(self : TypeArr) -> Iter[Type] {
  self.types.iter()
}

pub fn push(self : TypeArr, ty : Type) -> Unit {
  self.types.push(ty)
}

pub fn clear(self : TypeArr) -> Unit {
  self.types.clear()
}

pub fn length(self : TypeArr) -> Int {
  self.types.length()
}

pub fn op_get(self : TypeArr, index : Int) -> Type {
  self.types[index]
}

pub fn op_equal(self : TypeArr, other : TypeArr) -> Bool {
  if self.types.length() != other.types.length() {
    return false
  }
  loop self.types[0:], other.types[0:], true {
    [], [], p => break p
    ls, rs, p => continue ls[1:], rs[1:], ls[0] == rs[0] && p
  }
}

pub fn stringify(self : TypeArr, lbracket : Char, rbracket : Char) -> String {
  let lbracket_str = lbracket.to_string()
  let rbracket_str = rbracket.to_string()
  match self.types.length() {
    0 => lbracket_str + rbracket_str
    1 => lbracket_str + self.types[0].to_string() + rbracket_str
    _ => {
      let init = self.types[0].to_string()
      fn tyconcat(content, ty : Type) {
        content + ", " + ty.to_string()
      }

      lbracket_str + self.types[1:].iter().fold(tyconcat, ~init) + rbracket_str
    }
  }
}

/// ------------------------------------------------------------------------
/// TypeSet is a set of types, it stored unique types.
/// ------------------------------------------------------------------------
pub struct TypeSet {
  types : Array[Type]
}

pub fn TypeSet::default() -> TypeSet {
  TypeSet::{ types: Array::new() }
}

pub fn TypeSet::new(types : Array[Type]) -> TypeSet {
  let set = TypeSet::default()
  for ty in types {
    set.add(ty)
  }
  set
}

pub fn TypeSet::add(self : TypeSet, ty : Type) -> Unit {
  if self.types.iter().any(fn(t) { t == ty }) {
    return
  }
  self.types.push(ty)
}

pub fn TypeSet::iter(self : TypeSet) -> Iter[Type] {
  self.types.iter()
}

pub fn TypeSet::length(self : TypeSet) -> Int {
  self.types.length()
}

pub fn stringify(self : TypeSet) -> String {
  match self.length() {
    0 => "{}"
    _ => {
      let init = self.types[0].to_string()
      let content = self.types[1:]
        .iter()
        .fold(fn(content, ty) { content + ", " + ty.to_string() }, ~init)
      "{" + content + "}"
    }
  }
}

/// ------------------------------------------------------------------------
/// CompTy is a type that represents a sequence type, it cannot be defined by user.
///
/// ### Example:
///
/// - `Array[Int]`
/// - `Map[String, Int]`
/// ------------------------------------------------------------------------
struct CompTy {
  master : String
  subs : TypeArr
  instance : TypeSet
}

pub fn CompTy::new(master : String, subs : TypeArr) -> CompTy {
  CompTy::{ master, subs, instance: TypeSet::default() }
}

pub fn to_string(self : CompTy) -> String {
  match self.subs.length() {
    0 => self.master
    _ => self.master + self.subs.stringify('[', ']')
  }
}

pub fn mangling(self: CompTy) -> String {
  let mut content = "_comp_" + self.master + "_"
  for ty in self.subs.types {
    content += ty.mangling()
  }
  content
}

pub fn is_abstract(self : CompTy) -> Bool {
  if self.subs.length() == 0 {
    return false
  }
  self.subs.iter().any(fn(t) { t.is_abstract() })
}

pub fn op_equal(self : CompTy, other : CompTy) -> Bool {
  self.master == other.master && self.subs == other.subs
}

pub fn CompTy::array_ty(ele_ty : Type) -> CompTy {
  let subs = TypeArr::new([ele_ty])
  CompTy::new("Array", subs)
}

fn template_infer(self : CompTy, temp_map : Map[Int, Type]) -> Type {
  let new_subs = TypeArr::default()
  for t in self.subs.types {
    new_subs.push(t.template_infer(temp_map))
  }
  let sty = CompTy::new(self.master, new_subs)
  let ty = Type::CompTy(sty)
  // TODO: check if instance is already exist
  self.instance.add(ty)
  ty
}

// fn has_instance(self: CompTy, tys: TypeArr) -> Bool {
//   self.instance.iter().any(fn(t) { t == tys })
// }
//
// fn get_instance_struct(self: TemplateStructTy, tys: TypeArr) -> CompTy {
//   CompTy::new(self.master, tys)
// }
//
// fn get_instance_ty(self: TemplateStructTy, tys: TypeArr) -> Type {
//   Type::CompTy(self.get_instance_struct(tys))
// }

/// ------------------------------------------------------------------------
/// EnumTy is a type that represents a enum type.
///
/// ### Example:
///
/// - `Option[Int]`
/// - `Result[Int, String]`
/// ------------------------------------------------------------------------
struct EnumTy {
  master : String
  subs : TypeArr
  source : UpperCase?
  instance : TypeSet
}

pub fn EnumTy::new(
  master : String,
  subs : TypeArr,
  ~source : UpperCase? = None
) -> EnumTy {
  EnumTy::{ master, subs, source, instance: TypeSet::default() }
}

pub fn to_string(self : EnumTy) -> String {
  match self.subs.length() {
    0 => self.master
    _ => self.master + self.subs.stringify('[', ']')
  }
}

pub fn mangling(self: EnumTy) -> String {
  let mut content = "_enum_" + self.master + "_"
  for ty in self.subs.types {
    content += ty.mangling()
  }
  content
}

pub fn is_abstract(self : EnumTy) -> Bool {
  self.subs.iter().any(fn(t) { t.is_abstract() })
}

pub fn op_equal(self : EnumTy, other : EnumTy) -> Bool {
  self.master == other.master && self.subs == other.subs
}

pub fn template_infer(self : EnumTy, temp_map : Map[Int, Type]) -> Type {
  let new_subs = TypeArr::default()
  for t in self.subs.types {
    if t.is_abstract() {
      let nt = t.template_infer(temp_map)
      new_subs.push(nt)
    } else {
      new_subs.push(t)
    }
  }
  let ety = EnumTy::new(self.master, new_subs)
  let ty = Type::EnumTy(ety)
  self.instance.add(ty)
  ty
}

/// ------------------------------------------------------------------------
/// TupleTy is a type that represents a tuple type.
///
/// ### example:
///
/// - `(Int, Int)`
/// - `(Int, Double, (Int, Double) -> Double)`
/// ------------------------------------------------------------------------
struct TupleTy {
  types : TypeArr
}

pub fn TupleTy::new(types : TypeArr) -> TupleTy {
  TupleTy::{ types, }
}

pub fn length(self : TupleTy) -> Int {
  self.types.length()
}

pub fn to_string(self : TupleTy) -> String {
  self.types.stringify('(', ')')
}

pub fn mangling(self: TupleTy) -> String {
  let mut content = "_tup_"
  for ty in self.types {
    content += ty.mangling()
  }
  content
}

pub fn is_abstract(self : TupleTy) -> Bool {
  self.types.iter().any(fn(t) { t.is_abstract() })
}

pub fn op_equal(self : TupleTy, other : TupleTy) -> Bool {
  self.types == other.types
}

fn template_infer(self : TupleTy, temp_map : Map[Int, Type]) -> Type {
  let new_tys : TypeArr = TypeArr::new([])
  for t in self.types {
    if t.is_abstract() {
      let nt = t.template_infer(temp_map)
      new_tys.push(nt)
    } else {
      new_tys.push(t)
    }
  }
  Type::TupleTy(TupleTy::new(new_tys))
}

/// -----------------------------------------------------------------------
/// StruTy is a type that represents a struct type.
/// -----------------------------------------------------------------------
struct StruTy {
  master : String
  subs : TypeArr
  source : UpperCase?
  instance : TypeSet
}

pub fn StruTy::new(
  master : String,
  subs : TypeArr,
  ~source : UpperCase? = None
) -> StruTy {
  StruTy::{ master, subs, source, instance: TypeSet::default() }
}

pub fn to_string(self : StruTy) -> String {
  match self.subs.length() {
    0 => self.master
    _ => self.master + self.subs.stringify('[', ']')
  }
}

pub fn mangling(self: StruTy) -> String {
  let mut content = "_stru_" + self.master
  for ty in self.subs.types {
    content += ty.mangling()
  }
  content
}

pub fn is_abstract(self : StruTy) -> Bool {
  self.subs.iter().any(fn(t) { t.is_abstract() })
}

pub fn op_equal(self : StruTy, other : StruTy) -> Bool {
  self.master == other.master && self.subs == other.subs
}

fn template_infer(self : StruTy, temp_map : Map[Int, Type]) -> Type {
  let new_subs = TypeArr::default()
  for t in self.subs.types {
    if t.is_abstract() {
      let nt = t.template_infer(temp_map)
      new_subs.push(nt)
    } else {
      new_subs.push(t)
    }
  }
  let sty = StruTy::new(self.master, new_subs)
  let ty = Type::StruTy(sty)
  self.instance.add(ty)
  ty
}

/// ------------------------------------------------------------------------
/// FuncTy is a type that represents a function type.
///
/// ### example:
///
/// - `(Int, Int) -> Int`
/// - `(Double) -> (Int) -> Double`
///
/// ### Note
///
/// If function is a generic type, its instance field may have value
///
/// for example, for generic function: Array::make, its type is
/// `(Int, TempTy(0)) -> Array[TempTy(0)]`
///
/// when call `Array::make(1, 2)`, the instance field will have value:
/// `(Int, Int) -> Array[Int]`
/// ------------------------------------------------------------------------
struct FuncTy {
  param_types : TypeArr
  ret_type : Type
  source : LowerCase?
  instance : TypeSet
}

pub fn to_string(self : FuncTy) -> String {
  let arg_str = self.param_types.stringify('(', ')')
  arg_str + " -> " + self.ret_type.to_string()
}

pub fn mangling(self: FuncTy) -> String {
  let mut content = "_func_"
  for ty in self.param_types {
    content += ty.mangling()
  }
  content
}

pub fn mangling_with_func_name(self: FuncTy, name: String) -> String {
  let mut func_name = ""
  for c in name {
    if is_uppercase(c) {
      func_name += "_" + to_lowercase(c).to_string()
      continue
    }
    if c == ':' {
      func_name += "_"
      continue
    }
    func_name += c.to_string()
  }
  func_name + self.mangling()
}

pub fn FuncTy::new(
  param_types : TypeArr,
  ret_type : Type,
  ~source : LowerCase? = None
) -> FuncTy {
  FuncTy::{ param_types, ret_type, instance: TypeSet::default(), source }
}

pub fn FuncTy::builtin_not() -> FuncTy {
  FuncTy::new(TypeArr::new([Type::Boolean]), Type::Boolean)
}

pub fn FuncTy::main_func_type() -> FuncTy {
  FuncTy::new(TypeArr::new([]), Type::Unit)
}

pub fn is_abstract(self : FuncTy) -> Bool {
  self.param_types.iter().any(fn(t) { t.is_abstract() }) ||
  self.ret_type.is_abstract()
}

pub fn op_equal(self : FuncTy, other : FuncTy) -> Bool {
  self.param_types == other.param_types && self.ret_type == other.ret_type
}

fn template_infer(self : FuncTy, temp_map : Map[Int, Type]) -> Type {
  let new_param = TypeArr::default()
  for t in self.param_types {
    if t.is_abstract() {
      let nt = t.template_infer(temp_map)
      new_param.push(nt)
    } else {
      new_param.push(t)
    }
  }
  let new_ret = if self.ret_type.is_abstract() {
    self.ret_type.template_infer(temp_map)
  } else {
    self.ret_type
  }
  let fty = FuncTy::new(new_param, new_ret)
  let ty = Type::FuncTy(fty)
  self.instance.add(ty)
  ty
}

/// ---------------------------------------------------
/// TempTy is a type that for Generic Type.
///
/// ---------------------------------------------------
struct TempTy {
  id : Int
  constraints : Array[Constraint]
}

fn TempTy::new(id : Int, constraints : Array[Constraint]) -> TempTy {
  TempTy::{ id, constraints }
}

fn TempTy::default() -> TempTy {
  TempTy::{ id: 0, constraints: [] }
}

fn op_equal(self : TempTy, other : TempTy) -> Bool {
  self.id == other.id
}

fn contains_constrain(self : TempTy, cons : Constraint) -> Bool {
  self.constraints.iter().any(fn(c) { c == cons })
}

fn to_string(self : TempTy) -> String {
  let mut content = "TempTy(" + self.id.to_string()
  if self.constraints.length() > 0 {
    content += ": " + self.constraints[0].to_string()
    content = self.constraints[1:]
      .iter()
      .fold(fn(content, c) { content + ", " + c.to_string() }, init=content)
  }
  content + ")"
}

fn template_infer(self : TempTy, temp_map : Map[Int, Type]) -> Type {
  match temp_map.get(self.id) {
    Some(ty) => ty
    None => {
      println("Cannot infer type, loc: TempTy::template_infer")
      panic()
    }
  }
}

/// ---------------------------------------------------
/// ReasoningTy is a type that is not defined yet.
///
/// Note: ReasoningTy is used for type inference.
/// ---------------------------------------------------
struct ReasoningTy {
  id : Int
  constraints : Array[Constraint]
  rel_ast : Array[Analysis]
  rel_ident : Array[LowerCase]
  mut env : TypeEnv
}

fn ReasoningTy::new(id : Int) -> ReasoningTy {
  ReasoningTy::{
    id,
    constraints: [],
    rel_ast: [],
    rel_ident: [],
    env: TypeEnv::default(),
  }
}

fn to_string(self : ReasoningTy) -> String {
  let mut content = "ReasoningTy(" + self.id.to_string()
  if self.constraints.length() > 0 {
    content += ":" + self.constraints[0].to_string()
    content = self.constraints[1:]
      .iter()
      .fold(fn(content, c) { content + ", " + c.to_string() }, init=content)
  }
  content + ")"
}

fn op_equal(self : ReasoningTy, other : ReasoningTy) -> Bool {
  self.id == other.id && self.constraints == other.constraints
}

fn append_constrain(self : ReasoningTy, cons : Constraint) -> Unit {
  if self.constraints.iter().any(fn(c) { c == cons }) {
    return
  }
  self.constraints.push(cons)
}

/// ---------------------------------------------------
/// Assign a type to ReasoningTy
///
/// Support we have a ReasoningTy `ind`, its rel_ast include
/// `foo()`, `a + b`, and its rel_ident include `a`, `b`.
///
/// Now we have type: Int, which means, the type of `foo()`,
/// `a + b`, `a`, `b` is Int.
///
/// If we got another ReasoningTy `ind2`, which means
/// the type of `foo()`, `a + b`, `a`, `b` should follow with
/// `ind2`.
/// ---------------------------------------------------
fn assign(self : ReasoningTy, ty : Type) -> Unit {
  match ty {
    Type::ReasoningTy(ind) => {
      for ana in self.rel_ast {
        ana.ast_type = ty
        ind.rel_ast.push(ana)
      }
      for ident in self.rel_ident {
        ind.env.var_table.set(ident, ty)
        ind.rel_ident.push(ident)
      }
    }
    _ => {
      self.rel_ast.each(fn(ana) { ana.ast_type = ty })
      self.rel_ident.each(fn(ident) { self.env.var_table.set(ident, ty) })
    }
  }
}

/// ---------------------------------------------------
/// Constraint is a constraint for generic type.
///
/// For example: Ord, Eq, Add, etc.
///
/// Builtin Constraint includes:
///
/// - Ord
/// - Eq
/// - Add
/// - Sub
/// - Mul
/// - Div
/// - Rem
/// - Neg
/// - Not
/// ---------------------------------------------------
struct Constraint {
  name : String // Ord, Eq, Add, etc.
  support : Array[Type]
}

fn Constraint::new(name : String, support : Array[Type]) -> Constraint {
  Constraint::{ name, support }
}

fn op_equal(self : Constraint, other : Constraint) -> Bool {
  self.name == other.name
}

fn hash_combine(self : Constraint, hasher : Hasher) -> Unit {
  hasher.combine_string(self.name)
}

fn to_string(self : Constraint) -> String {
  self.name
}

fn contains(self : Constraint, ty : Type) -> Bool {
  match ty {
    ReasoningTy(_) => true
    TempTy(temp_ty) => temp_ty.contains_constrain(self)
    _ => self.support.iter().any(fn(t) { t == ty })
  }
}

// error constraint is used for error handling
fn Constraint::error() -> Constraint {
  Constraint::new("Error", [])
}

/// ---------------------------------------------------
/// Builtin Constraint
/// ---------------------------------------------------
fn Constraint::constraint_ord() -> Constraint {
  let support = [
    Type::UInt32,
    Type::UInt64,
    Type::Int32,
    Type::Int64,
    Type::Float32,
    Type::Double,
  ]
  Constraint::new("Ord", support)
}

fn Constraint::constraint_eq() -> Constraint {
  let support = [
    Type::UInt32,
    Type::UInt64,
    Type::Int32,
    Type::Int64,
    Type::Float32,
    Type::Double,
    Type::Boolean,
    Type::String,
    Type::Char,
  ]
  Constraint::new("Eq", support)
}

fn Constraint::constraint_add() -> Constraint {
  let support = [
    Type::UInt32,
    Type::UInt64,
    Type::Int32,
    Type::Int64,
    Type::Float32,
    Type::Double,
  ]
  Constraint::new("Add", support)
}

fn Constraint::constraint_sub() -> Constraint {
  let support = [
    Type::UInt32,
    Type::UInt64,
    Type::Int32,
    Type::Int64,
    Type::Float32,
    Type::Double,
  ]
  Constraint::new("Sub", support)
}

fn Constraint::constraint_mul() -> Constraint {
  let support = [
    Type::UInt32,
    Type::UInt64,
    Type::Int32,
    Type::Int64,
    Type::Float32,
    Type::Double,
  ]
  Constraint::new("Mul", support)
}

fn Constraint::constraint_div() -> Constraint {
  let support = [
    Type::UInt32,
    Type::UInt64,
    Type::Int32,
    Type::Int64,
    Type::Float32,
    Type::Double,
  ]
  Constraint::new("Div", support)
}

fn Constraint::constraint_rem() -> Constraint {
  let support = [Type::UInt32, Type::UInt64, Type::Int32, Type::Int64]
  Constraint::new("Rem", support)
}

fn Constraint::constraint_neg() -> Constraint {
  let support = [Type::Int32, Type::Int64, Type::Float32, Type::Double]
  Constraint::new("Neg", support)
}

fn Constraint::constraint_show() -> Constraint {
  let support = [
    Type::UInt32,
    Type::UInt64,
    Type::Int32,
    Type::Int64,
    Type::Float32,
    Type::Double,
    Type::Boolean,
    Type::Char,
    Type::String,
  ]
  Constraint::new("Show", support)
}

fn Constraint::constraint_index() -> Constraint {
  let support = [Type::builtin_array_ty()]
  Constraint::new("Index", support)
}

fn Constraint::builtin_constraints() -> Map[String, Constraint] {
  {
    "Ord": Constraint::constraint_ord(),
    "Eq": Constraint::constraint_eq(),
    "Add": Constraint::constraint_add(),
    "Sub": Constraint::constraint_sub(),
    "Mul": Constraint::constraint_mul(),
    "Div": Constraint::constraint_div(),
    "Rem": Constraint::constraint_rem(),
    "Neg": Constraint::constraint_neg(),
    "Show": Constraint::constraint_show(),
    "Index": Constraint::constraint_index(),
  }
}

/// ---------------------------------------------------
/// ## Type Analysis
/// ---------------------------------------------------
pub struct TypeAnalysis {
  context : Context
  top_env : TypeEnv
  mut curr_env : TypeEnv
  extern_type_table : Map[String, Type]
  err_tokens : Array[Token]
}

/// Note: extern_type_table will store builtin and extern type
/// when `TypeAnalysis::run` check it in context
pub fn TypeAnalysis::new(context : Context) -> TypeAnalysis {
  let env = TypeEnv::default()
  let analysis = TypeAnalysis::{
    context,
    top_env: env,
    curr_env: env,
    extern_type_table: Map::new(),
    err_tokens: Array::new(),
  }
  analysis
}

pub fn set_entry(self : TypeAnalysis, env : TypeEnv) -> Unit {
  self.curr_env = env
}

pub fn find_ident(self : TypeAnalysis, ident : LowerCase) -> Type {
  let ty = match self.curr_env.find_ident(ident) {
    Some(ty) => ty
    None =>
      match self.extern_type_table.get(ident.id) {
        Some(ty) => ty
        None => {
          ident.tok.write_error("Undefined variable: \{ident.id}")
          self.collect_errtok(ident.tok)
          let ty = Type::Undefine
          self.add_ident(ident, ty)
          ty
        }
      }
  }
  match ident.meta().analysis.ast_type {
    Undefine => ident.meta().mark_type(ty)
    _ => ()
  }
  ty
}

pub fn add_ident(self : TypeAnalysis, ident : LowerCase, ty : Type) -> Unit {
  self.curr_env.var_table.set(ident, ty)
}

pub fn collect_errtok(self : TypeAnalysis, err_tok : Token) -> Unit {
  self.err_tokens.push(err_tok)
}

pub fn find_upper_ident(self : TypeAnalysis, ident : UpperCase) -> Type {
  let ty = match self.curr_env.type_defs.get(ident) {
    Some(ty) => ty
    None =>
      match self.extern_type_table.get(ident.id) {
        Some(ty) => ty
        None => {
          ident.tok.write_error("Undefined type")
          self.collect_errtok(ident.tok)
          Type::Undefine
        }
      }
  }
  match ident.meta().analysis.ast_type {
    Undefine => ident.meta().mark_type(ty)
    _ => ()
  }
  ty
}

pub fn add_upper_ident(
  self : TypeAnalysis,
  ident : UpperCase,
  ty : Type
) -> Unit {
  self.curr_env.type_defs.set(ident, ty)
}

// This function assure the constrain is exist
fn get_constraint(
  self : TypeAnalysis,
  constraint_name : String,
  ~source : UpperCase? = None
) -> Constraint {
  match self.context.type_constraints.get(constraint_name) {
    Some(constraint) => constraint
    None =>
      match source {
        Some(source) => {
          source.tok.write_error("Undefined constraint \{constraint_name}")
          self.collect_errtok(source.tok)
          Constraint::error()
        }
        None => {
          println("Undefined constraint \{constraint_name}")
          panic()
        }
      }
  }
}

pub fn match_type(
  self : TypeAnalysis,
  atype : Type,
  btype : Type,
  temp_map : Map[Int, Type],
  errtok : Token
) -> Unit {
  match (atype, btype) {
    (Unit, Unit) => ()
    (Int32, Int32) => ()
    (Int64, Int64) => ()
    (Float32, Float32) => ()
    (Double, Double) => ()
    (Boolean, Boolean) => ()
    (String, String) => ()
    (TempTy(temp_ty), _) =>
      self.match_temp_type(temp_ty, btype, temp_map, errtok)
    (TupleTy(a_tuple), TupleTy(b_tuple)) =>
      self.match_tuple_type(a_tuple, b_tuple, temp_map, errtok)
    (CompTy(a_struct), CompTy(b_struct)) =>
      self.match_struct_type(a_struct, b_struct, temp_map, errtok)
    (FuncTy(a_func), FuncTy(b_func)) =>
      self.match_func_type(a_func, b_func, temp_map, errtok)
    _ => {
      errtok.write_error(
        "Type MisMatch, one is \{atype.to_string()}, the other is \{btype.to_string()}",
      )
      self.collect_errtok(errtok)
    }
  }
}

fn match_temp_type(
  self : TypeAnalysis,
  atype : TempTy,
  btype : Type,
  temp_map : Map[Int, Type],
  errtok : Token
) -> Unit {
  let id = atype.id
  match temp_map.get(id) {
    Some(ty) =>
      if ty != btype {
        errtok.write_error("Type MisMatch, one is \{ty}, the other is \{btype}")
        self.collect_errtok(errtok)
      }
    None => temp_map.set(id, btype)
  }
}

fn match_tuple_type(
  self : TypeAnalysis,
  atype : TupleTy,
  btype : TupleTy,
  temp_map : Map[Int, Type],
  errtok : Token
) -> Unit {
  if atype.types.length() != btype.types.length() {
    errtok.write_error("Type length is not match")
    self.collect_errtok(errtok)
    return
  }
  let elen = atype.length()
  for i = 0; i < elen; i = i + 1 {
    let at = atype.types[i]
    let bt = btype.types[i]
    self.match_type(at, bt, temp_map, errtok)
  }
}

fn match_struct_type(
  self : TypeAnalysis,
  atype : CompTy,
  btype : CompTy,
  temp_map : Map[Int, Type],
  errtok : Token
) -> Unit {
  let a_subs = atype.subs
  let b_subs = btype.subs
  let a_sub_tuple = Type::create_tuple_ty(a_subs)
  let b_sub_tuple = Type::create_tuple_ty(b_subs)
  self.match_type(a_sub_tuple, b_sub_tuple, temp_map, errtok)
}

pub fn match_func_type(
  self : TypeAnalysis,
  atype : FuncTy,
  btype : FuncTy,
  temp_map : Map[Int, Type],
  errtok : Token
) -> Unit {
  let a_param = atype.param_types
  let b_param = btype.param_types
  let a_param_tuple = Type::create_tuple_ty(a_param)
  let b_param_tuple = Type::create_tuple_ty(b_param)
  self.match_type(a_param_tuple, b_param_tuple, temp_map, errtok)
  self.match_type(atype.ret_type, btype.ret_type, temp_map, errtok)
}

/// ---------------------------------------------------
/// ## Type Env
/// ---------------------------------------------------
struct TypeEnv {
  var_table : Map[LowerCase, Type]
  type_defs : Map[UpperCase, Type]
  outer : TypeEnv?
}

fn TypeEnv::default() -> TypeEnv {
  TypeEnv::{ var_table: Map::new(), type_defs: Map::new(), outer: None }
}

fn TypeEnv::new(
  outer : TypeEnv,
  ~var_table : Map[LowerCase, Type] = Map::new(),
  ~type_defs : Map[UpperCase, Type] = Map::new()
) -> TypeEnv {
  TypeEnv::{ var_table, type_defs, outer: Some(outer) }
}

fn to_string(self : TypeEnv) -> String {
  let mut content = ""
  for ident, ty in self.var_table {
    content += "{ " + ident.id + " : " + ty.to_string() + " }\n"
  }
  for ident, ty in self.type_defs {
    content += "{ " + ident.id + " : " + ty.to_string() + " }\n"
  }
  content
}

fn to_json(self : TypeEnv) -> Json {
  let kv : Map[String, String] = Map::new()
  for ident, ty in self.var_table {
    kv.set(ident.id, ty.to_string())
  }
  kv.to_json()
}

fn output(self : TypeEnv, logger : Logger) -> Unit {
  logger.write_string(self.to_string())
}

fn find_ident(self : TypeEnv, ident : LowerCase) -> Type? {
  match self.var_table.get(ident) {
    Some(ty) => Some(ty)
    None =>
      match self.outer {
        Some(outer) => outer.find_ident(ident)
        None => None
      }
  }
}

/// ---------------------------------------------------
/// # analyze Top Decl
/// 
/// ## Did Not used the following function
/// ---------------------------------------------------
// fn analyze_top_decl(self : TypeAnalysis, top_decl : TopDecl) -> Type {
//   match top_decl {
//     FuncDef(func_def) => self.analyze_func_def(func_def)
//     _ => {
//       let token = top_decl.meta().get_head_token()
//       token.write_error("Unimplemented top decl type analysis")
//       self.collect_errtok(token)
//       Type::Undefine
//     }
//   }
// }

/// ---------------------------------------------------
/// # analyze Function Def Type
/// 
/// Note: Only analyze function type from function decl.
/// Did not analyze function body
///
/// use analyze_func_def to analyze function body.
/// ---------------------------------------------------
fn analyze_func_def_type(self : TypeAnalysis, func_def : FuncDef) -> Type {
  let generic_type_map = self.analyze_generic_types(func_def.generics)
  let curr_env = self.curr_env
  let env = TypeEnv::new(
    self.top_env,
    type_defs=Map::from_array(generic_type_map),
  )
  self.set_entry(env)
  let arg_ty = self.analyze_func_param_ty(func_def.params)
  let ret_ty = self.analyze_type_expr(func_def.ret_type)
  let func_ty = Type::create_func_ty(arg_ty, ret_ty, source=Some(func_def.name))
  self.set_entry(curr_env)
  self.add_ident(func_def.name, func_ty)
  func_def.meta().mark_type(func_ty)
  func_ty
}

fn analyze_generic_types(
  self : TypeAnalysis,
  generics : Array[GenericType]
) -> Array[(UpperCase, Type)] {
  let generic_type_map : Array[(UpperCase, Type)] = Array::new()
  for generic in generics {
    let constraints : Array[Constraint] = Array::new()
    for upper in generic.constraints {
      let constraint = self.get_constraint(upper.id)
      constraints.push(constraint)
    }
    let var_ty = TempTy::new(generic_type_map.capacity(), constraints)
    generic_type_map.push((generic.ident, Type::TempTy(var_ty)))
  }
  generic_type_map
}

fn analyze_func_param_ty(
  self : TypeAnalysis,
  func_params : Array[FuncParam]
) -> TypeArr {
  let params_ty : TypeArr = TypeArr::default()
  for param in func_params {
    let ty = self.analyze_type_expr(param.type_annote)
    param.meta().mark_type(ty)
    params_ty.push(ty)
  }
  params_ty
}

/// ---------------------------------------------------
/// ## analyze Func Def
/// 
/// ---------------------------------------------------
fn analyze_func_def(self : TypeAnalysis, func_def : FuncDef) -> Type {
  let func_ty = self.find_ident(func_def.name)
  self.top_env.var_table.set(func_def.name, func_ty)
  guard let Type::FuncTy(fty) = func_ty else {
    _ => {
      println("Error Happened, type is \{func_ty}")
      panic()
    }
  }
  let local_env = TypeEnv::new(self.curr_env)
  func_def.params.each(
    fn(p) { local_env.var_table.set(p.ident, p.meta().analysis.ast_type) },
  )
  let curr_env = self.curr_env
  self.set_entry(local_env)
  let block_ty = self.analyze_block(func_def.body)
  self.set_entry(curr_env)
  if block_ty != fty.ret_type {
    let errtok = func_def.body.meta().get_head_token()
    errtok.write_error(
      "Function return type mismatch, decl type is \{fty.ret_type}, while body type is \{block_ty}",
    )
    self.collect_errtok(errtok)
  }
  func_ty
}

fn analyze_main_func(self : TypeAnalysis, main_func : MainFunc) -> Type {
  let block_ty = self.analyze_block(main_func.body)
  if block_ty != Type::Unit {
    let errtok = main_func.body.meta().get_head_token()
    errtok.write_error("Main function must return Unit, not return \{block_ty}")
    self.collect_errtok(errtok)
  }
  Type::Unit
}

/// ---------------------------------------------------
/// ## analyze Closure
/// 
/// Note: In Minimoonbit, we allow closure definition
/// in the function body.
///
/// For example:
/// fn max(a, b) {
///   if a > b { a } else { b }
/// }
/// fn fact(n) {
///   if n == 1 { 1 } else { n * fact(n - 1) }
/// }
///
/// Type Inference Implementation:
/// when scann the Closure ast, assign parameter type
/// to IndefTy, assign the function type is ReasoningTy -> ReasoningTy
///
/// For example:
/// max : 
/// a: ReasoningTy(0), b: ReasoningTy(1)
/// max: ReasoningTy(0) -> ReasoningTy(1) -> ReasoningTy(0)
/// 
/// Then analyze the function body, change the UVarTy
/// to the real type.
/// For those still UVarTy after analyzing, change them to TempTy
/// Means that they are generic type.
/// max(1, 2) // OK
/// max(1, 2.0) // Error, because 1 and 2.0 are different type.
/// max(1.0, 2.0) // Ok
/// ---------------------------------------------------
fn analyze_closure(self : TypeAnalysis, closure : Closure) -> Type {
  let params_ty = TypeArr::default()
  let reasoning_cnt : Ref[Int] = Ref::new(0)
  for param in closure.params {
    let ty = self.analyze_closure_param(param, reasoning_cnt)
    params_ty.push(ty)
  }
  let ret_ty = match closure.ret_type {
    Some(ret_type) => self.analyze_type_expr(ret_type)
    None => Type::create_reasoning(reasoning_cnt.val)
  }
  let func_ty = Type::create_func_ty(params_ty, ret_ty)
  let curr_env = self.curr_env
  let local_env = TypeEnv::new(curr_env)
  // set ty in params_ty to local_env
  for i, param in closure.params {
    let ty = params_ty[i]
    match ty {
      Type::ReasoningTy(reasoning) => reasoning.env = local_env
      _ => ()
    }
    local_env.var_table.set(param.ident, ty)
  }
  match closure.name {
    Some(name) => local_env.var_table.set(name, func_ty)
    None => ()
  }
  let curr_env = self.curr_env
  self.set_entry(local_env)
  let block_ty = self.analyze_block(closure.body)

  // change parameters type and function type based on the local_env
  params_ty.clear()
  for param in closure.params {
    let ty = match self.find_ident(param.ident) {
      Type::ReasoningTy(reasoning) =>
        Type::create_temp_ty(reasoning.id, constraints=reasoning.constraints)
      ty => ty
    }
    params_ty.push(ty)
    param.meta().analysis.ast_type = ty
  }
  let ret_ty = match block_ty {
    Type::ReasoningTy(reasoning) =>
      Type::create_temp_ty(reasoning.id, constraints=reasoning.constraints)
    _ => block_ty
  }
  // let ty = Type::FuncTy(params_ty, ret_ty)
  let ty = Type::create_func_ty(params_ty, ret_ty)
  closure.meta().analysis.ast_type = ty
  self.set_entry(curr_env)
  match closure.name {
    Some(name) => {
      self.curr_env.var_table.set(name, ty)
      name.meta().mark_type(ty)
    }
    None => ()
  }
  ty
}

fn analyze_closure_param(
  self : TypeAnalysis,
  param : ClosureParam,
  reasoning_cnt : Ref[Int]
) -> Type {
  let ty = match param.type_annote {
    Some(type_expr) => self.analyze_type_expr(type_expr)
    None => {
      let ty = Type::create_reasoning(reasoning_cnt.val)
      reasoning_cnt.val += 1
      ty
    }
  }
  ty
}

/// ---------------------------------------------------
/// ## analyze Type Expr
/// 
/// ---------------------------------------------------
fn analyze_type_expr(self : TypeAnalysis, type_expr : TypeExpr) -> Type {
  match type_expr {
    IdentType(ident_type) => self.find_upper_ident(ident_type)
    TupleType(tuple_type) => self.analyze_tuple_type(tuple_type)
    ApplyType(apply_type) => self.analyze_apply_type(apply_type)
    FuncType(func_type) => self.analyze_func_type(func_type)
  }
}

fn analyze_tuple_type(self : TypeAnalysis, tuple_type : TupleType) -> Type {
  let ty_arr = tuple_type.types.map(fn(ty) { self.analyze_type_expr(ty) })
  let ty_arr = TypeArr::new(ty_arr)
  let ty = Type::create_tuple_ty(ty_arr)
  tuple_type.meta().mark_type(ty)
  ty
}

fn analyze_apply_type(self : TypeAnalysis, apply_type : ApplyType) -> Type {
  let master_ty = self.find_upper_ident(apply_type.type_name)
  if not(master_ty.is_abstract()) {
    let errtok = apply_type.meta().get_head_token()
    errtok.write_error("Cannot apply type to non-abstract type")
    self.collect_errtok(errtok)
    return Type::Undefine
  }
  let ele_tys = apply_type.type_args.map(fn(ty) { self.analyze_type_expr(ty) })
  let temp_map : Map[Int, Type] = Map::new()
  for i, ety in ele_tys {
    temp_map.set(i, ety)
  }
  let ty = master_ty.template_infer(temp_map)
  apply_type.meta().mark_type(ty)
  ty
}

fn analyze_func_type(self : TypeAnalysis, func_type : FuncType) -> Type {
  let arg_ty = self.analyze_type_expr(func_type.arg_type)
  let ret_ty = self.analyze_type_expr(func_type.ret_type)
  let arg_ty = match arg_ty {
    Type::TupleTy(ty_arr) => ty_arr.types
    _ => TypeArr::new([arg_ty])
  }
  let ty = Type::create_func_ty(arg_ty, ret_ty)
  func_type.meta().analysis.ast_type = ty
  ty
}

/// ---------------------------------------------------
/// ## analyze While Expr Type
/// 
/// ---------------------------------------------------
fn analyze_while_expr(self : TypeAnalysis, while_expr : WhileExpr) -> Type {
  let cond_ty = self.analyze_expr(while_expr.cond_expr)
  if cond_ty != Type::Boolean {
    let errtok = while_expr.cond_expr.meta().get_head_token()
    errtok.write_error(
      "Condition expression should be boolean, while this is \{cond_ty}",
    )
    self.collect_errtok(errtok)
  }
  let _ = self.analyze_block(while_expr.body)
  let ty = Type::Unit
  while_expr.meta().mark_type(ty)
  ty
}

/// ---------------------------------------------------
/// ## analyze If Expr Type
/// 
/// ---------------------------------------------------
fn analyze_if_expr(self : TypeAnalysis, if_expr : IfExpr) -> Type {
  let cond_ty = self.analyze_expr(if_expr.cond_expr)
  if cond_ty != Type::Boolean {
    let errtok = if_expr.cond_expr.meta().get_head_token()
    errtok.write_error(
      "Condition expression should be boolean, while this is \{cond_ty}",
    )
    self.collect_errtok(errtok)
  }
  let then_ty = self.analyze_block(if_expr.then_expr)
  match if_expr.else_clause {
    Some(else_clause) => {
      let else_ty = self.analyze_else_clause(else_clause)
      if then_ty != else_ty {
        let errtok = else_clause.meta().get_head_token()
        errtok.write_error(
          "Type Mismatch, this if expr then block is \{then_ty}, else block is \{else_ty}",
        )
        self.collect_errtok(errtok)
      }
      then_ty
    }
    None => Type::Unit
  }
}

fn analyze_else_clause(self : TypeAnalysis, else_clause : ElseClause) -> Type {
  match else_clause {
    Block(block_expr) => self.analyze_block(block_expr)
    IfExpr(if_expr) => self.analyze_if_expr(if_expr)
  }
}

/// ---------------------------------------------------
/// ## analyze Match Expr Type
/// 
/// ---------------------------------------------------
// write error directly
fn analyze_match_expr(self : TypeAnalysis, match_expr : MatchExpr) -> Type {
  let errtok = match_expr.meta().get_head_token()
  errtok.write_error("Unimplemented match expr type analysis")
  self.collect_errtok(errtok)
  Type::Undefine
}

/// ---------------------------------------------------
/// ## analyze Block Expr Type
/// 
/// ---------------------------------------------------
fn analyze_block(self : TypeAnalysis, block_expr : BlockExpr) -> Type {
  let mut ty = Type::Unit
  let curr_env = self.curr_env
  let local_env = TypeEnv::new(curr_env)
  self.set_entry(local_env)
  block_expr.state_exprs.each(fn(state) { ty = self.analyze_state(state) })
  self.set_entry(curr_env)
  block_expr.meta().mark_type(ty)
  ty
}

/// ---------------------------------------------------
/// ## analyze State Expr Type
/// 
/// StateExpr includes:
///
///     - LetExpr
///     - WhileExpr
///     - Break
///     - Continue
///     - Return
///     - Expr
///     - Null
/// ---------------------------------------------------
fn analyze_state(self : TypeAnalysis, state : StateExpr) -> Type {
  let ty = match state {
    Let(let_expr) => self.analyze_let_expr(let_expr)
    Assign(assign_expr) => self.analyze_assign(assign_expr)
    Expr(expr) => self.analyze_expr(expr)
    While(while_expr) => self.analyze_while_expr(while_expr)
    Break(_) => Type::Unit
    Continue(_) => Type::Unit
    Null(_) => Type::Unit
    _ => {
      let errtok = state.meta().get_head_token()
      errtok.write_error("Unimplemented state")
      self.collect_errtok(errtok)
      Type::Undefine
    }
  }
  state.meta().analysis.ast_type = ty
  ty
}

/// ---------------------------------------------------
/// ## analyze AssignExpr Type
/// 
/// ---------------------------------------------------
fn analyze_assign(self : TypeAnalysis, assign_expr : AssignExpr) -> Type {
  // check left value is valid
  let left = assign_expr.left
  let left_ty = self.analyze_left_value(left)
  let right_ty = self.analyze_expr(assign_expr.right)
  if left_ty != right_ty {
    let errtok = assign_expr.assign_op.tok
    errtok.write_error(
      "Type Mismatch, left value is \{left_ty}, right value is \{right_ty}",
    )
    self.collect_errtok(errtok)
  }
  let ty = Type::Unit
  assign_expr.meta().mark_type(ty)
  ty
}

fn analyze_left_value(self : TypeAnalysis, left_value : ApplyExpr) -> Type {
  match left_value {
    CallExpr(_) => {
      let errtok = left_value.meta().get_head_token()
      errtok.write_error("Function call could not be left value")
      self.collect_errtok(errtok)
    }
    Access(_) => ()
    Atomic(atom_expr) =>
      match atom_expr {
        Ident(_) | TupleExpr(_) => ()
        Literal(_) => {
          let errtok = left_value.meta().get_head_token()
          errtok.write_error("Literal could not be left value")
          self.collect_errtok(errtok)
        }
        ArrayExpr(_) => {
          let errtok = left_value.meta().get_head_token()
          errtok.write_error("Array Literal could not be left value")
          self.collect_errtok(errtok)
        }
        _ => {
          let errtok = left_value.meta().get_head_token()
          errtok.write_error("This could not be left value")
          self.collect_errtok(errtok)
        }
      }
  }
  self.analyze_apply_expr(left_value)
}

/// ---------------------------------------------------
/// ## analyze LetExpr Type
/// 
/// Note: LetExpr itself has only Unit type.
/// but it will define or shadow some variables.
/// 
/// let_expr: 'let' as_pat type_expr '=' Expr
/// as_pat: atom_pat ('as' LowerCase)?
/// atom_pat: LowerCase | Literal | TuplePat
/// ---------------------------------------------------
// TODO: check if the type_expr is valid
fn analyze_let_expr(self : TypeAnalysis, let_expr : LetExpr) -> Type {
  let expr_type = self.analyze_expr(let_expr.expr)
  match let_expr.type_annote {
    Some(type_expr) => {
      let type_expr_type = self.analyze_type_expr(type_expr)
      if type_expr_type != expr_type {
        let errtok = type_expr.meta().get_head_token()
        let expected = type_expr_type
        let got = expr_type
        errtok.write_error(
          "Type Mismatch, left is \{expected}, while get: \{got}",
        )
        self.collect_errtok(errtok)
      }
    }
    None => ()
  }
  let as_pat = let_expr.pat
  self.assign_type_to_as_pat(as_pat, expr_type)
  Type::Unit
}

fn assign_type_to_as_pat(
  self : TypeAnalysis,
  as_pat : AsPat,
  right_ty : Type
) -> Unit {
  let atom_pat = as_pat.atom_pat
  as_pat.meta().mark_type(right_ty)
  match (atom_pat, right_ty) {
    (Ident(id), _) => {
      self.add_ident(id, right_ty)
      id.meta().mark_type(right_ty)
      atom_pat.meta().mark_type(right_ty)
    }
    (Literal(l), _) => {
      let err = l.meta().get_head_token()
      err.write_error("Literal Could not be Let Pattern")
      self.collect_errtok(err)
    }
    (WildCard(w), _) => {
      w.meta().mark_type(right_ty)
      atom_pat.meta().mark_type(right_ty)
    }
    (Tuple(tuple_pat), TupleTy(ty_arr)) => {
      let as_pats = tuple_pat.as_pats
      if as_pats.length() != ty_arr.length() {
        let errtok = tuple_pat.meta().get_head_token()
        errtok.write_error("Tuple Pattern Mismatch")
        self.collect_errtok(errtok)
      }
      loop as_pats[0:], ty_arr.types.types[0:] {
        [], [] => break
        as_pats, ty_arr => {
          let (pat, ty) = (as_pats[0], ty_arr[0])
          self.assign_type_to_as_pat(pat, ty)
          continue as_pats[1:], ty_arr[1:]
        }
      }
    }
    (Tuple(_), _) => {
      let errtok = as_pat.meta().get_head_token()
      errtok.write_error(
        "Tuple Pattern Mismatch, could not match with \{right_ty}",
      )
      self.collect_errtok(errtok)
    }
  }
}

/// ---------------------------------------------------
/// ## analyze Tuple Expr Type
/// 
/// ---------------------------------------------------
fn analyze_tuple_expr(self : TypeAnalysis, tuple_expr : TupleExpr) -> Type {
  let ty_arr = TypeArr::default()
  for expr in tuple_expr.exprs {
    let ty = self.analyze_expr(expr)
    ty_arr.push(ty)
  }
  let ty = Type::create_tuple_ty(ty_arr)
  tuple_expr.meta().mark_type(ty)
  ty
}

/// ---------------------------------------------------
/// ## analyze Array Expr Type
/// 
/// Note: All types in the array must be the same.
/// ---------------------------------------------------
fn analyze_array_expr(self : TypeAnalysis, array_expr : ArrayExpr) -> Type {
  let ty_arr = []
  for expr in array_expr.exprs {
    let ty = self.analyze_expr(expr)
    ty_arr.push(ty)
  }
  // TODO: implement [] type inference
  if ty_arr.is_empty() {
    let errtok = array_expr.meta().get_head_token()
    errtok.write_error("Array Type Inference is Unimplemented yet")
    self.collect_errtok(errtok)
    return Type::Undefine
  }
  let ele_ty = ty_arr[0]
  for ty in ty_arr[1:] {
    if ty != ele_ty {
      let errtok = array_expr.meta().get_head_token()
      errtok.write_error(
        "Array Type Mismatch, first element is \{ele_ty}, while this is \{ty}",
      )
      self.collect_errtok(errtok)
      return Type::Undefine
    }
  }
  let ty = Type::create_array_ty(ele_ty)
  array_expr.meta().mark_type(ty)
  ty
}

/// ---------------------------------------------------
/// ## analyze Expr Type
/// 
/// ---------------------------------------------------
fn analyze_expr(self : TypeAnalysis, expr : Expr) -> Type {
  let ty = match expr {
    Apply(apply_expr) => self.analyze_apply_expr(apply_expr)
    Binary(binary_expr) => self.analyze_binary(binary_expr)
    IfExpr(if_expr) => self.analyze_if_expr(if_expr)
    Match(match_expr) => self.analyze_match_expr(match_expr)
    Block(block_expr) => self.analyze_block(block_expr)
  }
  expr.meta().mark_type(ty)
  ty
}

/// ---------------------------------------------------
/// ## analyze BinaryExpr Type
/// 
/// For All BinaryExpr, the type of left and right
/// must be same.
/// For normal operator: +, -, *, /
/// The result type is the same as the input type.
/// For comparison operator: ==, !=, <, >, <=, >=
/// The result type is Boolean.
/// ---------------------------------------------------
fn analyze_binary(self : TypeAnalysis, binary_expr : BinaryExpr) -> Type {
  let left_ty = self.analyze_expr(binary_expr.left)
  let right_ty = self.analyze_expr(binary_expr.right)
  let (left_ty, right_ty) = infer_reasoning_for_binary(left_ty, right_ty)
  let op_tok = binary_expr.op.tok
  if left_ty != right_ty {
    op_tok.write_error(
      "Type mismatch, left is \{left_ty}, while right is \{right_ty}",
    )
    self.collect_errtok(op_tok)
    return Type::Undefine
  }
  let ty = match op_tok.token_type {
    TokenType::Operator("*") => self.check_ty_constrain(left_ty, "Mul", op_tok)
    TokenType::Operator("/") => self.check_ty_constrain(left_ty, "Div", op_tok)
    TokenType::Operator("+") => self.check_ty_constrain(left_ty, "Add", op_tok)
    TokenType::Operator("-") => self.check_ty_constrain(left_ty, "Sub", op_tok)
    TokenType::Operator("<") => {
      let _ = self.check_ty_constrain(left_ty, "Ord", op_tok)
      Type::Boolean
    }
    TokenType::Operator(">") => {
      let _ = self.check_ty_constrain(left_ty, "Ord", op_tok)
      Type::Boolean
    }
    TokenType::Operator("==") => {
      let _ = self.check_ty_constrain(left_ty, "Eq", op_tok)
      Type::Boolean
    }
    TokenType::Operator("!=") => {
      let _ = self.check_ty_constrain(left_ty, "Eq", op_tok)
      Type::Boolean
    }
    TokenType::Operator("<=") => {
      let _ = self.check_ty_constrain(left_ty, "Ord", op_tok)
      let _ = self.check_ty_constrain(left_ty, "Eq", op_tok)
      Type::Boolean
    }
    TokenType::Operator(">=") => {
      let _ = self.check_ty_constrain(left_ty, "Ord", op_tok)
      let _ = self.check_ty_constrain(left_ty, "Eq", op_tok)
      Type::Boolean
    }
    TokenType::Operator("&&") => {
      if left_ty != Type::Boolean {
        op_tok.write_error("Type mismatch, \{left_ty} must be Boolean")
        self.collect_errtok(op_tok)
      }
      Type::Boolean
    }
    TokenType::Operator("||") => {
      if left_ty != Type::Boolean {
        op_tok.write_error("Type mismatch, \{left_ty} must be Boolean")
        self.collect_errtok(op_tok)
      }
      Type::Boolean
    }
    _ => {
      op_tok.write_error("This operator is Unimplemented yet")
      self.collect_errtok(op_tok)
      Type::Undefine
    }
  }
  binary_expr.meta().mark_type(ty)
  ty
}

/// This function will check if the type satisfy the constraint
/// It will return the type if satisfy, otherwise return Undefine
fn check_ty_constrain(
  self : TypeAnalysis,
  ty : Type,
  constrain_name : String,
  errtok : Token
) -> Type {
  let constraint = self.get_constraint(constrain_name)
  match ty {
    Type::ReasoningTy(reasoning) => reasoning.append_constrain(constraint)
    _ => ()
  }
  if not(constraint.contains(ty)) {
    errtok.write_error(
      "Type Constrain Mismatch, type \{ty} not satisfy constraint: \{constraint}",
    )
    Type::Undefine
  } else {
    ty
  }
}

fn infer_reasoning_for_binary(
  left_type : Type,
  right_type : Type
) -> (Type, Type) {
  match (left_type, right_type) {
    (Type::ReasoningTy(l), _) => {
      l.assign(right_type)
      (right_type, right_type)
    }
    (_, Type::ReasoningTy(r)) => {
      r.assign(left_type)
      (left_type, left_type)
    }
    _ => (left_type, right_type)
  }
}

/// ---------------------------------------------------
/// ## analyze UnaryExpr Type
/// ---------------------------------------------------
fn analyze_unary_expr(self : TypeAnalysis, unary_expr : UnaryExpr) -> Type {
  let ty = self.analyze_expr(unary_expr.expr)
  let op_tok = unary_expr.uop.tok
  let ty = match op_tok.token_type {
    TokenType::Operator("-") => {
      let _ = self.check_ty_constrain(ty, "Neg", op_tok)
      ty
    }
    _ => {
      op_tok.write_error("This operator is Unimplemented yet")
      self.collect_errtok(op_tok)
      Type::Undefine
    }
  }
  unary_expr.meta().mark_type(ty)
  ty
}

/// ---------------------------------------------------
/// ## analyze ApplyExpr Type
/// ---------------------------------------------------
fn analyze_apply_expr(self : TypeAnalysis, apply_expr : ApplyExpr) -> Type {
  let ty = match apply_expr {
    Atomic(atom_expr) => self.analyze_atom_expr(atom_expr)
    Access(access_expr) => self.analyze_access_expr(access_expr)
    CallExpr(call_expr) => self.analyze_call_expr(call_expr)
  }
  match ty {
    Type::ReasoningTy(ind) => ind.rel_ast.push(apply_expr.meta().analysis)
    _ => ()
  }
  apply_expr.meta().analysis.ast_type = ty
  ty
}

/// ---------------------------------------------------
/// ## analyze Access Expr
/// ---------------------------------------------------
fn analyze_access_expr(self : TypeAnalysis, access_expr : AccessExpr) -> Type {
  let ty = self.analyze_apply_expr(access_expr.apply_expr)
  let accessor = access_expr.accessor
  let ty = match accessor {
    TupleAcc(_) => {
      let errtok = accessor.meta().get_head_token()
      errtok.write_error("Tuple Accessor is Unimplemented yet")
      self.collect_errtok(errtok)
      Type::Undefine
    }
    FieldAcc(_) => {
      let errtok = accessor.meta().get_head_token()
      errtok.write_error("Field Accessor is Unimplemented yet")
      self.collect_errtok(errtok)
      Type::Undefine
    }
    ArrayAcc(expr) =>
      if ty.is_array() {
        let index_ty = self.analyze_expr(expr)
        if index_ty != Type::Int32 {
          let errtok = expr.meta().get_head_token()
          errtok.write_error(
            "Array Index must be Int32, while this is \{index_ty}",
          )
          self.collect_errtok(errtok)
        }
        guard let Type::CompTy(sty) = ty
        sty.subs[0]
      } else {
        let errtok = accessor.meta().get_head_token()
        errtok.write_error("Non Array Access is Unimplemented yet")
        self.collect_errtok(errtok)
        Type::Undefine
      }
  }
  access_expr.meta().mark_type(ty)
  ty
}

/// ---------------------------------------------------
/// ## analyze CallExpr
/// ---------------------------------------------------
fn analyze_call_expr(self : TypeAnalysis, call_expr : CallExpr) -> Type {
  let caller = call_expr.apply_expr
  let caller_type = self.analyze_apply_expr(caller)
  let (param_tys, _) = match caller_type {
    FuncTy(fty) => (fty.param_types, fty.ret_type)
    _ => {
      let errtok = caller.meta().get_head_token()
      errtok.write_error(
        "Trying to Call a Non-Function, type is \{caller_type.stringify()}",
      )
      self.collect_errtok(errtok)
      return Type::Undefine
    }
  }
  let args = call_expr.args
  let args_ty : TypeArr = TypeArr::default()
  args.each(fn(arg) { args_ty.push(self.analyze_expr(arg)) })
  let param_tuple = Type::create_tuple_ty(param_tys)
  let arg_tuple = Type::create_tuple_ty(args_ty)
  let temp_map : Map[Int, Type] = Map::new()
  self.match_type(
    param_tuple,
    arg_tuple,
    temp_map,
    call_expr.meta().get_head_token(),
  )
  let caller_type = caller_type.template_infer(temp_map)
  caller.meta().mark_type(caller_type)
  guard let FuncTy(caller_ty) = caller_type
  let ret_ty = caller_ty.ret_type
  call_expr.meta().mark_type(ret_ty)
  ret_ty
}

/// ---------------------------------------------------
/// ## analyze AtomExpr Type
/// ---------------------------------------------------
fn analyze_atom_expr(self : TypeAnalysis, atom_expr : AtomExpr) -> Type {
  let ty = match atom_expr {
    Literal(literal) => self.analyze_literal(literal)
    Ident(ident) => self.find_ident(ident)
    Paren(expr) => self.analyze_expr(expr)
    UnaryExpr(unary_expr) => self.analyze_unary_expr(unary_expr)
    Method(method_expr) => self.find_ident(method_expr.ident)
    TupleExpr(tuple_expr) => self.analyze_tuple_expr(tuple_expr)
    ArrayExpr(array_expr) => self.analyze_array_expr(array_expr)
    Closure(closure) => self.analyze_closure(closure)
    Unit(_) => Type::Unit
    _ => {
      let errtok = atom_expr.meta().get_head_token()
      errtok.write_error("Unimplemented Atom Expression")
      self.collect_errtok(errtok)
      Type::Undefine
    }
  }
  match ty {
    Type::ReasoningTy(ind) => {
      ind.rel_ast.push(atom_expr.meta().analysis)
      match atom_expr {
        Ident(ident) => ind.rel_ident.push(ident)
        _ => ()
      }
    }
    _ => ()
  }
  ty
}

/// ---------------------------------------------------
/// ## analyze Literal Type
/// ---------------------------------------------------
fn analyze_literal(self : TypeAnalysis, literal : Literal) -> Type {
  let ty = match literal.category {
    Boolean(_) => Type::Boolean
    Double(_) => Type::Double
    Integer(_) => Type::Int32
    Long(_) => Type::Int64
    Unsigned(_) => Type::UInt32
    UnsignedLong(_) => Type::UInt64
    Char(_) => Type::Char
    String(_) => Type::String
  }
  literal.meta().mark_type(ty)
  ty
}
