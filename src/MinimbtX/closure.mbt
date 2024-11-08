/// -----------------------------------------------------
/// Closure Context
/// -----------------------------------------------------
pub struct ClosureContext {
  context: Context
  extern_funcs : Map[Name, LowType]
  builtin_funcs : Map[Name, LowType]
  direct_funcs : Map[Name, LowType]
  global_vars  : Map[Name, (LowType, ClosureIR)]
  mut prog: ClosureProgram?
}

pub fn ClosureContext::new(context: Context) -> ClosureContext {
  ClosureContext::{
    context,
    extern_funcs: Map::new(),
    builtin_funcs: Map::new(),
    direct_funcs: Map::new(),
    global_vars: Map::new(),
    prog: None,
  }
}

pub fn ClosureContext::init(self: ClosureContext) -> Unit {
  let knf_context = match self.context.knf_context {
    Some(knf_context) => knf_context
    None => {
      println("Didn't pass knf conversion phrase!")
      panic()
    }
  }
  for _, name_ty in knf_context.extern_table {
    let (name, ty) = name_ty
    match ty {
      Type::FuncTy(_) => self.extern_funcs.set(name, ty.to_lowtype())
      _ => ()
    }
  }
  for _, name_ty in knf_context.builtin_table {
    let (name, ty) = name_ty
    match ty {
      Type::FuncTy(_) => self.builtin_funcs.set(name, ty.to_lowtype())
      _ => ()
    }
  }
  let knf = knf_context.knf

  fn collect(knf: Knf) -> Unit {
    match knf {
      Let((name, ty), sub_knf, next_knf) => {
        let ty = ty.to_lowtype()
        let value_ir = knf_to_closure_top_level(sub_knf)
        self.global_vars.set(name, (ty, value_ir))
        collect(next_knf)
      }
      LetTuple(_, _, next) => collect(next)
      // All Top Level LetRec declare direct function
      LetRec(knf_fdef, _, next) => {
        let fty = knf_fdef.func_type.to_lowtype()
        let fname = knf_fdef.name
        self.direct_funcs.set(fname, fty)
        collect(next)
      }
      Unit => return
      _ => {
        println("Top Level should be let, lettuple, letrec or Unit!")
        panic()
      }
    }
  }
  collect(knf)
}

/// -----------------------------------------------------
/// Closure Program
/// -----------------------------------------------------
pub struct ClosureProgram {
  context: ClosureContext
  func_defs : Array[ClosureIRFuncDef]
  global_vars : Map[Name, (LowType, ClosureIR)]
  mut body : ClosureIR // for main function
}

// run after init
pub fn createProgram(self: ClosureContext) -> ClosureProgram {
  match self.prog {
    None => ()
    Some(p) => {
      println("Program is already created!")
      return p
    }
  }

  let prog = ClosureProgram::{
    context: self,
    func_defs: Array::new(),
    global_vars: self.global_vars,
    body: ClosureIR::Unit
  }

  fn deal_func_def(knf: Knf) -> Unit {
    match knf {
      Let(_, _, next) => deal_func_def(next)
      LetTuple(_, _, next) => deal_func_def(next)
      LetRec(knf_fdef, body, next) => {
        let func_ty = knf_fdef.func_type.to_lowtype()
        let params = knf_fdef.params.map(fn(param) {
          let (name, ty) = param
          (name, ty.to_lowtype(closurefy=true))
        })
        let func_name = knf_fdef.name
        let func_def = prog.createTopFuncDef(
          func_name, func_ty, params, ClosureIR::Unit
        )
        let env = func_def.createEnv()
        let closure_body = knf_to_closure(body, env)
        if func_name.to_string() == "main" {
          prog.body = closure_body
        } else {
          let func_def = ClosureIRFuncDef::{
            ..func_def,
            body: closure_body,
          }
          prog.add_func_def(func_def)
        }
        deal_func_def(next)
      }
      Unit => return
      _ => {
        println("Top Level should be let, lettuple, letrec or Unit!, loc: createProgram")
        panic()
      }
    }
  }
  let knf = match self.context.knf_context {
    Some(knf_context) => knf_context.knf
    None => {
      println("Didn't pass knf conversion phrase!")
      panic()
    }
  }
  deal_func_def(knf)
  self.prog = Some(prog)
  prog
}

fn add_func_def(
  self: ClosureProgram, func_def: ClosureIRFuncDef
) -> Unit {
  let name = func_def.fname
  for fd in self.func_defs {
    let fname = fd.fname
    if name == fname {
      println("Function \{name} is already defined")
      panic()
    }
  }
  self.func_defs.push(func_def)
}

fn find_func_type(self : ClosureContext, name : Name) -> LowType? {
  match self.builtin_funcs.get(name) {
    Some(ty) => Some(ty)
    None => match self.direct_funcs.get(name) {
      Some(ty) => Some(ty)
      None => match self.extern_funcs.get(name) {
        Some(ty) => Some(ty)
        None => None
      }
    }
  }
}

fn find_global_var_type(self: ClosureContext, name: Name) -> LowType? {
  match self.global_vars.get(name) {
    Some((ty, _)) => Some(ty)
    None => match self.find_func_type(name) {
      Some(ty) => Some(ty)
      None => None
    }
  }
}

pub fn to_string(self: ClosureProgram) -> String {
  let func_defs_str = self.func_defs.map(fn(fd) { fd.to_string() }).join("\n")
  let mut global_vars_str = ""
  for name, pair in self.context.global_vars {
    let (ty, value) = pair
    global_vars_str += "global var : \{name} : \{ty} = \{value}\n"
  }
  let main_body = "def main {\n" + self.body.stringify(1)
  "Global Variables: [\{global_vars_str}]\n\n" + func_defs_str + "\n" + main_body + "}"

}

/// -----------------------------------------------------
/// ## ClosureIRFuncDef
/// -----------------------------------------------------
pub struct ClosureIRFuncDef {
  prog: ClosureProgram
  // if function define inside function, it has parent
  parent: ClosureIRFuncDef?
  // its name
  fname: Name
  // function params
  params: Array[(Name, LowType)]
  // function return type
  ret_type: LowType
  // function type, it has two possibilities: ClosureFn or DirectFn
  func_type: LowType
  // free variables
  free_vars: Array[(Name, LowType)]
  // all variables defined in this function
  vars: Map[Name, LowType]
  // function body
  body: ClosureIR
}

pub fn to_string(self: ClosureIRFuncDef) -> String {
  let free_vars_str = self.free_vars.map(fn(pair) {
    let (name, ty) = pair
    "\{name}: \{ty}"
  }).join(", ")
  let free_vars_str = match free_vars_str {
    "" => ""
    _ => "[\{free_vars_str}]"
  }
  let args_str = self.params
    .map(fn(pair) { pair.0.to_string() + " : " + pair.1.to_string() })
    .join(", ")
  let ret_type = self.ret_type.to_string()
  "def \{self.fname}\{free_vars_str}(\{args_str}) -> \{ret_type} {\n" +
  self.body.stringify(1) + "}\n"
}

pub fn ClosureIRFuncDef::new(
  prog: ClosureProgram,
  fname: Name,
  func_type: LowType,
  ~parent: ClosureIRFuncDef? = None,
) -> ClosureIRFuncDef {
  let ret_type = match func_type {
    LowType::ClosureFn(_, ret) => ret
    LowType::DirectFn(_, ret) => ret
    _ => {
      println("Function Type should be ClosureFn or DirectFn, now it is \{func_type}, loc: ClosureIRFuncDef::new")
      panic()
    }
  }
  ClosureIRFuncDef::{
    prog: prog,
    parent: parent,
    fname: fname,
    params: Array::new(),
    ret_type,
    func_type: func_type,
    free_vars: Array::new(),
    vars: Map::new(),
    body: ClosureIR::Unit
  }
}

pub fn createTopFuncDef(
  self: ClosureProgram,
  fname: Name,
  func_type: LowType,
  params: Array[(Name, LowType)],
  body: ClosureIR,
  ~vars: Map[Name, LowType] = Map::new(),
  ~free_vars: Array[(Name, LowType)] = Array::new()
) -> ClosureIRFuncDef {
  let ret_type = match func_type {
    LowType::ClosureFn(_, ret) => ret
    LowType::DirectFn(_, ret) => ret
    _ => {
      println("Function Type should be ClosureFn or DirectFn, now it is \{func_type}, loc: createTopFuncDef")
      panic()
    }
  }
  for param in params {
    let (param_name, param_ty) = param
    vars.set(param_name, param_ty)
  }
  ClosureIRFuncDef::{
    prog: self,
    parent: None,
    fname,
    params,
    ret_type,
    func_type,
    free_vars,
    vars,
    body,
  }
}

pub fn createInnerFuncDef(
  self: ClosureIRFuncDef,
  fname: Name, func_type: LowType, params: Array[(Name, LowType)]
) -> ClosureIRFuncDef {
  let ret_type = match func_type {
    LowType::ClosureFn(_, ret) => ret
    LowType::DirectFn(_, ret) => ret
    _ => {
      println("Function Type should be ClosureFn or DirectFn, now it is \{func_type}, loc: createInnerFuncDef")
      panic()
    }
  }
  let vars = Map::new()
  for param in params {
    let (param_name, param_ty) = param
    vars.set(param_name, param_ty)
  }
  ClosureIRFuncDef::{
    prog: self.prog,
    parent: Some(self),
    fname: fname,
    params,
    ret_type,
    func_type: func_type,
    free_vars: Array::new(),
    vars,
    body: ClosureIR::Unit
  }
}

pub fn get_var(self: ClosureIRFuncDef, name: Name) -> LowType? {
  match self.vars.get(name) {
    Some(ty) => Some(ty)
    None => match self.parent {
      Some(parent) => {
        match parent.get_var(name) {
          Some(ty) => {
            self.free_vars.push((name, ty))
            Some(ty)
          }
          None => None
        }
      }
      None => None
    }
  }
}

/// -----------------------------------------------------
/// Closure Name Env
///
/// Used for check free vars in function
/// -----------------------------------------------------
pub struct NameEnv {
  func: ClosureIRFuncDef
  env: Map[Name, LowType]
  alias: Map[Name, Name]
  vars: Array[(Name, LowType)]
  free_vars: Array[(Name, LowType)]
}

pub fn createEnv(self: ClosureIRFuncDef) -> NameEnv {
  let env = Map::new()
  for param in self.params {
    let (name, ty) = param
    env.set(name, ty)
  }
  NameEnv::{
    func: self,
    env: env,
    alias: Map::new(),
    vars: Array::new(),
    free_vars: Array::new()
  }
}

// use it when met `let` or `let tuple`
pub fn add_normal_name(
  self: NameEnv, name: Name, ty: LowType
) -> Unit {
  self.env.set(name, ty)
  self.vars.push((name, ty))
}

pub fn set_alias(self: NameEnv, name: Name, alias: Name) -> Unit {
  self.alias.set(name, alias)
}

// use it in get
pub fn add_free_var(
  self: NameEnv, name: Name, ty: LowType
) -> Unit {
  self.env.set(name, ty) // avoid duplication
  self.func.free_vars.push((name, ty))
  self.free_vars.push((name, ty))
}

fn find_func_type_in_context(self : NameEnv, name : Name) -> LowType? {
  self.func.prog.context.find_func_type(name)
}

fn find_global_var_type_in_context(self: NameEnv, name: Name) -> LowType? {
  self.func.prog.context.find_global_var_type(name)
}

pub fn get(self: NameEnv, name: Name) -> LowType {
  match self.env.get(name) {
    Some(ty) => ty
    None => match self.func.parent {
      Some(parent) => match parent.get_var(name) {
        Some(ty) => {
          self.add_free_var(name, ty)
          ty
        }
        None => match self.find_global_var_type_in_context(name) {
          Some(ty) => ty
          None => {
            println("Undefined Variable: \{name}")
            panic()
          }
        }
      }
      None => match self.find_global_var_type_in_context(name) {
        Some(ty) => ty
        None => {
          println("Undefined Variable: \{name}")
          panic()
        }
      }
    }
  }
}

// Capture is trying to capture free variables
pub fn capture(self: NameEnv, name: Name) -> NameEnv {
  match self.env.get(name) {
    Some(_) => self
    None => match self.func.parent {
      Some(parent) => match parent.get_var(name) {
        Some(ty) => {
          self.add_free_var(name, ty)
          self
        }
        None => match self.find_global_var_type_in_context(name) {
          Some(_) => self
          None => {
            println("In function: \{self.func.fname}")
            println("Undefined Variable: \{name}, loc: capture")
            panic()
          }
        }
      }
      None => match self.find_global_var_type_in_context(name) {
        Some(_) => self
        None => {
          println("Undefined Variable: \{name}")
          panic()
        }
      }
    }
  }
}

/// -----------------------------------------------------
/// LowType
///
/// ## Difference between ClosureFn and DirectFn
///
/// Esstencially, DirectFn is function or function pointer
/// while closureFn is a struct that contains function pointer
/// and free variables.
///
/// In llvm codegen phrase, DirectFn just need to push arguments
/// but if it is closureF, after all arguments are pushed, it needs
/// the address of closure struct pointer.
/// -----------------------------------------------------
pub enum LowType {
  Unit
  Int
  Int64
  UInt
  UInt64
  Float
  Double
  Boolean
  Ptr
  Array(LowType)
  Tuple(Array[LowType])
  ClosureFn(Array[LowType], LowType)
  DirectFn(Array[LowType], LowType)
}

pub fn to_string(self: LowType) -> String {
  match self {
    Unit => "Unit"
    Int => "Int"
    Int64 => "Int64"
    UInt => "UInt"
    UInt64 => "UInt64"
    Float => "Float"
    Double => "Double"
    Boolean => "Boolean"
    Ptr => "Ptr"
    Array(ty) => "Array[\{ty}]"
    Tuple(tup) => {
      let tup_str = tup.map(fn(t) { "\{t}" }).join(", ")
      "(" + tup_str + ")"
    }
    ClosureFn(args, ret) => {
      let args_str = args.map(fn(t) { t.to_string() }).join(", ")
      let ret_str = ret.to_string()
      "(closusre fn) (" + args_str + ") -> " + ret_str
    }
    DirectFn(args, ret) => {
      let args_str = args.map(fn(t) { t.to_string() }).join(", ")
      let ret_str = ret.to_string()
      "(direct fn) (" + args_str + ") -> " + ret_str
    }
  }
}

pub fn output(self: LowType, logger: Logger) -> Unit {
  logger.write_string(self.to_string())
}

pub fn to_lowtype(self: Type, ~closurefy: Bool = false) -> LowType {
  match self {
    Type::Unit => LowType::Unit
    Type::Int32 => LowType::Int
    Type::Int64 => LowType::Int64
    Type::UInt32 => LowType::UInt
    Type::UInt64 => LowType::UInt64
    Type::Float32 => LowType::Float
    Type::Double => LowType::Double
    Type::Boolean => LowType::Boolean
    Type::TupleTy(tup) => tuple_type_to_lowtype(tup, closurefy)
    Type::CompTy(comp) => comp_type_to_lowtype(comp, closurefy)
    Type::FuncTy(fty) => func_type_to_lowtype(fty, closurefy)
    _ => {
      println("Type \{self} cannot be converted to LowType")
      panic()
    }
  }
}

fn tuple_type_to_lowtype(tup_ty : TupleTy, closurefy: Bool) -> LowType {
  LowType::Tuple(tup_ty.types.types.map(fn(t) { t.to_lowtype(closurefy=true) }))
}

fn func_type_to_lowtype(ty : FuncTy, closurefy: Bool) -> LowType {
  let args = ty.param_types.types.map(fn(t) { t.to_lowtype(closurefy=true) })
  let ret = ty.ret_type.to_lowtype(closurefy = true) // Note this
  match closurefy {
    true => LowType::ClosureFn(args, ret)
    false => LowType::DirectFn(args, ret)
  }
}

fn comp_type_to_lowtype(ty: CompTy, closurefy: Bool) -> LowType {
  let subs = ty.subs.types
  let ptr_to_ty = match subs.length() {
    1 => subs[0].to_lowtype(~closurefy)
    _ => {
      let tys = subs.map(fn(t) { t.to_lowtype(~closurefy) })
      LowType::Tuple(tys)
    }
  }
  LowType::Array(ptr_to_ty)
}

/// -----------------------------------------------------------
/// ## Closure IR
///
/// ## Difference between call direct and call closure
///
/// When calling direct function, it is just a function pointer
/// and arguments. But when calling closure function, it is a
/// function pointer and arguments and closure struct pointer.
/// -----------------------------------------------------------
pub enum ClosureIR {
  Unit
  Int(Int)
  Int64(Int64)
  UInt(UInt)
  UInt64(UInt64)
  Double(Double)
  Boolean(Bool)
  Var(Name)
  Neg(Name)
  Add(Name, Name)
  Sub(Name, Name)
  Mul(Name, Name)
  Div(Name, Name)
  Rem(Name, Name)
  Eq(Name, Name)
  Ne(Name, Name)
  Gt(Name, Name)
  Lt(Name, Name)
  Ge(Name, Name)
  Le(Name, Name)
  Not(Name)
  And(Name, Name)
  Or(Name, Name)
  Beq(Name, Name, ClosureIR, ClosureIR)
  Bne(Name, Name, ClosureIR, ClosureIR)
  Bgt(Name, Name, ClosureIR, ClosureIR)
  Blt(Name, Name, ClosureIR, ClosureIR)
  Bge(Name, Name, ClosureIR, ClosureIR)
  Ble(Name, Name, ClosureIR, ClosureIR)
  Br(Name, ClosureIR, ClosureIR)
  Let((Name, LowType), ClosureIR, ClosureIR) // the last is the next IR
  LetTuple(Array[(Name, LowType)], Name, ClosureIR) // the last is the next IR
  MakeClosure((Name, LowType), ClosureDef, ClosureIR) // the last is the next IR
  CallClosure(Name, Array[Name])
  CallDirect(Name, Array[Name])
  MakeTuple(Array[Name])
  ArrayGet(Name, Name)
  ArrayPut(Name, Name, Name)
}

struct ClosureDef {
  fname: Name
  free_vars: Array[(Name, LowType)]
}

pub fn ClosureDef::new(
  fname: Name,
  free_vars: Array[(Name, LowType)]
) -> ClosureDef {
  ClosureDef::{ fname, free_vars }
}

pub fn ClosureDef::new_from_direct(
  fname: Name
) -> ClosureDef {
  ClosureDef::{ fname, free_vars: Array::new() }
}

pub fn to_string(self: ClosureDef) -> String {
  let free_vars_str = self.free_vars.map(fn(pair) {
    let (name, ty) = pair;
    "\{name}: \{ty}"
  }).join(", ")
  "\{self.fname} [\{free_vars_str}]"
}

pub fn to_string(self: ClosureIR) -> String {
  self.stringify(0)
}

pub fn get_last(self: ClosureIR) -> ClosureIR {
  match self {
    Let(_, _, next) => next.get_last()
    LetTuple(_, _, next) => next.get_last()
    MakeClosure(_, _, next) => next.get_last()
    _ => self
  }
}

// for ClosureIR::Let((name, ty), sub_ir, next_ir)
// sub_ir may be complex, for example, it may be another Let
// or letTuple, or MakeClsoure, so we need to linearlize it
pub fn linearlize_let(self: ClosureIR) -> ClosureIR {
  match self {
    Let((name, ty), sub_ir, next_ir) => match sub_ir {
      Let(name_ty, ssub, next) => {
        let linearlized = linearlize_let(Let((name, ty), next, next_ir))
        Let(name_ty, ssub, linearlized)
      }
      LetTuple(tup, tupname, next) => {
        let linearlized = linearlize_let(Let((name, ty), next, next_ir))
        LetTuple(tup, tupname, linearlized)
      }
      MakeClosure(closure_name_ty, closure_def, next) => {
        let linearlized = linearlize_let(Let((name, ty), next, next_ir))
        MakeClosure(closure_name_ty, closure_def, linearlized)
      }
      _ => self
    }
    _ => {
      println("linearlize_let should be called on Let")
      panic()
    }
  }
}


/// -----------------------------------------------------
/// ## Knf IR to Closure IR
/// -----------------------------------------------------
pub fn knf_to_closure_top_level(knf: Knf) -> ClosureIR {
  match knf {
    Unit => ClosureIR::Unit
    Int(i) => ClosureIR::Int(i)
    Int64(i) => ClosureIR::Int64(i)
    UInt(i) => ClosureIR::UInt(i)
    UInt64(i) => ClosureIR::UInt64(i)
    Double(f) => ClosureIR::Double(f)
    Boolean(b) => ClosureIR::Boolean(b)
    Var(name) => ClosureIR::Var(name)
    Neg(name) => ClosureIR::Neg(name)
    Not(name) => ClosureIR::Not(name)
    Add(lhs, rhs) => ClosureIR::Add(lhs, rhs)
    Sub(lhs, rhs) => ClosureIR::Sub(lhs, rhs)
    Mul(lhs, rhs) => ClosureIR::Mul(lhs, rhs)
    Div(lhs, rhs) => ClosureIR::Div(lhs, rhs)
    Rem(lhs, rhs) => ClosureIR::Rem(lhs, rhs)
    And(lhs, rhs) => ClosureIR::And(lhs, rhs)
    Or(lhs, rhs) => ClosureIR::Or(lhs, rhs)
    Eq(lhs, rhs) => ClosureIR::Eq(lhs, rhs)
    Ne(lhs, rhs) => ClosureIR::Ne(lhs, rhs)
    Gt(lhs, rhs) => ClosureIR::Gt(lhs, rhs)
    Lt(lhs, rhs) => ClosureIR::Lt(lhs, rhs)
    Ge(lhs, rhs) => ClosureIR::Ge(lhs, rhs)
    Le(lhs, rhs) => ClosureIR::Le(lhs, rhs)
    Tuple(names) => ClosureIR::MakeTuple(names)
    _ => {
      println("Top Leval to closure cannot be too complex")
      panic()
    }
  }
}

pub fn knf_to_closure(
  knf: Knf, env: NameEnv
) -> ClosureIR {
  // catch name first
  match knf {
    Unit => ClosureIR::Unit
    Int(i) => ClosureIR::Int(i)
    Int64(i) => ClosureIR::Int64(i)
    UInt(i) => ClosureIR::UInt(i)
    UInt64(i) => ClosureIR::UInt64(i)
    Double(f) => ClosureIR::Double(f)
    Boolean(b) => ClosureIR::Boolean(b)
    Var(name) => { 
      // if type is directFn, use closure to warp it
      // generate MakeClosure with empty free_vars
      let ty = env.get(name);
      match ty {
        LowType::DirectFn(_) => {
          let closure_def = ClosureDef::new_from_direct(name)
          let closure_name = name.createSibling()
          MakeClosure((closure_name, ty), closure_def, ClosureIR::Var(closure_name))
        }
        _ => ClosureIR::Var(name)
      }
    }
    Neg(name) => { let _ = env.capture(name); ClosureIR::Neg(name)}
    Not(name) => { let _ = env.capture(name); ClosureIR::Not(name)}
    Add(lhs, rhs) => { let _ = env.capture(lhs).capture(rhs); ClosureIR::Add(lhs, rhs)}
    Sub(lhs, rhs) => { let _ = env.capture(lhs).capture(rhs); ClosureIR::Sub(lhs, rhs)}
    Mul(lhs, rhs) => { let _ = env.capture(lhs).capture(rhs); ClosureIR::Mul(lhs, rhs)}
    Div(lhs, rhs) => { let _ = env.capture(lhs).capture(rhs); ClosureIR::Div(lhs, rhs)}
    Rem(lhs, rhs) => { let _ = env.capture(lhs).capture(rhs); ClosureIR::Rem(lhs, rhs)}
    And(lhs, rhs) => { let _ = env.capture(lhs).capture(rhs); ClosureIR::And(lhs, rhs)}
    Or(lhs, rhs) => { let _ = env.capture(lhs).capture(rhs); ClosureIR::Or(lhs, rhs)}
    Eq(lhs, rhs) => { let _ = env.capture(lhs).capture(rhs); ClosureIR::Eq(lhs, rhs)}
    Ne(lhs, rhs) => { let _ = env.capture(lhs).capture(rhs); ClosureIR::Ne(lhs, rhs)}
    Gt(lhs, rhs) => { let _ = env.capture(lhs).capture(rhs); ClosureIR::Gt(lhs, rhs)}
    Lt(lhs, rhs) => { let _ = env.capture(lhs).capture(rhs); ClosureIR::Lt(lhs, rhs)}
    Ge(lhs, rhs) => { let _ = env.capture(lhs).capture(rhs); ClosureIR::Ge(lhs, rhs)}
    Le(lhs, rhs) => { let _ = env.capture(lhs).capture(rhs); ClosureIR::Le(lhs, rhs)}
    Beq(lhs, rhs, then, elsc) => {
      let _ = env.capture(lhs).capture(rhs);
      let then_ir = knf_to_closure(then, env)
      let elsc_ir = knf_to_closure(elsc, env)
      ClosureIR::Beq(lhs, rhs, then_ir, elsc_ir)
    }
    Bne(lhs, rhs, then, elsc) => {
      let _ = env.capture(lhs).capture(rhs);
      let then_ir = knf_to_closure(then, env)
      let elsc_ir = knf_to_closure(elsc, env)
      ClosureIR::Bne(lhs, rhs, then_ir, elsc_ir)
    }
    Bgt(lhs, rhs, then, elsc) => {
      let _ = env.capture(lhs).capture(rhs);
      let then_ir = knf_to_closure(then, env)
      let elsc_ir = knf_to_closure(elsc, env)
      ClosureIR::Bgt(lhs, rhs, then_ir, elsc_ir)
    }
    Bge(lhs, rhs, then, elsc) => {
      let _ = env.capture(lhs).capture(rhs);
      let then_ir = knf_to_closure(then, env)
      let elsc_ir = knf_to_closure(elsc, env)
      ClosureIR::Bge(lhs, rhs, then_ir, elsc_ir)
    }
    Blt(lhs, rhs, then, elsc) => {
      let _ = env.capture(lhs).capture(rhs);
      let then_ir = knf_to_closure(then, env)
      let elsc_ir = knf_to_closure(elsc, env)
      ClosureIR::Blt(lhs, rhs, then_ir, elsc_ir)
    }
    Ble(lhs, rhs, then, elsc) => {
      let _ = env.capture(lhs).capture(rhs);
      let then_ir = knf_to_closure(then, env)
      let elsc_ir = knf_to_closure(elsc, env)
      ClosureIR::Ble(lhs, rhs, then_ir, elsc_ir)
    }
    Br(cond, then, elsc) => {
      let _ = env.capture(cond)
      let then_ir = knf_to_closure(then, env)
      let elsc_ir = knf_to_closure(elsc, env)
      ClosureIR::Br(cond, then_ir, elsc_ir)
    }
    Tuple(names) => {
      names.each(fn(name) { let _ = env.capture(name) })
      // if tuple conatins direct function, generate closure
      let mut ret_ir = ClosureIR::MakeTuple(names)
      for i, name in names {
        match env.find_global_var_type_in_context(name) {
          Some(LowType::DirectFn(_)) => {
            let closure_def = ClosureDef::new_from_direct(name)
            let closure_name = name.createSibling()
            names[i] = closure_name
            ret_ir = MakeClosure((closure_name, env.get(name)), closure_def, ret_ir)
          }
          _ => ()
        }
      }
      ret_ir
    }
    Get(arr, idx) => {
      let _ = env.capture(arr).capture(idx)
      ClosureIR::ArrayGet(arr, idx)
    }
    Put(arr, idx, value) => {
      let _ = env.capture(arr).capture(idx).capture(value)
      // if value is direct function, generate closure
      let ret_ir = ClosureIR::ArrayPut(arr, idx, value)
      match env.find_global_var_type_in_context(value) {
        Some(LowType::DirectFn(_)) => {
          let closure_def = ClosureDef::new_from_direct(value)
          let closure_name = value.createSibling()
          MakeClosure((closure_name, env.get(value)), closure_def, ret_ir)
        }
        _ => ret_ir
      }
    }
    ExternApply(name, args) => {
      let _ = env.capture(name)
      args.each(fn(arg) { let _ = env.capture(arg) })
      ClosureIR::CallDirect(name, args)
    }
    Let(_) => knf_let_to_closure(knf, env)
    LetTuple(_) => knf_let_tuple_to_closure(knf, env)
    LetRec(_) => knf_let_rec_to_closure(knf, env)
    Apply(_) => knf_apply_to_closure(knf, env)
    _ => {
      println("Unimplemented knf: \{knf.stringify(0)}")
      panic()
    }
  }
}

fn knf_let_to_closure(knf: Knf, env: NameEnv) -> ClosureIR {
  guard let Knf::Let((name, ty), value, next) = knf
  let name = match env.alias.get(name) {
    Some(alias) => alias
    None => name
  }
  let ty = ty.to_lowtype(closurefy = true)
  env.func.vars.set(name, ty)
  env.add_normal_name(name, ty)
  let value_ir = knf_to_closure(value, env)
  let next_ir = knf_to_closure(next, env)
  let ir = ClosureIR::Let((name, ty), value_ir, next_ir)
  linearlize_let(ir)
}

fn knf_let_tuple_to_closure(knf: Knf, env: NameEnv) -> ClosureIR {
  guard let Knf::LetTuple(name_list, name, next) = knf

  let _ = env.capture(name)
  let closure_name_list: Array[(Name, LowType)] = Array::new()
  name_list.each(fn (name_ty) {
    let (name, ty) = name_ty
    let name = match env.alias.get(name) {
      Some(alias) => alias
      None => name
    }
    let ty = ty.to_lowtype()
    if name.to_string() != "_" {
      env.func.vars.set(name, ty)
      env.add_normal_name(name, ty)
    }
    closure_name_list.push((name, ty))
  })
  let next_ir = knf_to_closure(next, env)
  ClosureIR::LetTuple(closure_name_list, name, next_ir)
}

fn knf_let_rec_to_closure(knf: Knf, env: NameEnv) -> ClosureIR {
  guard let Knf::LetRec(knf_func_def, body, next) = knf
  let func_ty = knf_func_def.func_type.to_lowtype(closurefy = true)
  let params = knf_func_def.params.map(fn(param) {
    let (name, ty) = param
    (name, ty.to_lowtype())
  })
  let closure_struct_name = Name::new("__capture__")
  params.push((closure_struct_name, LowType::Ptr)) // add closure struct pointer
  // create funcdef, but not add it to prog temporarily
  // let fname = knf_func_def.name
  let orig_name = knf_func_def.name
  let fname = Name::new(env.func.fname.to_string() + "_inner_" +
                        knf_func_def.name.to_string())
  let func_def = env.func.createInnerFuncDef(fname, func_ty, params)
  let new_env = func_def.createEnv()
  new_env.add_normal_name(fname, func_ty)
  new_env.set_alias(orig_name, fname)
  let body = knf_to_closure(body, new_env)

  // after run to_closure, let prog add top level funcdef
  let prog = env.func.prog
  let free_vars = func_def.free_vars
  let vars = func_def.vars
  let func_def = prog.createTopFuncDef(
    fname, func_ty, params, body, ~free_vars, ~vars
  )
  prog.add_func_def(func_def)
  let closure_def = ClosureDef::new(fname, free_vars)
  env.add_normal_name(orig_name, func_ty)
  env.func.vars.set(orig_name, func_ty)
  let next_ir = knf_to_closure(next, env)
  MakeClosure((orig_name, func_ty), closure_def, next_ir)
}

pub fn knf_apply_to_closure(knf: Knf, env: NameEnv) -> ClosureIR {
  guard let Knf::Apply(name, args) = knf
  let name = match env.alias.get(name) {
    Some(alias) => alias
    None => name
  }
  let _ = env.capture(name)
  args.each(fn(arg) { let _ = env.capture(arg) })

  // if args contains direct function, generate closure, use `MakeClosure`
  let func_ty = env.get(name)
  let mut ret_ir = match func_ty {
    LowType::DirectFn(_) => ClosureIR::CallDirect(name, args)
    LowType::ClosureFn(_) => ClosureIR::CallClosure(name, args)
    _ => {
      println("Unknown error happened, call invalid something")
      panic()
    }
  }
  for i, arg in args {
    match env.find_global_var_type_in_context(arg) {
      Some(LowType::DirectFn(_)) => {
        let closure_def = ClosureDef::new_from_direct(arg)
        let closure_name = arg.createSibling()
        args[i] = closure_name
        ret_ir = MakeClosure((closure_name, env.get(arg)), closure_def, ret_ir)
      }
      _ => ()
    }
  }
  ret_ir
}
