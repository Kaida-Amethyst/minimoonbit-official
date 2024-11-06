/// --------------------------------------------------
/// ## Function
/// --------------------------------------------------
pub struct Function {
  name : String
  mut func_type : Type
  args : Array[Argument]
  body : Array[BasicBlock]
  mut entry_block: BasicBlock?
  phi_nodes: Array[PHINode]
  call_insts: Array[CallInst]
  value_attr: ValueAttribute
  mut value_number : Int
  mut label_number : Int
  mut stack_size: Int?
  mut extra_alloc: Int?
  module : Module
}

pub fn get_type(self: Function) -> Type {
  self.func_type
}

pub fn type_convert(self: Function, new_ty: Type) -> Unit {
  self.func_type = new_ty
  self.value_attr.type_convert(new_ty)
}

pub fn op_equal(self: Function, other: Function) -> Bool {
  self.name == other.name
}

pub fn Function::create(
  func_type : Type, name : String, module: Module
) -> Function {
  let mut argno = -1
  guard let Type::FunctionTy(arg_types, _)  = func_type
  let args = arg_types.map(fn (ty) { argno += 1; Argument::create(ty, argno)})
  let value_attr = ValueAttribute::default(func_type)
  let body : Array[BasicBlock] = []
  let entry_block : BasicBlock? = None
  let phi_nodes : Array[PHINode] = []
  let call_insts : Array[CallInst] = []
  let value_number = args.length()
  let f = Function::{
    args, body, entry_block, func_type, name,
    value_attr, value_number, phi_nodes, call_insts,
    label_number: 0, stack_size: None, extra_alloc: None,
    module
  }
  args.each(fn (arg) { arg.func = Some(f) })
  module.add_function(f)
  f
}

pub fn set_name(self: Function, name: String) -> Unit {
  self.value_attr.name = Some(name)
}

pub fn get_argument(self : Function, argno : Int) -> Argument {
  if argno < self.args.length() {
    self.args[argno]
  } else {
    println("Argument number out of range, loc: Function::get_argument")
    panic()
  }
}

pub fn as_value(self : Function) -> Value {
  Value::Function(self)
}

pub fn trivial_stringify(self: Function) -> String {
  "@" + self.name
}

pub fn to_string(self : Function) -> String {
  let Type::FunctionTy(_, ret_type) = self.func_type
  let head = match self.body.length() {
    0 => "declare "
    _ => "define "
  }
  let mut r = head + ret_type.to_string() + " @" + self.name + "("
  let args_str = self.args.map(fn (arg) {
    arg.get_type().to_string() + " " +
    arg.trivial_stringify()
  }).join(", ")
  r += args_str + ")"

  self.body.sort()
  if self.body.length() > 0 {
    r += " {\n"
    r += self.body[0].to_string()
    for bb in self.body[1:] {
      r += "\n" + bb.to_string()
    }
    r += "}\n"
  } else {
    r += "\n"
  }
  r
}

pub fn is_undefined(self : Function) -> Bool {
  self.body.length() == 0
}

pub fn get_phi_nodes(self : Function) -> Array[PHINode] {
  self.phi_nodes
}

pub fn clear_live_analysis(self: Function) -> Unit {
  for bb in self.body {
    bb.clear_live_analysis()
  }
  self.value_attr.analysis.clear_live_analysis()
}

pub fn clear_virtual_reg(self: Function) -> Unit {
  for arg in self.args {
    arg.clear_virtual_reg()
  }
  for bb in self.body {
    bb.clear_virtual_reg()
  }
  self.value_attr.analysis.clear_virtual_reg()
}

/// --------------------------------------------------
/// ## Function Argument
/// --------------------------------------------------
struct Argument {
  argno : Int
  mut func: Function?
  value_attr: ValueAttribute
}

pub fn Argument::create(
  ty : Type, argno : Int
) -> Argument {
  let value_attr = ValueAttribute::create(ty, None, None, [], [])
  Argument::{ argno, func:None, value_attr}
}

pub fn op_equal(self: Argument, other: Argument) -> Bool {
  self.func == other.func && self.argno == other.argno
}

fn get_type(self: Argument) -> Type {
  self.value_attr.value_type
}

fn get_id(self: Argument) -> Int {
  self.argno
}

fn set_name(self: Argument, name: String) -> Unit {
  self.value_attr.name = Some(name)
}

pub fn trivial_stringify(self: Argument) -> String {
  // "%" + self.argno.to_string()
  match self.value_attr.name {
    Some(name) => "%" + name
    None => "%" + self.argno.to_string()
  }
}

pub fn output(self : Function, logger : Logger) -> Unit {
  logger.write_string(self.to_string())
}

pub fn as_value(self : Argument) -> Value {
  Value::Argument(self)
}

pub fn get_attribute(self : Argument) -> ValueAttribute {
  self.value_attr
}

pub fn clear_virtual_reg(self: Argument) -> Unit {
  self.value_attr.analysis.clear_virtual_reg()
}

pub fn add_user[T: IsValue](self: Argument, user: T) -> Unit {
  let user = user.as_value()
  if self.value_attr.users.contains(user) {
    return
  }
  self.value_attr.users.push(user)
}
