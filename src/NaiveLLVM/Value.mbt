/// --------------------------------------------------
/// ## Value
/// --------------------------------------------------
pub enum Value {
  Constant(Constant)
  Argument(Argument) // Type, ArgNo
  Instruction(Instruction)
  BasicBlock(BasicBlock)
  Function(Function)
}

pub struct ValueAttribute {
  mut value_type : Type
  mut name: String?
  id: Int?
  users : Array[Value]
  uses : Array[Value]
  analysis: AnalysisManager
  mut parent: BasicBlock?
}

pub fn ValueAttribute::create(
  value_type: Type, name: String?, id: Int?,
  uses: Array[Value], users: Array[Value], ~parent: BasicBlock? = None
) -> ValueAttribute {
  let analysis = AnalysisManager::default()
  ValueAttribute::{ value_type, name, id, users, uses, analysis, parent }
}

pub fn ValueAttribute::default(
  value_type: Type
) -> ValueAttribute {
  let name : String? = None
  let id : Int? = None
  let users = []
  let uses = []
  let analysis = AnalysisManager::default()
  let parent : BasicBlock? = None
  ValueAttribute::{ value_type, name, id, users, uses, analysis, parent }
}

pub fn set_parent(self: ValueAttribute, parent: BasicBlock) -> Unit {
  self.parent = Some(parent)
}

pub fn type_convert(self: ValueAttribute, new_ty: Type) -> Unit {
  // self.value_type = new_ty
  // if self.value_type == new_ty {
  //   return
  // }
  self.value_type = new_ty
  // match (self.value_type, new_ty) {
  //   (Type::PointerTy(_), Type::PointerTy(_)) => {
  //     self.value_type = new_ty
  //   }
  //   // check arg ty is same first, then convert ret ty
  //   (Type::FunctionTy(arg_tys, ret_ty),
  //   Type::FunctionTy(arg_tys2, ret_ty2)) =>{
  //
  //   }
  //   _ => {
  //     println("Value Type Conversion is not allowd for \{self.value_type} to \{new_ty}!")
  //     panic()
  //   }
  // }
}

pub trait IsValue {
  as_value(Self) -> Value
}

pub fn as_value(self: Value) -> Value {
  self
}

pub fn get_type(self : Value) -> Type {
  match self {
    Value::Constant(c) => c.get_type()
    Value::Argument(argu) => argu.get_type()
    Value::Instruction(inst) => inst.get_type()
    Value::Function(func) => func.get_type()
    Value::BasicBlock(_) => {
      println("Unimplemented get_type for BasicBlock")
      panic()
    }
  }
}

pub fn get_id(self: Value) -> Int? {
  match self {
    Value::Constant(_) => None
    Value::Argument(argu) => Some(argu.get_id())
    Value::Instruction(inst) => inst.get_id()
    Value::Function(_) => None
    Value::BasicBlock(_) => None
  }
}

pub fn get_name(self: Value) -> String? {
  match self {
    Value::Constant(c) => None
    Value::Argument(argu) => argu.value_attr.name
    Value::Instruction(inst) => inst.get_attribute().name
    Value::Function(func) => Some(func.name)
    Value::BasicBlock(bb) => bb.name
  }
}

pub fn insert_before[T: IsInst](self: Value, inst: T) -> Unit {
  let inst = inst.as_inst()
  let bb = match self.get_attribute().parent {
    Some(b) => b
    None => {
      println("insert_before failed!, \{self.trivial_stringify()} has no parent")
      match self {
        Value::Instruction(inst) => println(inst.to_string())
      }
      panic()
    }
  }
  let mut idx = None
  for i, inst in bb.inst_list {
    if inst == dyn::cast(self) {
      idx = Some(i)
    }
  }
  let idx = match idx {
    Some(i) => i
    None => {
      println("insert_before failed!, possibly it's not inst")
      panic()
    }
  }
  bb.inst_list.insert(idx, inst)
  inst.get_attribute().parent = Some(bb)
}

pub fn insert_after[T: IsInst](self: Value, inst: T) -> Unit {
  let inst = inst.as_inst()
  let bb = match self.get_attribute().parent {
    Some(b) => b
    None => {
      println("insert_after failed! \{self.trivial_stringify()} has no parent")
      panic()
    }
  }
  let mut idx = None
  for i, inst in bb.inst_list {
    if inst.as_value() == self.as_value() {
      idx = Some(i)
    }
  }
  let idx = match idx {
    Some(i) => i
    None => {
      println("insert_after failed!")
      panic()
    }
  }
  bb.inst_list.insert(idx + 1, inst)
  inst.get_attribute().parent = Some(bb)
}


pub fn get_users(self: Value) -> Array[Value] {
  self.get_attribute().users
}

pub fn get_uses(self: Value) -> Array[Value] {
  self.get_attribute().uses
}

pub fn replace_use(self: Value, old: Value, new: Value) -> Unit {
  let uses = self.get_attribute().uses
  for i, use in uses {
    if use == old {
      uses[i] = new
    }
    match self {
      Value::Instruction(Store(sinst)) => {
        if sinst.value == old {
          sinst.value = new
        } else if sinst.pointer == old {
          sinst.pointer = new
        }
      }
      Value::Instruction(GetElementPtr(gep)) => {
        if gep.pointer == old {
          gep.pointer = new
        }
        for i, idx in gep.index_list {
          if idx == old {
            gep.index_list[i] = new
          }
        }
      }
      _ => ()
    }
  }
}

pub fn add_user[T: IsValue](self: Value, user: T) -> Unit {
  match self {
    Value::Argument(argu) => argu.add_user(user)
    Value::Instruction(inst) => inst.add_user(user)
    _ => ()
  }
}

pub fn set_name(self: Value, name: String) -> Unit {
  match self {
    Value::Instruction(inst) => inst.set_name(name)
    Value::Argument(argu) => argu.set_name(name)
    Value::Constant(_) => {
      println("Unimplemented set_name for Constant")
      panic()
    }
    Value::Function(f) => f.set_name(name)
    Value::BasicBlock(_) => {
      println("Unimplemented set_name for BasicBlock")
      panic()
    }
  }
}

// Note:  Value::trivial_stringify() will not to string all the content,
// It only to string the name, or simple value
pub fn trivial_stringify(self : Value) -> String {
  match self {
    Value::Constant(c) => c.trivial_stringify()
    Value::Argument(argu) => argu.trivial_stringify()
    Value::Instruction(inst) => inst.trivial_stringify()
    Value::BasicBlock(bb) => bb.trivial_stringify()
    Value::Function(func) => func.trivial_stringify()
  }
}

pub fn op_equal(self: Value, other: Value) -> Bool {
  // self.trivial_stringify() == other.trivial_stringify()
  let sid = match self.hasAttribute() {
    true => self.get_attribute().id
    false => None
  }
  let oid = match other.hasAttribute() {
    true => other.get_attribute().id
    false => None
  }
  match (sid, oid) {
    (Some(sid), Some(oid)) => sid == oid
    (Some(_), None) => false
    (None, Some(_)) => false
    (None, None) => {
      match (self, other) {
        (Constant(c1), Constant(c2)) => c1 == c2
        (Argument(a1), Argument(a2)) => a1 == a2
        (Instruction(i1), Instruction(i2)) => i1 == i2
        (BasicBlock(b1), BasicBlock(b2)) => b1 == b2
        (Function(f1), Function(f2)) => f1 == f2
        _ => false
      }

    }
  }
}

pub fn hash_combine(self: Value, hasher: Hasher) -> Unit {
  hasher.combine_string(self.trivial_stringify())
}

pub fn Value::getInt32(i: Int) -> Value {
  let constant_i32 = Constant::getInt32(i)
  Value::Constant(constant_i32)
}

pub fn output(self : Value, logger : Logger) -> Unit {
  logger.write_string(self.trivial_stringify())
}

pub fn get_attribute(self: Value) -> ValueAttribute {
  match self {
    Value::Argument(argu) => argu.get_attribute()
    Value::Instruction(inst) => inst.get_attribute()
    Value::Constant(c) => { println("Unimplemented get_attribute for Constant: \{c}"); panic() }
    // Value::Function(_) => { println("Unimplemented get_attribute for Function"); panic() }
    Value::Function(f) => f.value_attr
    Value::BasicBlock(_) => { println("Unimplemented get_attribute for BasicBlock"); panic() }
  }
}

pub fn hasAttribute(self: Value) -> Bool {
  match self {
    Value::Argument(_) => true
    Value::Instruction(_) => true
    _ => false
  }
}

pub fn get_live_analysis(self: Value) -> LiveAnalysis? {
  self.get_attribute().analysis.get_live_analysis()
}

pub fn type_convert(self: Value, new_ty: Type) -> Unit {
  self.get_attribute().type_convert(new_ty)
}

pub fn get_vreg(self: Value) -> VirtualReg? {
  // TODO
  panic()
}

trait dyn {
  cast(Value) -> Self;
}

impl dyn for Instruction with cast(val: Value) {
  match val {
    Value::Instruction(inst) => inst
    _ => {
      println("cast failed!")
      panic()
    }
  }
}

impl dyn for Constant with cast(val: Value) {
  match val {
    Value::Constant(c) => c
    _ => {
      println("cast failed!")
      panic()
    }
  }
}

/// --------------------------------------------------
/// dyn_cast
/// --------------------------------------------------
impl dyn for Argument with cast(val: Value) {
  match val {
    Value::Argument(argu) => argu
    _ => {
      println("cast failed!")
      panic()
    }
  }
}

impl dyn for Function with cast(val: Value) {
  match val {
    Value::Function(func) => func
    _ => {
      println("cast failed!")
      panic()
    }
  }
}

impl dyn for BasicBlock with cast(val: Value) {
  match val {
    Value::BasicBlock(bb) => bb
    _ => {
      println("cast failed!")
      panic()
    }
  }
}

impl dyn for BinaryInst with cast(val: Value) {
  match val {
    Value::Instruction(Instruction::Binary(bin)) => bin
    _ => {
      println("cast failed!")
      panic()
    }
  }
}

impl dyn for UnaryInst with cast(val: Value) {
  match val {
    Value::Instruction(Instruction::Unary(un)) => un
    _ => {
      println("cast failed!")
      panic()
    }
  }
}

impl dyn for PHINode with cast(val: Value) {
  match val {
    Value::Instruction(Instruction::PHI(phi)) => phi
    _ => {
      println("cast failed!")
      panic()
    }
  }
}

impl dyn for CmpInst with cast(val: Value) {
  match val {
    Value::Instruction(Instruction::Compare(cmp)) => cmp
    _ => {
      println("cast failed!")
      panic()
    }
  }
}
impl dyn for BranchInst with cast(val: Value) {
  match val {
    Value::Instruction(Instruction::Branch(br)) => br
    _ => {
      println("cast failed!")
      panic()
    }
  }
}

impl dyn for CallInst with cast(val: Value) {
  match val {
    Value::Instruction(Instruction::Call(call)) => call
    _ => {
      println("cast failed!")
      panic()
    }
  }
}

impl dyn for ReturnInst with cast(val: Value) {
  match val {
    Value::Instruction(Instruction::Return(ret)) => ret
    _ => {
      println("cast failed!")
      panic()
    }
  }
}

impl dyn for StoreInst with cast(val: Value) {
  match val {
    Value::Instruction(Instruction::Store(st)) => st
    _ => {
      println("cast failed!")
      panic()
    }
  }
}

impl dyn for AllocaInst with cast(val: Value) {
  match val {
    Value::Instruction(Instruction::Alloca(alloca)) => alloca
    _ => {
      println("cast failed!")
      panic()
    }
  }
}

impl dyn for GetElementPtrInst with cast (val: Value) {
  match val {
    Value::Instruction(Instruction::GetElementPtr(gep)) => gep
    _ => {
      println("cast failed!")
      panic()
    }
  }
}

/// --------------------------------------------------
/// Isa
/// --------------------------------------------------
trait Isa {}

fn Isa::constant(val: Value) -> Bool {
  match val {
    Value::Constant(_) => true
    _ => false
  }
}

fn Isa::argument(val: Value) -> Bool {
  match val {
    Value::Argument(_) => true
    _ => false
  }
}

fn Isa::instruction(val: Value) -> Bool {
  match val {
    Value::Instruction(_) => true
    _ => false
  }
}

fn Isa::basicBlock(val: Value) -> Bool {
  match val {
    Value::BasicBlock(_) => true
    _ => false
  }
}

fn Isa::function(val: Value) -> Bool {
  match val {
    Value::Function(_) => true
    _ => false
  }
}

fn Isa::binaryInst(val: Value) -> Bool {
  match val {
    Value::Instruction(Instruction::Binary(_)) => true
    _ => false
  }
}

fn Isa::unaryInst(val: Value) -> Bool {
  match val {
    Value::Instruction(Instruction::Unary(_)) => true
    _ => false
  }
}

fn Isa::phiNode(val: Value) -> Bool {
  match val {
    Value::Instruction(Instruction::PHI(_)) => true
    _ => false
  }
}

fn Isa::cmpInst(val: Value) -> Bool {
  match val {
    Value::Instruction(Instruction::Compare(_)) => true
    _ => false
  }
}

fn Isa::branchInst(val: Value) -> Bool {
  match val {
    Value::Instruction(Instruction::Branch(_)) => true
    _ => false
  }
}

fn Isa::callInst(val: Value) -> Bool {
  match val {
    Value::Instruction(Instruction::Call(_)) => true
    _ => false
  }
}

fn Isa::returnInst(val: Value) -> Bool {
  match val {
    Value::Instruction(Instruction::Return(_)) => true
    _ => false
  }
}

fn Isa::storeInst(val: Value) -> Bool {
  match val {
    Value::Instruction(Instruction::Store(_)) => true
    _ => false
  }
}

fn Isa::allocaInst(val: Value) -> Bool {
  match val {
    Value::Instruction(Instruction::Alloca(_)) => true
    _ => false
  }
}

fn Isa::getElementPtrInst(val: Value) -> Bool {
  match val {
    Value::Instruction(Instruction::GetElementPtr(_)) => true
    _ => false
  }
}
