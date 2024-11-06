/// --------------------------------------------------
/// ## Instructions
/// --------------------------------------------------
enum Instruction {
  Binary(BinaryInst)
  Unary(UnaryInst)
  PHI(PHINode)
  Compare(CmpInst)
  Branch(BranchInst)
  Call(CallInst)
  Return(ReturnInst)
  Alloca(AllocaInst)
  Store(StoreInst)
  GetElementPtr(GetElementPtrInst)
}

fn as_value(self: Instruction) -> Value {
  Value::Instruction(self)
}

fn as_inst(self: Instruction) -> Instruction {
  self
}

pub trait IsInst {
  as_inst(Self) -> Instruction
}

fn get_type(self: Instruction) -> Type {
  match self {
    Binary(bin) => bin.value_attr.value_type
    Unary(un) => un.value_attr.value_type
    PHI(phi) => phi.value_attr.value_type
    Compare(cmp) => cmp.value_attr.value_type
    Branch(br) => br.value_attr.value_type
    Call(call) => call.value_attr.value_type
    Return(ret) => ret.value_attr.value_type
    Alloca(alloc) => alloc.value_attr.value_type
    Store(store) => store.value_attr.value_type
    GetElementPtr(gep) => gep.value_attr.value_type
    _ => {
      println("Instruction get_type failed!")
      panic()
    }
  }
}

fn get_id(self: Instruction) -> Int? {
  match self {
    Binary(bin) => bin.value_attr.id
    Unary(un) => un.value_attr.id
    PHI(phi) => phi.value_attr.id
    Compare(cmp) => cmp.value_attr.id
    Branch(br) => br.value_attr.id
    Call(call) => call.value_attr.id
    Return(ret) => ret.value_attr.id
    Alloca(alloc) => alloc.value_attr.id
    Store(store) => store.value_attr.id
    GetElementPtr(gep) => gep.value_attr.id
    _ => None
  }
}

fn set_name(self: Instruction, name: String) -> Unit {
  match self {
    Binary(bin) => bin.value_attr.name = Some(name)
    Unary(un) => un.value_attr.name = Some(name)
    PHI(phi) => phi.value_attr.name = Some(name)
    Compare(cmp) => cmp.value_attr.name = Some(name)
    Branch(_) => { println("Branch instruction cannot set name!"); panic()}
    Call(call) => call.value_attr.name = Some(name)
    Return(ret) => ret.value_attr.name = Some(name)
    Alloca(alloc) => alloc.value_attr.name = Some(name)
    Store(_) => { println("Store instruction cannot set name!"); panic()}
    GetElementPtr(gep) => gep.value_attr.name = Some(name)
    _ => {
      println("Instruction set_name failed!")
      panic()
    }
  }
}

fn get_uses(self: Instruction) -> Array[Value] {
  match self {
    Binary(bin) => bin.value_attr.uses
    Unary(un) => un.value_attr.uses
    PHI(phi) => phi.value_attr.uses
    Compare(cmp) => cmp.value_attr.uses
    Branch(br) => br.value_attr.uses
    Call(call) => call.value_attr.uses
    Return(ret) => ret.value_attr.uses
    Alloca(alloc) => alloc.value_attr.uses
    Store(store) => store.value_attr.uses
    GetElementPtr(gep) => gep.value_attr.uses
    _ => {
      println("Instruction get_uses failed!")
      panic()
    }
  }
}

fn get_users(self: Instruction) -> Array[Value] {
  match self {
    Binary(bin) => bin.value_attr.users
    Unary(un) => un.value_attr.users
    PHI(phi) => phi.value_attr.users
    Compare(cmp) => cmp.value_attr.users
    Branch(br) => br.value_attr.users
    Call(call) => call.value_attr.users
    Return(ret) => ret.value_attr.users
    Alloca(alloc) => alloc.value_attr.users
    Store(store) => store.value_attr.users
    GetElementPtr(gep) => gep.value_attr.users
    _ => {
      println("Instruction get_users failed!")
      panic()
    }
  }
}

fn add_user[T: IsValue](self: Instruction, user: T) -> Unit {
  match self {
    Binary(bin) => bin.add_user(user)
    Unary(un) => un.add_user(user)
    PHI(phi) => phi.add_user(user)
    Compare(cmp) => cmp.add_user(user)
    Branch(_) => { println("Branch instruction cannot add user!"); panic() }
    Call(call) => call.add_user(user)
    Return(ret) => ret.add_user(user)
    Alloca(alloc) => alloc.add_user(user)
    Store(_) => { println("Store instruction cannot add user!"); panic()}
    GetElementPtr(gep) => gep.add_user(user)
    _ => {
      println("Instruction add_user failed!")
      panic()
    }
  }
}

fn to_string(self: Instruction) -> String {
  match self {
    Binary(bin) => bin.to_string()
    Unary(un) => un.to_string()
    PHI(phi) => phi.to_string()
    Compare(cmp) => cmp.to_string()
    Branch(br) => br.to_string()
    Call(call) => call.to_string()
    Return(ret) => ret.to_string()
    Alloca(alloc) => alloc.to_string()
    Store(store) => store.to_string()
    GetElementPtr(gep) => gep.to_string()
    _ => "Unimplement Instruction to_string!"
  }
}

fn trivial_stringify(self: Instruction) -> String {
  match self {
    Binary(bin) => bin.trivial_stringify()
    Unary(un) => un.trivial_stringify()
    PHI(phi) => phi.trivial_stringify()
    Compare(cmp) => cmp.trivial_stringify()
    Call(call) => call.trivial_stringify()
    Alloca(alloc) => alloc.trivial_stringify()
    GetElementPtr(gep) => gep.trivial_stringify()
    _ => ""
  }
}

fn get_attribute(self: Instruction) -> ValueAttribute {
  match self {
    Binary(bin) => bin.value_attr
    Unary(un) => un.value_attr
    PHI(phi) => phi.value_attr
    Compare(cmp) => cmp.value_attr
    Branch(br) => br.value_attr
    Call(call) => call.value_attr
    Return(ret) => ret.value_attr
    Alloca(alloc) => alloc.value_attr
    Store(store) => store.value_attr
    GetElementPtr(gep) => gep.value_attr
    _ => {
      println("Instruction get_attribute failed!")
      panic()
    }
  }
}

fn get_live_analysis(self: Instruction) -> LiveAnalysis? {
  self.get_attribute().analysis.get_live_analysis()
}

fn set_parent(self: Instruction, parent: BasicBlock) -> Unit {
  match self {
    Binary(bin) => bin.value_attr.set_parent(parent)
    Unary(un) => un.value_attr.set_parent(parent)
    PHI(phi) => phi.value_attr.set_parent(parent)
    Compare(cmp) => cmp.value_attr.set_parent(parent)
    Branch(br) => br.value_attr.set_parent(parent)
    Call(call) => call.value_attr.set_parent(parent)
    Return(ret) => ret.value_attr.set_parent(parent)
    Alloca(alloc) => alloc.value_attr.set_parent(parent)
    Store(store) => store.value_attr.set_parent(parent)
    GetElementPtr(gep) => gep.value_attr.set_parent(parent)
    _ => {
      println("Instruction get_attribute failed!")
      panic()
    }
  }
}

fn op_equal(self: Instruction, other: Instruction) -> Bool {
  match (self, other) {
    (Binary(bin1), Binary(bin2)) => bin1 == bin2
    (Unary(un1), Unary(un2)) => un1 == un2
    (PHI(phi1), PHI(phi2)) => phi1 == phi2
    (Compare(cmp1), Compare(cmp2)) => cmp1 == cmp2
    (Branch(br1), Branch(br2)) => br1 == br2
    (Call(call1), Call(call2)) => call1 == call2
    (Return(ret1), Return(ret2)) => ret1 == ret2
    (Alloca(alloc1), Alloca(alloc2)) => alloc1 == alloc2
    (Store(store1), Store(store2)) => store1 == store2
    (GetElementPtr(gep1), GetElementPtr(gep2)) => gep1 == gep2
    _ => false
  }
}

pub fn erase_from_parent(self: Instruction) -> Unit {
  let parent = self.get_attribute().parent
  match parent {
    Some(p) => p.remove_instruction(self)
    _ => ()
  }
}

pub fn insert_before[T: IsInst](self: Instruction, inst: T) -> Unit {
  let inst = inst.as_inst()
  let bb = match self.get_attribute().parent {
    Some(b) => b
    None => {
      println("CallInst insert_before failed!")
      panic()
    }
  }
  let mut idx = 0
  for i, inst in bb.inst_list {
    if inst.as_value() == self.as_value() {
      idx = i
    }
  }
  bb.inst_list.insert(idx, inst)
  inst.get_attribute().parent = Some(bb)
}

pub fn insert_after[T: IsInst](self: Instruction, inst: T) -> Unit {
  let inst = inst.as_inst()
  let bb = match self.get_attribute().parent {
    Some(b) => b
    None => {
      println("CallInst insert_after failed!")
      panic()
    }
  }
  let mut idx = 0
  for i, inst in bb.inst_list {
    if inst.as_value() == self.as_value() {
      idx = i
    }
  }
  bb.inst_list.insert(idx + 1, inst)
  inst.get_attribute().parent = Some(bb)
}

pub fn clear_live_analysis(self: Instruction) -> Unit {
  self.get_attribute().analysis.clear_live_analysis()
}

pub fn clear_virtual_reg(self: Instruction) -> Unit {
  self.get_attribute().analysis.clear_virtual_reg()
}

/// --------------------------------------------------
/// ## Binary Operator
/// --------------------------------------------------
struct BinaryInst {
  value_attr : ValueAttribute
  opcode : BinaryOperator
}

enum BinaryOperator {
  Add
  Sub
  Mul
  Div
  Rem
  And
  Or
  Xor
} derive(Eq)

fn to_string_int(self: BinaryOperator) -> String {
  match self {
    BinaryOperator::Add => "add"
    BinaryOperator::Sub => "sub"
    BinaryOperator::Mul => "mul"
    BinaryOperator::Div => "div"
    BinaryOperator::Rem => "rem"
    BinaryOperator::And => "and"
    BinaryOperator::Or => "or"
    BinaryOperator::Xor => "xor"
  }
}

fn to_string_double(self: BinaryOperator) -> String {
  match self {
    BinaryOperator::Add => "fadd"
    BinaryOperator::Sub => "fsub"
    BinaryOperator::Mul => "fmul"
    BinaryOperator::Div => "fdiv"
    _ => {
      println("Unsupported binary operator!")
      panic()
    }
  }
}

fn BinaryInst::create(
  binop: BinaryOperator,
  lhs: Value, rhs: Value, number: Int, ~name: String? = None,
  ~allow_ptr_arith: Bool = false
  ) -> BinaryInst {
  let value_type = align_value_type(lhs, rhs, ~allow_ptr_arith)
  let uses = [lhs, rhs]
  let users = []
  let value_attr = ValueAttribute::create(
    value_type, name, Some(number), uses, users)
  let inst = BinaryInst::{
    value_attr,
    opcode: binop,
  }
  lhs.add_user(inst)
  rhs.add_user(inst)
  inst
}

fn to_string(self : BinaryInst) -> String {
  let mut result = self.trivial_stringify() + " = "
  let op_str = match self.value_attr.value_type {
    Type::Int32Ty => self.opcode.to_string_int() + " nsw"
    Type::DoubleTy => self.opcode.to_string_double()
    Type::PointerTy(_) => self.opcode.to_string_int() + " ptr"
    _ => {
      println("Unsupported binary operator!")
      panic()
    }
  }
  result += op_str + " " + self.value_attr.value_type.to_string()
  let operands_str = self.value_attr.uses.map(
    fn (u) { u.trivial_stringify() }
  ).join(", ")
  result + " " + operands_str
}

fn trivial_stringify(self: BinaryInst) -> String {
  match (self.value_attr.name, self.value_attr.id) {
    (Some(name), _) => "%" + name
    (_, Some(id)) => "%" + id.to_string()
    _ => ""
  }
}

fn output(self : BinaryInst, logger : Logger) -> Unit {
  logger.write_string(self.to_string())
}

fn as_value(self: BinaryInst) -> Value {
  Value::Instruction(Instruction::Binary(self))
}

fn as_inst(self: BinaryInst) -> Instruction {
  Instruction::Binary(self)
}

fn op_equal(self: BinaryInst, other: BinaryInst) -> Bool {
  self.opcode == other.opcode &&
  self.value_attr.id == other.value_attr.id
}

fn get_attribute(self: BinaryInst) -> ValueAttribute {
  self.value_attr
}

fn get_uses(self: BinaryInst) -> Array[Value] {
  self.value_attr.uses
}

fn get_users(self: BinaryInst) -> Array[Value] {
  self.value_attr.users
}

fn add_user[T: IsValue](self: BinaryInst, user: T) -> Unit {
  let user = user.as_value()
  if self.value_attr.users.contains(user) {
    return
  }
  self.value_attr.users.push(user)
}

fn get_lhs(self: BinaryInst) -> Value {
  self.value_attr.uses[0]
}

fn get_rhs(self: BinaryInst) -> Value {
  self.value_attr.uses[1]
}

fn get_type(self: BinaryInst) -> Type {
  self.value_attr.value_type
}

fn clear_live_analysis(self: BinaryInst) -> Unit {
  self.value_attr.analysis.clear_live_analysis()
}

/// --------------------------------------------------
/// ## Unary Operator
/// --------------------------------------------------
struct UnaryInst {
  opcode : UnaryOperator
  value_attr : ValueAttribute
}

enum UnaryOperator {
  Load
} derive(Eq)

fn to_string(self: UnaryOperator) -> String {
  match self {
    UnaryOperator::Load => "load"
  }
}

fn UnaryInst::create(
  unaryop: UnaryOperator, value: Value, load_type: Type,
  number: Int, ~name: String? = None) -> UnaryInst {
  let uses = [value]
  let users = []
  let value_attr = ValueAttribute::create(load_type, name, Some(number), uses, users)
  let inst = UnaryInst::{
    opcode: unaryop,
    value_attr
  }
  value.add_user(inst)
  inst
}

fn to_string(self : UnaryInst) -> String {
  match self.opcode {
    UnaryOperator::Load => {
      let use = self.value_attr.uses[0]
      let mut result = self.trivial_stringify() + " = "
      result += self.opcode.to_string() + " " + self.value_attr.value_type.to_string() + ", "
      result + use.get_type().to_string() + " " + use.trivial_stringify() + ", align " + align_of(self.value_attr.value_type).to_string()
    }
  }

  // let mut result = self.trivial_stringify() + " = "
  // result += self.opcode.to_string() + " " + self.value_attr.value_type.to_string() + ","
  // let operands_str = self.value_attr.uses.map(
  //   fn (u) { u.get_type().to_string() + " " + u.trivial_stringify() }
  // ).join(", ")
  // result + " " + operands_str
}

fn trivial_stringify(self: UnaryInst) -> String {
  match (self.value_attr.name, self.value_attr.id) {
    (Some(name), _) => "%" + name
    (_, Some(id)) => "%" + id.to_string()
    _ => ""
  }
}

pub fn as_value(self: UnaryInst) -> Value {
  Value::Instruction(Instruction::Unary(self))
}

pub fn as_inst(self: UnaryInst) -> Instruction {
  Instruction::Unary(self)
}

fn get_uses(self: UnaryInst) -> Array[Value] {
  self.value_attr.uses
}

fn get_users(self: UnaryInst) -> Array[Value] {
  self.value_attr.users
}

fn add_user[T: IsValue](self: UnaryInst, user: T) -> Unit {
  let user = user.as_value()
  if self.value_attr.users.contains(user) {
    return
  }
  self.value_attr.users.push(user)
}

pub fn get_operand(self: UnaryInst) -> Value {
  self.value_attr.uses[0]
}

pub fn op_equal(self: UnaryInst, other: UnaryInst) -> Bool {
  self.opcode == other.opcode &&
  self.value_attr.id == other.value_attr.id
}

fn clear_live_analysis(self: UnaryInst) -> Unit {
  self.value_attr.analysis.clear_live_analysis()
}

/// --------------------------------------------------
/// ## Alloca Instruction
///
/// ## Note
///
/// In LLVM, Alloca is one of Unary Instruction, but in
/// our implementation, we treat it as a separate
/// --------------------------------------------------
struct AllocaInst {
  value_attr : ValueAttribute
  value_type : Type
  align : Int
}

fn AllocaInst::create(
  value_type: Type, number: Int, ~name: String? = None
) -> AllocaInst {
  let ptr_type = Type::getPointerTy()
  let value_attr = ValueAttribute::create(ptr_type, name, Some(number), [], [])
  AllocaInst::{
    value_attr,
    value_type,
    align : align_of(value_type)
  }
}

pub fn to_string(self: AllocaInst) -> String {
  let head = self.trivial_stringify() + " = "
  head + "alloca \{self.value_type}, align \{self.align}"
}

pub fn trivial_stringify(self: AllocaInst) -> String {
  match (self.value_attr.name, self.value_attr.id) {
    (Some(name), _) => "%" + name
    (_, Some(id)) => "%" + id.to_string()
    _ => ""
  }
}

pub fn as_value(self: AllocaInst) -> Value {
  Value::Instruction(Instruction::Alloca(self))
}

pub fn as_inst(self: AllocaInst) -> Instruction {
  Instruction::Alloca(self)
}

pub fn op_equal(self: AllocaInst, other: AllocaInst) -> Bool {
  self.value_attr.id == other.value_attr.id
}

pub fn get_uses(self: AllocaInst) -> Array[Value] {
  self.value_attr.uses
}

pub fn get_users(self: AllocaInst) -> Array[Value] {
  self.value_attr.users
}

pub fn add_user[T: IsValue](self: AllocaInst, user: T) -> Unit {
  let user = user.as_value()
  if self.value_attr.users.contains(user) {
    return
  }
  self.value_attr.users.push(user)
}

fn clear_live_analysis(self: AllocaInst) -> Unit {
  self.value_attr.analysis.clear_live_analysis()
}

/// --------------------------------------------------
/// ## PHI Node
/// --------------------------------------------------
struct PHINode {
  value_attr : ValueAttribute
  reserved : Int
  incoming: Array[(Value, BasicBlock)]
}

fn PHINode::create(
  value_type : Type,
  reserved: Int, number: Int,
  ~name: String? = None
) -> PHINode {
  let uses = []
  let users = []
  let value_attr = ValueAttribute::create(value_type, name, Some(number), uses, users)
  PHINode::{
    value_attr,
    reserved,
    incoming: []
  }
}

pub fn addIncoming[T: IsValue](self: PHINode, value: T, block: BasicBlock) -> Unit {
  let value = value.as_value()
  self.value_attr.uses.push(value)
  let incoming = (value, block)
  if self.incoming.length() >= self.reserved {
    println("PHINode incoming is full!")
    panic()
  }
  self.incoming.push(incoming)
  value.add_user(self)
}

pub fn to_string(self: PHINode) -> String {
  let head = self.trivial_stringify() + " = "
  let incomings = self.incoming.map(fn (pair) { let (v, b) = pair;
    "[" + v.trivial_stringify() + ", %" + b.trivial_stringify() + "]"
  }).join(", ")
  let tail = "phi " + self.value_attr.value_type.to_string() + " " + incomings
  head + tail
}

pub fn trivial_stringify(self: PHINode) -> String {
  match (self.value_attr.name, self.value_attr.id) {
    (Some(name), _) => "%" + name
    (_, Some(id)) => "%" + id.to_string()
    _ => ""
  }
}

pub fn as_value(self: PHINode) -> Value {
  Value::Instruction(Instruction::PHI(self))
}

pub fn as_inst(self: PHINode) -> Instruction {
  Instruction::PHI(self)
}

pub fn get_incomings(self: PHINode) -> Array[(Value, BasicBlock)] {
  self.incoming
}

pub fn get_uses(self: PHINode) -> Array[Value] {
  self.value_attr.uses
}

pub fn get_users(self: PHINode) -> Array[Value] {
  self.value_attr.users
}

pub fn add_user[T: IsValue](self: PHINode, user: T) -> Unit {
  let user = user.as_value()
  if self.value_attr.users.contains(user) {
    return
  }
  self.value_attr.users.push(user)
}

pub fn op_equal(self: PHINode, other: PHINode) -> Bool {
  self.value_attr.id == other.value_attr.id
}

pub fn clear_live_analysis(self: PHINode) -> Unit {
  self.value_attr.analysis.clear_live_analysis()
}

/// --------------------------------------------------
/// ## Compare Instruction
/// --------------------------------------------------
struct CmpInst {
  value_attr : ValueAttribute
  predicate: Predicate
}

enum Predicate {
  FCMP_FALSE ///< 0 0 0 0    Always false (always folded)
  FCMP_OEQ   ///< 0 0 0 1    True if ordered and equal
  FCMP_OGT   ///< 0 0 1 0    True if ordered and greater than
  FCMP_OGE   ///< 0 0 1 1    True if ordered and greater than or equal
  FCMP_OLT   ///< 0 1 0 0    True if ordered and less than
  FCMP_OLE   ///< 0 1 0 1    True if ordered and less than or equal
  FCMP_ONE   ///< 0 1 1 0    True if ordered and operands are unequal
  FCMP_ORD   ///< 0 1 1 1    True if ordered (no nans)
  FCMP_UNO   ///< 1 0 0 0    True if unordered: isnan(X) | isnan(Y)
  FCMP_UEQ   ///< 1 0 0 1    True if unordered or equal
  FCMP_UGT  ///< 1 0 1 0    True if unordered or greater than
  FCMP_UGE  ///< 1 0 1 1    True if unordered, greater than, or equal
  FCMP_ULT  ///< 1 1 0 0    True if unordered or less than
  FCMP_ULE  ///< 1 1 0 1    True if unordered, less than, or equal
  FCMP_UNE  ///< 1 1 1 0    True if unordered or not equal
  FCMP_TRUE ///< 1 1 1 1    Always true (always folded)
  ICMP_EQ  ///< equal
  ICMP_NE  ///< not equal
  ICMP_UGT ///< unsigned greater than
  ICMP_UGE ///< unsigned greater or equal
  ICMP_ULT ///< unsigned less than
  ICMP_ULE ///< unsigned less or equal
  ICMP_SGT ///< signed greater than
  ICMP_SGE ///< signed greater or equal
  ICMP_SLT ///< signed less than
  ICMP_SLE ///< signed less or equal
} derive(Eq)

fn to_string(self: Predicate) -> String {
  match self {
    Predicate::ICMP_SGT => "icmp sgt"
    Predicate::ICMP_SGE => "icmp sge"
    Predicate::ICMP_SLT => "icmp slt"
    Predicate::ICMP_SLE => "icmp sle"
    Predicate::ICMP_UGT => "icmp ugt"
    Predicate::ICMP_UGE => "icmp uge"
    Predicate::ICMP_ULT => "icmp ult"
    Predicate::ICMP_ULE => "icmp ule"
    Predicate::ICMP_EQ => "icmp eq"
    Predicate::ICMP_NE => "icmp ne"
    Predicate::FCMP_OEQ => "fcmp oeq"
    Predicate::FCMP_OGT => "fcmp ogt"
    Predicate::FCMP_OGE => "fcmp oge"
    Predicate::FCMP_OLT => "fcmp olt"
    Predicate::FCMP_OLE => "fcmp ole"
    Predicate::FCMP_ONE => "fcmp one"
    Predicate::FCMP_ORD => "fcmp ord"
    Predicate::FCMP_UNO => "fcmp uno"
    Predicate::FCMP_UEQ => "fcmp ueq"
    Predicate::FCMP_UGT => "fcmp ugt"
    Predicate::FCMP_UGE => "fcmp uge"
    Predicate::FCMP_ULT => "fcmp ult"
    Predicate::FCMP_ULE => "fcmp ule"
    Predicate::FCMP_UNE => "fcmp une"
    _ => {
      println("Predicate to_string failed!")
      panic()
    }
  }
}

fn CmpInst::create(
  predicate: Predicate, lhs: Value, rhs: Value,
  number: Int, ~name: String? = None) -> CmpInst {
  let value_type = align_value_type(lhs, rhs)
  let uses = [lhs, rhs]
  let users = []
  let value_attr = ValueAttribute::create(value_type, name, Some(number), uses, users)
  let inst = CmpInst::{
    value_attr,
    predicate
  }
  lhs.add_user(inst)
  rhs.add_user(inst)
  inst
}

pub fn to_string(self: CmpInst) -> String {
  let head = self.trivial_stringify() + " = "
  head + self.predicate.to_string() + " " +
  self.value_attr.value_type.to_string() + " " +
  self.value_attr.uses[0].trivial_stringify() + ", " +
  self.value_attr.uses[1].trivial_stringify()
}

pub fn trivial_stringify(self: CmpInst) -> String {
  match (self.value_attr.name, self.value_attr.id) {
    (Some(name), _) => "%" + name
    (_, Some(id)) => "%" + id.to_string()
    _ => ""
  }
}

pub fn as_value(self: CmpInst) -> Value {
  Value::Instruction(Instruction::Compare(self))
}

pub fn as_inst(self: CmpInst) -> Instruction {
  Instruction::Compare(self)
}

pub fn op_equal(self: CmpInst, other: CmpInst) -> Bool {
  self.predicate == other.predicate &&
  self.value_attr.id == other.value_attr.id
}

pub fn get_lhs(self: CmpInst) -> Value {
  self.value_attr.uses[0]
}

pub fn get_rhs(self: CmpInst) -> Value {
  self.value_attr.uses[1]
}

pub fn get_uses(self: CmpInst) -> Array[Value] {
  self.value_attr.uses
}

pub fn get_users(self: CmpInst) -> Array[Value] {
  self.value_attr.users
}

pub fn add_user[T: IsValue](self: CmpInst, user: T) -> Unit {
  let user = user.as_value()
  if self.value_attr.users.contains(user) {
    return
  }
  self.value_attr.users.push(user)
}

fn clear_live_analysis(self: CmpInst) -> Unit {
  self.value_attr.analysis.clear_live_analysis()
}

/// --------------------------------------------------
/// Branch Instruction
/// --------------------------------------------------
struct BranchInst {
  value_attr : ValueAttribute
  condition: Value?
  if_true: BasicBlock?
  if_false: BasicBlock?
}

fn BranchInst::create_cond_br(
  condition: Value,
  true_block: BasicBlock, false_block: BasicBlock
) -> BranchInst {
  let value_type = Type::getBoolTy()
  let uses = [condition]
  let users = []
  let value_attr = ValueAttribute::create(value_type, None, None, uses, users)
  let inst = BranchInst::{
    value_attr,
    condition: Some(condition),
    if_true: Some(true_block),
    if_false:Some(false_block)
  }
  condition.add_user(inst)
  inst
}

fn BranchInst::create_br(
  target: BasicBlock
) -> BranchInst {
  let value_type = Type::getBoolTy()
  let uses = []
  let users = []
  let value_attr = ValueAttribute::create(value_type, None, None, uses, users)
  BranchInst::{
    value_attr,
    condition: None,
    if_true: Some(target),
    if_false: None
  }
}

fn to_string(self: BranchInst) -> String {
  match (self.condition, self.if_true, self.if_false) {
    (Some(cond), Some(true_block), Some(false_block)) => {
      "br i1 \{cond.trivial_stringify()}, label %\{true_block.trivial_stringify()}, label %\{false_block.trivial_stringify()}"
    }
    (None, Some(target), None) => {
      "br label %\{target.trivial_stringify()}"
    }
    _ => {
      println("print branch inst failed!")
      panic()
    }
  }
}

fn as_value(self: BranchInst) -> Value {
  Value::Instruction(Instruction::Branch(self))
}

pub fn as_inst(self: BranchInst) -> Instruction {
  Instruction::Branch(self)
}

pub fn op_equal(self: BranchInst, other: BranchInst) -> Bool {
  self.value_attr.id == other.value_attr.id
}

pub fn get_uses(self: BranchInst) -> Array[Value] {
  self.value_attr.uses
}

pub fn get_users(self: BranchInst) -> Array[Value] {
  self.value_attr.users
}

// No add user for branch instruction

fn clear_live_analysis(self: BranchInst) -> Unit {
  self.value_attr.analysis.clear_live_analysis()
}

/// --------------------------------------------------
/// Call Instruction
/// --------------------------------------------------
struct CallInst {
  value_attr : ValueAttribute
  function_type: Type
  mut callee: Value
  arg_list: Array[Value]
}

fn CallInst::create(
  function_type: Type,
  callee: Value,
  arg_list: Array[Value],
  number: Int?, ~name: String? = None
) -> CallInst {
  guard let Type::FunctionTy(_, ret_type) = function_type
  let uses = arg_list
  let users = []
  let value_attr = ValueAttribute::create(
    ret_type, name, number, uses, users
  )
  let inst = CallInst::{
    value_attr,
    callee,
    function_type,
    arg_list
  }
  callee.add_user(inst)
  arg_list.each(fn (arg) { arg.add_user(inst) })
  inst
}

pub fn to_string(self: CallInst) -> String {
  let trivial_str = self.trivial_stringify()
  let mut result = match trivial_str {
    "" => ""
    _ => trivial_str + " = "
  }
  result += "call \{self.value_attr.value_type} \{self.callee.trivial_stringify()}"
  let args_str = self.arg_list.map(fn (arg) {
    arg.get_type().to_string() + " " +
    arg.trivial_stringify()
  }).join(", ")
  result + "(" + args_str + ")"
}

fn trivial_stringify(self: CallInst) -> String {
  match (self.value_attr.name, self.value_attr.id) {
    (Some(name), _) => "%" + name
    (_, Some(id)) => "%" + id.to_string()
    _ => ""
  }
}

pub fn as_value(self: CallInst) -> Value {
  Value::Instruction(Instruction::Call(self))
}

pub fn as_inst(self: CallInst) -> Instruction {
  Instruction::Call(self)
}

pub fn op_equal(self: CallInst, other: CallInst) -> Bool {
  // self.value_attr.id == other.value_attr.id
  match (self.value_attr.id, other.value_attr.id) {
    (Some(id1), Some(id2)) => id1 == id2
    (_, Some(_)) => false
    (Some(_), _) => false
    (None, None) => {
      // if this situation happened, compare callee, if also equal
      // comprare arg_list, if callee, arg_list are all equal, then
      // it is equal
      if self.callee == other.callee {
        self.arg_list == other.arg_list
      } else {
        false
      }
    }
  }
}

pub fn return_void(self: CallInst) -> Bool {
  self.value_attr.value_type == Type::VoidTy
}

pub fn get_live_out(self: CallInst) ->  @hashset.T[Value]? {
  match self.value_attr.analysis.get_live_analysis() {
    Some(live) => Some(live.live_out)
    None => None
  }
}

pub fn insert_before[T: IsInst](self: CallInst, inst: T) -> Unit {
  let inst = inst.as_inst()
  let bb = match self.value_attr.parent {
    Some(b) => b
    None => {
      println("CallInst insert_before failed!")
      panic()
    }
  }
  let mut idx = 0
  for i, inst in bb.inst_list {
    if inst.as_value() == self.as_value() {
      idx = i
    }
  }
  bb.inst_list.insert(idx, inst)
}

pub fn insert_after[T: IsInst](self: CallInst, inst: T) -> Unit {
  let inst = inst.as_inst()
  let bb = match self.value_attr.parent {
    Some(b) => b
    None => {
      println("CallInst insert_after failed!")
      panic()
    }
  }
  let mut idx = 0
  for i, inst in bb.inst_list {
    if inst.as_value() == self.as_value() {
      idx = i
    }
  }
  bb.inst_list.insert(idx + 1, inst)
}

pub fn get_uses(self: CallInst) -> Array[Value] {
  self.value_attr.uses
}

pub fn get_users(self: CallInst) -> Array[Value] {
  self.value_attr.users
}

pub fn replace_arg(self: CallInst, old: Value, new: Value) -> Unit {
  for i, arg in self.arg_list {
    if arg == old {
      self.arg_list[i] = new
      // old.remove_user(self)
      new.add_user(self)
    }
  }
}

pub fn add_user[T: IsValue](self: CallInst, user: T) -> Unit {
  let user = user.as_value()
  if self.value_attr.users.contains(user) {
    return
  }
  self.value_attr.users.push(user)
}

fn clear_live_analysis(self: CallInst) -> Unit {
  self.value_attr.analysis.clear_live_analysis()
}

/// --------------------------------------------------
/// Return Instruction
/// --------------------------------------------------
struct ReturnInst {
  value_attr : ValueAttribute
}

fn ReturnInst::create[T: IsValue](
  return_value: T
) -> ReturnInst {
  let return_value = return_value.as_value()
  let value_type = return_value.get_type()
  let uses = [return_value]
  let users = []
  let value_attr = ValueAttribute::create(value_type, None, None, uses, users)
  let inst = ReturnInst::{
    value_attr,
  }
  return_value.add_user(inst)
  inst
}

fn ReturnInst::createVoid() -> ReturnInst {
  let value_attr = ValueAttribute::default(Type::VoidTy)
  ReturnInst::{
    value_attr,
  }
}

fn get_return_value(self: ReturnInst) -> Value? {
  match self.value_attr.uses.length() {
    1 => Some(self.value_attr.uses[0])
    0 => None
    _ => {
      println("could not return multiple values!")
      panic()
    }
  }
}

fn to_string(self : ReturnInst) -> String {
  match self.value_attr.uses.length() {
    1 => "ret " + self.value_attr.value_type.to_string() + " " + self.value_attr.uses[0].trivial_stringify()
    0 => "ret void"
    _ => {
      println("could not return multiple values!")
      panic()
    }
  }
}

pub fn as_value(self: ReturnInst) -> Value {
  Value::Instruction(self.as_inst())
}

pub fn as_inst(self: ReturnInst) -> Instruction {
  Instruction::Return(self)
}

pub fn op_equal(self: ReturnInst, other: ReturnInst) -> Bool {
  self.value_attr.id == other.value_attr.id
}

pub fn get_uses(self: ReturnInst) -> Array[Value] {
  self.value_attr.uses
}

pub fn get_users(self: ReturnInst) -> Array[Value] {
  self.value_attr.users
}

pub fn add_user[T: IsValue](self: ReturnInst, user: T) -> Unit {
  let user = user.as_value()
  if self.value_attr.users.contains(user) {
    return
  }
  self.value_attr.users.push(user)
}

fn clear_live_analysis(self: ReturnInst) -> Unit {
  self.value_attr.analysis.clear_live_analysis()
}

/// --------------------------------------------------
/// Store Instruction
/// --------------------------------------------------
struct StoreInst {
  value_attr : ValueAttribute
  mut value: Value
  mut pointer: Value
  align: Int
}

fn StoreInst::create(
  value: Value, pointer: Value
) -> StoreInst {
  let uses = [value, pointer]
  let users = []
  let value_attr = ValueAttribute::create(Type::VoidTy, None, None, uses, users)
  let inst = StoreInst::{
    value_attr,
    value,
    pointer,
    align: align_of(value.get_type())
  }
  value.add_user(inst)
  pointer.add_user(inst)
  inst
}

fn to_string(self: StoreInst) -> String {
  let value_type_str = self.value.get_type().to_string()
  let value_name_str = self.value.trivial_stringify()
  let ptr_type_str = self.pointer.get_type().to_string()
  let ptr_name_str = self.pointer.trivial_stringify()
  let align_str = self.align.to_string()
  "store \{value_type_str} \{value_name_str}, \{ptr_type_str} \{ptr_name_str}, align \{align_str}"
  // "store \{self.value.get_type()} \{self.value.trivial_stringify()}, \{self.pointer.get_type()} \{self.pointer.trivial_stringify()}, align \{self.align}"
}

fn as_value(self: StoreInst) -> Value {
  Value::Instruction(Instruction::Store(self))
}

fn as_inst(self: StoreInst) -> Instruction {
  Instruction::Store(self)
}

fn op_equal(self: StoreInst, other: StoreInst) -> Bool {
  self.value == other.value && self.pointer == other.pointer
}

fn get_uses(self: StoreInst) -> Array[Value] {
  self.value_attr.uses
}

fn get_users(self: StoreInst) -> Array[Value] {
  self.value_attr.users
}

// No add user for store instruction

fn clear_live_analysis(self: StoreInst) -> Unit {
  self.value_attr.analysis.clear_live_analysis()
}

/// --------------------------------------------------
/// GetElementPtr Instruction
/// --------------------------------------------------
struct GetElementPtrInst {
  value_attr : ValueAttribute
  pointedTy: Type
  mut pointer: Value
  index_list: Array[Value]
}

fn GetElementPtrInst::create(
  pointedTy: Type,
  pointer: Value, index_list: Array[Value],
  number: Int, ~name: String? = None
) -> GetElementPtrInst {
  if not(pointer.get_type().is_pointer()) {
    println("GetElementPtrInst pointer type is not pointer! it is \{pointer.get_type()}")
    panic()
  }
  let ptr_type = Type::getPointerTy(ty=pointedTy)
  let uses = [pointer] + index_list
  let users = []
  let value_attr = ValueAttribute::create(ptr_type, name, Some(number), uses, users)
  let inst = GetElementPtrInst::{
    value_attr,
    pointedTy,
    pointer,
    index_list
  }
  pointer.add_user(inst)
  index_list.each(fn (index) { index.add_user(inst) })
  inst
}

pub fn get_element_type(self: GetElementPtrInst) -> Type {
  self.pointedTy
}

fn to_string(self: GetElementPtrInst) -> String {
  let mut result = self.trivial_stringify() + " = "
  result += "getelementptr inbounds \{self.pointedTy}, \{self.pointer.get_type()} \{self.pointer.trivial_stringify()}"
  let index_str = self.index_list.map(fn (index) { index.get_type().to_string() + " " + index.trivial_stringify() }).join(", ")
  result + ", " + index_str
}

pub fn trivial_stringify(self: GetElementPtrInst) -> String {
  match (self.value_attr.name, self.value_attr.id) {
    (Some(name), _) => "%" + name
    (_, Some(id)) => "%" + id.to_string()
    _ => ""
  }
}

pub fn as_value(self: GetElementPtrInst) -> Value {
  Value::Instruction(Instruction::GetElementPtr(self))
}

pub fn as_inst(self: GetElementPtrInst) -> Instruction {
  Instruction::GetElementPtr(self)
}

pub fn get_uses(self: GetElementPtrInst) -> Array[Value] {
  self.value_attr.uses
}

pub fn get_users(self: GetElementPtrInst) -> Array[Value] {
  self.value_attr.users
}

pub fn add_user[T: IsValue](self: GetElementPtrInst, user: T) -> Unit {
  let user = user.as_value()
  if self.value_attr.users.contains(user) {
    return
  }
  self.value_attr.users.push(user)
}

pub fn op_equal(self: GetElementPtrInst, other: GetElementPtrInst) -> Bool {
  self.value_attr.id == other.value_attr.id
}

fn clear_live_analysis(self: GetElementPtrInst) -> Unit {
  self.value_attr.analysis.clear_live_analysis()
}
