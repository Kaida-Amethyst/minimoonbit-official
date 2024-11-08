struct IRBuilder {
  // TODO: Add Basic Block
  function : Function
  mut insert_point : BasicBlock?
  opt: Bool
}

pub fn IRBuilder::new(function : Function, ~opt: Bool = true) -> IRBuilder {
  IRBuilder::{
    function,
    insert_point: None,
    opt
  }
}

pub fn IRBuilder::setInsertPoint(self : IRBuilder, block : BasicBlock) -> Unit {
  self.insert_point = Some(block)
}

pub fn setEntry(self : IRBuilder, entry_block: BasicBlock) -> Unit {
  match self.function.entry_block {
    Some(_) => {
      println("Error: Entry block is already set")
      panic()
    }
    None => {
      self.function.entry_block = Some(entry_block)
    }
  }
}

pub fn push_instruction[T: IsInst](
  self : IRBuilder,
  inst : T
) -> Unit {
  let inst = inst.as_inst()
  match self.insert_point {
    Some(block) => {
      block.push_instruction(inst)
      inst.set_parent(block)
    }
    None => {
      println("Insert point is not set")
      panic()
    }
  }
  match inst {
    PHI(phi) => self.function.phi_nodes.push(phi)
    Call(call) => self.function.call_insts.push(call)
    _ => ()
  }
}

fn get_value_number(self : IRBuilder) -> Int {
  self.function.value_number
}

pub fn getInsertBlock(self: IRBuilder) -> BasicBlock {
  match self.insert_point {
    Some(block) => block
    None => {
      println("Insert point is not set")
      panic()
    }
  }
}

/// ---------------------------------------------------------
/// ## Create Binary Instruction
/// ---------------------------------------------------------
fn create_binary_inst(
  self : IRBuilder,
  lhs : Value, rhs : Value,
  opcode : BinaryOperator,
  ~name : String? = None
) -> BinaryInst {
  let bin_inst = BinaryInst::create(
    opcode, lhs, rhs, self.get_value_number(), name=name
  )
  self.function.value_number += 1
  bin_inst
}

pub fn createAdd[TL: IsValue, TR: IsValue](
  self : IRBuilder,
  lhs : TL, rhs : TR,
  ~name : String? = None
) -> Value {
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  if self.opt {
    match (lhs, rhs) {
      (Constant(lconst), Constant(rconst)) => {
        let folded = binary_constant_fold(BinaryOperator::Add, lconst, rconst)
        return folded.as_value()
      }
      _ => ()
    }
  }
  let res = self.create_binary_inst(lhs, rhs, BinaryOperator::Add, ~name)
  self.push_instruction(res)
  res.as_value()
}

pub fn createSub[TL: IsValue, TR: IsValue](
  self : IRBuilder,
  lhs : TL, rhs : TR,
  ~name : String? = None
) -> Value {
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  if self.opt {
    match (lhs, rhs) {
      (Constant(lconst), Constant(rconst)) => {
        let folded = binary_constant_fold(BinaryOperator::Sub, lconst, rconst)
        return folded.as_value()
      }
      _ => ()
    }
  }
  let res = self.create_binary_inst(lhs, rhs, BinaryOperator::Sub, ~name)
  self.push_instruction(res)
  res.as_value()
}

pub fn createMul[TL: IsValue, TR: IsValue](
  self : IRBuilder,
  lhs : TL, rhs : TR,
  ~name : String? = None
) -> Value {
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  if self.opt {
    match (lhs, rhs) {
      (Constant(lconst), Constant(rconst)) => {
        let folded = binary_constant_fold(BinaryOperator::Mul, lconst, rconst).as_value()
        return folded
      }
      _ => ()
    }
  }
  let res = self.create_binary_inst(lhs, rhs, BinaryOperator::Mul, ~name)
  self.push_instruction(res)
  res.as_value()
}

pub fn createDiv[TL: IsValue, TR: IsValue](
  self : IRBuilder,
  lhs : TL, rhs : TR,
  ~name : String? = None
) -> Value {
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  if self.opt {
    match (lhs, rhs) {
      (Constant(lconst), Constant(rconst)) => {
        let folded = binary_constant_fold(BinaryOperator::Div, lconst, rconst).as_value()
        return folded
      }
      _ => ()
    }
  }
  let res = self.create_binary_inst(lhs, rhs, BinaryOperator::Div, ~name)
  self.push_instruction(res)
  res.as_value()
}

pub fn createAnd[TL: IsValue, TR: IsValue](
  self : IRBuilder,
  lhs : TL, rhs : TR,
  ~name : String? = None
) -> Value {
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let res = self.create_binary_inst(lhs, rhs, BinaryOperator::And, ~name)
  self.push_instruction(res)
  res.as_value()
}

pub fn createOr[TL: IsValue, TR: IsValue](
  self : IRBuilder,
  lhs : TL, rhs : TR,
  ~name : String? = None
) -> Value {
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let res = self.create_binary_inst(lhs, rhs, BinaryOperator::Or, ~name)
  self.push_instruction(res)
  res.as_value()
}

pub fn createXor[TL: IsValue, TR: IsValue](
  self : IRBuilder,
  lhs : TL, rhs : TR,
  ~name : String? = None
) -> Value {
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let res = self.create_binary_inst(lhs, rhs, BinaryOperator::Xor, ~name)
  self.push_instruction(res)
  res.as_value()
}

fn binary_constant_fold(
  opcode : BinaryOperator,
  lhs : Constant, rhs : Constant
) -> Constant {
  fn unreachable() -> Unit {
    println("Constant Folding Error: unreachable code!, loc: binary_constant_fold")
    panic()
  }
  match opcode {
    BinaryOperator::Add => {
      match (lhs, rhs) {
        (ConstantInt(l), ConstantInt(r)) => ConstantInt(l + r)
        (ConstantFP(l),  ConstantFP(r)) => ConstantFP(l + r)
        _ => {unreachable(); panic()}
      }
    }
    BinaryOperator::Sub => {
      match (lhs, rhs) {
        (ConstantInt(l), ConstantInt(r)) => ConstantInt(l - r)
        (ConstantFP(l),  ConstantFP(r)) => ConstantFP(l - r)
        _ => {unreachable(); panic()}
      }
    }
    BinaryOperator::Mul => {
      match (lhs, rhs) {
        (ConstantInt(l), ConstantInt(r)) => ConstantInt(l * r)
        (ConstantFP(l),  ConstantFP(r)) => ConstantFP(l * r)
        _ => {unreachable(); panic()}
      }
    }
    BinaryOperator::Div => {
      match (lhs, rhs) {
        (ConstantInt(l), ConstantInt(r)) => ConstantInt(l / r)
        (ConstantFP(l), ConstantFP(r)) => ConstantFP(l / r)
        _ => {unreachable(); panic()}
      }
    }
    _ => {unreachable(); panic()}
  }
}

/// ------------------------------------------------------
/// ## Create Unary Instruction
/// ------------------------------------------------------
pub fn createLoad[T: IsValue](
  self : IRBuilder,
  load_type : Type,
  source : T,
  ~name : String? = None
) -> UnaryInst {
  let load_inst = UnaryInst::create(
    UnaryOperator::Load, source.as_value(), load_type,
    self.get_value_number(), ~name
  )
  self.function.value_number += 1
  self.push_instruction(load_inst)
  load_inst
}

/// ------------------------------------------------------
/// ## Create PHI Node
/// ------------------------------------------------------
pub fn createPHI(
  self: IRBuilder,
  value_type: Type,
  reserved: Int,
  ~name: String = ""
) -> PHINode {
  let name = str_to_option(name)
  let phi = PHINode::create(value_type, reserved, self.get_value_number(), ~name)
  self.function.value_number += 1
  self.push_instruction(phi)
  phi
}

/// ------------------------------------------------------
/// ## Create Compare Instruction
/// ------------------------------------------------------
fn create_compare_inst(
  self: IRBuilder,
  predicate: Predicate,
  lhs: Value, rhs: Value,
  ~name: String? = None
) -> CmpInst {
  let cmp_inst = CmpInst::create(predicate, lhs, rhs, self.get_value_number(), ~name)
  self.function.value_number += 1
  cmp_inst
}

pub fn createICmpSGT[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::ICMP_SGT, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}

pub fn createICmpSLE[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::ICMP_SLE, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}

pub fn createICmpSLT[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::ICMP_SLT, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}

pub fn createICmpSGE[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::ICMP_SGE, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}

pub fn createICmpEQ[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::ICMP_EQ, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}

pub fn createICmpNE[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::ICMP_NE, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}

pub fn createICmpUGT[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::ICMP_UGT, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}

pub fn createICmpULE[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::ICMP_ULE, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}

pub fn createICmpULT[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::ICMP_ULT, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}

pub fn createICmpUGE[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::ICMP_UGE, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}

pub fn createFCmpOEQ[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::FCMP_OEQ, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}

pub fn createFCmpONE[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::FCMP_ONE, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}

pub fn createFCmpOGT[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::FCMP_OGT, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}

pub fn createFCmpOGE[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::FCMP_OGE, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}

pub fn createFCmpOLT[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::FCMP_OLT, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}

pub fn createFCmpOLE[TL: IsValue, TR: IsValue](
  self: IRBuilder, lhs: TL, rhs: TR, ~name: String = ""
) -> Value {
  let name = str_to_option(name)
  let lhs = lhs.as_value()
  let rhs = rhs.as_value()
  let cmp_inst = self.create_compare_inst(Predicate::FCMP_OLE, lhs, rhs, ~name)
  self.push_instruction(cmp_inst)
  cmp_inst.as_value()
}


/// ------------------------------------------------------
/// ## Create Branch Instruction
/// ------------------------------------------------------
pub fn createCondBr[T: IsValue](
  self : IRBuilder,
  condition : T,
  true_block : BasicBlock,
  false_block : BasicBlock
) -> BranchInst {
  let condition = condition.as_value()
  let br_inst = BranchInst::create_cond_br(condition, true_block, false_block)
  self.push_instruction(br_inst)
  br_inst
}

pub fn createBr(
  self: IRBuilder,
  target: BasicBlock
) -> BranchInst {
  let br_inst = BranchInst::create_br(target)
  self.push_instruction(br_inst)
  br_inst
}
/// ------------------------------------------------------
/// ## Create Call Instruction
/// ------------------------------------------------------
pub fn createCall[T: IsValue, T2: IsValue](
  self: IRBuilder,
  function_type: Type,
  callee: T,
  arg_list: Array[T2],
  ~name : String = ""
) -> Value {
  let name = str_to_option(name)
  let callee = callee.as_value()
  let arg_list = arg_list.map(fn (x) {x.as_value()})
  guard let Type::FunctionTy(_, ret_ty) = function_type else {
    _ => {
      println("Error: createCall, function_type is not FunctionTy")
      panic()
    }
  }
  let call_inst = match ret_ty {
    Type::VoidTy => {
      let call_inst = CallInst::create(
        function_type, callee, arg_list, None, ~name
      )
      self.push_instruction(call_inst)
      call_inst
    }
    _ => {
      let number = Some(self.get_value_number())
      let call_inst = CallInst::create(
        function_type, callee, arg_list, number, ~name
      )
      self.push_instruction(call_inst)
      self.function.value_number += 1
      call_inst
    }
  }
  call_inst.as_value()
}


/// ------------------------------------------------------
/// ## Create Return Instruction
/// ------------------------------------------------------
pub fn createRet[T: IsValue](self : IRBuilder, value : T) -> ReturnInst {
  let ret_inst = ReturnInst::create(value)
  self.push_instruction(ret_inst)
  ret_inst
}

pub fn createRetVoid(self: IRBuilder) -> ReturnInst {
  let ret_inst = ReturnInst::createVoid()
  self.push_instruction(ret_inst)
  ret_inst
}

/// ------------------------------------------------------
/// ## Create Alloca Instruction
/// ------------------------------------------------------
pub fn createAlloca(
  self: IRBuilder, value_type: Type, ~name:String = ""
) -> AllocaInst {
  let name = str_to_option(name)
  let alloca_inst = AllocaInst::create(value_type, self.get_value_number(), ~name)
  self.push_instruction(alloca_inst)
  self.function.value_number += 1
  alloca_inst
}

/// ------------------------------------------------------
/// ## Create Store Instruction
/// ------------------------------------------------------
pub fn createStore[T1: IsValue, T2: IsValue](
  self: IRBuilder, value: T1, target: T2
) -> StoreInst {
  let value = value.as_value()
  let target = target.as_value()
  let store_inst = StoreInst::create(value, target)
  self.push_instruction(store_inst)
  store_inst
}

/// ------------------------------------------------------
/// ## Create Get Element Pointer Instruction
/// ------------------------------------------------------
pub fn createGEP[T: IsValue, I: IsValue](
  self: IRBuilder, value_type: Type, ptr_value: T, 
  index_list: Array[I]
) -> GetElementPtrInst {
  let ptr_value = ptr_value.as_value()
  let index_list = index_list.map(fn (x) {x.as_value()})
  let gep_inst = GetElementPtrInst::create(
    value_type, ptr_value, index_list,
    self.function.value_number
  )
  self.push_instruction(gep_inst)
  self.function.value_number += 1
  gep_inst
}
