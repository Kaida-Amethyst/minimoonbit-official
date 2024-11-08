enum RiscVPhysicalLocation {
  SReg(@riscv.Reg)
  FReg(@riscv.FReg)
  Mem(@riscv.MemAccess[@riscv.Reg, Int])
  // outside means the value is not only in reg
  // but also in memory which outside the satck, it only
  // used when func arg is passed by stack (because the func has too many args)
  OutsideSReg(@riscv.Reg, @riscv.MemAccess[@riscv.Reg, Int])
  OutsideFReg(@riscv.FReg, @riscv.MemAccess[@riscv.Reg, Int])
  Label(String)
}
/// ----------------------------------------------
/// Risc-V Code Generation
/// ----------------------------------------------
struct RiscVEmitter {
  module: Module
  code: Array[@riscv.RvAsm]
  funcs: Array[@riscv.AssemblyFunction]
  free_sregs: Array[@riscv.Reg]
  debug: Bool
}

pub fn RiscVEmitter::new(module: Module, debug: Bool) -> RiscVEmitter {
  RiscVEmitter::{
    module: module,
    code: Array::new(),
    funcs: Array::new(),
    free_sregs: Array::new(),
    debug: debug
  }
}

pub fn clear_free_sregs(self: RiscVEmitter) -> Unit {
  self.free_sregs.clear()
}

fn query_loc(self: RiscVEmitter, val: Value) -> RiscVPhysicalLocation {
  match val {
    Constant(ConstantInt(0)) => return RiscVPhysicalLocation::SReg(@riscv.Reg::Zero)
    Function(f) => return RiscVPhysicalLocation::Label(f.name)
    _ => ()
  }
  if not(val.hasAttribute()) {println("value has no attribute"); println(val) ;panic()}
  match val.get_attribute().analysis.riscvPhyLoc {
    Some(loc) => loc
    None => {
      println("find some val didn't have physical location, loc: query_loc, val is \{val.trivial_stringify()}")
      match val {
        Value::Instruction(inst) => {
          println("inst is \{inst.to_string()}")
        }
        Value::Argument(arg) => {
          println("arg is \{arg.trivial_stringify()}")
        }
        _ => {
          println("not inst or arg")
        }
      }
      panic()
    }
  }
}

fn query_sreg(self: RiscVEmitter, val: Value) -> @riscv.Reg {
  match val {
    Constant(ConstantInt(0)) => return @riscv.Reg::Zero
    Constant(ConstantInt(_)) => {
      println("Try to get reg from non-zero constant")
      panic()
    }
    Constant(ConstantFP(_)) => {
      println("Try to get reg from constant fp")
      panic()
    }
    _ => ()
  }
  let phyloc = self.query_loc(val)
  match phyloc {
    SReg(reg) => reg
    OutsideSReg(reg, mem) => {
      self.push_code(@riscv.RvAsm::Ld(reg, mem))
      reg
    }
    OutsideFReg(_, _) => {
      println("val : \{val.trivial_stringify()} not in sreg, it is in freg, and outside stack!")
      panic()
    }
    FReg(_) => {
      println("val : \{val.trivial_stringify()} not in sreg, it is in freg!")
      panic()
    }
    Mem(_) => {
      println("val: \{val.trivial_stringify()} not in sreg, it is in memory!")
      panic()
    }
    Label(_) => {
      println("val: \{val.trivial_stringify()} not in sreg, it is in label!")
      panic()
    }
  }
} 

fn query_freg(self: RiscVEmitter, val: Value) -> @riscv.FReg {
  let phyloc = self.query_loc(val)
  match phyloc {
    FReg(reg) => reg
    OutsideFReg(reg, mem) => {
      self.push_code(@riscv.RvAsm::Fld(reg, mem))
      reg
    }
    OutsideSReg(_, _) => {
      println("val : \{val.trivial_stringify()} not in freg, it is in sreg and outside stack!")
      panic()
    }
    SReg(_) => {
      println("val : \{val.trivial_stringify()} not in freg, it is in sreg!")
      panic()
    }
    Mem(_) => {
      println("val : \{val.trivial_stringify()} not in freg, it is in memory!")
      panic()
    }
    Label(_) => {
      println("val : \{val.trivial_stringify()} not in freg, it is in label!")
      panic()
    }
  }
}

fn query_memory(self: RiscVEmitter, val: Value) -> @riscv.MemAccess[@riscv.Reg, Int] {
  let phyloc = self.query_loc(val)
  match phyloc {
    Mem(mem) => mem
    OutsideSReg(_, mem) => mem
    OutsideFReg(_, mem) => mem
    SReg(_) => {
      println("val not in memory, it is in sreg!")
      panic()
    }
    FReg(_) => {
      println("val not in memory, it is in freg!")
      panic()
    }
    Label(_) => {
      println("val: \{val.trivial_stringify()} not in memory, it is in label!")
      panic()
    }
  }
}

fn push_code(self: RiscVEmitter, code: @riscv.RvAsm) -> Unit {
  self.code.push(code)
}

fn push_comment_if_debug(self: RiscVEmitter, comment: String) -> Unit {
  if self.debug {
    self.push_code(@riscv.RvAsm::Comment(comment))
  }
}

pub fn run(self: RiscVEmitter) -> Unit {
  self.transformModuleToRiscV()
}

pub fn get_code(self: RiscVEmitter) -> Array[@riscv.AssemblyFunction] {
  self.funcs
}

fn collect_free_sreg(self: RiscVEmitter, func:Function) -> Array[@riscv.Reg] {
  let sregs = @hashset.T::from_array([
    @riscv.Reg::A0, @riscv.Reg::A1, @riscv.Reg::A2, @riscv.Reg::A3,
    @riscv.Reg::A4, @riscv.Reg::A5, @riscv.Reg::A6, @riscv.Reg::A7,
    @riscv.Reg::T0, @riscv.Reg::T1, @riscv.Reg::T2, @riscv.Reg::T3,
    @riscv.Reg::T4, @riscv.Reg::T5, @riscv.Reg::T6,
    @riscv.Reg::S1, @riscv.Reg::S2, @riscv.Reg::S3, @riscv.Reg::S4,
    @riscv.Reg::S5, @riscv.Reg::S6, @riscv.Reg::S7, @riscv.Reg::S8,
    @riscv.Reg::S9, @riscv.Reg::S10, @riscv.Reg::S11,
  ])

  for arg in func.args {
    match self.query_loc(arg.as_value()) {
      SReg(reg) => {
        sregs.remove(reg)
      }
      _ => ()
    }
  }

  for bb in func.body {
    for inst in bb.inst_list {
      match inst {
        Store(_) => continue
        Call(c) => {
          if c.return_void() {
            continue
          }
          match self.query_loc(c.as_value()) {
            SReg(reg) => {
              sregs.remove(reg)
            }
            _ => ()
          }
        }
        _ => ()
      }
    }
  }
  sregs.iter().to_array()
}

fn collect_free_freg(self: RiscVEmitter, func: Function) -> Array[@riscv.FReg] {
  let fregs = @hashset.T::from_array([
    @riscv.FReg::Fa0, @riscv.FReg::Fa1, @riscv.FReg::Fa2, @riscv.FReg::Fa3,
    @riscv.FReg::Fa4, @riscv.FReg::Fa5, @riscv.FReg::Fa6, @riscv.FReg::Fa7,
    @riscv.FReg::Fs0, @riscv.FReg::Fs1, @riscv.FReg::Fs2, @riscv.FReg::Fs3,
    @riscv.FReg::Fs4, @riscv.FReg::Fs5, @riscv.FReg::Fs6, @riscv.FReg::Fs7,
    @riscv.FReg::Fs8, @riscv.FReg::Fs9, @riscv.FReg::Fs10, @riscv.FReg::Fs11,
    @riscv.FReg::Ft0, @riscv.FReg::Ft1, @riscv.FReg::Ft2, @riscv.FReg::Ft3,
    @riscv.FReg::Ft4, @riscv.FReg::Ft5, @riscv.FReg::Ft6, @riscv.FReg::Ft7,
    @riscv.FReg::Ft8, @riscv.FReg::Ft9, @riscv.FReg::Ft10, @riscv.FReg::Ft11,
  ])

  for bb in func.body {
    for inst in bb.inst_list {
      match inst {
        Store(_) => continue
        Call(c) => {
          if c.return_void() {
            continue
          }
          match self.query_loc(c.as_value()) {
            FReg(reg) => {
              fregs.remove(reg)
            }
            _ => ()
          }
        }
        _ => ()
      }
    }
  }
  fregs.iter().to_array()
}

/// --------------------------------------------------------
/// Transform Module to Risc-V
/// --------------------------------------------------------
fn transformModuleToRiscV(
  self: RiscVEmitter
) -> Unit {
  for f in self.module.functions {
    match f.body.length() {
      0 => continue
      _ => ()
    }
    let phi_elim_pass = PhiEliminationPass::new()
    phi_elim_pass.run(f)
    let peephole_pass = PeepholePass::new()
    peephole_pass.run(f)
    let promote_imm_pass = PromoteImmPass::new()
    promote_imm_pass.run(f)
    let promote_reg2mem_pass = PromoteReg2MemPass::new()
    promote_reg2mem_pass.run(f)
    let virtual_reg_alloc_pass = VirtualRegisterAllocPass::new()
    virtual_reg_alloc_pass.run(f)
    let riscv_reg_alloc_pass = RiscVRegisterAllocPass::new()
    riscv_reg_alloc_pass.run(f)
    let riscv_mem_alloc_pass = RiscVMemoryAllocPass::new()
    riscv_mem_alloc_pass.run(f)

    self.transformFunctionToRiscV(f)
  }
}

/// --------------------------------------------------------
/// Transform Function to Risc-V
/// --------------------------------------------------------
fn transformFunctionToRiscV(
  self: RiscVEmitter,
  func: Function,
) -> Unit {
  let fname = if func.name == "main" {
    "minimbt_main"
  } else {
    func.name
  }
  let asm_func : @riscv.AssemblyFunction = {
    name: fname,
    export: true,
    body: Array::new(),
  }
  self.code.clear()

  self.clear_free_sregs()
  let free_sregs = self.collect_free_sreg(func)
  free_sregs.each(fn (r) {self.free_sregs.push(r)})

  self.push_comment_if_debug("Function: \{func.name}")
  // self.push_code(@riscv.RvAsm::Label(func.name))
  // 1. first alloc stack space, generate code:
  //   - addi sp, sp, -stack_size
  let stack_size = match func.stack_size {
    Some(size) => size
    None => {
      println("Function stack size not set, loc: transformFunctionToRiscV")
      panic()
    }
  }
  let sp = @riscv.Reg::Sp
  let stack_alloc_inst = @riscv.RvAsm::Addi(sp, sp, -stack_size)
  self.push_code(stack_alloc_inst)

  let (s_saved, f_saved) = collect_saved_regs(func)


  // 2. add fp = sp + stack_size
  self.push_code(@riscv.RvAsm::Addi(@riscv.Reg::Fp, sp, stack_size))

  let mut offset = match func.extra_alloc {
    Some(extra) => extra
    None => 0
  }

  // 2. save callee-saved registers, ra, s1, ..., s11
  // self.push_comment_if_debug("Save callee-saved registers")
  self.push_code(@riscv.RvAsm::Sd(@riscv.Reg::Ra, @riscv.MemAccess::{base: sp, offset}))
  offset += 8
  self.push_code(@riscv.RvAsm::Sd(@riscv.Reg::Fp, @riscv.MemAccess::{base: sp, offset}))
  offset += 8
  for saved_sreg in s_saved {
    self.push_code(@riscv.RvAsm::Sd(saved_sreg, @riscv.MemAccess::{base: sp, offset}))
    offset += 8
  }

  // // 3. save save register fs0 ~ fs11
  // self.push_comment_if_debug("Save callee-saved floating point registers")
  for saved_freg in f_saved {
    self.push_code(@riscv.RvAsm::Fsd(saved_freg, @riscv.MemAccess::{base: sp, offset}))
    offset += 8
  }

  // 3. generate code for function body
  self.push_comment_if_debug("Function body")
  for bb in func.body {
    self.transformBasicBlockToRiscV(bb)
  }

  let mut offset = match func.extra_alloc {
    Some(extra) => extra
    None => 0
  }
  // 4. generate Return
  // restore save register s1 ~ s11
  self.push_comment_if_debug("Restore callee-saved registers")
  self.push_code(@riscv.RvAsm::Ld(@riscv.Reg::Ra, @riscv.MemAccess::{base: @riscv.Reg::Sp, offset}))
  offset += 8
  self.push_code(@riscv.RvAsm::Ld(@riscv.Reg::Fp, @riscv.MemAccess::{base: @riscv.Reg::Sp, offset}))
  offset += 8
  for saved_sreg in s_saved {
    self.push_code(@riscv.RvAsm::Ld(saved_sreg, @riscv.MemAccess::{base: @riscv.Reg::Sp, offset}))
    offset += 8
  }

  for saved_freg in f_saved {
    self.push_code(@riscv.RvAsm::Fld(saved_freg, @riscv.MemAccess::{base: @riscv.Reg::Sp, offset}))
    offset += 8
  }

  // restore save register fs0 ~ fs11
  // self.push_comment_if_debug("Restore callee-saved floating point registers")

  self.push_code(@riscv.RvAsm::Addi(@riscv.Reg::Sp, @riscv.Reg::Sp, stack_size))

  self.push_code(@riscv.RvAsm::Ret)

  self.code.each(fn (c) {asm_func.body.push(c)})
  self.funcs.push(asm_func)
}

/// --------------------------------------------------------
/// Transform BasickBlock to Risc-V
/// --------------------------------------------------------
fn transformBasicBlockToRiscV(
  self: RiscVEmitter,
  bb: BasicBlock,
) -> Unit {
  self.push_comment_if_debug("BasicBlock: \{bb.get_label()}")
  self.push_code(@riscv.RvAsm::Label(bb.get_label()))
  for inst in bb.inst_list {
    self.transformInstToRiscV(inst)
  }
}

/// --------------------------------------------------------
/// Transform Instruction to Risc-V
/// --------------------------------------------------------
fn transformInstToRiscV(
  self: RiscVEmitter,
  inst: Instruction,
) -> Unit {
  match inst {
    Binary(bin) => self.transformBinaryToRiscV(bin)
    Unary(un) => self.transformUnaryToRiscV(un)
    Compare(comp) => {
      // if comp has only one user and this user is branch, do not generate code
      let users = comp.get_users()
      if users.length() == 1 {
        let user = users[0]
        match user {
          Value::Instruction(Branch(_)) => return
          _ => {
            println("unimplement single compare inst to riscv")
            panic()
          }
        }
      }
    }
    Branch(br) => self.transformBranchToRiscV(br)
    Call(call) => self.transformCallToRiscV(call)
    Return(ret) => self.transformReturnToRiscV(ret)
    Store(store) => self.transformStoreToRiscV(store)
    GetElementPtr(gep) => self.transformGetElementPtrToRiscV(gep)
    Alloca(_) => ()
    PHI(_) => ()
    _ => {
      println("Unsupported instruction, loc: transformInstToRiscV")
      println(inst.to_string())
      panic()
    }
  }
}

/// --------------------------------------------------------
/// Transform Binary Instructions to Risc-V
/// --------------------------------------------------------
fn transformSAddToRiscV(self: RiscVEmitter, inst: BinaryInst) -> Unit {
  let lhs = inst.get_lhs()
  let rhs = inst.get_rhs()
  let dst_sreg = self.query_sreg(inst.as_value())
  let code = match (lhs, rhs) {
    // use li instruction
    (Value::Constant(l), Value::Constant(r)) => {
      guard let ConstantInt(lval) = l
      guard let ConstantInt(rval) = r
      let imm = lval + rval
      @riscv.RvAsm::Li(dst_sreg, imm.to_string())
    }
    (Value::Constant(ConstantInt(0)), _) => {
      let rhs_sreg = self.query_sreg(rhs)
      // @riscv.RvAsm::Addi(dst_sreg, rhs_sreg, n.to_int())
      if dst_sreg != rhs_sreg {
        @riscv.RvAsm::Mv(dst_sreg, rhs_sreg)
      } else {
        @riscv.RvAsm::Nop
      }
    }
    (_, Value::Constant(ConstantInt(0))) => {
      let lhs_sreg = self.query_sreg(lhs)
      // @riscv.RvAsm::Addi(dst_sreg, lhs_sreg, n.to_int())
      if dst_sreg != lhs_sreg {
        @riscv.RvAsm::Mv(dst_sreg, lhs_sreg)
      } else {
        @riscv.RvAsm::Nop
      }
    }
    (Value::Constant(ConstantInt(n)), _) => {
      let rhs_sreg = self.query_sreg(rhs)
      @riscv.RvAsm::Addi(dst_sreg, rhs_sreg, n.to_int())
    }
    (_, Value::Constant(ConstantInt(n))) => {
      let lhs_sreg = self.query_sreg(lhs)
      @riscv.RvAsm::Addi(dst_sreg, lhs_sreg, n.to_int())
    }
    _ => {
      let lhs_sreg = self.query_sreg(lhs)
      let rhs_sreg = self.query_sreg(rhs)
      @riscv.RvAsm::Add(dst_sreg, lhs_sreg, rhs_sreg)
    }
  }
  self.push_comment_if_debug(inst.to_string())
  self.push_code(code)
}

fn transformSSubToRiscV(self: RiscVEmitter, inst: BinaryInst) -> Unit {
  let lhs = inst.get_lhs()
  let rhs = inst.get_rhs()
  let dst_sreg = self.query_sreg(inst.as_value())
  // let lhs_sreg = self.query_sreg(lhs)
  // let rhs_sreg = self.query_sreg(rhs)
  // let code = @riscv.RvAsm::Sub(dst_sreg, lhs_sreg, rhs_sreg)
  // self.push_code(code)
  // if rhs is constant, use addi, for example: x - 1 -> x + (-1)
  // if lhs is constant, use sub 0, then add
  // for example: 1 - x -> (0 - x) + 1
  // if both constant, use li
  self.push_comment_if_debug(inst.to_string())
  match (lhs, rhs) {
    (Value::Constant(l), Value::Constant(r)) => {
      guard let ConstantInt(lval) = l
      guard let ConstantInt(rval) = r
      let imm = lval - rval
      self.push_code(@riscv.RvAsm::Li(dst_sreg, imm.to_string()))
    }
    (Value::Constant(ConstantInt(n)), _) => {
      let rhs_sreg = self.query_sreg(rhs)
      self.push_code(@riscv.RvAsm::Sub(dst_sreg, @riscv.Reg::Zero, rhs_sreg))
      self.push_code(@riscv.RvAsm::Addi(dst_sreg, dst_sreg, n.to_int()))
    }
    (_, Value::Constant(ConstantInt(n))) => {
      let lhs_sreg = self.query_sreg(lhs)
      self.push_code(@riscv.RvAsm::Addi(dst_sreg, lhs_sreg, -n.to_int()))
    }
    _ => {
      let lhs_sreg = self.query_sreg(lhs)
      let rhs_sreg = self.query_sreg(rhs)
      self.push_code(@riscv.RvAsm::Sub(dst_sreg, lhs_sreg, rhs_sreg))
    }
  }
  
}

fn transformSMulToRiscV(self: RiscVEmitter, inst: BinaryInst) -> Unit {
  let lhs = inst.get_lhs()
  let rhs = inst.get_rhs()
  let dst_sreg = self.query_sreg(inst.as_value())
  let lhs_sreg = self.query_sreg(lhs)
  let rhs_sreg = self.query_sreg(rhs)
  let code = @riscv.RvAsm::Mul(dst_sreg, lhs_sreg, rhs_sreg)
  self.push_comment_if_debug(inst.to_string())
  self.push_code(code)
}

fn transformSDivToRiscV(self: RiscVEmitter, inst: BinaryInst) -> Unit {
  let lhs = inst.get_lhs()
  let rhs = inst.get_rhs()
  let dst_sreg = self.query_sreg(inst.as_value())
  let lhs_sreg = self.query_sreg(lhs)
  let rhs_sreg = self.query_sreg(rhs)
  let code = @riscv.RvAsm::Mul(dst_sreg, lhs_sreg, rhs_sreg)
  self.push_comment_if_debug(inst.to_string())
  self.push_code(code)
}

fn transformSRemToRiscV(self: RiscVEmitter, inst: BinaryInst) -> Unit {
  let lhs = inst.get_lhs()
  let rhs = inst.get_rhs()
  let dst_sreg = self.query_sreg(inst.as_value())
  let lhs_sreg = self.query_sreg(lhs)
  let rhs_sreg = self.query_sreg(rhs)
  let code = @riscv.RvAsm::Rem(dst_sreg, lhs_sreg, rhs_sreg)
  self.push_comment_if_debug(inst.to_string())
  self.push_code(code)
}

fn transformFAddToRiscV(self: RiscVEmitter, inst: BinaryInst) -> Unit {
  let lhs = inst.get_lhs()
  let rhs = inst.get_rhs()
  let dst_freg = self.query_freg(inst.as_value())
  self.push_comment_if_debug(inst.to_string())
  match (lhs, rhs) {
    (Value::Constant(l), Value::Constant(r)) => {
      guard let ConstantFP(lval) = l
      guard let ConstantFP(rval) = r
      let imm = lval + rval
      let imm = imm.reinterpret_as_i64()
      self.push_code(@riscv.RvAsm::Li(@riscv.Reg::S10, imm.to_string()))
      self.push_code(@riscv.RvAsm::FmvDX(dst_freg, @riscv.Reg::S10))
    }
    (Value::Constant(ConstantFP(0.0)), _) => {
      self.push_code(@riscv.RvAsm::FmvD(dst_freg, self.query_freg(rhs)))
    }
    (_, Value::Constant(ConstantFP(0.0))) => {
      self.push_code(@riscv.RvAsm::FmvD(dst_freg, self.query_freg(lhs)))
    }
    _ => {
      let lhs_freg = self.query_freg(lhs)
      let rhs_freg = self.query_freg(rhs)
      let code = @riscv.RvAsm::FaddD(dst_freg, lhs_freg, rhs_freg)
      self.push_code(code)
    }
  }
}

fn transformFSubToRiscV(self: RiscVEmitter, inst: BinaryInst) -> Unit {
  let lhs = inst.get_lhs()
  let rhs = inst.get_rhs()
  let dst_freg = self.query_freg(inst.as_value())
  let lhs_freg = self.query_freg(lhs)
  let rhs_freg = self.query_freg(rhs)
  let code = @riscv.RvAsm::FsubD(dst_freg, lhs_freg, rhs_freg)
  self.push_comment_if_debug(inst.to_string())
  self.push_code(code)
}

fn transformFMulToRiscV(self: RiscVEmitter, inst: BinaryInst) -> Unit {
  let lhs = inst.get_lhs()
  let rhs = inst.get_rhs()
  let dst_freg = self.query_freg(inst.as_value())
  let lhs_freg = self.query_freg(lhs)
  let rhs_freg = self.query_freg(rhs)
  let code = @riscv.RvAsm::FmulD(dst_freg, lhs_freg, rhs_freg)
  self.push_comment_if_debug(inst.to_string())
  self.push_code(code)
}

fn transformFDivToRiscV(self: RiscVEmitter, inst: BinaryInst) -> Unit {
  let lhs = inst.get_lhs()
  let rhs = inst.get_rhs()
  let dst_freg = self.query_freg(inst.as_value())
  let lhs_freg = self.query_freg(lhs)
  let rhs_freg = self.query_freg(rhs)
  let code = @riscv.RvAsm::FdivD(dst_freg, lhs_freg, rhs_freg)
  self.push_comment_if_debug(inst.to_string())
  self.push_code(code)
}

// Note: Add Code
fn transformBinaryToRiscV(
  self: RiscVEmitter,
  inst:  BinaryInst,
) -> Unit {
  let val_type = inst.get_type()
  match (inst.opcode, val_type.is_fp()) {
    (BinaryOperator::Add, false) => self.transformSAddToRiscV(inst)
    (BinaryOperator::Sub, false) => self.transformSSubToRiscV(inst)
    (BinaryOperator::Mul, false) => self.transformSMulToRiscV(inst)
    (BinaryOperator::Div, false) => self.transformSDivToRiscV(inst)
    (BinaryOperator::Rem, false) => self.transformSRemToRiscV(inst)
    (BinaryOperator::Add, true) => self.transformFAddToRiscV(inst)
    (BinaryOperator::Sub, true) => self.transformFSubToRiscV(inst)
    (BinaryOperator::Mul, true) => self.transformFMulToRiscV(inst)
    (BinaryOperator::Div, true) => self.transformFDivToRiscV(inst)
    _ => {
      println("Unsupported instruction, loc: transformToRiscV")
      panic()
    }
  }
}

/// ---------------------------------------------------------
/// Transform Unary Instruction to Risc-V
/// ---------------------------------------------------------
fn transformLoadToRiscV(
  self: RiscVEmitter, inst: UnaryInst
) -> Unit {
  let dst_reg = self.query_loc(inst.as_value())
  let code = match dst_reg {
    RiscVPhysicalLocation::SReg(sreg) => {
      let ptr = inst.get_operand()
      match ptr {
        Value::Function(f) => {
          @riscv.RvAsm::La(sreg, f.name)
        }
        _ => {
          let ptr = match self.query_loc(ptr) {
            RiscVPhysicalLocation::Mem(ptr) => ptr
            RiscVPhysicalLocation::SReg(ptr) => @riscv.MemAccess::{base: ptr, offset: 0}
            RiscVPhysicalLocation::OutsideSReg(ptr, mem) => {
              self.push_code(@riscv.RvAsm::Ld(ptr, mem))
              @riscv.MemAccess::{base: ptr, offset: 0}
            }
            _ => {
              println("Unsupported instruction, loc: transformLoadToRiscV, try to load from a invalid location")
              println(inst.to_string())
              panic()
            }
          }
          @riscv.RvAsm::Ld(sreg, ptr)
        }
      }
    }
    RiscVPhysicalLocation::FReg(freg) => {
      let ptr = inst.get_operand()
      let ptr = match self.query_loc(ptr) {
        RiscVPhysicalLocation::Mem(ptr) => ptr
        RiscVPhysicalLocation::SReg(ptr) => @riscv.MemAccess::{base: ptr, offset: 0}
        RiscVPhysicalLocation::OutsideSReg(ptr, mem) => {
          self.push_code(@riscv.RvAsm::Ld(ptr, mem))
          @riscv.MemAccess::{base: ptr, offset: 0}
        }
        _ => {
          println("Unsupported instruction, loc: transformLoadToRiscV, try to load from a invalid location")
          println(inst.to_string())
          panic()
        }
      }
      @riscv.RvAsm::Fld(freg, ptr)
    }
    _ => {
      println("Unsupported instruction, loc: transformLoadToRiscV, it's not in sreg or freg")
      println(inst.to_string())
      panic()
    }
  }
  self.push_comment_if_debug(inst.to_string())
  self.push_code(code)
}

fn transformUnaryToRiscV(self: RiscVEmitter, inst: UnaryInst) -> Unit {
  match inst.opcode {
    UnaryOperator::Load => self.transformLoadToRiscV(inst)
    _ => {
      println("Unsupported instruction, loc: transformUnaryToRiscV")
      panic()
    }
  }
}

/// ---------------------------------------------------------
/// Transform Branch Instruction to Risc-V
/// ---------------------------------------------------------
fn transformFCondBrToRiscV(
  self: RiscVEmitter, inst: BranchInst
) -> Unit {
  let condition = inst.condition.unwrap()
  let if_label = inst.if_true.unwrap().get_label()
  let else_label = inst.if_false.unwrap().get_label()
  guard let Instruction(condition) = condition
  guard let Compare(cond) = condition
  let lhs = cond.get_lhs()
  let rhs = cond.get_rhs()
  let lhs_sreg = self.query_freg(lhs)
  let rhs_sreg = self.query_freg(rhs)
  self.push_comment_if_debug(inst.to_string())
  match cond.predicate {
    // Predicate::FCMP_OEQ => {
    //   self.push_code(@riscv.RvAsm::FbeqD(lhs_sreg, rhs_sreg, if_label))
    //   self.push_code(@riscv.RvAsm::J(else_label))
    // }
    // Predicate::FCMP_OLE => {
    //   self.push_comment_if_debug(condition.to_string())
    //   self.push_code(@riscv.RvAsm::FbleD(lhs_sreg, rhs_sreg, if_label))
    //   self.push_comment_if_debug(inst.to_string())
    //   self.push_code(@riscv.RvAsm::J(else_label))
    // }
    _ => {
      println("Unsupported instruction, loc: transformFCondBrToRiscV")
      panic()
    }
  }
}


fn transformSCondBrToRiscV(
  self: RiscVEmitter, inst: BranchInst
) -> Unit {
  let condition = inst.condition.unwrap()
  let if_label = inst.if_true.unwrap().get_label()
  let else_label = inst.if_false.unwrap().get_label()
  guard let Instruction(condition) = condition
  guard let Compare(cond) = condition
  let lhs = cond.get_lhs()
  let rhs = cond.get_rhs()
  let lhs_sreg = self.query_sreg(lhs)
  let rhs_sreg = self.query_sreg(rhs)
  self.push_comment_if_debug(condition.to_string())
  self.push_comment_if_debug(inst.to_string())
  match cond.predicate {
    Predicate::ICMP_EQ => {
      self.push_code(@riscv.RvAsm::Beq(lhs_sreg, rhs_sreg, if_label))
      self.push_code(@riscv.RvAsm::J(else_label))
    }
    Predicate::ICMP_NE => {
      self.push_code(@riscv.RvAsm::Bne(lhs_sreg, rhs_sreg, if_label))
      self.push_code(@riscv.RvAsm::J(else_label))
    }
    Predicate::ICMP_SLE => {
      self.push_code(@riscv.RvAsm::Ble(lhs_sreg, rhs_sreg, if_label))
      self.push_code(@riscv.RvAsm::J(else_label))
    }
    Predicate::ICMP_SLT => {
      self.push_code(@riscv.RvAsm::Blt(lhs_sreg, rhs_sreg, if_label))
      self.push_code(@riscv.RvAsm::J(else_label))
    }
    Predicate::ICMP_SGE => {
      self.push_code(@riscv.RvAsm::Bge(lhs_sreg, rhs_sreg, if_label))
      self.push_code(@riscv.RvAsm::J(else_label))
    }
    Predicate::ICMP_SGT => {
      self.push_code(@riscv.RvAsm::Bgt(lhs_sreg, rhs_sreg, if_label))
      self.push_code(@riscv.RvAsm::J(else_label))
    }
    _ => {
      println("Unsupported instruction, loc: transformSCondBrToRiscV")
      panic()
    }
  }
}

fn transformCondBrToRiscV(
  self: RiscVEmitter, inst: BranchInst
) -> Unit {
  match inst.condition.unwrap().get_type().is_fp() {
    true => transformFCondBrToRiscV(self, inst)
    false => transformSCondBrToRiscV(self, inst)
  }
}

fn transformBrToRiscV(
  self: RiscVEmitter, inst: BranchInst
) -> Unit {
  let if_label = inst.if_true.unwrap().get_label()
  self.push_comment_if_debug(inst.to_string())
  self.push_code(@riscv.RvAsm::J(if_label))
}


fn transformBranchToRiscV(
  self: RiscVEmitter, inst: BranchInst
) -> Unit {
  match inst.condition {
    Some(_) => transformCondBrToRiscV(self, inst)
    None => transformBrToRiscV(self, inst)
  }
}

/// ------------------------------------------------------
/// Transform Call Instruction to Risc-V
/// ------------------------------------------------------
fn transformCallToRiscV(
  self: RiscVEmitter, inst: CallInst
) -> Unit {
  let sarg_regs = [
    @riscv.Reg::A0,
    @riscv.Reg::A1,
    @riscv.Reg::A2,
    @riscv.Reg::A3,
    @riscv.Reg::A4,
    @riscv.Reg::A5,
    @riscv.Reg::A6,
    @riscv.Reg::A7,
  ]
  let farg_regs = [
    @riscv.FReg::Fa0,
    @riscv.FReg::Fa1,
    @riscv.FReg::Fa2,
    @riscv.FReg::Fa3,
    @riscv.FReg::Fa4,
    @riscv.FReg::Fa5,
    @riscv.FReg::Fa6,
    @riscv.FReg::Fa7,
  ]

  let mut sarg_cnt = 0
  let mut farg_cnt = 0
  let execeed_val : Array[Value] = []
  for arg in inst.arg_list {
    // if arg cnt is more than 8, store them in stack, and no need use indirect reg
    if sarg_cnt >= 8 || farg_cnt >= 8 {
      execeed_val.push(arg)
      continue
    }
    match self.query_loc(arg) {
      RiscVPhysicalLocation::SReg(sreg) => {
        if sreg != sarg_regs[sarg_cnt] {
          self.push_code(@riscv.RvAsm::Mv(sarg_regs[sarg_cnt], sreg))
        }
        sarg_cnt += 1
      }
      RiscVPhysicalLocation::FReg(freg) => {
        if freg != farg_regs[farg_cnt] {
          self.push_code(@riscv.RvAsm::FmvD(farg_regs[farg_cnt], freg))
        }
        farg_cnt += 1
      }
      RiscVPhysicalLocation::Mem(mem) => {
        let arg_type = arg.get_type()
        match arg_type.is_fp() {
          false => self.push_code(@riscv.RvAsm::Ld(sarg_regs[sarg_cnt], mem))
          true  => self.push_code(@riscv.RvAsm::Fld(farg_regs[farg_cnt], mem))
        }
      }
      _ => {
        println("arg is \{arg.trivial_stringify()}")
        println("Unsupported instruction, loc: transformCallToRiscV")
        panic()
      }
    }
  }
  let mut offset = 0
  for exc in execeed_val {
    match exc.get_type().is_fp() {
      false => {
        let sreg = self.query_sreg(exc)
        self.push_code(@riscv.RvAsm::Sd(sreg, @riscv.MemAccess::{base: @riscv.Reg::Sp, offset}))
        offset += 8
      }
      true => {
        let freg = self.query_freg(exc)
        self.push_code(@riscv.RvAsm::Fsd(freg, @riscv.MemAccess::{base: @riscv.Reg::Sp, offset}))
      }
    }
    offset += 8
  }

  self.push_comment_if_debug(inst.to_string())
  match inst.callee {
    Value::Function(func) => {
      self.push_code(@riscv.RvAsm::Call(func.name))
    }
    _ => {
      // let reg = self.query_sreg(inst.callee)
      // self.push_code(@riscv.RvAsm::Jalr(reg))
      match self.query_loc(inst.callee) {
        RiscVPhysicalLocation::SReg(sreg) => {
          self.push_code(@riscv.RvAsm::Jalr(sreg))
        }
        _ => {
          println("Unsupported instruction, loc: transformCallToRiscV")
          panic()
        }
      }
    }
  }
  // if call inst return something, move the return value to the dst register
  match inst.return_void() {
    true => ()
    false => {
      let dst_reg = self.query_loc(inst.as_value())
      match dst_reg {
        RiscVPhysicalLocation::SReg(sreg) => {
          if sreg != @riscv.Reg::A0 {
            self.push_code(@riscv.RvAsm::Mv(sreg, @riscv.Reg::A0))
          }
        }
        RiscVPhysicalLocation::FReg(freg) => {
          if freg != @riscv.FReg::Fa0 {
            self.push_code(@riscv.RvAsm::FmvD(freg, @riscv.FReg::Fa0))
          }
        }
        RiscVPhysicalLocation::Mem(mem) => {
          let val_type = inst.as_value().get_type()
          match val_type.is_fp() {
            false => self.push_code(@riscv.RvAsm::Sd(@riscv.Reg::A0, mem))
            true  => self.push_code(@riscv.RvAsm::Fsd(@riscv.FReg::Fa0, mem))
          }
        }
        _ => {
          println("Unsupported instruction, loc: transformCallToRiscV")
          panic()
        }
      }
    }
  }
}

/// ---------------------------------------------------------
/// Transform Return Instruction to Risc-V
///
/// Note:
///
/// Do not generate `ret`, if return something, move it to A0
/// if return void, do nothing
/// ---------------------------------------------------------
fn transformReturnToRiscV(
  self: RiscVEmitter, inst: ReturnInst
) -> Unit {
  self.push_comment_if_debug(inst.to_string())
  match inst.get_return_value() {
    Some(ret) => {
      let ret_reg = self.query_loc(ret)
      match ret_reg {
        RiscVPhysicalLocation::SReg(sreg) => {
          if sreg != @riscv.Reg::A0 {
            self.push_code(@riscv.RvAsm::Mv(@riscv.Reg::A0, sreg))
          }
        }
        RiscVPhysicalLocation::FReg(freg) => {
          if freg != @riscv.FReg::Fa0 {
            self.push_code(@riscv.RvAsm::FmvD(@riscv.FReg::Fa0, freg))
          }
        }
        RiscVPhysicalLocation::Mem(mem) => {
          let val_type = ret.get_type()
          match val_type.is_fp() {
            false => self.push_code(@riscv.RvAsm::Ld(@riscv.Reg::A0, mem))
            true  => self.push_code(@riscv.RvAsm::Fld(@riscv.FReg::Fa0, mem))
          }
        }
        _ => {
          println("Unsupported instruction, loc: transformReturnToRiscV")
          panic()
        }
      }
    }
    None => ()
  }
}

/// ---------------------------------------------------------
/// Transform Store Instruction to Risc-V
/// ---------------------------------------------------------
fn transformStoreToRiscV(
  self: RiscVEmitter, inst: StoreInst
) -> Unit {
  let val_type = inst.value.get_type()
  // if it is in memeory, ret memaccess
  // if it is sreg, ret MemAccess {offset: 0, base: sreg}
  let ptr = match self.query_loc(inst.pointer) {
    RiscVPhysicalLocation::Mem(mem) => mem
    RiscVPhysicalLocation::SReg(sreg) => @riscv.MemAccess::{base: sreg, offset: 0}
    _ => {
      println("Unsupported instruction, loc: transformStoreToRiscV")
      panic()
    }
  }
  match val_type.is_fp() {
    true => {
      let freg = self.query_freg(inst.value)
      self.push_code(@riscv.RvAsm::Fsd(freg, ptr))
    }
    false => {
      let sreg = self.query_sreg(inst.value)
      match sizeof(val_type) {
        1 => self.push_code(@riscv.RvAsm::Sb(sreg, ptr))
        2 => self.push_code(@riscv.RvAsm::Sh(sreg, ptr))
        4 => self.push_code(@riscv.RvAsm::Sw(sreg, ptr))
        8 => self.push_code(@riscv.RvAsm::Sd(sreg, ptr))
        _ => {
          println("Unsupported instruction, loc: transformStoreToRiscV")
          panic()
        }
      }
    }
  }
}

/// ---------------------------------------------------------
/// Transform GetElementPtr Instruction to Risc-V
/// ---------------------------------------------------------
// 1. first check the index_list is all_const, if so, skip,
// do not generate asm code.
// 2. if idx is not const, get the first element of index_list
// generate add target, ptr, idx * sizeof(element_type)
// 3. remember load inst
fn transformGetElementPtrToRiscV(
  self: RiscVEmitter, inst: GetElementPtrInst
) -> Unit {
  let idxList = inst.index_list
  let mut all_const = true
  idxList.each(fn (idx) { if not(Isa::constant(idx)) { all_const = false } })
  if all_const { return }
  let idx = idxList[0]
  let ptr = inst.pointer
  let pointedTy = inst.pointedTy
  let ptr_reg = self.query_sreg(ptr)
  let idx_reg = self.query_sreg(idx)
  let valSize = sizeof(pointedTy)
  let sreg = self.query_sreg(inst.as_value())
  self.push_comment_if_debug(inst.to_string())
  if sreg != idx_reg {
    self.push_code(@riscv.RvAsm::Li(sreg, valSize.to_string()))
    self.push_code(@riscv.RvAsm::Mul(sreg, idx_reg, sreg))
    self.push_code(@riscv.RvAsm::Add(sreg, ptr_reg, sreg))
  } else {
    // Note:: if sreg == idx_reg, we need to use a temp reg to store the valSize
    // no need to consider ptr_reg == sreg, becuase in this case, ptr_reg must be not sreg
    self.push_code(@riscv.RvAsm::Li(@riscv.Reg::S10, valSize.to_string()))
    self.push_code(@riscv.RvAsm::Mul(sreg, idx_reg,@riscv.Reg::S10))
    self.push_code(@riscv.RvAsm::Add(sreg, ptr_reg, sreg))
  }
}
