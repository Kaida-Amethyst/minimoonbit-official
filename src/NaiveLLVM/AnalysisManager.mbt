/// --------------------------------------------------
/// Analysis Manager
/// --------------------------------------------------
pub struct AnalysisManager {
  mut live: LiveAnalysis?
  mut virtualReg: VirtualReg?
  mut riscvPhyLoc: RiscVPhysicalLocation?
}

pub fn AnalysisManager::default() -> AnalysisManager {
  AnalysisManager::{
    live: None,
    virtualReg: None,
    riscvPhyLoc: None
  }
}

pub fn set_live_analysis(
  self: AnalysisManager, live: LiveAnalysis
) -> Unit {
  self.live = Some(live)
}

pub fn get_live_analysis(self: AnalysisManager) -> LiveAnalysis? {
  self.live
}

pub fn clear_live_analysis(self: AnalysisManager) -> Unit {
  self.live = None
}

pub fn setPhysicalSReg(
  self: AnalysisManager, sreg: @riscv.Reg
) -> Unit {
  let phyloc = RiscVPhysicalLocation::SReg(sreg)
  self.riscvPhyLoc = Some(phyloc)
}

pub fn setPhysicalFReg(
  self: AnalysisManager, freg: @riscv.FReg
) -> Unit {
  let phyloc = RiscVPhysicalLocation::FReg(freg)
  self.riscvPhyLoc = Some(phyloc)
}

pub fn setPhysicalMem(
  self: AnalysisManager, mem: @riscv.MemAccess[@riscv.Reg, Int]
) -> Unit {
  let phyloc = RiscVPhysicalLocation::Mem(mem)
  self.riscvPhyLoc = Some(phyloc)
}

pub fn setPhysicalOutsideSReg(
  self: AnalysisManager, sreg: @riscv.Reg, offset: Int
) -> Unit {
  let outsideMem = @riscv.MemAccess::{base: @riscv.Reg::Fp, offset: offset}
  let phyloc = RiscVPhysicalLocation::OutsideSReg(sreg, outsideMem)
  self.riscvPhyLoc = Some(phyloc)
}

pub fn setPhysicalOutsideFReg(
  self: AnalysisManager, freg: @riscv.FReg, offset: Int
) -> Unit {
  let outsideMem = @riscv.MemAccess::{base: @riscv.Reg::Fp, offset: offset}
  let phyloc = RiscVPhysicalLocation::OutsideFReg(freg, outsideMem)
  self.riscvPhyLoc = Some(phyloc)
}
/// --------------------------------------------------
/// Live Analysis
/// --------------------------------------------------
pub struct LiveAnalysis {
  live_in: @hashset.T[Value]
  live_out:@hashset.T[Value]
}

pub fn LiveAnalysis::default() -> LiveAnalysis {
  LiveAnalysis::{
    live_in: @hashset.T::new(),
    live_out: @hashset.T::new()
  }
}

fn hashset_to_array[T](set: @hashset.T[T]) -> Array[T] {
  let array = Array::new()
  set.each(fn (e) { array.push(e) })
  array
}

pub fn debug_live_analysis(func: Function) -> Unit {
  let entry = match func.entry_block {
    Some(b) => b
    None => return
  }
  println("\n====================debug live analysis==================\n")

  func.body.sort()
  for bb in func.body {
    let analysis = match bb.get_live_analysis() {
      Some(a) => a
      None => break
    }
    println("Block: " + bb.number.unwrap().to_string())
    println("Live in: " + hashset_to_array(analysis.live_in).to_string())
    println("Live out: " +  hashset_to_array(analysis.live_out).to_string())
    println("")
    for inst in bb.inst_list {
      let value_attr = inst.get_attribute()
      let analysis = match value_attr.analysis.get_live_analysis() {
        Some(a) => a
        None => {
          println("No live analysis for instruction!")
          panic()
        }
      }
      println("Live in: " + hashset_to_array(analysis.live_in).to_string())
      println("Inst: " + inst.to_string())
      println("Live out: " +  hashset_to_array(analysis.live_out).to_string())
      println("")
    }
    println("=========================================================")
  }
}

pub fn analyze_live(module: Module) -> Unit {
  for func in module.functions {
    analyze_live_for_function(func);
    // debug_live_analysis(func);
  }
  println("live analysis done!")
}

pub fn find_last_block(bb: BasicBlock) -> BasicBlock {
  let visited = @hashset.T::new()
  let queue : Deque[BasicBlock] = Deque::new()
  let leaf = Array::new()
  queue.push_back(bb)
  let mut count = 0
  while not(queue.is_empty()) {
    let current = queue.pop_front().unwrap()
    let current_id = current.number.unwrap()
    if visited.contains(current_id) {
      count += 1
      continue
    }
    visited.insert(current_id)
    if current.successors.length() == 0 {
      leaf.push(current)
    } else {
      for succ in current.successors {
        queue.push_back(succ)
      }
    }
    count += 1
  }
  // leaf[0]
  match leaf.length() {
    1 => leaf[0]
    0 => {
      println("Not Found final block!")
      panic()
    }
    _ => {
      println("Found multiple final blocks!")
      panic()
    }
  }
}

fn analyze_live_for_basic_block(bb: BasicBlock) -> Unit {
  // if basic Block has been analyzed, return
  let live_analysis = match bb.get_live_analysis() {
    Some(a) => a
    None => return
  }

  let live_out = live_analysis.live_out
  // union all succeeors' live_in to live_in
  for succ in bb.successors {
    let succ_live_in = match succ.get_live_in() {
      Some(l) => l
      None => @hashset.T::new()
    }
    succ_live_in.each(fn (e) { live_out.insert(e) })
  }
  //  currently, what live_out stored is bb's live_in
  live_out.each(fn (e) { live_analysis.live_in.insert(e) })
  let live_in = live_analysis.live_in

  for inst in bb.inst_list.rev() {
    let analysis = LiveAnalysis::default()
    // copy last live in to current live in and live out
    live_in.each(fn (e) { analysis.live_out.insert(e) })

    // remove dst from live_in
    let value_attr = inst.get_attribute()
    live_in.remove(inst.as_value())

    // Note: for phi node, not uses to live_in
    match inst {
      PHI(phi) => {
        for incoming in phi.incoming {
          let (val, bb) = incoming
          let bb_live_out = bb.get_live_out().unwrap()
          if not(Isa::constant(val)) {
            bb_live_out.insert(val)
          }
        }
      }
      // For Store Instruction, the address if it is a gep,
      // the gep's ptr is also added into live_in
      Store(sinst) => {
        let pointer = sinst.pointer
        if Isa::getElementPtrInst(pointer) {
          let gep : GetElementPtrInst = dyn::cast(pointer)
          live_in.insert(gep.pointer)
        }
        sinst.value_attr.uses.each(fn (u) {
          match u.get_id() {  // make sure only reg can be added into live_in
            Some(_) => live_in.insert(u)
            _ => ()
          }
        })
      }
      // For load instruction, the address if it is a gep,
      // the gep's ptr is also added into live_in
      Unary(un) => {
        if un.opcode == UnaryOperator::Load {
          let pointer = un.get_operand()
          if Isa::getElementPtrInst(pointer) {
            let gep : GetElementPtrInst = dyn::cast(pointer)
            live_in.insert(gep.pointer)
          }
          match pointer.get_id() {  // make sure only reg can be added into live_in
            Some(_) => live_in.insert(pointer)
            _ => ()
          }
        }
      }
      // check callee if it is a non-function value
      Call(call) => {
        match call.callee {
          Value::Function(_) => {
            call.arg_list.each(fn (u) {
              match u.get_id() {  // make sure only reg can be added into live_in
                Some(_) => live_in.insert(u)
                _ => ()
              }
            })
          }
          callee => {
            if callee.hasAttribute() {
              match callee.get_id() {
                Some(_) => live_in.insert(callee)
                None => ()
              }
            }
            call.arg_list.each(fn (u) {
              match u.get_id() {  // make sure only reg can be added into live_in
                Some(_) => live_in.insert(u)
                _ => ()
              }
            })
          }
        }
      }
      _ => {
        value_attr.uses.each(fn (u) {
          match u.get_id() {  // make sure only reg can be added into live_in
            Some(_) => live_in.insert(u)
            _ => ()
          }
        })
      }
    }
    // add each element in live_in to analysis.live_in
    live_in.each(fn (e) { analysis.live_in.insert(e) })
    value_attr.analysis.set_live_analysis(analysis)
  }

  // now live in is bb's live_in
  live_in.each(fn (e) { live_analysis.live_in.insert(e) })
  bb.analysis.set_live_analysis(live_analysis)
}

fn analyze_live_for_function(func: Function) -> Unit {
  match func.body.length() {
    0 => return
    _ => ()
  }
  match func.value_attr.analysis.get_live_analysis() {
    Some(_) => return
    None => ()
  }
  let entry = match func.entry_block {
    Some(b) => b
    None => return
  }
  // add live analysis for all blocks
  for bb in func.body {
    bb.analysis.set_live_analysis(LiveAnalysis::default())
  }

  let final = find_last_block(entry)

  let queue : Deque[BasicBlock] = Deque::new()
  queue.push_back(final)
  while not(queue.is_empty()) {
    let bb = queue.pop_front().unwrap()
    analyze_live_for_basic_block(bb)
    // just push, if already analyzed, it will be ignored
    bb.predecessors.each(fn (p) {queue.push_back(p)})
  }
  func.value_attr.analysis.set_live_analysis(LiveAnalysis::default()) // to avoid redundant analysis
}

/// --------------------------------------------------
/// Virtual Register
/// --------------------------------------------------
pub fn setVirtualReg(
  self: AnalysisManager, virtualReg: VirtualReg
) -> Unit {
  self.virtualReg = Some(virtualReg)
}

pub fn get_virtual_reg(self: AnalysisManager) -> VirtualReg? {
  self.virtualReg
}

pub fn hasVirtualReg(self: AnalysisManager) -> Bool {
  match self.virtualReg {
    Some(_) => true
    None => false
  }
}

pub fn isVirtualReg(
  self: AnalysisManager,
  vreg: VirtualReg
) -> Bool {
  match self.virtualReg {
    Some(v) => v == vreg
    None => false
  }
}

pub fn clear_virtual_reg(self: AnalysisManager) -> Unit {
  self.virtualReg = None
}
