struct BasicBlock {
  name : String?
  mut number: Int?
  inst_list : Array[Instruction]
  parent : Function
  predecessors: Array[BasicBlock]
  successors: Array[BasicBlock]
  analysis: AnalysisManager
}

fn as_value(self : BasicBlock) -> Value {
  Value::BasicBlock(self)
}

fn trivial_stringify(self: BasicBlock) -> String {
  match (self.name, self.number) {
    (Some(name), _) => name
    (None, Some(number)) => "\{self.parent.name}_L\{number}"
    _ => ""
  }
}

fn op_equal(self: BasicBlock, other: BasicBlock) -> Bool {
  match (self.number, other.number) {
    (None, None) => true
    (Some(i), Some(j)) => i == j
    _ => false
  }
}

fn compare(self: BasicBlock, other: BasicBlock) -> Int {
  match (self.number, other.number) {
    (None, None) => 0
    (Some(i), Some(j)) => i - j
    (Some(i), None) => i
    (None, Some(j)) => -j
  }
}

pub fn BasicBlock::create(~name : String = "", ~parent : Function) -> BasicBlock {
  let name = str_to_option(name)
  let number = None
  let inst_list = []
  let predecessors = []
  let successors = []
  let analysis = AnalysisManager::default()
  let bb = BasicBlock::{ name, number, inst_list, parent, predecessors, successors, analysis }
  parent.body.push(bb)
  bb
}

pub fn push_instruction(self : BasicBlock, inst : Instruction) -> Unit {
  match self.number {
    None => {
      self.number = Some(self.parent.label_number)
      self.parent.label_number += 1
    }
    Some(_) => ()
  }
  self.inst_list.push(inst)
  match inst {
    Branch(inst) => {
      match inst.if_true {
        Some(bb) => { bb.predecessors.push(self); self.successors.push(bb) }
        None => ()
      }
      match inst.if_false {
        Some(bb) => { bb.predecessors.push(self); self.successors.push(bb) }
        None => ()
      }
    }
    _ => ()
  }
}

pub fn remove_instruction(self : BasicBlock, inst : Instruction) -> Unit {
  for i, candi in self.inst_list {
    if candi == inst {
      let _ = self.inst_list.remove(i)
      break
    }
  }
}

// not just push inst, insert inst before the last branch inst
pub fn insertToLast_exceptBr(self : BasicBlock, inst : Instruction) -> Unit {
  let mut idx = 0
  for i, inst in self.inst_list {
    if Isa::branchInst(inst.as_value()) {
      idx = i
      break
    }
  }
  self.inst_list.insert(idx, inst)
}

pub fn output(self : BasicBlock, logger : Logger) -> Unit {
  logger.write_string(self.to_string())
}

pub fn to_string(self : BasicBlock) -> String {
  let prev_str = self.predecessors.map(fn (bb) { "%" + bb.trivial_stringify()}).join(", ")
  let prev_str = match prev_str {
    "" => ""
    _ => "                                                     ; preds = " + prev_str
  }
  let mut r = self.trivial_stringify() + ":" + prev_str + "\n"
  for inst in self.inst_list {
    r += "  " + inst.to_string() + "\n"
  }
  r
}

pub fn get_live_in(self : BasicBlock) -> @hashset.T[Value]? {
  match self.analysis.get_live_analysis() {
    Some(l) => Some(l.live_in)
    None => None
  }
}

pub fn get_live_out(self : BasicBlock) -> @hashset.T[Value]? {
  match self.analysis.get_live_analysis() {
    Some(l) => Some(l.live_out)
    None => None
  }
}

pub fn get_live_analysis(self : BasicBlock) -> LiveAnalysis? {
  self.analysis.get_live_analysis()
}

pub fn get_label(self : BasicBlock) -> String {
  match self.number {
    Some(i) => "\{self.parent.name}_L\{i}"
    None => ""
  }
}

pub fn clear_live_analysis(self : BasicBlock) -> Unit {
  for inst in self.inst_list {
    inst.clear_live_analysis()
  }
  self.analysis.clear_live_analysis()
}

pub fn clear_virtual_reg(self : BasicBlock) -> Unit {
  for inst in self.inst_list {
    inst.clear_virtual_reg()
  }
  self.analysis.clear_virtual_reg()
}
