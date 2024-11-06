/// ---------------------------------------------------
/// PassManager
/// ---------------------------------------------------
pub struct PassManager {
  passes: Array[Pass]
}

pub fn PassManager::new() -> PassManager {
  PassManager::{
    passes: Array::new(),
  }
}

pub fn add_pass[T: IsPass](self: PassManager, pass: T) -> Unit {
  self.passes.push(pass.to_pass())
}

pub fn iter(self: PassManager) -> Iter[Pass] {
  self.passes.iter()
}

pub fn run_on_func(self: PassManager, func: Function) -> Unit {
  self.passes.each(fn (p) {p.run(func)})
}

/// ---------------------------------------------------
/// Pass
/// ---------------------------------------------------
pub enum Pass {
  // EarlyCSE(EarlyCSEPass)
  PhiElimination(PhiEliminationPass)
  Peephole(PeepholePass)
  PromoteImm(PromoteImmPass)
  PromoteReg2Mem(PromoteReg2MemPass)
  VirtualRegisterAlloc(VirtualRegisterAllocPass)
  RISCVRegisterAlloc(RiscVRegisterAllocPass)
  RISCVMemoryAlloc(RiscVMemoryAllocPass)
}

pub trait IsPass {
  to_pass(Self) -> Pass
}

pub fn run(self: Pass, func: Function) -> Unit {
  match self {
    Pass::Peephole(pass) => pass.run(func)
    Pass::PromoteImm(pass) => pass.run(func)
    Pass::PromoteReg2Mem(pass) => pass.run(func)
    Pass::VirtualRegisterAlloc(pass) => { pass.run(func) }
    Pass::PhiElimination(pass) => pass.run(func)
    Pass::RISCVRegisterAlloc(pass) => { pass.run(func) }
    Pass::RISCVMemoryAlloc(pass) => { pass.run(func) }
  }
}

/// ---------------------------------------------------
/// Peephole Pass
///
/// This pass conatins many simple passes, if matched
/// some pattern, use it
/// ---------------------------------------------------
pub struct PeepholePass {}

pub fn PeepholePass::new() -> PeepholePass {
  PeepholePass::{}
}

pub fn to_pass(self: PeepholePass) -> Pass {
  Pass::Peephole(self)
}

// ## Function Pointer Store
// if we found `store @{func}, ...` insert this store instruction
// a `%{n} = load @{func}` before it
fn functionPointerStore(func: Function) -> Unit {
  for bb in func.body {
    for inst in bb.inst_list {
      match inst {
        Store(s) => {
          match Isa::function(s.value) {
            true => {
              let load_inst = UnaryInst::create(
                UnaryOperator::Load, s.value, s.value.get_type(),
                func.value_number
              )
              inst.as_value().insert_before(load_inst)
              s.as_value().replace_use(s.value, load_inst.as_value())
              s.value = load_inst.as_value()
              func.value_number += 1
            }
            false => ()
          }
        }
        _ => ()
      }
    }
  }
}

// some variable must be in register, but it may be in memory
// for these variables, add load inst before their users
// 1. case 1: call {func_pointer} ..., and func_pointer is in memory
fn memload2reg(self: PeepholePass, func: Function) -> Unit {
  for bb in func.body {
    for inst in bb.inst_list {
      match inst {
        Call(c) => {
          match c.callee {
            Value::Instruction(GetElementPtr(_)) => {
              let load_inst = UnaryInst::create(
                UnaryOperator::Load, c.callee, c.callee.get_type(),
                func.value_number
              )
              func.value_number += 1
              c.as_value().insert_before(load_inst)
              c.callee = load_inst.as_value()
            }
            _ => ()
          }
        }
        _ => ()
      }
    }
  }
}

// sometimes, we may meet one special case:
// fn foo(%0, %1) :
//   ...
//   call bar(%1, %0)
//
// In Such case, if we do %1 <- %0, then %0 <- %1, it is
// problematic, because %1 is used before it is assigned
// hence, we need to adjust the argument passing order
// %t0 <- %1, %t1 <- %0, then call bar(%t0, %t1)
// what we gonna do is to recognize all misplaced arg
// then insert a new inst before call, to adjust the order
// for call bar (%1, %0), we choose to insert to new addi inst:
// %t0 = add i32/f32 %1, 0;
// %t1 = add i32/f32 %0, 0;
fn adjustCallInstArgPassingOrder(self: PeepholePass, func: Function) -> Unit {
  let params = func.args.map(fn (arg) {arg.as_value()})
  let call_insts :Array[CallInst] = Array::new() 
  for bb in func.body {
    for inst in bb.inst_list {
      match inst {
        Call(c) => call_insts.push(c)
        _ => ()
      }
    }
  }

  for call in call_insts {
    let paramUsed : Array[Value] = Array::new()
    for arg in call.arg_list {
      if params.contains(arg) {
        paramUsed.push(arg)
      }
    }
    if paramUsed.length() <= 1 { continue }
    for arg in paramUsed {
      let bin = match arg.get_type().is_fp() {
        true => BinaryInst::create(
          BinaryOperator::Add, arg, Value::Constant(ConstantFP(0.0)),
          func.value_number, allow_ptr_arith = true
        )
        false => BinaryInst::create(
          BinaryOperator::Add, arg, Value::Constant(ConstantInt(0)),
          func.value_number, allow_ptr_arith = true
        )
      }
      func.value_number += 1
      bin.value_attr.parent = call.value_attr.parent
      call.replace_arg(arg, bin.as_value())
      call.insert_before(bin)
    }
  }

  // how we decide which arg we adjust?
  // suppose we have param uses : {1, 2, 4, 5, 3, 3}
  // now we sort it to 1, 2, 3, 3, 4, 5
  // compare them one by one, 1 == 1, ok, 2 = 2, ok
  // 4 != 3, promote 4, 5 ! = 3, promote 5, 3 == 3, ok, 3 == 3, ok
  // done
  // fn sort_argument(a1: Value, a2: Value) -> Int {
  //   a1.get_id().unwrap() - a2.get_id().unwrap()
  // }
  // for call in call_insts {
  //   // 1. collect all arg which is function params
  //   let paramUsed : Array[Value] = Array::new()
  //   for arg in call.arg_list {
  //     if params.contains(arg) {
  //       paramUsed.push(arg)
  //     }
  //   }
  //   if paramUsed.length() <= 1 { continue }
  //   let sortedParamUsed = paramUsed.iter().to_array()
  //   sortedParamUsed.sort_by(sort_argument)
  //   loop paramUsed[0:], sortedParamUsed[0:] {
  //     [], [] => break
  //     [], _ => break
  //     paramUsed, sortedParamUsed => {
  //       let this_param = paramUsed[0]
  //       let this_sorted_param = sortedParamUsed[0]
  //       if this_param == this_sorted_param {
  //         continue paramUsed[1:], sortedParamUsed[1:]
  //       } else {
  //         let bin = match this_param.get_type().is_fp() {
  //           false => BinaryInst::create(
  //             BinaryOperator::Add, this_param, Value::Constant(ConstantInt(0)),
  //             func.value_number, allow_ptr_arith = true
  //           )
  //           true => BinaryInst::create(
  //             BinaryOperator::Add, this_param, Value::Constant(ConstantFP(0.0)),
  //             func.value_number, allow_ptr_arith = true
  //           )
  //         }
  //         func.value_number += 1
  //         bin.value_attr.parent = call.value_attr.parent
  //         call.replace_arg(this_param, bin.as_value())
  //         call.insert_before(bin)
  //         continue paramUsed[1:], sortedParamUsed[0:]
  //       }
  //     }
  //   }
}

// if func pointer is in `getElementPtr`, insert load inst before call
pub fn run(self: PeepholePass, func: Function) -> Unit {

  functionPointerStore(func)

  memload2reg(self, func)

  adjustCallInstArgPassingOrder(self, func)
}

/// --------------------------------------------------
/// Promote Imm
///
/// Promote immediate from instruction to other instruction
///
///
/// In riscv, we only have addi, subi, ...
/// but if we want to do `%mul = mul nsw %1, 2`, it cannot
/// be transformed to riscv directly.
///
/// hence we need to promote the immediate to other instruction
///
/// Methods:
///
/// for all instrutions which has immediate, promote it to
/// instruction: `%{something} = add 0, %{immediate}`
///
/// In the later risvc code gen phrase, `add 0, %{immediate}`
/// will be transformed to `li {%reg} %{immediate}`
///
/// However, if the imm is 0, we do not need to promote it
/// because in riscv, we have zero register
///
/// for floating instruction, if operand conatins non-zero constant
/// promote its IEEE754 constant to a new constant
/// using `reinterpret_as_int64`
/// --------------------------------------------------
pub struct PromoteImmPass {}

pub fn PromoteImmPass::new() -> PromoteImmPass {
  PromoteImmPass::{}
}

pub fn to_pass(self: PromoteImmPass) -> Pass {
  Pass::PromoteImm(self)
}

pub fn run(self: PromoteImmPass, func: Function) -> Unit {
  match func.body.length() {
    0 => return
    _ => ()
  }

  func.body.sort()

  let zeroi32 = Value::Constant(ConstantInt(0))
  let zerof32 = Value::Constant(ConstantFP(0.0))
  let mut promote_cnt = 0

  for bb in func.body {
    // first is the inst which is already in BasicBlock,
    // second is the inst that going to insert before the inst
    // cannot insert inst directly in loop
    let insertBeforeRecord : Array[(Value, Value)] = Array::new()
    for i, inst in bb.inst_list {
      let dstValType = inst.get_type()
      let uses = inst.as_value().get_attribute().uses
      // exclude one case that bin 0, t or bin t, 0
      let mut exclude = false
      match inst {
        // do not promote getelementptr
        GetElementPtr(_) => { exclude = true }
        // for phi node, if one of incoming is constant(any constant)
        // generate a new Imm inst after its' incoming block,
        // and replace the constant with the new Imm inst
        PHI(phi) => {
          let new_incoming : Array[(Value, BasicBlock)] = Array::new()
          for inc in phi.incoming {
            let (val, bb) = inc
            if Isa::constant(val) {
              let bininst = match val {
                Value::Constant(ConstantInt(_)) => BinaryInst::create(
                  BinaryOperator::Add, zeroi32, val, func.value_number,
                  name = Some("Imm" + promote_cnt.to_string()))
                Value::Constant(ConstantFP(_)) => BinaryInst::create(
                  BinaryOperator::Add, zerof32, val, func.value_number,
                  name = Some("Imm" + promote_cnt.to_string()))
                _ => {
                  println("PHI Node promote imm error, not constant")
                  panic()
                }
              }
              bb.insertToLast_exceptBr(bininst.as_inst())
              new_incoming.push((bininst.as_value(), bb))
              promote_cnt += 1
              func.value_number += 1
            } else {
              new_incoming.push(inc)
            }
          }
          phi.incoming.clear()
          new_incoming.each(fn (inc) { phi.incoming.push(inc) })
          continue
        }
        Binary(b) => {
          for use in uses {
            match use {
              Value::Constant(ConstantInt(0)) => exclude = true
              Value::Constant(ConstantFP(0.0)) => exclude = true
              _ => ()
            }
          }
          if (b.opcode == BinaryOperator::Add || b.opcode == BinaryOperator::Sub) &&
            dstValType.is_integer() {
            for use in uses {
              match use {
                // in riscv, addi x, imm, imm can be 12 bits
                Value::Constant(ConstantInt(n)) => {
                  if n > -1024 && n < 1024 {
                    exclude = true
                  }
                }
                _ => ()
              }
            }
          }
        }
        _ => ()
      }
      if exclude { continue }

      for j, use in uses {
        match use {
          Value::Constant(c) => match c {
            ConstantInt(imm) => {
              if imm != 0 {
                let bininst = BinaryInst::create(
                  BinaryOperator::Add,
                  zeroi32, use, func.value_number,
                  name = Some("Imm" + promote_cnt.to_string())
                )
                insertBeforeRecord.push((inst.as_value(), bininst.as_value()))
                uses[j] = bininst.as_value()
                match inst {
                  Store(sinst) => {
                    if use == sinst.value {
                      sinst.value = bininst.as_value()
                    }
                  }
                  _ => ()
                }
                promote_cnt += 1
                func.value_number += 1
              }
            }
            ConstantFP(_) => {
              let bininst = BinaryInst::create(
                BinaryOperator::Add, zerof32, use, func.value_number,
                name = Some("Imm" + promote_cnt.to_string())
              )
              insertBeforeRecord.push((inst.as_value(), bininst.as_value()))
              uses[j] = bininst.as_value()
              match inst {
                Store(sinst) => {
                  if use == sinst.value {
                    sinst.value = bininst.as_value()
                  }
                }
                _ => ()
              }
              promote_cnt += 1
              func.value_number += 1
            }
          }
          _ => ()
        }
      }
    }
    for pair in insertBeforeRecord {
      let (ori_inst, insert_inst) = pair
      let insert_inst : Instruction = dyn::cast(insert_inst)
      ori_inst.insert_before(insert_inst)
    }
  }
}

/// ---------------------------------------------------
///
/// ---------------------------------------------------

/// ---------------------------------------------------
/// PromoteReg2Mem Pass
///
/// Some reg must be in memeory rather than register
///
/// for example: for the following code
///
/// %c = ...
/// call foo(...)
/// ... = %c ..
///
/// Here, %c is live after call, so the %c is better to
/// stored in memory rather than register
///
/// Method:
///
/// 1. create alloca inst in the entry block
/// 2. insert store inst after inst that create val
/// 3. for all users, insert load inst before it, and replace use
/// ---------------------------------------------------
pub struct PromoteReg2MemPass {
  // a map store the promoted value, to avoid duplicate promotion
  promoted: Map[String, @hashset.T[Int]]  // function -> { id }
}

pub fn PromoteReg2MemPass::new() -> PromoteReg2MemPass {
  PromoteReg2MemPass::{
    promoted: Map::new()
  }
}

pub fn to_pass(self: PromoteReg2MemPass) -> Pass {
  Pass::PromoteReg2Mem(self)
}

pub fn promote(self: PromoteReg2MemPass, val: Value, func: Function) -> Unit {
  // first, check if the value has been promoted
  let valId = val.get_id().unwrap()
  let fname = func.name
  match self.promoted.get(fname) {
    Some(promoted) => {
      if promoted.contains(valId) {
        return
      } else {
        promoted.insert(valId)
      }
    }
    None => {
      let promoted = @hashset.T::new()
      promoted.insert(valId)
      self.promoted.set(fname, promoted)
    }
  }

  let entry = func.entry_block.unwrap()
  // count alloca inst number
  let mut alloca_cnt = 0
  for inst in entry.inst_list {
    match inst {
      Alloca(_) => alloca_cnt += 1
      _ => ()
    }
  }
  func.body.sort()
  // 1. create alloca inst in the entry block
  let val_type = val.get_type()
  let alloca_name = "promoteAlloca" + alloca_cnt.to_string()
  let allocainst = AllocaInst::create(
    val_type, func.value_number, name = Some(alloca_name)
  )
  entry.inst_list.insert(0, allocainst.as_inst())
  allocainst.as_value().get_attribute().parent = Some(entry)
  func.value_number += 1

  // 2. insert store inst after inst that create val
  let store_inst = StoreInst::create(val, allocainst.as_value())
  if Isa::argument(val) {
    allocainst.as_value().insert_after(store_inst)
  } else {
    val.insert_after(store_inst)
  }

  // 3. for all users, insert load inst before it, and replace use
  for user in val.get_users() {
    if user == store_inst.as_value() {
      continue
    }
    let load_inst = UnaryInst::create(
      UnaryOperator::Load, allocainst.as_value(),
      val.get_type(), func.value_number
    )
    func.value_number += 1
    // if user is phi node, insert load inst to the incoming block
    if Isa::phiNode(user) {
      let phi : PHINode = dyn::cast(user)
      let new_incoming : Array[(Value, BasicBlock)] = Array::new()
      for inc in phi.incoming {
        let (value, bb) = inc
        if value == val {
          let load = load_inst.as_inst()
          bb.insertToLast_exceptBr(load)
          new_incoming.push((load_inst.as_value(), bb))
        } else {
          new_incoming.push(inc)
        }
      }
      phi.incoming.clear()
      new_incoming.each(fn (inc) { let (v, bb) = inc; phi.addIncoming(v, bb) })
    } else {
      user.insert_before(load_inst)
      user.replace_use(val, load_inst.as_value())
    }
  }
  // println("=======================debug promote ===========================")
  // println(func)

}

pub fn run(self: PromoteReg2MemPass, func: Function) -> Unit {
  match func.body.length() {
    0 => return
    _ => ()
  }
  func.clear_live_analysis()
  analyze_live_for_function(func)

  // debug_live_analysis(func)

  for call_inst in func.call_insts {
    let live_out = call_inst.get_live_out().unwrap()
    for val in live_out {
      if val == call_inst.as_value() {
        continue
      }
      self.promote(val, func)
    }
  }
}

/// ---------------------------------------------------
/// PHI elimination
/// ---------------------------------------------------
pub struct PhiEliminationPass {}

pub fn PhiEliminationPass::new() -> PhiEliminationPass {
  PhiEliminationPass::{}
}

pub fn to_pass(self: PhiEliminationPass) -> Pass {
  Pass::PhiElimination(self)
}

// do not eliminate directly, insert new value into the
// incoming block
//
// for example:
// suppose we have %3 = phi [%1, L1], [%2, L2]
// not just eliminate it
// in L1 BB, insert %4 = addi %1, 0 at the last of BB
// in L2 BB, insert %5 = addi %2, 0 at the last of BB
// then replace %1 with %4, %2 with %5
// now %3 = phi [%4, L1], [%5, L2]
// do not remove it
pub fn run(self: PhiEliminationPass, func: Function) -> Unit {
  let phi_nodes : Array[PHINode] = Array::new()
  for bb in func.body {
    for inst in bb.inst_list {
      match inst {
        PHI(phi) => phi_nodes.push(phi)
        _ => ()
      }
    }
  }
  for phi in phi_nodes {
    let valTy = phi.as_value().get_type()
    let new_incoming : Array[(Value, BasicBlock)] = Array::new()
    for inc in phi.incoming {
      let (val, bb) = inc
      let bin = if valTy.is_integer() {
        BinaryInst::create(
          BinaryOperator::Add, val, Value::Constant(ConstantInt(0)),
          func.value_number
        )
      } else {
        BinaryInst::create(
          BinaryOperator::Add, val, Value::Constant(ConstantFP(0.0)),
          func.value_number
        )
      }
      func.value_number += 1
      bin.value_attr.parent = Some(bb)
      bb.insertToLast_exceptBr(bin.as_inst())
      new_incoming.push((bin.as_value(), bb))
    }
    phi.incoming.clear()
    phi.value_attr.uses.clear()
    new_incoming.each(fn (inc) {
      let (val, bb) = inc;
      phi.addIncoming(val, bb)
    })
  }
}

// pub fn run(self: PhiEliminationPass, func: Function) -> Unit {
//   let phi_nodes = func.get_phi_nodes()
//   if phi_nodes.length() == 0 {
//     return
//   }
//   func.body.sort()
//   let mut phi_count = 0
//   for phi_node in phi_nodes {
//     let replace_map : Map[Int, String] = Map::new()
//     let new_value_id = "phi" + phi_count.to_string()
//     let phi_id = phi_node.as_value().get_id().unwrap()
//     replace_map.set(phi_id, new_value_id)
//     for incoming in phi_node.incoming {
//       let (value, _) = incoming
//       let value_id = value.get_id().unwrap()
//       replace_map.set(value_id, new_value_id)
//     }
//     phi_count += 1
//     for bb in func.body {
//       for inst in bb.inst_list {
//         // let value_attr = inst.get_attribute()
//         match inst.get_id() {
//           Some(i) => match replace_map.get(i) {
//             Some(new_id) => inst.set_name(new_id)
//             None => ()
//           }
//           None => ()
//         }
//         for use in inst.get_uses() {
//           match use.get_id() {
//             Some(i) => match replace_map.get(i) {
//               Some(new_id) => use.set_name(new_id)
//               None => ()
//             }
//             None => ()
//           }
//         }
//       }
//     }
//     phi_node.as_inst().erase_from_parent()
//   }
//   func.phi_nodes.clear()
// }

/// ---------------------------------------------------
/// Virtual Register allocation Pass
/// ---------------------------------------------------
struct VirtualReg {
  id: Int
  kind: VirtualRegKind
} derive (Eq, Hash)

enum VirtualRegKind {
  IA // integer arg
  FA // float arg
  IT // integer temp
  FT // float temp
  M  // memory
} derive (Eq, Hash, Show)

fn to_string(self: VirtualReg) -> String {
  let k = match self.kind {
    VirtualRegKind::IA => "a"
    VirtualRegKind::FA => "fa"
    VirtualRegKind::IT => "t"
    VirtualRegKind::FT => "ft"
    VirtualRegKind::M  => "m"
  }
  "v" + k + self.id.to_string()
}

fn VirtualReg::new(id: Int, ~kind: VirtualRegKind = IT) -> VirtualReg {
  VirtualReg::{
    id,
    kind
  }
}

pub struct VirtualRegisterAllocPass {}

pub fn VirtualRegisterAllocPass::new() -> VirtualRegisterAllocPass {
  VirtualRegisterAllocPass::{}
}

pub fn to_pass(self: VirtualRegisterAllocPass) -> Pass {
  Pass::VirtualRegisterAlloc(self)
}

/// ------------------------------------------------
/// Conflict Graph
/// ------------------------------------------------
struct ConflictGraph {
  edges: Map[Value, @hashset.T[Value]]
}

fn ConflictGraph::new() -> ConflictGraph {
  ConflictGraph::{
    edges: Map::new()
  }
}

// NOTE: the graph is for register allocation
// hence, do not add node when value came from memory
// like `alloca`, `getelementptr`
fn is_memory(val: Value) -> Bool {
  match val {
    Value::Instruction(Alloca(_)) => true
    // for gep inst, only if its index is constant, it should not be in graph
    Value::Instruction(GetElementPtr(gep)) => {
      let idxList = gep.index_list
      let mut all_const = true
      idxList.each(fn (idx) {
        match idx {
          Value::Constant(_) => ()
          _ => all_const = false
        }
      })
      all_const
    }
    _ => false
  }
}

fn add_edge(self: ConflictGraph, a: Value, b: Value) -> Unit {
  if is_memory(a) || is_memory(b) || Isa::function(a) || Isa::function(b) {
    return
  }
  if a == b {
    match self.edges.get(a) {
      Some(_) => return
      None => {
        let a_edges = @hashset.T::new();
        self.edges.set(a, a_edges);
        return 
      }
    }
  }
  match self.edges.get(a) {
    Some(a_edges) => a_edges.insert(b)
    None => {
      let a_edges = @hashset.T::new();
      a_edges.insert(b);
      self.edges.set(a, a_edges);
    }
  }
  match self.edges.get(b) {
    Some(b_edges) => b_edges.insert(a)
    None => {
      let b_edges = @hashset.T::new();
      b_edges.insert(a);
      self.edges.set(b, b_edges);
    }
  }
}

/// ------------------------------------------------
/// Color
/// ------------------------------------------------
// scan from 0 to .., until some number not in colored
// for example
//   colored = [2, 1, 4], it should be 0
//   colored = [2, 1, 0], it should be 3
//   colored = [2, 0, 3], it should be 1
fn color_node(colored: Array[VirtualReg], ~kind : VirtualRegKind) -> VirtualReg {
  let mut r = 0;
  while colored.contains(VirtualReg::new(r, ~kind)) {
    r += 1
  }
  VirtualReg::new(r, ~kind)
}

fn collect_colored(
  nodes: @hashset.T[Value], colored: Map[Value, VirtualReg]
) -> Array[VirtualReg] {
  let result = Array::new();
  for node in nodes {
    match colored.get(node) {
      Some(reg) => result.push(reg)
      None => ()
    }
  }
  result
}


// 1. first, choose those node without color
// 2. second, find the node who has most colored neigbors
fn color_graph(
  map: ConflictGraph,
  init: Map[Value, VirtualReg],
  mutual: Map[Value, @hashset.T[Value]],
  ~kind: VirtualRegKind = IT
) -> Map[Value, VirtualReg] {
  let result = init
  // 1. find the node without color
  // Value, and its colored neigbors
  let uncolored : Array[(Value, Array[VirtualReg])] = Array::new()
  for node, edges in map.edges {
    match result.get(node) {
      Some(_) => continue
      None => {
        let colored = collect_colored(edges, result)
        uncolored.push((node, colored))
      }
    }
  }
  // if uncolored is empty, return result
  if uncolored.is_empty() {
    return result
  }

  // 3. find the node who has most colored neigbors
  uncolored.sort_by(fn (pair1, pair2) {
    let (_, neigbors1) = pair1;
    let (_, neigbors2) = pair2;
    neigbors1.length() - neigbors2.length()
  })
  let (node, colored) = uncolored.pop().unwrap()
  let reg = color_node(colored, ~kind)
  match mutual.get(node) {
    Some(mutuals) => {
      for mutual in mutuals {
        result.set(mutual, reg)
        mutual.get_attribute().analysis.setVirtualReg(reg)
      }
    }
    None => {
      result.set(node, reg)
      node.get_attribute().analysis.setVirtualReg(reg)
    }
  }
  color_graph(map, result, mutual, ~kind)
}

fn split_int_and_float(
  value_list: Iter[Value]
) -> (Array[Value], Array[Value]) {
  let int_list = Array::new();
  let float_list = Array::new();
  for value in value_list {
    match value.get_type().is_fp() {
      true  => float_list.push(value)
      false => int_list.push(value)
    }
  }
  (int_list, float_list)
}

// for example: %2 = phi [%1, BB2], [%3, BB2]
// mutualSet will add:
// %2 => { %1, %2, %3 }
// %1 => { %1, %2, %3 }
// %3 => { %1, %2, %3 }
fn collect_mutual(
  phi: PHINode, mutualSet: Map[Value, @hashset.T[Value]]
) -> Unit {
  // first collect node set, { %1, %2, %3 }, except constant
  let nodeSet : @hashset.T[Value] = @hashset.T::new()
  nodeSet.insert(phi.as_value())
  for inc in phi.incoming {
    let (val, _) = inc
    if Isa::constant(val) {
      continue
    }
    nodeSet.insert(val)
  }


  // then add empty set to mutualSet
  for node in nodeSet {
    match mutualSet.get(node) {
      Some(_) => return
      None => {
        let mutual = @hashset.T::new()
        mutual.insert(node)
        for n2 in nodeSet { mutual.insert(n2) }
        mutualSet.set(node, mutual)
      }
    }
  }

}

fn isPHINode(self: Value) -> Bool {
  match self {
    Value::Instruction(PHI(_)) => true
    _ => false
  }
}

fn isPHINode(self: Instruction) -> Bool {
  match self {
    PHI(_) => true
    _ => false
  }
}

// return 2 elements,
// the first is virtual register allocation result for int
// the second is virtual register allocation result for float
pub fn run(
  self: VirtualRegisterAllocPass, func: Function
) -> Unit {
  match func.body.length() {
    0 => return
    _ => ()
  }
  func.body.sort()
  // 1. run phi elimination pass
  // let phi_elim_pass = PhiEliminationPass::new()
  // phi_elim_pass.run(func)

  // 1. run live analysis
  func.clear_live_analysis()
  analyze_live_for_function(func)

  // 2. clear virtual reg
  func.clear_virtual_reg()

  // debug_live_analysis(func)
  // 3. build conflict graph
  // Note: Here, we distinguish int and float
  let conflict_graph_int = ConflictGraph::new()
  let conflict_graph_fp = ConflictGraph::new()
  let arg_values = func.args.map(fn (arg) {arg.as_value()})
  let (arg_ints, arg_fps) = split_int_and_float(arg_values.iter())
  let init_map_int : Map[Value, VirtualReg] = Map::new()
  let init_map_fp : Map[Value, VirtualReg] = Map::new()

  // init init_map_int, init_map_fp, conflict_graph_int, conflict_graph_fp
  // Note: cannot give too large arg vreg number, if s_arg_reg_used >= 8, use IT
  let mut s_arg_reg_used = 0
  let mut f_arg_reg_used = 0
  for arg in arg_ints {
    let vreg = match s_arg_reg_used < 8 {
      true => VirtualReg::new(s_arg_reg_used, kind = VirtualRegKind::IA)
      false => VirtualReg::new(s_arg_reg_used - 8, kind = VirtualRegKind::IT)
    }
    s_arg_reg_used += 1
    init_map_int.set(arg, vreg)
    arg.get_attribute().analysis.setVirtualReg(vreg)
  }
  for arg in arg_fps {
    let vreg = match f_arg_reg_used < 8 {
      true => VirtualReg::new(f_arg_reg_used, kind = VirtualRegKind::FA)
      false => VirtualReg::new(f_arg_reg_used - 8, kind = VirtualRegKind::FT)
    }
    f_arg_reg_used += 1
    init_map_fp.set(arg, vreg)
    arg.get_attribute().analysis.setVirtualReg(vreg)
  }
  for arg1 in arg_ints {
    for arg2 in arg_ints {
      conflict_graph_int.add_edge(arg1, arg2)
    }
  }
  for arg1 in arg_fps {
    for arg2 in arg_fps {
      conflict_graph_fp.add_edge(arg1, arg2)
    }
  }

  // mutual set is for deal phi node
  // suppose we have phi node: `% 3 = PHI [%1, ..], [%2, ..]`
  // %1, %2, %3 must have same color
  // so we set mutual set: 
  // %1 -> [%1, %2, %3]
  // %2 -> [%1, %2, %3],
  // %3 -> [%1, %2, %3]
  let mutualSet : Map[Value, @hashset.T[Value]] = Map::new()

  // println(" ----------- before virtual register allocation -----------------")
  // debug_live_analysis(func)
  for bb in func.body {
    for inst in bb.inst_list {
      if inst.isPHINode() {
        guard let Instruction::PHI(phi) = inst
        collect_mutual(phi, mutualSet)
      }
      let live_analysis = inst.get_live_analysis().unwrap()
      let live_out = live_analysis.live_out
      let (live_ints, live_fps) = split_int_and_float(live_out.iter())
      for l1 in live_ints {
        for l2 in live_ints {
          conflict_graph_int.add_edge(l1, l2)
        }
      }
      for l1 in live_fps {
        for l2 in live_fps {
          conflict_graph_fp.add_edge(l1, l2)
        }
      }
    }
  }
  // println(func)
  // println("----------- debugging info -----------------")
  // println("conflict graph for int:")
  // for node, edges in conflict_graph_int.edges {
  //   println("\{node.trivial_stringify()} -> \{edges}")
  // }
  let result_int = color_graph(
    conflict_graph_int, init_map_int, mutualSet,
    kind = VirtualRegKind::IT,
  )
  let result_fp = color_graph(
    conflict_graph_fp, init_map_fp,
    kind = VirtualRegKind::FT, mutualSet
  )
  // if not(result_int.is_empty()) {
  //   println("virtual register allocation result for int:")
  //   for v, vreg in result_int {
  //     println("\{v.trivial_stringify()} -> \{vreg}")
  //   }
  //   println("")
  // }
  // if not(result_fp.is_empty()) {
  //   println("virtual register allocation result for float:")
  //   for v, vreg in result_fp {
  //     println("\{v.trivial_stringify()} -> \{vreg}")
  //   }
  //   println("")
  // }

  let spill_set_s : @hashset.T[Value] = @hashset.T::new()
  let spill_set_f : @hashset.T[Value] = @hashset.T::new()

  for val, vreg in result_int {
    val.set_name(vreg.to_string())
    val.get_attribute().analysis.setVirtualReg(vreg)
    // 18 is t0 ~ t6, 7 regs, s1~s9, 9 regs, 7 + 9 = 16
    // However, we must consider spill problem, so here we use 14: 16 - 2
    if vreg.kind == VirtualRegKind::IT && vreg.id > 14 { // if `vt > 14`, spill it
      spill_set_s.insert(val)
      // println("adding to spill set: \{vreg}")
    }
  }
  for val, vreg in result_fp {
    val.set_name(vreg.to_string())
    val.get_attribute().analysis.setVirtualReg(vreg)
    // 19 is ft0 ~ ft11, 11 regs, fs0~fs11, 12 regs, 11 + 12 = 23, 23 - 2 = 21
    if vreg.kind == VirtualRegKind::FT && vreg.id > 21 { // if `vft > 21`, spill it
      spill_set_f.insert(val)
    }
  }

  // if spill_set has more than 2 type of sregs, need to spill them all
  let spill_sregs : @hashset.T[VirtualReg] = @hashset.T::new()
  let spill_fregs : @hashset.T[VirtualReg] = @hashset.T::new()

  spill_set_s.each(fn (v) { spill_sregs.insert(v.get_attribute().analysis.virtualReg.unwrap()) })
  spill_set_f.each(fn (v) { spill_fregs.insert(v.get_attribute().analysis.virtualReg.unwrap()) })


  if spill_sregs.size() >= 2 || spill_fregs.size() >= 2 {
    if spill_set_s.size() > 2 {
      let promoteReg2MemPass = PromoteReg2MemPass::new()
      for val in spill_set_s {
        promoteReg2MemPass.promote(val, func)
      }
    }

    if spill_set_f.size() > 2 {
      let promoteReg2MemPass = PromoteReg2MemPass::new()
      for val in spill_set_f {
        promoteReg2MemPass.promote(val, func)
      }
    }
    self.run(func)
  }
}

/// ---------------------------------------------------
/// RISCVRegisterAllocPass 
///
/// ---------------------------------------------------
pub struct RiscVRegisterAllocPass {}

pub fn RiscVRegisterAllocPass::new() -> RiscVRegisterAllocPass {
  RiscVRegisterAllocPass::{}
}

pub fn to_pass(self: RiscVRegisterAllocPass) -> Pass {
  Pass::RISCVRegisterAlloc(self)
}

fn check_sreg_saved(srg: @riscv.Reg) -> Bool {
  let s_save_reg = [
    @riscv.Reg::S1, @riscv.Reg::S2, @riscv.Reg::S3, @riscv.Reg::S4,
    @riscv.Reg::S5, @riscv.Reg::S6, @riscv.Reg::S7, @riscv.Reg::S8,
    @riscv.Reg::S9, @riscv.Reg::S10, @riscv.Reg::S11
  ]
  if s_save_reg.contains(srg) {
    return true
  }
  return false
}

fn check_freg_saved(frg: @riscv.FReg) -> Bool {
  let f_save_reg = [
    @riscv.FReg::Fs0, @riscv.FReg::Fs1, @riscv.FReg::Fs2, @riscv.FReg::Fs3,
    @riscv.FReg::Fs4, @riscv.FReg::Fs5, @riscv.FReg::Fs6, @riscv.FReg::Fs7,
    @riscv.FReg::Fs8, @riscv.FReg::Fs9, @riscv.FReg::Fs10, @riscv.FReg::Fs11,
  ]

  if f_save_reg.contains(frg) {
    return true
  }
  return false
}

  // vt{n}, alloc sreg[n]
let non_arg_sregs : Array[@riscv.Reg] = [
  @riscv.Reg::T0, @riscv.Reg::T1, @riscv.Reg::T2, @riscv.Reg::T3,
  @riscv.Reg::T4, @riscv.Reg::T5, @riscv.Reg::T6,
  // @riscv.Reg::S0,  not use S0 here
  @riscv.Reg::S1, @riscv.Reg::S2, @riscv.Reg::S3, @riscv.Reg::S4,
  @riscv.Reg::S5, @riscv.Reg::S6, @riscv.Reg::S7, @riscv.Reg::S8,
  @riscv.Reg::S9, // @riscv.Reg::S10, not alloc S10
  @riscv.Reg::S11,
]

// for vf{n}, alloc freg[n]
let non_arg_fregs : Array[@riscv.FReg] = [
  @riscv.FReg::Ft0, @riscv.FReg::Ft1, @riscv.FReg::Ft2, @riscv.FReg::Ft3,
  @riscv.FReg::Ft4, @riscv.FReg::Ft5, @riscv.FReg::Ft6, @riscv.FReg::Ft7,
  @riscv.FReg::Ft8, @riscv.FReg::Ft9, @riscv.FReg::Ft10, @riscv.FReg::Ft11,
  @riscv.FReg::Fs0, @riscv.FReg::Fs1, @riscv.FReg::Fs2, @riscv.FReg::Fs3,
  @riscv.FReg::Fs4, @riscv.FReg::Fs5, @riscv.FReg::Fs6, @riscv.FReg::Fs7,
  @riscv.FReg::Fs8, @riscv.FReg::Fs9, @riscv.FReg::Fs10, @riscv.FReg::Fs11,
]

pub fn run(self: RiscVRegisterAllocPass, func: Function) -> Unit {
  match func.body.length() {
    0 => return
    _ => ()
  }

  let mut sargs = 0
  let mut fargs = 0
  for arg in func.args {
    let analysis = arg.get_attribute().analysis
    let vreg = analysis.virtualReg.unwrap()
    match vreg {
      {id : n, kind: IA} => sargs = if sargs > (n+1) { sargs } else { n + 1 }
      {id : n, kind: FA} => fargs = if fargs > (n+1) { fargs } else { n + 1 }
      _ => {println("Should not alloca other virtual reg to arg"); panic()}
    }
    match vreg {
      {id : 0, kind:IA} => analysis.setPhysicalSReg(@riscv.Reg::A0)
      {id : 1, kind:IA} => analysis.setPhysicalSReg(@riscv.Reg::A1)
      {id : 2, kind:IA} => analysis.setPhysicalSReg(@riscv.Reg::A2)
      {id : 3, kind:IA} => analysis.setPhysicalSReg(@riscv.Reg::A3)
      {id : 4, kind:IA} => analysis.setPhysicalSReg(@riscv.Reg::A4)
      {id : 5, kind:IA} => analysis.setPhysicalSReg(@riscv.Reg::A5)
      {id : 6, kind:IA} => analysis.setPhysicalSReg(@riscv.Reg::A6)
      {id : 7, kind:IA} => analysis.setPhysicalSReg(@riscv.Reg::A7)
      {id : 0, kind:FA} => analysis.setPhysicalFReg(@riscv.FReg::Fa0)
      {id : 1, kind:FA} => analysis.setPhysicalFReg(@riscv.FReg::Fa1)
      {id : 2, kind:FA} => analysis.setPhysicalFReg(@riscv.FReg::Fa2)
      {id : 3, kind:FA} => analysis.setPhysicalFReg(@riscv.FReg::Fa3)
      {id : 4, kind:FA} => analysis.setPhysicalFReg(@riscv.FReg::Fa4)
      {id : 5, kind:FA} => analysis.setPhysicalFReg(@riscv.FReg::Fa5)
      {id : 6, kind:FA} => analysis.setPhysicalFReg(@riscv.FReg::Fa6)
      {id : 7, kind:FA} => analysis.setPhysicalFReg(@riscv.FReg::Fa7)
      _ => {
        println("Virtual Arg Reg should not be over 7")
        panic()
      }
    }
  }
  
  let saregs = [
    @riscv.Reg::A0, @riscv.Reg::A1, @riscv.Reg::A2, @riscv.Reg::A3,
    @riscv.Reg::A4, @riscv.Reg::A5, @riscv.Reg::A6, @riscv.Reg::A7,
  ]

  let faregs = [
    @riscv.FReg::Fa0, @riscv.FReg::Fa1, @riscv.FReg::Fa2, @riscv.FReg::Fa3,
    @riscv.FReg::Fa4, @riscv.FReg::Fa5, @riscv.FReg::Fa6, @riscv.FReg::Fa7,
  ]



  let s_saved: @hashset.T[@riscv.Reg] = @hashset.T::new()
  let f_saved: @hashset.T[@riscv.FReg] = @hashset.T::new()

  for bb in func.body {
    let erase_list : Array[Instruction] = Array::new()
    for inst in bb.inst_list {
      // println("debug: inst is \{inst}")
      let inst_analysis = inst.as_value().get_attribute().analysis
      match inst {
        Instruction::Alloca(_) => continue
        Instruction::Store(_) => continue
        Instruction::Branch(_) => continue
        Instruction::Return(_) => continue
        Instruction::GetElementPtr(gep) => {
          let mut all_const = true
          let idxList = gep.index_list
          idxList.each(fn (idx) {
            match idx {
              Value::Constant(_) => ()
              _ => all_const = false
            }
          })
          if all_const {
            continue
          }
        }
        Instruction::Call(_) => {
          if inst.get_type().is_void() {
            continue
          }
        }
        _ => match inst_analysis.virtualReg {
          None => {
            // println("warning: \{inst} didn't alloc virtual reg")
            // panic()
            // inst.erase_from_parent()
            erase_list.push(inst)
            continue
          }
          _ => ()
        }
      }
      let vreg = inst_analysis.virtualReg
      let vreg = match vreg {
        None => {
          // println("warning: \{inst} didn't alloc virtual reg")
          // panic()
          // inst.erase_from_parent()
          erase_list.push(inst)
          continue
        }
        Some(vreg) => vreg
      }
      // println("debug: vreg is \{vreg}, \{vreg.id}, \{vreg.kind}")
      match vreg {
        {id: n, kind: IA} => sargs = if sargs > (n+1) { sargs } else { n+1 }
        {id: n, kind: FA} => fargs = if fargs > (n+1) { fargs } else { n+1 }
        _ => ()
      }
    }
    for inst in erase_list {
      inst.erase_from_parent()
    }
  }


  // println("func \{func.name}, sargs: \{sargs}, fargs: \{fargs}")

  for bb in func.body {
    for inst in bb.inst_list {
      let inst_analysis = inst.as_value().get_attribute().analysis
      match inst {
        Instruction::Alloca(_) => continue
        Instruction::Store(_) => continue
        Instruction::Branch(_) => continue
        Instruction::Return(_) => continue
        Instruction::GetElementPtr(gep) => {
          let mut all_const = true
          let idxList = gep.index_list
          idxList.each(fn (idx) {
            match idx {
              Value::Constant(_) => ()
              _ => all_const = false
            }
          })
          if all_const {
            continue
          }
        }
        Instruction::Call(_) => {
          if inst.get_type().is_void() {
            continue
          }
        }
        _ => match inst_analysis.virtualReg {
          None => {
            println("warning: \{inst} didn't alloc virtual reg")
            panic()
          }
          _ => ()
        }
      }
      let vreg = inst_analysis.virtualReg
      let vreg = match vreg {
        None => {
          println("warning: \{inst} didn't alloc virtual reg")
          panic()
        }
        Some(vreg) => vreg
      }
      match vreg {
        {id : n, kind:IA} => {
          if n < saregs.length() {
            inst_analysis.setPhysicalSReg(saregs[n])
          }
        }
        {id : n, kind:FA} => {
          if n < faregs.length() {
            inst_analysis.setPhysicalFReg(faregs[n])
          }
        }
        {id : n, kind:IT} => {
          if n >= non_arg_sregs.length() {
            println("Error! Too many virtual regs, n = \{n}")
            panic()
          }
          let sreg = non_arg_sregs[n]
          if check_sreg_saved(sreg) { s_saved.insert(sreg) }
          inst_analysis.setPhysicalSReg(non_arg_sregs[n])
        }
        {id : n, kind:FT} => {
          let freg = non_arg_fregs[n]
          if check_freg_saved(freg) { f_saved.insert(freg) }
          inst_analysis.setPhysicalFReg(non_arg_fregs[n])
        }
        _ => ()
      }
    }
  }
}

/// ---------------------------------------------------
/// RiscVMemoryAllocPass
///
/// Note:
///
/// for instruction `alloca` and `getelementptr`, assign
/// them to `vm`
///
/// for example:
///
/// `%1 = alloca i32` -> `%vm0 = alloca i32`
/// `%2 = getelementptr i32, i32* %1, i32 1` -> 
///       `%vm1 = getelementptr i32, i32* %vm0, i32 1``
/// ---------------------------------------------------
pub struct RiscVMemoryAllocPass {}

pub fn RiscVMemoryAllocPass::new() -> RiscVMemoryAllocPass {
  RiscVMemoryAllocPass::{}
}

pub fn to_pass(self: RiscVMemoryAllocPass) -> Pass {
  Pass::RISCVMemoryAlloc(self)
}

pub fn collect_saved_regs(func: Function) -> (Array[@riscv.Reg], Array[@riscv.FReg]) {
  let save_sregs_set = @hashset.T::new()
  let save_frags_set = @hashset.T::new()

  // need 1 sreg handle some special cases during asm codegen
  save_sregs_set.insert(@riscv.Reg::S10)

  for bb in func.body {
    for inst in bb.inst_list {
      let phyloc = inst.as_value().get_attribute().analysis.riscvPhyLoc
      match phyloc {
        Some(SReg(sreg)) => {
          if check_sreg_saved(sreg) {
            save_sregs_set.insert(sreg)
          }
        }
        Some(FReg(freg)) => {
          if check_freg_saved(freg) {
            save_frags_set.insert(freg)
          }
        }
        _ => ()
      }
    }
  }
  let save_sregs = save_sregs_set.iter().to_array()
  let save_frags = save_frags_set.iter().to_array()
  save_sregs.sort()
  save_frags.sort()
  (save_sregs, save_frags)
}

fn countOverFlowArg(call: CallInst) -> Int {
  let args = call.arg_list
  // if call inst has less than 8 arguments, return 0
  // because it must has no overflow arguments
  if args.length() <= 8 {
    return 0
  }
  // if it has more than 8 arguments, we need to count sargs and fargs
  let mut sargs = 0
  let mut fargs = 0
  for arg in args {
    let argTy = arg.get_type()
    match argTy.is_fp() {
      true => fargs += 1
      false => sargs += 1
    }
  }
  let overflowed_sarg = if sargs > 8 { sargs - 8 } else { 0 }
  let overflowed_farg = if fargs > 8 { fargs - 8 } else { 0 }
  overflowed_sarg + overflowed_farg
}

// function probably call some other function which has too many arguments (over 8)
// for this case, first, collect all callInst which has too many arguments
// calcuate how many stack we need to save. for example, if func call foo, which
// has 9 arguments, and func call bar, which has 10 arguments, then we need to
// extra 2 * 8 = 16 bytes stack to save them
// need insert `store` before these call
fn handleExcessArgCall(self: RiscVMemoryAllocPass, func: Function) -> Unit {
  let hasOverflowedArgCall : Array[CallInst] = Array::new()
  let mut maxOverflowed = 0
  for bb in func.body {
    for inst in bb.inst_list {
      match inst {
        Call(call) => {
          let overflowed = countOverFlowArg(call)
          if overflowed > 0 {
            hasOverflowedArgCall.push(call)
            maxOverflowed = max_int(maxOverflowed, overflowed)
          }
        }
        _ => ()
      }
    }
  }
  if maxOverflowed == 0 { return }
  // alloc extra stack for overflowed arguments
  let entry = func.entry_block.unwrap()
  let allocas : Array[AllocaInst] = Array::new()
  for i = 0; i < maxOverflowed; i = i + 1 {
    let alloc_name = "OverflowArg" + i.to_string()
    let alloc = AllocaInst::create(
      Type::Int32Ty, func.value_number, name = Some(alloc_name)
    )
    func.value_number += 1
    entry.inst_list.insert(0, alloc.as_inst())
    alloc.as_value().get_attribute().parent = Some(entry)
    allocas.insert(0, alloc) // keep order in entry block's inst_list
  }

  let extra_alloc = maxOverflowed * 8
  func.extra_alloc = Some(extra_alloc)

  // for all has overflowed call, insert store before them
  for call in hasOverflowedArgCall {
    let mut sargs = 0
    let mut fargs = 0
    let mut storeIdx = 0
    for arg in call.arg_list {
      match arg.get_type().is_fp() {
        true => fargs += 1
        false => sargs += 1
      }
      // guard
      if storeIdx > allocas.length() {
        println("Error! storeIdx > allocas.length(), check code")
        panic()
      }
      if sargs > 8 || fargs > 8 {
        let store = StoreInst::create(
          arg, allocas[storeIdx].as_value()
        )
        call.as_value().insert_before(store)
        func.value_number += 1
        storeIdx += 1
      }
    }
  }
}

pub fn run(self: RiscVMemoryAllocPass, func: Function) -> Unit {
  match func.body.length() {
    0 => return
    _ => ()
  }
  let entry = func.entry_block.unwrap()
  let mut vm_cnt = 0

  // 1. first, handle itself excess arguments, suppose func has 1 extra sarg, and 1 extra farg
  // the sarg alloced vt0, farg alloced vft0,
  // alloc the sarg to OutsideSreg(Reg::T0, MemAccess{base: Reg::Fp, offset: 0})
  // alloc the farg to OutsideFreg(FReg::Ft0, MemAccess{base: Reg::Fp, offset: 8})
  let mut outsideOffset = 0
  for arg in func.args {
    let analysis = arg.get_attribute().analysis
    let vreg = match analysis.virtualReg {
      Some(vreg) => vreg
      None => {
        println("Error! Didn't run VirtualRegisterAllocPass")
        println("\{arg.trivial_stringify()} didn't alloc virtual reg")
        panic()
      }
    }
    match vreg {
      {id: n, kind: IT} => {
        if n > non_arg_sregs.length() {
          println("Error! Too many virtual regs, n = \{n}, loc: RiscVMemoryAllocPass ")
          panic()
        }
        analysis.setPhysicalOutsideSReg(non_arg_sregs[n], outsideOffset)
        outsideOffset += 8
      }
      {id: n, kind: FT} => {
        if n > non_arg_fregs.length() {
          println("Error! Too many virtual regs, n = \{n}, loc: RiscVMemoryAllocPass ")
          panic()
        }
        analysis.setPhysicalOutsideFReg(non_arg_fregs[n], outsideOffset)
        outsideOffset += 8
      }
      _ => ()
    }
  }

  // 2. second, handle call inst which has too many arguments
  handleExcessArgCall(self, func)

  // initialzie stack size as 0
  // but remember, we need to save ra and fp, so after alloca, we need to add 16
  let mut stack_size = 0
  let mut offset = stack_size
  // 3. collect save registers and fsave registers
  let (save_sregs, save_frags) = collect_saved_regs(func)
  stack_size += save_sregs.length() * 8 + save_frags.length() * 8

  // 4. deal with alloca
  for inst in entry.inst_list {
    match inst {
      Alloca(alloca) => {
        let analysis = inst.get_attribute().analysis
        let vm = VirtualReg::new(vm_cnt, kind = VirtualRegKind::M)
        analysis.setVirtualReg(vm)
        vm_cnt += 1

        let ty_size = sizeof(alloca.value_type)
        let ty_align= align_of(alloca.value_type)
        offset = align_to(stack_size, ty_align)
        stack_size = stack_size + ty_size
        let mem = @riscv.MemAccess::{base:@riscv.Reg::Sp, offset}
        analysis.setPhysicalMem(mem)
      }
      _ => ()
    }
  }

  // here, stack_size need to add 16, because we need to save ra and fp
  stack_size += 16
  func.stack_size = Some(stack_size)

  // 2. deal with getelementptr
  for bb in func.body {
    for inst in bb.inst_list {
      match inst {
        GetElementPtr(gep) => {
          let mut all_const = true
          for idx in gep.index_list {
            match idx {
              Value::Constant(ConstantInt(_)) => ()
              _ => { all_const = false }
            }
          }
          // if gep's index is not all constant, skip it,
          // because it need generate asm, and its not in memory
          if not(all_const) { continue }
          let inst_analysis = inst.get_attribute().analysis
          let vreg = VirtualReg::new(vm_cnt, kind = VirtualRegKind::M)
          inst_analysis.setVirtualReg(vreg)
          vm_cnt += 1
          let ptr_val = gep.pointer
          // There are two possibilities:
          // 1. ptr_val is alloca, which means it's a memaccess
          //   then dst_val must be MemAccess, where
          //   base = ptr_val.base
          //   offset = ptr_val.offset + gep_offset
          //   where gep_offset = sizeof(value_type) * index
          // 2. ptr_val is a sreg, then dst_val should be MemAccess,
          //   where base = ptr_val, offset = sizeof(value_type) * index
          let ptr_phy_loc = match ptr_val.get_attribute().analysis.riscvPhyLoc {
            Some(phy_loc) => phy_loc
            None => {
              println("Error! Didn't run RISCVRegisterAllocPass")
              panic()
            }
          }
          // calculate gep offset
          let mut gep_offset = 0
          let mut pointedTy = gep.pointedTy
          for idxV in gep.index_list {
            guard let Value::Constant(ConstantInt(idx)) = idxV
            match pointedTy {
              // if it is pointer, check the pointed type is equal to curr_type
              Type::StructTy(struTy) => {
                let fields = struTy.fields
                gep_offset += struTy.offsets[idx.to_int()]
                pointedTy = fields[idx.to_int()]
              }
              vty => {
                let val_size = sizeof(vty)
                gep_offset += val_size * idx.to_int()
                pointedTy = vty
              }
            }
          }

          // let gep_offset = val_size * index
          let mem = match ptr_phy_loc {
            Mem(mem) => {
              let (base, offset) = (mem.base, mem.offset)
              let offset = offset + gep_offset
              @riscv.MemAccess::{base, offset}
            }
            SReg(sreg) => @riscv.MemAccess::{base:sreg, offset:gep_offset}
            _ => {
              println("Error! ptr_phy_loc is not Mem or SReg")
              panic()
            }
          }
          inst_analysis.setPhysicalMem(mem)
        }
        _ => ()
      }
    }
  }
}
