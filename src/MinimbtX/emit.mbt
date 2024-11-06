/// ---------------------------------------------------------------------
/// Emit Riscv
/// ---------------------------------------------------------------------


// pub fn stack_alloc_for_func_def(
//   func_def: ClosureIRFuncDef,
//   reg_map: Map[Name, Reg],
//   freg_map: Map[Name, FReg]
// ) -> (Map[Name, Int], Int) {
//   let mut stack_point: Int = 0
//   let stack_map : Map[Name, Int] = Map::new()
//   let param_tys : Array[(Name, LowType)] = func_def.params
//   for param_ty in param_tys {
//     let (param, ty) = param_ty
//     let has_reg = if ty.is_floating() {
//       match freg_map.get(param) {
//         Some(freg) => true
//         None => false
//       }
//     } else {
//       match reg_map.get(param) {
//         Some(reg) => true
//         None => false
//       }
//     }
//     if not(has_reg) {
//       let size = ty.size()
//       stack_point -= size
//       stack_map.set(param, stack_point)
//     }
//   }
// }
