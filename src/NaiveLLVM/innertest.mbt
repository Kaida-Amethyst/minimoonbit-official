fn simple_func() -> Function {
  let simple_func = Module::create("simple_func")
  let i32ty = Type::getInt32Ty()
  let func_type = Type::getFunctionTy(i32ty, [i32ty, i32ty])
  let func = Function::create(func_type, "foo", simple_func)
  let entry_bb = BasicBlock::create(name="entry", parent=func)
  let builder = IRBuilder::new(func)
  builder.setInsertPoint(entry_bb)
  let arg0 = func.get_argument(0)
  let arg1 = func.get_argument(1)
  let r1 = builder.createAdd(arg0, arg1)
  let r2 = builder.createSub(arg0, arg1)
  let r = builder.createMul(r1, r2)
  let _ = builder.createRet(r)
  func
}

fn simple_max() -> Function {
  let simple_max = Module::create("simple_max")
  let i32ty = Type::getInt32Ty()
  let func_type = Type::getFunctionTy(i32ty, [i32ty, i32ty])
  let func = Function::create(func_type, "max", simple_max)
  let entry_bb = BasicBlock::create(parent=func)
  let then_bb = BasicBlock::create(parent=func)
  let else_bb = BasicBlock::create(parent=func)
  let merge_bb = BasicBlock::create(parent=func)
  let builder = IRBuilder::new(func)
  builder.setInsertPoint(entry_bb)
  let arg0 = func.get_argument(0)
  let arg1 = func.get_argument(1)
  let cond = builder.createICmpSGT(arg0, arg1)  // a > b
  let _ = builder.createCondBr(cond, then_bb, else_bb)
  builder.setInsertPoint(then_bb)
  let _ = builder.createBr(merge_bb)
  builder.setInsertPoint(else_bb)
  let _ = builder.createBr(merge_bb)
  builder.setInsertPoint(merge_bb)
  let phi = builder.createPHI(i32ty, 2, name="max_val")
  phi.addIncoming(arg0, then_bb)
  phi.addIncoming(arg1, else_bb)
  let _ = builder.createRet(phi)
  func
}

test "simple func" {
  let _ = simple_func()
  let _ = simple_max()
  // match simple_func?() {
  //   Ok(_) => ()
  //   Err(_) => fail!("IR Error")
  // }
}
