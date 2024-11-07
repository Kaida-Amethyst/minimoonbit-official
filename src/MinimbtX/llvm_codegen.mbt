/// --------------------------------------------
/// Code Gen
/// --------------------------------------------
pub struct LLVMCodeGen {
  prog: ClosureProgram
  global_vars: Map[String, @llvm.Value]
  extern_vars: Map[String, @llvm.Type]
  builtin_func: Map[String, @llvm.Type]
  module: @llvm.Module
}

pub fn LLVMCodeGen::create(
  prog: ClosureProgram, name: String
) -> LLVMCodeGen {
  let module = @llvm.Module::create(name)
  let code_gen = LLVMCodeGen::{
    prog,
    extern_vars: Map::new(),
    global_vars: Map::new(),
    builtin_func: Map::new(),
    module
  }
  code_gen.collect_extern_vars()
  code_gen.collect_function_defs()
  code_gen.collect_builtin_funcs()
  code_gen.collect_non_func_global_vars()
  code_gen
}

pub fn create_builder(
  self: LLVMCodeGen, func_def: ClosureIRFuncDef
) -> LLVMBuilder {
  LLVMBuilder::new(self, func_def)
}

pub fn create_main_builder(
  self: LLVMCodeGen, body: ClosureIR
) -> LLVMBuilder {
  LLVMBuilder::new_main(self, body)
}

pub fn collect_extern_vars(self: LLVMCodeGen) -> Unit {
  for ename, etype in self.prog.context.extern_funcs {
    let llvm_type = etype.to_llvm_type()
    self.extern_vars.set(ename.to_string(), llvm_type)
  }
  // library functions
  // 1. malloc: void* malloc(int32_t size)
  // 2. create_array: void* create_array(int32_t n_element, int32_t init)
  // 3. create_float_array: void* create_float_array(double n_element, double init)
  // 4. create_ptr_array: void* create_ptr_array(int32_t n_element, void* init)
  let malloc_ty = @llvm.Type::FunctionTy(
    [@llvm.Type::Int32Ty], @llvm.Type::getPointerTy()
  )
  self.extern_vars.set("malloc", malloc_ty)
  let create_array_ty = @llvm.Type::FunctionTy(
    [@llvm.Type::Int32Ty, @llvm.Type::Int32Ty],
    @llvm.Type::getPointerTy(ty=@llvm.Type::Int32Ty)
  )
  self.extern_vars.set("create_array", create_array_ty)
  let create_float_array_ty = @llvm.Type::FunctionTy(
    [@llvm.Type::DoubleTy, @llvm.Type::DoubleTy],
    @llvm.Type::getPointerTy(ty=@llvm.Type::DoubleTy)
  )
  self.extern_vars.set("create_float_array", create_float_array_ty)
  let create_ptr_array_ty = @llvm.Type::FunctionTy(
    [@llvm.Type::Int32Ty, @llvm.Type::getPointerTy()],
    @llvm.Type::getPointerTy(ty=@llvm.Type::getPointerTy())
  )
  self.extern_vars.set("create_ptr_array", create_ptr_array_ty)
}

pub fn collect_builtin_funcs(self: LLVMCodeGen) -> Unit {
  for bname, btype in self.prog.context.builtin_funcs {
    let llvm_type = btype.to_llvm_type()
    self.builtin_func.set(bname.to_string(), llvm_type)
  }
}

// Note: Do not use LLVM's GlobalVariable
// first, check the llvm type is constant
// if it is constant, generate constant Value
pub fn collect_non_func_global_vars(self: LLVMCodeGen) -> Unit {
  for tup in self.prog.global_vars {
    let (name, (_, ir)) = tup
    let global_var = match ir {
      ClosureIR::Int(i) => @llvm.Constant::getInt32(i)
      ClosureIR::Double(d) => @llvm.Constant::getDouble(d)
      _ => {
        println("Unsupported global variable type: " + ir.stringify(0))
        panic()
      }
    }
    self.global_vars.set(name.to_string(), global_var.as_value())
  }
}

pub fn find_function(self: LLVMCodeGen, name: String) -> @llvm.Function? {
  match self.module.getFunction(name) {
    Some(func) => Some(func)
    None => match self.extern_vars.get(name) {
      Some(ty) => Some(@llvm.Function::create(ty, name, self.module))
      None => None
    }
  }
}

pub fn find_builtin_func(self: LLVMCodeGen, name: String) -> @llvm.Type? {
  self.builtin_func.get(name)
}

pub fn collect_function_defs(self: LLVMCodeGen) -> Unit {
  for func_def in self.prog.func_defs {
    let ret_ty = func_def.ret_type.to_llvm_type()
    let arg_ty = func_def.params.map(fn (pair) {
      let (_, ty) = pair; ty.to_llvm_type()
    })
    // if it has free variables, its last argument is a pointer
    // need to convert it to a pointer to a struct
    if func_def.free_vars.length() > 0 {
      let _ = arg_ty.pop().unwrap()
      let tuple_type : Array[@llvm.Type] = Array::new()
      // first element is a pointer to function
      tuple_type.push(@llvm.Type::getPointerTy())
      for free_var in func_def.free_vars {
        let (_, var_ty) = free_var
        tuple_type.push(var_ty.to_llvm_type())
      }
      let tuple_type = @llvm.Type::getTupleTy(tuple_type)
      let last_arg_ty = @llvm.Type::getPointerTy(ty=tuple_type)
      arg_ty.push(last_arg_ty)
    }

    // If function capture free variable, its last argument is a pointer
    let func_ty = @llvm.getFunctionTy(ret_ty, arg_ty)
    let func: @llvm.Function = @llvm.Function::create(
      func_ty, func_def.fname.to_string(), self.module
    )
    let func_value = func.as_value()
    self.global_vars.set(func_def.fname.to_string(), func_value)
  }

  match self.prog.body {
    ClosureIR::Unit => ()
    _ => {
      let ret_ty = LowType::Int.to_llvm_type()
      let arg_ty = Array::new()
      let func_ty = @llvm.getFunctionTy(ret_ty, arg_ty)
      let func: @llvm.Function = @llvm.Function::create(
        func_ty, "main", self.module
      )
      let func_value = func.as_value()
      self.global_vars.set("main", func_value)
    }
  }
}

pub fn run(self: LLVMCodeGen) -> @llvm.Module {
  for func_def in self.prog.func_defs {
    let builder = self.create_builder(func_def)
    let _ = builder.run()
  }
  let main_builder = self.create_main_builder(self.prog.body)
  let _ = main_builder.run()
  self.module
}

/// --------------------------------------------
/// LLVM Code Gen Manager
/// --------------------------------------------
pub struct LLVMBuilder {
  manager: LLVMCodeGen
  func_name: String
  params: Array[(Name, LowType)]
  ret_type: LowType
  body: ClosureIR
  free_vars: Array[(Name, LowType)]
  var_table: Map[String, @llvm.Value]
  closure_table: Map[String, @llvm.Type]
  mut labels : Int
  builder: @llvm.IRBuilder
  function: @llvm.Function
}

pub fn LLVMBuilder::new(
  manager: LLVMCodeGen,
  func_def: ClosureIRFuncDef,
) -> LLVMBuilder {
  let func = manager.global_vars.get(func_def.fname.to_string()).unwrap()
  guard let @llvm.Value::Function(func) = func
  let builder = @llvm.IRBuilder::new(func)
  LLVMBuilder::{
    manager,
    func_name: func_def.fname.to_string(),
    params: func_def.params,
    ret_type: func_def.ret_type,
    body: func_def.body,
    free_vars: func_def.free_vars,
    var_table: Map::new(),
    closure_table: Map::new(),
    labels: 0,
    builder,
    function: func
  }
}

pub fn LLVMBuilder::new_main(
  manager: LLVMCodeGen,
  body: ClosureIR,
) -> LLVMBuilder {
  let func = manager.global_vars.get("main").unwrap()
  guard let @llvm.Value::Function(func) = func
  let builder = @llvm.IRBuilder::new(func)
  LLVMBuilder::{
    manager,
    func_name: "main",
    params: Array::new(),
    ret_type: LowType::Unit,
    body,
    free_vars: Array::new(),
    var_table: Map::new(),
    closure_table: Map::new(),
    labels: 0,
    builder,
    function: func
  }
}

pub fn add_var[T: @llvm.IsValue](self: LLVMBuilder, name: Name, ty: LowType, value: T) -> Unit {
  self.var_table.set(name.to_string(), value.as_value())
  match ty {
    LowType::ClosureFn(arg_ty, ret_ty) => {
      let arg_types = arg_ty.map(fn (ty) {ty.to_llvm_type()})
      let ret_type  = ret_ty.to_llvm_type()
      let fty = @llvm.Type::FunctionTy(arg_types, ret_type)
      self.closure_table.set(name.to_string(), fty)
    }
    _ => ()
  }
}

pub fn run(self: LLVMBuilder) -> Unit {
  let mut argno = 0
  for pair in self.params {
    let (name, ty) = pair
    let arg = self.function.get_argument(argno)
    self.add_var(name, ty, arg)
    // self.var_table.set(name.to_string(), arg.as_value())
    argno += 1
  }
  let entry_bb = @llvm.BasicBlock::create(name="entry", parent = self.function)
  self.builder.setEntry(entry_bb)
  self.builder.setInsertPoint(entry_bb)

  // add all global variables into var_table
  for tup in self.manager.global_vars {
    let (name, value) = tup
    self.var_table.set(name.to_string(), value)
  }

  // load all free variables
  if self.free_vars.length() > 0 {
    // first, get closure struct type
    let tuple_type : Array[@llvm.Type] = Array::new()
    tuple_type.push(@llvm.Type::getPointerTy()) // closure function pointer
    for free_var in self.free_vars {
      let (_, var_ty) = free_var
      tuple_type.push(var_ty.to_llvm_type())
    }
    let tuple_type = @llvm.Type::getTupleTy(tuple_type, module=Some(self.manager.module))
    let last_arg = self.function.get_argument(argno - 1)
    for ind, free_var in self.free_vars {
      let (var_name, ty) = free_var
      let llvm_ty = ty.to_llvm_type()
      let this_ptr = self.builder.createGEP(tuple_type, last_arg, [0, ind + 1])
      let var_value = self.builder.createLoad(llvm_ty, this_ptr)
      self.add_var(var_name, ty, var_value)
    }
  }

  // emit body
  if self.func_name == "main" {
    let _ = self.emit(self.body)
    let zeroi32 = @llvm.Constant::getInt32(0)
    let _ = self.builder.createRet(zeroi32)
  } else {
    let (v, _) = self.emit(self.body)
    match v {
      Some(v) => {
        let _ = self.builder.createRet(v)
        guard let @llvm.Type::FunctionTy(arg_tys, ret_type) = self.function.func_type
        if ret_type != v.get_type() {
          let new_func_type = @llvm.Type::FunctionTy(arg_tys, v.get_type())
          self.function.type_convert(new_func_type)
        }
      }
      _ => {
        let _ = self.builder.createRetVoid()
      }
    }
  }
}

/// ------------------------------------------------------------
/// ## LowType To LLVM Type
/// ------------------------------------------------------------
fn to_llvm_type(self: LowType) -> @llvm.Type {
  match self {
    Unit => @llvm.Type::VoidTy
    Int => @llvm.Type::Int32Ty
    Double => @llvm.Type::DoubleTy
    Float => @llvm.Type::FloatTy
    Boolean => @llvm.Type::BoolTy
    Ptr => @llvm.Type::getPointerTy()
    Array(ty) => @llvm.Type::getPointerTy(ty=ty.to_llvm_type())
    Tuple(arr) => {
      let tupTy = @llvm.Type::getTupleTy(arr.map(fn (ty) {ty.to_llvm_type()}))
      @llvm.Type::getPointerTy(ty=tupTy)
    }
    DirectFn(arg_ty, ret_ty) => {
      let arg_types = arg_ty.map(fn (ty) {ty.to_llvm_type()})
      let ret_type  = ret_ty.to_llvm_type()
      @llvm.Type::FunctionTy(arg_types, ret_type)
    }
    // for closure function, it type is actually a struct
    // but only closureFn cannot determin which closure it is
    // so here just return a void pointer type
    ClosureFn(_) => @llvm.Type::getPointerTy(ty=@llvm.Type::getPointerTy())
    _ => {
      println("Unimplemented lowtype to llvm type" + self.to_string())
      panic()
    }
  }
} 

pub fn malloc(self: LLVMBuilder, size: Int) -> @llvm.Value {
  let malloc_func = self.manager.find_function("malloc").unwrap()
  let malloc_ty = malloc_func.get_type()
  let arg_list = [@llvm.Constant::getInt32(size).as_value()]
  let ptr = self.builder.createCall(malloc_ty, malloc_func, arg_list)
  ptr
}

pub fn malloc_by_value(self: LLVMBuilder, value: @llvm.Value) -> @llvm.Value {
  let malloc_func = self.manager.find_function("malloc").unwrap()
  let malloc_ty = malloc_func.get_type()
  let ptr = self.builder.createCall(malloc_ty, malloc_func, [value])
  ptr
}

// 1. __builtin_array_make(n, ele)
//    first check ele ty is floating, or int, or Pointer
//    if it is int, call function `create_array(int32_t n_element, int32_t init)`
//    if it is floating, call function `create_float_array(double n_element, double init)`
//    if it is pointer, call function `create_ptr_array(int32_t n_element, void* init)`
pub fn call_builtin_func(
  self: LLVMBuilder, fname: String, arg_list: Array[Name]
) -> (@llvm.Value?, @llvm.BasicBlock) {
  let fty = self.manager.find_builtin_func(fname).unwrap()
  if not(fname.starts_with("__builtin_array_make")) {
    println("Unimplemented builtin function: " + fname)
    panic()
  }
  guard let @llvm.Type::FunctionTy(arg_tys, _) = fty
  let arg_list = arg_list.map(fn (name) {
    self.var_table.get(name.to_string()).unwrap()
  })
  let ele_ty = arg_tys[1]
  let ptr = match ele_ty {
    @llvm.Type::Int32Ty => {
      let libfunc = self.manager.find_function("create_array").unwrap()
      let libfunc_ty = libfunc.get_type()
      let ptr = self.builder.createCall(libfunc_ty, libfunc, arg_list)
      ptr
    }
    @llvm.Type::DoubleTy => {
      let libfunc = self.manager.find_function("create_float_array").unwrap()
      let libfunc_ty = libfunc.get_type()
      let ptr = self.builder.createCall(libfunc_ty, libfunc, arg_list)
      ptr
    }
    @llvm.Type::PointerTy(_) => {
      let libfunc = self.manager.find_function("create_ptr_array").unwrap()
      let libfunc_ty = libfunc.get_type()
      let ptr = self.builder.createCall(libfunc_ty, libfunc, arg_list)
      ptr
    }
    _ => {
      println("Array::make, the init element is not Int, Double or pointer-like")
      println("Type: " + ele_ty.to_string())
      panic()
    }
  }
  (Some(ptr), self.builder.getInsertBlock())
}

pub fn emit(
  self: LLVMBuilder,
  ir: ClosureIR
) -> (@llvm.Value?, @llvm.BasicBlock) {
  let zeroi32 = @llvm.Constant::getInt32(0).as_value()
  match ir {
    Let((name, ty), sub_ir, next) => {
      // if sub_ir is ArrayGet(ptr, n)
      // generatr getelementptr, and load
      match sub_ir {
        ArrayGet(ptr, n) => {
          let valTy = ty.to_llvm_type()
          let ptr_val = self.var_table.get(ptr.to_string()).unwrap()
          let ptr_ty = @llvm.Type::getPointerTy(ty=valTy)
          ptr_val.type_convert(ptr_ty)
          let idx_val = self.var_table.get(n.to_string()).unwrap()
          let gep = self.builder.createGEP(valTy, ptr_val, [idx_val])
          let load = self.builder.createLoad(valTy, gep)
          self.add_var(name, ty, load)
          self.emit(next)
        }
        _ => {
          let (value, _) = self.emit(sub_ir)
          match value {
            Some(value) => self.add_var(name, ty, value)
            None => ()
          }
          self.emit(next)
        }
      }
    }
    LetTuple(name_tys, name, next) => {
      let ptr_val = self.var_table.get(name.to_string()).unwrap()
      let ptr_ty = ptr_val.get_type()
      guard let @llvm.Type::PointerTy(tuple_type) = ptr_ty
      guard let @llvm.Type::StructTy(tuple_tys) = tuple_type
      for i, name_ty in name_tys {
        let (name, ty) = name_ty
        if name.to_string() != "_" {
          let dataTy = tuple_tys.fields[i]
          let gep = self.builder.createGEP(tuple_type, ptr_val, [0, i])
          let load = self.builder.createLoad(dataTy, gep)
          self.add_var(name, ty, load)
        }
      }
      self.emit(next)
    }
    Int(i) => (
      Some(@llvm.Constant::getInt32(i).as_value()),
      self.builder.getInsertBlock()
    )
    Double(d) => (
      Some(@llvm.Constant::getDouble(d).as_value()),
      self.builder.getInsertBlock()
    )
    Var(name) => (
      Some(self.var_table.get(name.to_string()).unwrap()),
      self.builder.getInsertBlock()
    )
    Not(name) => {
      let v = self.var_table.get(name.to_string()).unwrap()
      let r = self.builder.createXor(v, @llvm.Constant::getInt32(-2147483648).as_value())
      (Some(r), self.builder.getInsertBlock())
    }
    And(lhs, rhs) => {
      let v = self.var_table.get(lhs.to_string()).unwrap()
      let r = self.builder.createAnd(v, self.var_table.get(rhs.to_string()).unwrap())
      (Some(r), self.builder.getInsertBlock())
    }
    Or(lhs, rhs) => {
      let v = self.var_table.get(lhs.to_string()).unwrap()
      let r = self.builder.createOr(v, self.var_table.get(rhs.to_string()).unwrap())
      (Some(r), self.builder.getInsertBlock())
    }
    Neg(name) => {
      let v = self.var_table.get(name.to_string()).unwrap()
      let r = self.builder.createSub(zeroi32, v) 
      (Some(r), self.builder.getInsertBlock())
    }
    Add(lhs, rhs) => {
      let v = self.builder.createAdd(
        self.var_table.get(lhs.to_string()).unwrap(),
        self.var_table.get(rhs.to_string()).unwrap()
      )
      (Some(v), self.builder.getInsertBlock())
    }
    Sub(lhs, rhs) => {
      let v = self.builder.createSub(
        self.var_table.get(lhs.to_string()).unwrap(),
        self.var_table.get(rhs.to_string()).unwrap()
      )
      (Some(v), self.builder.getInsertBlock())
    }
    Mul(lhs, rhs) => {
      let v = self.builder.createMul(
        self.var_table.get(lhs.to_string()).unwrap(),
        self.var_table.get(rhs.to_string()).unwrap()
      )
      (Some(v), self.builder.getInsertBlock())
    }
    Div(lhs, rhs) => {
      let v = self.builder.createDiv(
        self.var_table.get(lhs.to_string()).unwrap(),
        self.var_table.get(rhs.to_string()).unwrap()
      )
      (Some(v), self.builder.getInsertBlock())
    }
    Eq(lhs, rhs) => {
      let lhs = self.var_table.get(lhs.to_string()).unwrap()
      let rhs = self.var_table.get(rhs.to_string()).unwrap()
      let cmp = match lhs.get_type().is_floating() {
        true => self.builder.createFCmpOEQ(lhs, rhs)
        false => self.builder.createICmpEQ(lhs, rhs)
      }
      (Some(cmp), self.builder.getInsertBlock())
    }
    Ne(lhs, rhs) => {
      let lhs = self.var_table.get(lhs.to_string()).unwrap()
      let rhs = self.var_table.get(rhs.to_string()).unwrap()
      let cmp = match lhs.get_type().is_floating() {
        true => self.builder.createFCmpONE(lhs, rhs)
        false => self.builder.createICmpNE(lhs, rhs)
      }
      (Some(cmp), self.builder.getInsertBlock())
    }
    Lt(lhs, rhs) => {
      let lhs = self.var_table.get(lhs.to_string()).unwrap()
      let rhs = self.var_table.get(rhs.to_string()).unwrap()
      let cmp = match lhs.get_type().is_floating() {
        true => self.builder.createFCmpOLT(lhs, rhs)
        false => self.builder.createICmpSLT(lhs, rhs)
      }
      (Some(cmp), self.builder.getInsertBlock())
    }
    Le(lhs, rhs) => {
      let lhs = self.var_table.get(lhs.to_string()).unwrap()
      let rhs = self.var_table.get(rhs.to_string()).unwrap()
      let cmp = match lhs.get_type().is_floating() {
        true => self.builder.createFCmpOLE(lhs, rhs)
        false => self.builder.createICmpSLE(lhs, rhs)
      }
      (Some(cmp), self.builder.getInsertBlock())
    }
    Gt(lhs, rhs) => {
      let lhs = self.var_table.get(lhs.to_string()).unwrap()
      let rhs = self.var_table.get(rhs.to_string()).unwrap()
      let cmp = match lhs.get_type().is_floating() {
        true => self.builder.createFCmpOGT(lhs, rhs)
        false => self.builder.createICmpSGT(lhs, rhs)
      }
      (Some(cmp), self.builder.getInsertBlock())
    }
    Ge(lhs, rhs) => {
      let lhs = self.var_table.get(lhs.to_string()).unwrap()
      let rhs = self.var_table.get(rhs.to_string()).unwrap()
      let cmp = match lhs.get_type().is_floating() {
        true => self.builder.createFCmpOGE(lhs, rhs)
        false => self.builder.createICmpSGE(lhs, rhs)
      }
      (Some(cmp), self.builder.getInsertBlock())
    }
    Beq(lhs, rhs, then, elec) => {
      let lhs = self.var_table.get(lhs.to_string()).unwrap()
      let rhs = self.var_table.get(rhs.to_string()).unwrap()
      let cond = match lhs.get_type().is_floating() {
        true => self.builder.createFCmpOEQ(lhs, rhs)
        false => self.builder.createICmpEQ(lhs, rhs)
      }
      let then_bb = @llvm.BasicBlock::create(parent = self.function)
      let elec_bb = @llvm.BasicBlock::create(parent = self.function)
      let merge_bb = @llvm.BasicBlock::create(parent = self.function)
      let _ = self.builder.createCondBr(cond, then_bb, elec_bb)
      self.builder.setInsertPoint(then_bb)
      let (then_value, then_value_bb) = self.emit(then)
      let _ = self.builder.createBr(merge_bb)
      self.builder.setInsertPoint(elec_bb)
      let (elec_value, elec_value_bb) = self.emit(elec)
      let _ = self.builder.createBr(merge_bb)
      self.builder.setInsertPoint(merge_bb)
      match (then_value, elec_value) {
        (Some(t), Some(e)) => {
          let phi = self.builder.createPHI(t.get_type(), 2)
          phi.addIncoming(t, then_value_bb)
          phi.addIncoming(e, elec_value_bb)
          (Some(phi.as_value()), self.builder.getInsertBlock())
        }
        (None, None) => {
          (None, self.builder.getInsertBlock())
        }
        _ => {
          println("Error branch!")
          panic()
        }
      }
    }
    Bne(lhs, rhs, then, elec) => {
      let lhs = self.var_table.get(lhs.to_string()).unwrap()
      let rhs = self.var_table.get(rhs.to_string()).unwrap()
      let cond = match lhs.get_type().is_floating() {
        true => self.builder.createFCmpONE(lhs, rhs)
        false => self.builder.createICmpNE(lhs, rhs)
      }
      let then_bb = @llvm.BasicBlock::create(parent = self.function)
      let elec_bb = @llvm.BasicBlock::create(parent = self.function)
      let merge_bb = @llvm.BasicBlock::create(parent = self.function)
      let _ = self.builder.createCondBr(cond, then_bb, elec_bb)
      self.builder.setInsertPoint(then_bb)
      let (then_value, then_value_bb) = self.emit(then)
      let _ = self.builder.createBr(merge_bb)
      self.builder.setInsertPoint(elec_bb)
      let (elec_value, elec_value_bb) = self.emit(elec)
      let _ = self.builder.createBr(merge_bb)
      self.builder.setInsertPoint(merge_bb)
      match (then_value, elec_value) {
        (Some(t), Some(e)) => {
          let phi = self.builder.createPHI(t.get_type(), 2)
          phi.addIncoming(t, then_value_bb)
          phi.addIncoming(e, elec_value_bb)
          (Some(phi.as_value()), self.builder.getInsertBlock())
        }
        (None, None) => {
          (None, self.builder.getInsertBlock())
        }
        _ => {
          println("Error branch!")
          panic()
        }
      }
    }
    Bgt(lhs, rhs, then, elec) => {
      let lhs = self.var_table.get(lhs.to_string()).unwrap()
      let rhs = self.var_table.get(rhs.to_string()).unwrap()
      let cond = match lhs.get_type().is_floating() {
        true => self.builder.createFCmpOGT(lhs, rhs)
        false => self.builder.createICmpSGT(lhs, rhs)
      }
      let then_bb = @llvm.BasicBlock::create(parent = self.function)
      let elec_bb = @llvm.BasicBlock::create(parent = self.function)
      let merge_bb = @llvm.BasicBlock::create(parent = self.function)
      let _ = self.builder.createCondBr(cond, then_bb, elec_bb)
      self.builder.setInsertPoint(then_bb)
      let (then_value, then_value_bb) = self.emit(then)
      let _ = self.builder.createBr(merge_bb)
      self.builder.setInsertPoint(elec_bb)
      let (elec_value, elec_value_bb) = self.emit(elec)
      let _ = self.builder.createBr(merge_bb)
      self.builder.setInsertPoint(merge_bb)
      match (then_value, elec_value) {
        (Some(t), Some(e)) => {
          let phi = self.builder.createPHI(t.get_type(), 2)
          phi.addIncoming(t, then_value_bb)
          phi.addIncoming(e, elec_value_bb)
          (Some(phi.as_value()), self.builder.getInsertBlock())
        }
        (None, None) => {
          (None, self.builder.getInsertBlock())
        }
        _ => {
          println("Error branch!")
          panic()
        }
      }
    }
    Bge(lhs, rhs, then, elec) => {
      let lhs = self.var_table.get(lhs.to_string()).unwrap()
      let rhs = self.var_table.get(rhs.to_string()).unwrap()
      let cond = match lhs.get_type().is_floating() {
        true => self.builder.createFCmpOGE(lhs, rhs)
        false => self.builder.createICmpSGE(lhs, rhs)
      }
      let then_bb = @llvm.BasicBlock::create(parent = self.function)
      let elec_bb = @llvm.BasicBlock::create(parent = self.function)
      let merge_bb = @llvm.BasicBlock::create(parent = self.function)
      let _ = self.builder.createCondBr(cond, then_bb, elec_bb)
      self.builder.setInsertPoint(then_bb)
      let (then_value, then_value_bb) = self.emit(then)
      let _ = self.builder.createBr(merge_bb)
      self.builder.setInsertPoint(elec_bb)
      let (elec_value, elec_value_bb) = self.emit(elec)
      let _ = self.builder.createBr(merge_bb)
      self.builder.setInsertPoint(merge_bb)
      match (then_value, elec_value) {
        (Some(t), Some(e)) => {
          let phi = self.builder.createPHI(t.get_type(), 2)
          phi.addIncoming(t, then_value_bb)
          phi.addIncoming(e, elec_value_bb)
          (Some(phi.as_value()), self.builder.getInsertBlock())
        }
        (None, None) => {
          (None, self.builder.getInsertBlock())
        }
        _ => {
          println("Error branch!")
          panic()
        }
      }
    }
    Blt(lhs, rhs, then, elec) => {
      let lhs = self.var_table.get(lhs.to_string()).unwrap()
      let rhs = self.var_table.get(rhs.to_string()).unwrap()
      let cond = match lhs.get_type().is_floating() {
        true => self.builder.createFCmpOLT(lhs, rhs)
        false => self.builder.createICmpSLT(lhs, rhs)
      }
      let then_bb = @llvm.BasicBlock::create(parent = self.function)
      let elec_bb = @llvm.BasicBlock::create(parent = self.function)
      let merge_bb = @llvm.BasicBlock::create(parent = self.function)
      let _ = self.builder.createCondBr(cond, then_bb, elec_bb)
      self.builder.setInsertPoint(then_bb)
      let (then_value, then_value_bb) = self.emit(then)
      let _ = self.builder.createBr(merge_bb)
      self.builder.setInsertPoint(elec_bb)
      let (elec_value, elec_value_bb) = self.emit(elec)
      let _ = self.builder.createBr(merge_bb)
      self.builder.setInsertPoint(merge_bb)
      match (then_value, elec_value) {
        (Some(t), Some(e)) => {
          let phi = self.builder.createPHI(t.get_type(), 2)
          phi.addIncoming(t, then_value_bb)
          phi.addIncoming(e, elec_value_bb)
          (Some(phi.as_value()), self.builder.getInsertBlock())
        }
        (None, None) => {
          (None, self.builder.getInsertBlock())
        }
        _ => {
          println("Error branch!")
          panic()
        }
      }
    }
    Ble(lhs, rhs, then, elec) => {
      let lhs = self.var_table.get(lhs.to_string()).unwrap()
      let rhs = self.var_table.get(rhs.to_string()).unwrap()
      let cond = match lhs.get_type().is_floating() {
        true => self.builder.createFCmpOLE(lhs, rhs)
        false => self.builder.createICmpSLE(lhs, rhs)
      }
      let then_bb = @llvm.BasicBlock::create(parent = self.function)
      let elec_bb = @llvm.BasicBlock::create(parent = self.function)
      let merge_bb = @llvm.BasicBlock::create(parent = self.function)
      let _ = self.builder.createCondBr(cond, then_bb, elec_bb)
      self.builder.setInsertPoint(then_bb)
      let (then_value, then_value_bb) = self.emit(then)
      let _ = self.builder.createBr(merge_bb)
      self.builder.setInsertPoint(elec_bb)
      let (elec_value, elec_value_bb) = self.emit(elec)
      let _ = self.builder.createBr(merge_bb)
      self.builder.setInsertPoint(merge_bb)
      match (then_value, elec_value) {
        (Some(t), Some(e)) => {
          let phi = self.builder.createPHI(t.get_type(), 2)
          phi.addIncoming(t, then_value_bb)
          phi.addIncoming(e, elec_value_bb)
          (Some(phi.as_value()), self.builder.getInsertBlock())
        }
        (None, None) => {
          (None, self.builder.getInsertBlock())
        }
        _ => {
          println("Error branch!")
          panic()
        }
      }
    }
    Br(name, then, elec) => {
      let cond = self.var_table.get(name.to_string()).unwrap()
      let then_bb = @llvm.BasicBlock::create(parent = self.function)
      let elec_bb = @llvm.BasicBlock::create(parent = self.function)
      let merge_bb = @llvm.BasicBlock::create(parent = self.function)
      let _ = self.builder.createCondBr(cond, then_bb, elec_bb)
      self.builder.setInsertPoint(then_bb)
      let (then_value, then_value_bb) = self.emit(then)
      let _ = self.builder.createBr(merge_bb)
      self.builder.setInsertPoint(elec_bb)
      let (elec_value, elec_value_bb) = self.emit(elec)
      let _ = self.builder.createBr(merge_bb)
      self.builder.setInsertPoint(merge_bb)
      match (then_value, elec_value) {
        (Some(t), Some(e)) => {
          let phi = self.builder.createPHI(t.get_type(), 2)
          phi.addIncoming(t, then_value_bb)
          phi.addIncoming(e, elec_value_bb)
          (Some(phi.as_value()), self.builder.getInsertBlock())
        }
        (None, None) => {
          (None, self.builder.getInsertBlock())
        }
        _ => {
          println("Error branch!")
          panic()
        }
      }
    }
    MakeTuple(name_list) => {
      let name_types = name_list.map(fn (name) {
        self.var_table.get(name.to_string()).unwrap().get_type()
      })
      let tuple_ty = @llvm.Type::getTupleTy(name_types, module=Some(self.manager.module))
      let tuple_size = @llvm.sizeof(tuple_ty)
      let malloc_ptr = self.malloc(tuple_size)
      for i, name in name_list {
        let gep = self.builder.createGEP(tuple_ty, malloc_ptr, [0, i])
        let value = self.var_table.get(name.to_string()).unwrap()
        let _ = self.builder.createStore(value, gep)
      }
      (Some(malloc_ptr), self.builder.getInsertBlock())
    }
    MakeClosure((name, closure_ty), closure_def, next) => {
      let tuple_type : Array[@llvm.Type] = Array::new()
      tuple_type.push(@llvm.Type::getPointerTy())
      for free_var in closure_def.free_vars {
        let (_, var_ty) = free_var
        let ty = var_ty.to_llvm_type()
        tuple_type.push(ty)
      }
      let tuple_type = @llvm.Type::getTupleTy(tuple_type, module=Some(self.manager.module))
      let tuple_size = @llvm.sizeof(tuple_type)
      let ptr = self.malloc(tuple_size)
      ptr.type_convert(@llvm.Type::getPointerTy(ty=tuple_type))
      let func = self.manager.find_function(closure_def.fname.to_string()).unwrap()
      let gep0 = self.builder.createGEP(tuple_type, ptr, [0, 0])
      let _ = self.builder.createStore(func.as_value(), gep0)
      for i, free_var in closure_def.free_vars {
        let (var_name, _) = free_var
        let value = self.var_table.get(var_name.to_string()).unwrap()
        let gep = self.builder.createGEP(tuple_type, ptr, [0, i + 1])
        let _ = self.builder.createStore(value, gep)
      }
      self.add_var(name, closure_ty, ptr)
      self.emit(next)
    }
    CallDirect(n, name_list) => {
      let fname = n.to_string()
      match self.manager.find_builtin_func(fname) {
        Some(_) => return call_builtin_func(self, fname, name_list)
        None => ()
      }
      match self.manager.find_function(fname) {
        Some(func) => {
          let func_ty = func.get_type()
          let arg_list = name_list.map(fn (name) {
            self.var_table.get(name.to_string()).unwrap()
          })
          let c = self.builder.createCall(func_ty, func, arg_list)
          match c.get_type() {
            @llvm.Type::VoidTy => (None, self.builder.getInsertBlock())
            _ => (Some(c), self.builder.getInsertBlock())
          }
        }
        None => {
          println("didn't find function: " + n.to_string())
          panic()
        }
      }
    }
    CallClosure(n, name_list) => {
      let closure_name = n.to_string()
      // for recursive closure function, cannot find in var_table
      match self.manager.find_function(closure_name) {
        Some(func) => {
          let arg_list = name_list.map(fn (name) {
            self.var_table.get(name.to_string()).unwrap()
          })
          let last_func_arg = self.function.get_argument(
            func.args.length() - 1
          )
          arg_list.push(last_func_arg.as_value())
          let func_ty = func.get_type()
          let c = self.builder.createCall(func_ty, func, arg_list)
          match c.get_type() {
            @llvm.Type::VoidTy => (None, self.builder.getInsertBlock())
            _ => (Some(c), self.builder.getInsertBlock())
          }
        }
        None => {
          let closure_ptr = self.var_table.get(closure_name).unwrap()
          let closure_ty = self.closure_table.get(closure_name).unwrap()
          let ptr_type = closure_ptr.get_type()
          match ptr_type {
            // if it is PointerTy, there are two possibilities
            // 1. it is a function pointer, which is ptr(void)
            //    just createCall
            // 2. it point to a closure struct, need to insert GEP
            //   to get the function pointer
            @llvm.Type::PointerTy(@llvm.Type::VoidTy) => {
              let arg_list = name_list.map(fn (name) {
                self.var_table.get(name.to_string()).unwrap()
              })
              arg_list.push(closure_ptr)
              let c = self.builder.createCall(closure_ty, closure_ptr, arg_list)
              match c.get_type() {
                @llvm.Type::VoidTy => (None, self.builder.getInsertBlock())
                _ => (Some(c), self.builder.getInsertBlock())
              }
            }
            @llvm.Type::PointerTy(_) => {
              let func_addr = self.builder.createGEP(
                @llvm.Type::getPointerTy(), closure_ptr, [0]
              )
              let func_ptr = self.builder.createLoad(@llvm.Type::getPointerTy(), func_addr)
              let arg_list = name_list.map(fn (name) {
                self.var_table.get(name.to_string()).unwrap()
              })
              arg_list.push(closure_ptr)
              let c = self.builder.createCall(closure_ty, func_ptr, arg_list)
              match c.get_type() {
                @llvm.Type::VoidTy => (None, self.builder.getInsertBlock())
                _ => (Some(c), self.builder.getInsertBlock())
              }
            }
            @llvm.Type::FunctionTy(_) => {
              let arg_list = name_list.map(fn (name) {
                self.var_table.get(name.to_string()).unwrap()
              })
              let c = self.builder.createCall(closure_ty, closure_ptr, arg_list)
              match c.get_type() {
                @llvm.Type::VoidTy => (None, self.builder.getInsertBlock())
                _ => (Some(c), self.builder.getInsertBlock())
              }
            }
            _ => {
              println("Closure type is not a pointer, or function type")
              panic()
            }
          }
        }
      }
    }
    // ArrayGet(arr, idx) => {
    //   let arr_ptr = self.var_table.get(arr.to_string()).unwrap()
    //   let idx = self.var_table.get(idx.to_string()).unwrap()
    //   let arr_ty = arr_ptr.get_type()
    //   guard let @llvm.Type::PointerTy(arr_ty) = arr_ty else {
    //     _ => {
    //       println("ArrayGet: arr_ty is not a pointer type")
    //       panic()
    //     }
    //   }
    //   let gep = self.builder.createGEP(arr_ty, arr_ptr, [idx])
    //   let value = self.builder.createLoad(gep.get_element_type(), gep)
    //   (Some(value.as_value()), self.builder.getInsertBlock())
    // }
    ArrayPut(arr, idx, value) => {
      let arr_ptr = self.var_table.get(arr.to_string()).unwrap()
      let idx = self.var_table.get(idx.to_string()).unwrap()
      let value = self.var_table.get(value.to_string()).unwrap()
      let valTy = value.get_type()
      guard let @llvm.Type::PointerTy(_) = arr_ptr.get_type() else {
        _ => {
          println("ArrayPut: arr_ty is not a pointer type")
          panic()
        }
      }
      let arr_ty = @llvm.Type::getPointerTy(ty=valTy)
      arr_ptr.type_convert(arr_ty)
      let gep = self.builder.createGEP(valTy, arr_ptr, [idx])
      let _ = self.builder.createStore(value, gep)
      (None, self.builder.getInsertBlock())
    }
    Unit => {
      (None, self.builder.getInsertBlock())
    }
    _ => {
      println("Unimplemented IR: " + ir.stringify(0))
      panic()
    }
  }
}
