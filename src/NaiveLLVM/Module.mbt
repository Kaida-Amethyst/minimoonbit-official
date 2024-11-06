pub struct Module {
  name: String
  functions: Array[Function]
  struct_types: Array[StructType]
}

pub fn Module::create(name: String) -> Module {
  Module::{
    name,
    functions: Array::new(),
    struct_types: Array::new()
  }
}

pub fn add_function(self: Module, function: Function) -> Unit {
  self.functions.push(function)
}

pub fn add_struct_type(self: Module, struct_type: StructType) -> Unit {
  // check if it already exists
  for s in self.struct_types {
    if s.name == struct_type.name {
      return
    }
  }
  self.struct_types.push(struct_type)
}

pub fn getFunction(self: Module, name: String) -> Function? {
  let mut f : Function? = None
  for func in self.functions {
    if func.name == name {
      f = Some(func)
      break
    }
  }
  f
}

pub fn to_string(self: Module) -> String {
  let header = "; ModuleID = '" + self.name + "'\n"
  let struct_defs = self.struct_types.map(fn (s) {s.to_string()}).join("\n")
  let main_content = self.functions.map(fn (f) {f.to_string()}).join("\n")
  header + struct_defs + "\n\n" + main_content
}

pub fn output(self: Module, logger: Logger) -> Unit {
  logger.write_string(self.to_string())
}

pub fn remove_function(self: Module, name: String) -> Unit {
  for i, funcs in self.functions {
    if funcs.name == name {
      let _ = self.functions.remove(i)
      break
    }
  }
}

pub fn replace_function(self: Module, name: String, function: Function) -> Unit {
  for i, funcs in self.functions {
    if funcs.name == name {
      self.functions[i] = function
      break
    }
  }
}
