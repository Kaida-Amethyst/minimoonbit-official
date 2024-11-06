pub enum Type {
  Int8Ty
  Int16Ty
  Int32Ty
  Int64Ty
  UInt8Ty
  UInt16Ty
  UInt32Ty
  UInt64Ty
  FloatTy
  DoubleTy // 64-bit floating point
  BoolTy
  VoidTy
  PointerTy(Type)
  ArrayTy(Type, Int) // Fixed size array
  StructTy(StructType) // Struct type
  FunctionTy(Array[Type], Type) // Function type
} derive(Eq)

pub fn output(self : Type, logger : Logger) -> Unit {
  logger.write_string(self.to_string())
}

pub fn to_string(self : Type) -> String {
  match self {
    Type::Int8Ty => "i8"
    Type::Int16Ty => "i16"
    Type::Int32Ty => "i32"
    Type::Int64Ty => "i64"
    Type::UInt8Ty => "u8"
    Type::UInt16Ty => "u16"
    Type::UInt32Ty => "u32"
    Type::UInt64Ty => "u64"
    Type::FloatTy => "float"
    Type::DoubleTy => "double"
    Type::BoolTy => "i1"
    Type::PointerTy(_) => "ptr"
    // Type::PointerTy(p) => "ptr(\{p})"
    Type::FunctionTy(_) => "ptr"
    // Type::FunctionTy(_) => "ptr"
    Type::VoidTy => "void"
    Type::ArrayTy(element_type, size) => {
      "[\{size} x \{element_type}]" 
    }
    Type::StructTy(struc) => "%struct.\{struc.name}"
    _ => ""
  }
}

pub fn Type::getInt32Ty() -> Type {
  Type::Int32Ty
}

pub fn Type::getInt64Ty() -> Type {
  Type::Int64Ty
}

pub fn Type::getUInt32Ty() -> Type {
  Type::UInt32Ty
}

pub fn Type::getUInt64Ty() -> Type {
  Type::UInt64Ty
}

pub fn Type::getDoubleTy() -> Type {
  Type::DoubleTy
}

pub fn Type::getBoolTy() -> Type {
  Type::BoolTy
}

pub fn Type::getPointerTy(~ty: Type = Type::VoidTy) -> Type {
  Type::PointerTy(ty)
}

pub fn Type::getFunctionTy(
  ret_type: Type,
  arg_types : Array[Type]
) -> Type {
  Type::FunctionTy(arg_types, ret_type)
}

pub fn Type::getArrayTy(element_type : Type, size : Int) -> Type {
  Type::ArrayTy(element_type, size)
}

pub fn Type::getStructTy(
  struct_name: String, element_types : Array[Type], ~module: Module? = None
) -> Type {
  let struc = StructType::create(struct_name, element_types, ~module)
  Type::StructTy(struc)
}

pub fn Type::getTupleTy(
  fields: Array[Type], ~module: Module? = None
) -> Type {
  let name = "tuple_" + fields.map(fn (f) {f.to_string()}).join("_")
  let struc = StructType::create(name, fields, ~module)
  Type::StructTy(struc)
}

pub fn is_pointer(self : Type) -> Bool {
  match self {
    Type::PointerTy(_) => true
    _ => false
  }
}

pub fn is_integer(self : Type) -> Bool {
  match self {
    Type::Int8Ty => true
    Type::Int16Ty => true
    Type::Int32Ty => true
    Type::Int64Ty => true
    Type::UInt8Ty => true
    Type::UInt16Ty => true
    Type::UInt32Ty => true
    Type::UInt64Ty => true
    _ => false
  }
}

pub fn is_signed(self : Type) -> Bool {
  match self {
    Type::Int8Ty => true
    Type::Int16Ty => true
    Type::Int32Ty => true
    Type::Int64Ty => true
    _ => false
  }
}

pub fn is_unsigned(self : Type) -> Bool {
  match self {
    Type::UInt8Ty => true
    Type::UInt16Ty => true
    Type::UInt32Ty => true
    Type::UInt64Ty => true
    _ => false
  }
}

pub fn is_floating(self : Type) -> Bool {
  match self {
    Type::FloatTy => true
    Type::DoubleTy => true
    _ => false
  }
}

pub fn is_fp(self : Type) -> Bool {
  match self {
    Type::FloatTy => true
    Type::DoubleTy => true
    _ => false
  }
}

pub fn is_bool(self : Type) -> Bool {
  match self {
    Type::BoolTy => true
    _ => false
  }
}

pub fn is_void(self : Type) -> Bool {
  match self {
    Type::VoidTy => true
    _ => false
  }
}

pub fn is_function(self : Type) -> Bool {
  match self {
    Type::FunctionTy(_, _) => true
    _ => false
  }
}

pub fn sizeof(self : Type) -> Int {
  match self {
    Type::Int8Ty => 1
    Type::Int16Ty => 2
    Type::Int32Ty => 8
    Type::Int64Ty => 8
    Type::UInt8Ty => 1
    Type::UInt16Ty => 2
    Type::UInt32Ty => 4
    Type::UInt64Ty => 8
    Type::FloatTy => 4
    Type::DoubleTy => 8
    Type::BoolTy => 1
    Type::PointerTy(_) => 8
    Type::ArrayTy(element_type, size) => element_type.sizeof() * size
    Type::StructTy(struc) => struc.size
    Type::FunctionTy(_, _) => 8
    _ => 0
  }
}

pub fn align_of(self: Type) -> Int {
  match self {
    Type::Int8Ty => 1
    Type::Int16Ty => 2
    Type::Int32Ty => 4
    Type::Int64Ty => 8
    Type::UInt8Ty => 1
    Type::UInt16Ty => 2
    Type::UInt32Ty => 4
    Type::UInt64Ty => 8
    Type::FloatTy => 4
    Type::DoubleTy => 8
    Type::BoolTy => 1
    Type::PointerTy(_) => 8
    Type::ArrayTy(element_type, _) => element_type.align_of()
    Type::StructTy(struc) => struc.align
    Type::FunctionTy(_) => 8
    _ => 0
  }
}
