fn align_value_type(lhs : Value, rhs : Value, ~allow_ptr_arith: Bool = false) -> Type {
  let lhs_type = match lhs {
    Value::Constant(c) => c.get_type()
    Value::Argument(a) => a.get_type()
    Value::Instruction(lhs_inst) => lhs_inst.get_type()
    _ => {
      println("Invalid value")
      panic()
    }
  }
  let rhs_type = match rhs {
    Value::Constant(c) => c.get_type()
    Value::Argument(a) => a.get_type()
    Value::Instruction(i) => i.get_type()
    _ => {
      println("Invalid value")
      panic()
    }
  }
  align_type(lhs_type, rhs_type, ~allow_ptr_arith)
}

fn align_type(lhs : Type, rhs : Type, ~allow_ptr_arith: Bool = false) -> Type {
  match (lhs, rhs) {
    (Type::Int8Ty, Type::Int8Ty) => Type::Int8Ty
    (Type::Int16Ty, Type::Int16Ty) => Type::Int16Ty
    (Type::Int32Ty, Type::Int32Ty) => Type::Int32Ty
    (Type::Int64Ty, Type::Int64Ty) => Type::Int64Ty
    (Type::UInt8Ty, Type::UInt8Ty) => Type::UInt8Ty
    (Type::UInt16Ty, Type::UInt16Ty) => Type::UInt16Ty
    (Type::UInt32Ty, Type::UInt32Ty) => Type::UInt32Ty
    (Type::UInt64Ty, Type::UInt64Ty) => Type::UInt64Ty
    (Type::FloatTy, Type::FloatTy) => Type::FloatTy
    (Type::DoubleTy, Type::DoubleTy) => Type::DoubleTy
    (Type::BoolTy, Type::BoolTy) => Type::BoolTy
    (Type::PointerTy(p1), Type::PointerTy(p2)) => {
      Type::PointerTy(align_type(p1, p2))
    }
    (Type::PointerTy(p), Type::Int32Ty(_)) => {
      if allow_ptr_arith {
        Type::PointerTy(p)
      } else {
        println("Type mismatch, align type failed")
        println("ltype is \{lhs}")
        println("rtype is \{rhs}")
        panic()
      }
    }
    (Type::Int32Ty(_), Type::PointerTy(p)) => {
      if allow_ptr_arith {
        Type::PointerTy(p)
      } else {
        println("Type mismatch, align type failed")
        println("ltype is \{lhs}")
        println("rtype is \{rhs}")
        panic()
      }
    }
    _ => {
      println("Type mismatch, align type failed")
      println("ltype is \{lhs}")
      println("rtype is \{rhs}")
      panic()
    }
  }
}

fn str_to_option(s : String) -> String? {
  match s {
    "" => None
    _ => Some(s)
  }
}

fn max_int(a: Int, b : Int) -> Int {
  if a > b {
    a
  } else {
    b
  }
}

