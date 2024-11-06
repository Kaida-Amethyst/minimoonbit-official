pub enum Constant {
  ConstantInt(Int64)
  ConstantFP(Double)
}

pub fn Constant::getInt32(i: Int) -> Constant {
  ConstantInt(i.to_int64())
}

pub fn Constant::getDouble(d: Double) -> Constant {
  ConstantFP(d)
}

pub fn as_value(self: Constant) -> Value {
  Value::Constant(self)
}

pub fn get_type(self: Constant) -> Type {
  match self {
    ConstantInt(_) => Type::Int32Ty
    ConstantFP(_) => Type::DoubleTy
  }
}

pub fn trivial_stringify(self: Constant) -> String {
  match self {
    ConstantInt(i) => i.to_string()
    ConstantFP(d) => {
      // if s has only integer part, add ".0" to it
      let s = d.to_string()
      let mut has_dot = false
      for c in s.to_array() {
        if c == '.' {
          has_dot = true
          break
        }
      }
      if has_dot {
        s
      } else {
        s + ".0"
      }
    }
  }
}

fn op_equal(self: Constant, other: Constant) -> Bool {
  match (self, other) {
    (ConstantInt(i1), ConstantInt(i2)) => i1 == i2
    (ConstantFP(d1), ConstantFP(d2)) => d1 == d2
    _ => false
  }
}

pub fn to_string(self: Constant) -> String {
  self.trivial_stringify()
}

pub impl IsValue for Int with as_value(self: Int) {
  Constant::getInt32(self).as_value()
}

pub impl IsValue for Double with as_value(self: Double) {
  Constant::getDouble(self).as_value()
}
