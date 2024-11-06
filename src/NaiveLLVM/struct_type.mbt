pub struct StructType {
  name: String
  fields: Array[Type]
  offsets: Array[Int]
  size: Int
  align: Int
}

pub fn StructType::create(name: String, fields: Array[Type], ~module: Module? = None) -> StructType {
  let (size, align, offsets) = count_size_and_align(fields)
  let t = StructType::{
    name,
    fields,
    offsets,
    size,
    align
  }
  match module {
    Some(module) => module.add_struct_type(t)
    None => ()
  }
  t
}

fn StructType::create_tuple(fields: Array[Type], ~module: Module? = None) -> StructType {
  let name = "tuple_" + fields.map(fn (f) {f.to_string()}).join("_")
  StructType::create(name, fields, ~module)
}

fn align_to(offset: Int, align: Int) -> Int {
  (offset + align - 1) / align * align
}

pub fn count_size_and_align(fields: Array[Type]) -> (Int, Int, Array[Int]) {
  let mut offset = 0
  let mut align = 1
  let offsets = Array::new()
  for ty in fields {
    let ty_align = align_of(ty)
    offset = align_to(offset, ty_align)
    offsets.push(offset)
    offset += sizeof(ty)
    if ty_align > align {
      align = ty_align
    }
  }
  let size = align_to(offset, align)
  (size, align, offsets)
}

// llvm
pub fn to_string(self: StructType) -> String {
  // let mut s = "struct ".to_string() + self.name + " {"
  // for field in self.fields {
  //   s += "  " + field.to_string() + ", "
  // }
  // s += "}"
  // s
  let fields = self.fields.map(fn (f) {f.to_string()}).join(", ")
  "%struct.\{self.name} = type { \{fields} }"
}

pub fn op_equal(self: StructType, other: StructType) -> Bool {
  self.name == other.name
}
