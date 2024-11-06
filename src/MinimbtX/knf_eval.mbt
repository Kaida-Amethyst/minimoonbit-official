// pub enum KnfEvalValue {
//   Unit
//   Int(Int)
//   Int64(Int64)
//   UInt(UInt)
//   UInt64(UInt64)
//   Double(Double)
//   Boolean(Bool)
//   Tuple(Array[KnfEvalValue])
//   Array(Array[KnfEvalValue])
//   Func(KnfFuncDef, Knf)
// }
//
// pub fn to_string(self: KnfEvalValue) -> String {
//   match self {
//     Unit => "()"
//     Int(i) => i.to_string()
//     Int64(i) => i.to_string() + "L"
//     UInt(u) => u.to_string() + "U"
//     UInt64(u) => u.to_string() + "UL"
//     Double(f) => f.to_string()
//     Boolean(b) => b.to_string()
//     Tuple(values) => "(" + values.map(fn (v){ v.to_string() }).join(", ") + ")"
//     Array(values) => "[" + values.map(fn (v){ v.to_string() }).join(", ") + "]"
//     Func(f, _) => f.name.to_string()
//   }
// }
//
// pub fn neg(self: KnfEvalValue) -> KnfEvalValue {
//   match self {
//     Int(i) => KnfEvalValue::Int(-i)
//     Int64(i) => KnfEvalValue::Int64(-i)
//     Double(f) => KnfEvalValue::Double(-f)
//     _ => {
//       println("Invalid type for neg");
//       panic()
//     }
//   }
// }
//
// pub fn knf_not(self: KnfEvalValue) -> KnfEvalValue {
//   match self {
//     Boolean(true) => KnfEvalValue::Boolean(false)
//     Boolean(false) => KnfEvalValue::Boolean(true)
//     _ => {
//       println("Invalid type for not");
//       panic()
//     }
//   }
// }
//
// pub fn add(self: KnfEvalValue, other: KnfEvalValue) -> KnfEvalValue {
//   match (self, other) {
//     (Int(i1), Int(i2)) => KnfEvalValue::Int(i1 + i2)
//     (Int64(i1), Int64(i2)) => KnfEvalValue::Int64(i1 + i2)
//     (UInt(u1), UInt(u2)) => KnfEvalValue::UInt(u1 + u2)
//     (UInt64(u1), UInt64(u2)) => KnfEvalValue::UInt64(u1 + u2)
//     (Double(f1), Double(f2)) => KnfEvalValue::Double(f1 + f2)
//     _ => {
//       println("Invalid type for add");
//       panic()
//     }
//   }
// }
//
// pub fn sub(self: KnfEvalValue, other: KnfEvalValue) -> KnfEvalValue {
//   match (self, other) {
//     (Int(i1), Int(i2)) => KnfEvalValue::Int(i1 - i2)
//     (Int64(i1), Int64(i2)) => KnfEvalValue::Int64(i1 - i2)
//     (UInt(u1), UInt(u2)) => KnfEvalValue::UInt(u1 - u2)
//     (UInt64(u1), UInt64(u2)) => KnfEvalValue::UInt64(u1 - u2)
//     (Double(f1), Double(f2)) => KnfEvalValue::Double(f1 - f2)
//     _ => {
//       println("Invalid type for sub");
//       panic()
//     }
//   }
// }
//
// pub fn mul(self: KnfEvalValue, other: KnfEvalValue) -> KnfEvalValue {
//   match (self, other) {
//     (Int(i1), Int(i2)) => KnfEvalValue::Int(i1 * i2)
//     (Int64(i1), Int64(i2)) => KnfEvalValue::Int64(i1 * i2)
//     (UInt(u1), UInt(u2)) => KnfEvalValue::UInt(u1 * u2)
//     (UInt64(u1), UInt64(u2)) => KnfEvalValue::UInt64(u1 * u2)
//     (Double(f1), Double(f2)) => KnfEvalValue::Double(f1 * f2)
//     _ => {
//       println("Invalid type for mul");
//       panic()
//     }
//   }
// }
//
// pub fn div(self: KnfEvalValue, other: KnfEvalValue) -> KnfEvalValue {
//   match (self, other) {
//     (Int(i1), Int(i2)) => KnfEvalValue::Int(i1 / i2)
//     (Int64(i1), Int64(i2)) => KnfEvalValue::Int64(i1 / i2)
//     (UInt(u1), UInt(u2)) => KnfEvalValue::UInt(u1 / u2)
//     (UInt64(u1), UInt64(u2)) => KnfEvalValue::UInt64(u1 / u2)
//     (Double(f1), Double(f2)) => KnfEvalValue::Double(f1 / f2)
//     _ => {
//       println("Invalid type for div");
//       panic()
//     }
//   }
// }
//
// pub fn rem(self: KnfEvalValue, other: KnfEvalValue) -> KnfEvalValue {
//   match (self, other) {
//     (Int(i1), Int(i2)) => KnfEvalValue::Int(i1 % i2)
//     (Int64(i1), Int64(i2)) => KnfEvalValue::Int64(i1 % i2)
//     (UInt(u1), UInt(u2)) => KnfEvalValue::UInt(u1 % u2)
//     (UInt64(u1), UInt64(u2)) => KnfEvalValue::UInt64(u1 % u2)
//     _ => {
//       println("Invalid type for rem");
//       panic()
//     }
//   }
// }
//
// pub fn and(self: KnfEvalValue, other: KnfEvalValue) -> KnfEvalValue {
//   match (self, other) {
//     (Boolean(b1), Boolean(b2)) => KnfEvalValue::Boolean(b1 && b2)
//     _ => {
//       println("Invalid type for and");
//       panic()
//     }
//   }
// }
//
// pub fn or(self: KnfEvalValue, other: KnfEvalValue) -> KnfEvalValue {
//   match (self, other) {
//     (Boolean(b1), Boolean(b2)) => KnfEvalValue::Boolean(b1 || b2)
//     _ => {
//       println("Invalid type for or");
//       panic()
//     }
//   }
// }
//
// pub fn eq(self: KnfEvalValue, other: KnfEvalValue) -> KnfEvalValue {
//   match (self, other) {
//     (Int(i1), Int(i2)) => KnfEvalValue::Boolean(i1 == i2)
//     (Int64(i1), Int64(i2)) => KnfEvalValue::Boolean(i1 == i2)
//     (UInt(u1), UInt(u2)) => KnfEvalValue::Boolean(u1 == u2)
//     (UInt64(u1), UInt64(u2)) => KnfEvalValue::Boolean(u1 == u2)
//     (Double(f1), Double(f2)) => KnfEvalValue::Boolean(f1 == f2)
//     (Boolean(b1), Boolean(b2)) => KnfEvalValue::Boolean(b1 == b2)
//     _ => {
//       println("Invalid type for eq");
//       panic()
//     }
//   }
// }
//
// pub fn ne(self: KnfEvalValue, other: KnfEvalValue) -> KnfEvalValue {
//   match (self, other) {
//     (Int(i1), Int(i2)) => KnfEvalValue::Boolean(i1 != i2)
//     (Int64(i1), Int64(i2)) => KnfEvalValue::Boolean(i1 != i2)
//     (UInt(u1), UInt(u2)) => KnfEvalValue::Boolean(u1 != u2)
//     (UInt64(u1), UInt64(u2)) => KnfEvalValue::Boolean(u1 != u2)
//     (Double(f1), Double(f2)) => KnfEvalValue::Boolean(f1 != f2)
//     (Boolean(b1), Boolean(b2)) => KnfEvalValue::Boolean(b1 != b2)
//     _ => {
//       println("Invalid type for ne");
//       panic()
//     }
//   }
// }
//
// pub fn gt(self: KnfEvalValue, other: KnfEvalValue) -> KnfEvalValue {
//   match (self, other) {
//     (Int(i1), Int(i2)) => KnfEvalValue::Boolean(i1 > i2)
//     (Int64(i1), Int64(i2)) => KnfEvalValue::Boolean(i1 > i2)
//     (UInt(u1), UInt(u2)) => KnfEvalValue::Boolean(u1 > u2)
//     (UInt64(u1), UInt64(u2)) => KnfEvalValue::Boolean(u1 > u2)
//     (Double(f1), Double(f2)) => KnfEvalValue::Boolean(f1 > f2)
//     _ => {
//       println("Invalid type for gt");
//       panic()
//     }
//   }
// }
//
// pub fn lt(self: KnfEvalValue, other: KnfEvalValue) -> KnfEvalValue {
//   match (self, other) {
//     (Int(i1), Int(i2)) => KnfEvalValue::Boolean(i1 < i2)
//     (Int64(i1), Int64(i2)) => KnfEvalValue::Boolean(i1 < i2)
//     (UInt(u1), UInt(u2)) => KnfEvalValue::Boolean(u1 < u2)
//     (UInt64(u1), UInt64(u2)) => KnfEvalValue::Boolean(u1 < u2)
//     (Double(f1), Double(f2)) => KnfEvalValue::Boolean(f1 < f2)
//     _ => {
//       println("Invalid type for lt");
//       panic()
//     }
//   }
// }
//
// pub fn ge(self: KnfEvalValue, other: KnfEvalValue) -> KnfEvalValue {
//   match (self, other) {
//     (Int(i1), Int(i2)) => KnfEvalValue::Boolean(i1 >= i2)
//     (Int64(i1), Int64(i2)) => KnfEvalValue::Boolean(i1 >= i2)
//     (UInt(u1), UInt(u2)) => KnfEvalValue::Boolean(u1 >= u2)
//     (UInt64(u1), UInt64(u2)) => KnfEvalValue::Boolean(u1 >= u2)
//     (Double(f1), Double(f2)) => KnfEvalValue::Boolean(f1 >= f2)
//     _ => {
//       println("Invalid type for ge");
//       panic()
//     }
//   }
// }
//
// pub fn le(self: KnfEvalValue, other: KnfEvalValue) -> KnfEvalValue {
//   match (self, other) {
//     (Int(i1), Int(i2)) => KnfEvalValue::Boolean(i1 <= i2)
//     (Int64(i1), Int64(i2)) => KnfEvalValue::Boolean(i1 <= i2)
//     (UInt(u1), UInt(u2)) => KnfEvalValue::Boolean(u1 <= u2)
//     (UInt64(u1), UInt64(u2)) => KnfEvalValue::Boolean(u1 <= u2)
//     (Double(f1), Double(f2)) => KnfEvalValue::Boolean(f1 <= f2)
//     _ => {
//       println("Invalid type for le");
//       panic()
//     }
//   }
// }
//
// pub struct KnfValueEnv {
//   env: Map[Name, KnfEvalValue]
// }
//
// pub fn KnfValueEnv::default() -> KnfValueEnv {
//   KnfValueEnv::{
//     env: Map::new()
//   }
// }
//
// pub fn get(self: KnfValueEnv, name: Name) -> KnfEvalValue {
//   match self.env.get(name) {
//     Some(value) => value
//     None => {
//       println("Not found name: " + name.to_string());
//       panic()
//     }
//   }
// }
//
// pub fn set(self: KnfValueEnv, name: Name, value: KnfEvalValue) -> Unit {
//   self.env.set(name, value);
// }
//
//
// pub fn eval(self: Knf, ~env: KnfValueEnv = KnfValueEnv::default()) -> KnfEvalValue {
//   match self {
//     Unit => KnfEvalValue::Unit
//     Int(i32) => KnfEvalValue::Int(i32)
//     Int64(i64) => KnfEvalValue::Int64(i64)
//     UInt(u32) => KnfEvalValue::UInt(u32)
//     UInt64(u64) => KnfEvalValue::UInt64(u64)
//     Double(f64) => KnfEvalValue::Double(f64)
//     Boolean(b) => KnfEvalValue::Boolean(b)
//     Var(name) => env.get(name)
//     Neg(name) => env.get(name).neg()
//     Not(name) => env.get(name).knf_not()
//     Add(name1, name2) => env.get(name1).add(env.get(name2))
//     Sub(name1, name2) => env.get(name1).sub(env.get(name2))
//     Mul(name1, name2) => env.get(name1).mul(env.get(name2))
//     Div(name1, name2) => env.get(name1).div(env.get(name2))
//     Rem(name1, name2) => env.get(name1).rem(env.get(name2))
//     And(name1, name2) => env.get(name1).and(env.get(name2))
//     Or(name1, name2) => env.get(name1).or(env.get(name2))
//     Eq(name1, name2) => env.get(name1).eq(env.get(name2))
//     Ne(name1, name2) => env.get(name1).ne(env.get(name2))
//     Gt(name1, name2) => env.get(name1).gt(env.get(name2))
//     Lt(name1, name2) => env.get(name1).lt(env.get(name2))
//     Ge(name1, name2) => env.get(name1).ge(env.get(name2))
//     Le(name1, name2) => env.get(name1).le(env.get(name2))
//     Beq(name1, name2, knf1, knf2) => {
//       match env.get(name1).eq(env.get(name2)) {
//         KnfEvalValue::Boolean(true) => eval(knf1, env=env)
//         KnfEvalValue::Boolean(false) => eval(knf2, env=env)
//         _ => {println("Invalid type for beq"); panic()}
//       }
//     }
//     Bne(name1, name2, knf1, knf2) => {
//       match env.get(name1).ne(env.get(name2)) {
//         KnfEvalValue::Boolean(true) => eval(knf1, env=env)
//         KnfEvalValue::Boolean(false) => eval(knf2, env=env)
//         _ => {println("Invalid type for bne"); panic()}
//       }
//     }
//     Bgt(name1, name2, knf1, knf2) => {
//       match env.get(name1).gt(env.get(name2)) {
//         KnfEvalValue::Boolean(true) => eval(knf1, env=env)
//         KnfEvalValue::Boolean(false) => eval(knf2, env=env)
//         _ => {println("Invalid type for bgt"); panic()}
//       }
//     }
//     Blt(name1, name2, knf1, knf2) => {
//       match env.get(name1).lt(env.get(name2)) {
//         KnfEvalValue::Boolean(true) => eval(knf1, env=env)
//         KnfEvalValue::Boolean(false) => eval(knf2, env=env)
//         _ => {println("Invalid type for blt"); panic()}
//       }
//     }
//     Bge(name1, name2, knf1, knf2) => {
//       match env.get(name1).ge(env.get(name2)) {
//         KnfEvalValue::Boolean(true) => eval(knf1, env=env)
//         KnfEvalValue::Boolean(false) => eval(knf2, env=env)
//         _ => {println("Invalid type for bge"); panic()}
//       }
//     }
//     Ble(name1, name2, knf1, knf2) => {
//       match env.get(name1).le(env.get(name2)) {
//         KnfEvalValue::Boolean(true) => eval(knf1, env=env)
//         KnfEvalValue::Boolean(false) => eval(knf2, env=env)
//         _ => {println("Invalid type for ble"); panic()}
//       }
//     }
//     Tuple(names) => KnfEvalValue::Tuple(names.map(fn (name){ env.get(name) }))
//     Array(names) => KnfEvalValue::Array(names.map(fn (name){ env.get(name) }))
//     Let(name, knf) => {
//       let value = knf.eval(env=env);
//       env.set(name, value);
//       KnfEvalValue::Unit
//     }
//     // Assign(lhs, rhs) => {
//     //   env.set(lhs, env.get(rhs));
//     // }
//     GetItem(name, index) => {
//       let index = index.eval(env=env);
//       match (env.get(name), index) {
//         (KnfEvalValue::Array(array), KnfEvalValue::Int(i)) => array[i]
//         (KnfEvalValue::Tuple(tuple), KnfEvalValue::Int(i)) => tuple[i]
//         _ => { println("Invalid type for get_item"); panic() }
//       }
//     }
//     Apply(func_name, args) => {
//       let func = env.get(func_name);
//       match func {
//         KnfEvalValue::Func(func, body) => {
//           let new_env = KnfValueEnv::{ env: Map::new() };
//           for ind, arg in args {
//             new_env.set(func.args[ind], env.get(arg));
//           }
//           if is_builtin_func(func.name) {
//             eval_builtin(func, new_env);
//             return KnfEvalValue::Unit
//           } else {
//             body.eval(env=new_env)
//           }
//         }
//         _ => { println("Invalid type for apply"); panic() }
//       }
//     }
//     LetRec(func_def, body) => {
//       match func_def.name.annote {
//         "main" => {
//           body.eval(env=env);
//         }
//         _ => {
//           env.set(func_def.name, KnfEvalValue::Func(func_def, body));
//           KnfEvalValue::Unit
//         }
//       }
//     }
//     Block(knfs) => {
//       let mut final_value = KnfEvalValue::Unit
//       for knf in knfs {
//         final_value = knf.eval(env=env);
//       }
//       final_value
//     }
//   }
// }
//
// fn is_builtin_func(fname: Name) -> Bool {
//   let builtin_funcs = ["println"];
//   builtin_funcs.contains(fname.annote)
// }
//
// fn eval_builtin(func: KnfFuncDef, env: KnfValueEnv) -> Unit {
//   match func.name.annote {
//     "println" => {
//       let arg = func.args[0];
//       let value = env.get(arg);
//       println(value.to_string());
//     }
//     _ => {
//       println("Invalid builtin func: "+ func.name.annote)
//       panic()
//     }
//   }
// }
