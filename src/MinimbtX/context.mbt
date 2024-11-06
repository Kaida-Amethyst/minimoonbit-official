enum CompilePhase {
  SourceCode
  LexicalAnalysis
  Parsing
  TypeChecking
  KnfConversion
  ClosureConversion
  LLVMCodegen
  AssemblyCodegen
  Done
}

pub struct Context {
  code : String
  mut phase : CompilePhase
  line_start_index : Map[Int, Int]
  line_end_index : Map[Int, Int]
  mut line_count : Int
  tokens : Array[Token]
  // Type Analysis Part
  builtin_types : Map[String, Type]
  function_types : Map[String, Type]
  func_defs : Map[String, FuncDef]
  extern_funcs : Map[String, Type]
  struct_types : Map[String, Type]
  struct_defs : Map[String, StructDef]
  extern_structs : Map[String, Type]
  enum_types : Map[String, Type]
  enum_defs : Map[String, EnumDef]
  extern_enums : Map[String, Type]
  let_exprs : Array[LetExpr]
  // type_defs : Array[TypeDef]
  // trait_defs : Array[TraitDef]
  // impl_defs : Array[ImplDef]
  mut main_func : MainFunc?
  program : Program
  type_constraints : Map[String, Constraint]
  mut knf_context : KnfContext?
  mut closure_context: ClosureContext?
  mut llvm_module: @llvm.Module?
}

pub fn Context::new(code : String) -> Context {
  Context::{
    code,
    phase: CompilePhase::SourceCode,
    line_start_index: { 0: 0 },
    line_end_index: { 0: 0 },
    line_count: 1,
    tokens: [],
    builtin_types: builtin_types(),
    function_types: Map::new(),
    func_defs: Map::new(),
    extern_funcs: extern_funcs(),
    struct_types: Map::new(),
    struct_defs: Map::new(),
    extern_structs: Map::new(),
    enum_types: Map::new(),
    extern_enums: Map::new(),
    enum_defs: Map::new(),
    let_exprs: [],
    main_func: None,
    program: Program::new([]),
    type_constraints: Constraint::builtin_constraints(),
    knf_context: None,
    closure_context: None,
    llvm_module: None,
  }
}

fn builtin_types() -> Map[String, Type] {
  let map = Map::new()
  map.set("Int", Type::Int32)
  map.set("Int64", Type::Int64)
  map.set("UInt", Type::UInt32)
  map.set("UInt64", Type::UInt64)
  map.set("Float", Type::Float32)
  map.set("Double", Type::Double)
  map.set("Bool", Type::Boolean)
  map.set("Unit", Type::Unit)
  map.set("String", Type::String)
  map.set("Char", Type::Char)
  // Builtin Functions
  map.set("not", Type::builtin_not())
  map.set("Array::make", Type::builtin_array_make())
  // Builtin Structs
  map.set("Array", Type::builtin_array_ty())
  map
}

pub fn extern_funcs() -> Map[String, Type] {
  let map = Map::new()
  map.set(
    "print_int",
    Type::create_func_ty(TypeArr::new([Type::Int32]), Type::Unit),
  )
  map.set("print_endline", Type::create_func_ty(TypeArr::new([]), Type::Unit))
  map.set(
    "int_of_float",
    Type::create_func_ty(TypeArr::new([Type::Double]), Type::Int32),
  )
  map.set(
    "truncate",
    Type::create_func_ty(TypeArr::new([Type::Double]), Type::Int32),
  )
  map.set(
    "float_of_int",
    Type::create_func_ty(TypeArr::new([Type::Int32]), Type::Double),
  )
  map.set(
    "sin",
    Type::create_func_ty(TypeArr::new([Type::Double]), Type::Double),
  )
  map.set(
    "cos",
    Type::create_func_ty(TypeArr::new([Type::Double]), Type::Double),
  )
  map.set(
    "sqrt",
    Type::create_func_ty(TypeArr::new([Type::Double]), Type::Double),
  )
  map.set(
    "abs_float",
    Type::create_func_ty(TypeArr::new([Type::Double]), Type::Double),
  )
  // only for debug
  // 1. print_double()
  map.set(
    "print_double",
    Type::create_func_ty(TypeArr::new([Type::Double]), Type::Unit),
  )
  map
}

pub fn push_token(self : Context, tok : Token) -> Unit {
  self.tokens.push(tok)
}

pub fn phase_advance(self : Context) -> Unit {
  self.phase = match self.phase {
    CompilePhase::SourceCode => CompilePhase::LexicalAnalysis
    CompilePhase::LexicalAnalysis => CompilePhase::Parsing
    CompilePhase::Parsing => CompilePhase::TypeChecking
    CompilePhase::TypeChecking => CompilePhase::KnfConversion
    CompilePhase::KnfConversion => CompilePhase::ClosureConversion
    CompilePhase::ClosureConversion => CompilePhase::LLVMCodegen
    CompilePhase::LLVMCodegen => CompilePhase::AssemblyCodegen
    CompilePhase::AssemblyCodegen => CompilePhase::Done
    CompilePhase::Done => CompilePhase::Done
  }
}

// just temporary function, remove it when finish whole project
pub fn set_phase(self : Context, phase : CompilePhase) -> Unit {
  self.phase = phase
}

pub fn get_line(self : Context, line : Int) -> String {
  guard let Some(start) = self.line_start_index[line]
  guard let Some(end) = self.line_end_index[line]
  self.code.substring(~start, ~end)
}

pub fn find_constraint(self : Context, name : String) -> Constraint? {
  self.type_constraints.get(name)
}

pub fn push_func_def(self : Context, func_def : FuncDef) -> Unit!Error {
  let item_key = func_def.name.id
  match self.func_defs.get(item_key) {
    Some(_) => raise RedefineFunctionError(func_def)
    None => self.func_defs.set(item_key, func_def)
  }
}

pub fn push_struct_def(self : Context, struct_def : StructDef) -> Unit!Error {
  let item_key = struct_def.name.id
  match self.struct_defs.get(item_key) {
    Some(_) => raise RedefineStructError(struct_def)
    None => self.struct_defs.set(item_key, struct_def)
  }
}

pub fn push_enum_def(self : Context, enum_def : EnumDef) -> Unit!Error {
  let item_key = enum_def.name.id
  match self.enum_defs.get(item_key) {
    Some(_) => raise RedefineEnumError(enum_def)
    None => self.enum_defs.set(item_key, enum_def)
  }
}

pub fn push_top_decl(self : Context, top_decl : TopDecl) -> Unit!Error {
  match top_decl {
    FuncDef(func_def) => self.push_func_def!(func_def)
    MainFunc(main_func) => self.set_main_func!(main_func)
    StructDef(struct_def) => self.push_struct_def!(struct_def)
    EnumDef(enum_def) => self.push_enum_def!(enum_def)
    _ => ()
  }
  self.program.push(top_decl)
}

pub fn set_main_func(self : Context, main_func : MainFunc) -> Unit!Error {
  match self.main_func {
    Some(_) => raise ExtraMainFunctionError(main_func)
    None => self.main_func = Some(main_func)
  }
}

pub fn push_let_expr(self : Context, let_expr : LetExpr) -> Unit {
  self.let_exprs.push(let_expr)
}

// LexicalAnalysis
pub fn run(self : Lexer) -> Unit!Error {
  match self.context.phase {
    CompilePhase::SourceCode => self.context.phase_advance() // SourceCode -> LexicalAnalysis
    _ => {
      println("LexicalAnalysis phase has been passed")
      panic()
    }
  }
  while self.is_not_end() {
    let _ = self.advance!()

  }
  self.line_feed()
  let _ = self.create_eof()

}

// Parsing
pub fn run(self : Parser) -> Unit!Error {
  match self.context.phase {
    CompilePhase::SourceCode => {
      let lexer = Lexer::new(self.context)
      lexer.run!()
      self.context.phase_advance() // SourceCode -> LexicalAnalysis
    }
    CompilePhase::LexicalAnalysis => self.context.phase_advance() // LexicalAnalysis -> Parsing
    _ => {
      println("Parsing phase has been passed")
      panic()
    }
  }
  let context = self.context
  while not(self.current_is_eof()) {
    let top_decl = self.parse_top_decl!()
    context.push_top_decl!(top_decl)
    while self.current_is_terminator() {
      self.forward()
    }
  }
}

type! TypeCheckError String

pub fn output(self : TypeCheckError, logger : Logger) -> Unit {
  let TypeCheckError(msg) = self
  logger.write_string("TypeCheckFailed: \{msg}")
}

// TypeChecking
pub fn run(self : TypeAnalysis) -> Unit!Error {
  match self.context.phase {
    CompilePhase::SourceCode | CompilePhase::LexicalAnalysis => {
      let parser = Parser::new(self.context)
      parser.run!()
      self.context.phase_advance() // LexicalAnalysis -> Parsing
    }
    CompilePhase::Parsing => self.context.phase_advance() // Desugaring -> TypeChecking
    _ => {
      println("TypeChecking phase has been passed")
      panic()
    }
  }
  for func_name, extfty in self.context.builtin_types {
    self.extern_type_table.set(func_name, extfty)
  }
  for func_name, extfty in self.context.extern_funcs {
    self.extern_type_table.set(func_name, extfty)
  }
  for struct_name, extty in self.context.extern_structs {
    self.extern_type_table.set(struct_name, extty)
  }
  for enum_name, extty in self.context.extern_enums {
    self.extern_type_table.set(enum_name, extty)
  }
  for func_name, func_def in self.context.func_defs {
    let func_ty = self.analyze_func_def_type(func_def)
    self.context.function_types.set(func_name, func_ty)
  }
  for let_expr in self.context.let_exprs {
    let _ = self.analyze_let_expr(let_expr)

  }
  for _, func_def in self.context.func_defs {
    let _ = self.analyze_func_def(func_def)

  }
  match self.context.main_func {
    Some(main_func) => {
      let _ = self.analyze_main_func(main_func)

    }
    None => ()
  }
  for errtok in self.err_tokens {
    errtok.throw_err()
  }
  if self.err_tokens.length() > 0 {
    raise TypeCheckError("")
  }
}

// KnfConversion
pub fn run(self : KnfContext) -> Knf!Error {
  match self.context.phase {
    CompilePhase::SourceCode
    | CompilePhase::LexicalAnalysis | CompilePhase::Parsing => {
      let type_analysis = TypeAnalysis::new(self.context)
      type_analysis.run!()
      self.context.phase_advance() // Parsing -> TypeChecking
    }
    CompilePhase::TypeChecking => self.context.phase_advance() // TypeChecking -> KnfConversion
    _ => {
      println("KnfConversion phase has been passed")
      panic()
    }
  }
  let builder = self.create_builder()
  for _, e in self.context.let_exprs {
    builder.add_knf(e.to_knf(builder))
  }
  for _, fd in self.context.func_defs {
    builder.add_knf(fd.to_knf(builder))
  }
  match self.context.main_func {
    Some(main_func) => self.main_knf = main_func.to_knf(builder)
    None => ()
  }
  let main_name = builder.new_ident_name("main", Type::Unit)
  let main_func_type = FuncTy::main_func_type()
  let main_func_type = Type::FuncTy(main_func_type)
  let main_func_def = KnfFuncDef::new(main_name, [], Type::Unit, main_func_type)
  let main_knf = Knf::LetRec(main_func_def, self.main_knf, Knf::Unit)
  builder.add_knf(main_knf)
  self.context.knf_context = Some(self)
  self.knf = builder.knf
  self.knf
}

// ClosureConversion
pub fn run(self: ClosureContext) -> ClosureProgram!Error {
  match self.context.phase {
    CompilePhase::SourceCode
    | CompilePhase::LexicalAnalysis
    | CompilePhase::Parsing
    | CompilePhase::TypeChecking => {
      let knf_context = KnfContext::new(self.context)
      let _ = knf_context.run!()
      self.context.phase_advance() // TypeChecking -> KnfConversion
    }
    CompilePhase::KnfConversion => self.context.phase_advance()
    _ => {
      println("Closure Conversion phase has been passed")
      panic()
    }
  }
  self.init()
  let prog = self.createProgram()
  prog
}
