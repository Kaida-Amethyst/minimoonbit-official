// pub trait Ast {
//   meta(Self) -> Meta
// }

/// ---------------------------------------------------
/// # Meta Information
/// ---------------------------------------------------
/// Meta information for AST nodes
///   - Location:
///       Store Index of head and last token in context
pub struct Location {
  head_idx : Int
  last_idx : Int
  head_token : Token
  last_token : Token
}

pub fn Location::new(
  head_idx : Int,
  last_idx : Int,
  head_token : Token,
  last_token : Token
) -> Location {
  Location::{ head_idx, last_idx, head_token, last_token }
}

pub struct Meta {
  location : Location
  analysis : Analysis
}

pub fn Meta::new(location : Location) -> Meta {
  Meta::{ location, analysis: Analysis::default() }
}

pub fn get_head_idx(self : Meta) -> Int {
  self.location.head_idx
}

pub fn get_last_idx(self : Meta) -> Int {
  self.location.last_idx
}

pub fn get_head_token(self : Meta) -> Token {
  self.location.head_token
}

pub fn mark_type(self : Meta, ast_type : Type) -> Unit {
  self.analysis.ast_type = ast_type
}

pub fn query_type(self : Meta) -> Type {
  self.analysis.ast_type
}

/// ---------------------------------------------------
/// # Analysis
/// ---------------------------------------------------
/// Analysis information for AST nodes
///   - Type: Node's type
///     Will be `Undefine` under Parsing phase
///   TODO: Use chain, name mangling, etc.
pub struct Analysis {
  mut ast_type : Type
}

pub fn Analysis::default() -> Analysis {
  Analysis::{ ast_type: Type::Undefine }
}

/// ---------------------------------------------------
/// # Program
/// ---------------------------------------------------
pub struct Program {
  top_decls : Array[TopDecl]
}

pub fn Program::new(top_decls : Array[TopDecl]) -> Program {
  Program::{ top_decls, }
}

pub fn push(self : Program, top_decl : TopDecl) -> Unit {
  self.top_decls.push(top_decl)
}

/// ---------------------------------------------------
/// # TopDecl
///
/// ### BNF:
///
/// ```
/// top_decl ::=
///     func_def   | 
///     main_func  |
///     struct_def |
///     enum_def   |   (Unimplemented)
///     type_def   |   (Unimplemented)
///     impl_def   |   (Unimplemented)
///     trait_def  |   (Unimplemented)
///     let_expr       (Unimplemented)
/// ```
/// ---------------------------------------------------
pub enum TopDecl {
  FuncDef(FuncDef)
  MainFunc(MainFunc)
  LetExpr(LetExpr)
  StructDef(StructDef)
  EnumDef(EnumDef)
}

pub fn meta(self : TopDecl) -> Meta {
  match self {
    FuncDef(f) => f.meta()
    MainFunc(m) => m.meta()
    LetExpr(l) => l.meta()
    StructDef(s) => s.meta()
    EnumDef(e) => e.meta()
  }
}

/// ---------------------------------------------------
/// ## Ast Node --- FuncDef
///
/// ### BNF:
///
/// - `func_def ::= 'fn' func_name generic_types? parameter ret_type func_body ;`
/// - `func_name ::= (UpperCase '::')? LowerCase ;`
/// - `generic_types ::= '[' generic_types (',' generic_types)* ']' ;`
/// - `generic_type ::= UpperCase (':' constraint)? ;`
/// - `constraint ::= UpperCase ('+' UpperCase)* ;`
/// ---------------------------------------------------
pub struct FuncDef {
  meta : Meta
  name : LowerCase
  generics : Array[GenericType]
  params : Array[FuncParam]
  ret_type : TypeExpr
  body : BlockExpr
}

pub fn FuncDef::new(
  meta : Meta,
  name : LowerCase,
  generics : Array[GenericType],
  params : Array[FuncParam],
  ret_type : TypeExpr,
  body : BlockExpr
) -> FuncDef {
  FuncDef::{ meta, name, generics, params, ret_type, body }
}

pub fn meta(self : FuncDef) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- Struct Defintion
///
/// ### BNF:
///
/// - `struct_def ::= 'struct' UpperCase generic_types '{' struct_field* '}' ;`
/// - `struct_field ::= LowerCase ':' type_expr ;`
/// - `generic_types ::= '[' generic_type (',' generic_type)* ']' ;`
/// - `generic_type ::= UpperCase (':' constraint)? ;`
/// - `constraint ::= UpperCase ('+' UpperCase)* ;`
/// ---------------------------------------------------
pub struct StructDef {
  meta : Meta
  name : UpperCase
  generic : Array[GenericType]
  fields : Array[StructField]
  static_funcs : Array[FuncDef]
  member_funcs : Array[FuncDef]
}

pub fn StructDef::new(
  meta : Meta,
  name : UpperCase,
  generic : Array[GenericType],
  fields : Array[StructField]
) -> StructDef {
  StructDef::{
    meta,
    name,
    generic,
    fields,
    static_funcs: Array::new(),
    member_funcs: Array::new(),
  }
}

pub fn meta(self : StructDef) -> Meta {
  self.meta
}

pub struct StructField {
  meta : Meta
  name : LowerCase
  type_annote : TypeExpr
}

pub fn StructField::new(
  meta : Meta,
  name : LowerCase,
  type_annote : TypeExpr
) -> StructField {
  StructField::{ meta, name, type_annote }
}

pub fn meta(self : StructField) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- Enum Defintion
/// ---------------------------------------------------
pub struct EnumDef {
  meta : Meta
  name : UpperCase
  generic : Array[GenericType]
  variants : Array[EnumVariant]
}

pub fn EnumDef::new(
  meta : Meta,
  name : UpperCase,
  generic : Array[GenericType],
  variants : Array[EnumVariant]
) -> EnumDef {
  EnumDef::{ meta, name, generic, variants }
}

pub fn meta(self : EnumDef) -> Meta {
  self.meta
}

pub struct EnumVariant {
  meta : Meta
  name : UpperCase
  field : EnumField
}

pub fn EnumVariant::new(
  meta : Meta,
  name : UpperCase,
  field : EnumField
) -> EnumVariant {
  EnumVariant::{ meta, name, field }
}

pub fn meta(self : EnumVariant) -> Meta {
  self.meta
}

pub struct EnumField {
  meta : Meta
  fields : Array[TypeExpr]
}

pub fn EnumField::new(meta : Meta, fields : Array[TypeExpr]) -> EnumField {
  EnumField::{ meta, fields }
}

pub fn meta(self : EnumField) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- GenericType
///
/// ### BNF:
///
/// - `generic_type ::= UpperCase (':' constraints)? ;`
/// - `constraints ::= UpperCase ('+' UpperCase)* ;`
///
/// ### Example:
///
/// - `T`
/// - `T: Ord`
/// - `T: Ord + Show`
/// ---------------------------------------------------
pub struct GenericType {
  meta : Meta
  ident : UpperCase
  constraints : Array[UpperCase]
}

pub fn GenericType::new(
  meta : Meta,
  ident : UpperCase,
  constraints : Array[UpperCase]
) -> GenericType {
  GenericType::{ meta, ident, constraints }
}

/// ---------------------------------------------------
/// ## Ast Node --- FuncParam
///
/// ### BNF:
///
/// - `func_param ::= LowerCase ':' type_expr ;`
///
/// ### Example:
///
/// - `x: Int`
/// - `y: Double`
/// - `p: Point[Double, Double]`
/// ---------------------------------------------------
pub struct FuncParam {
  meta : Meta
  ident : LowerCase
  type_annote : TypeExpr
}

pub fn FuncParam::new(
  meta : Meta,
  ident : LowerCase,
  type_annote : TypeExpr
) -> FuncParam {
  FuncParam::{ meta, ident, type_annote }
}

pub fn meta(self : FuncParam) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- MainFunc
///
/// ### Note
///
/// Main function is a special function, there is only one
/// main function in a program
///
/// ### BNF
///
/// - `main_func ::= 'fn' 'main' func_body ;`
/// ---------------------------------------------------
pub struct MainFunc {
  meta : Meta
  body : BlockExpr
}

pub fn MainFunc::new(meta : Meta, body : BlockExpr) -> MainFunc {
  MainFunc::{ meta, body }
}

pub fn meta(self : MainFunc) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// # ## Ast Node --- WildCard
/// ---------------------------------------------------
pub struct WildCard {
  meta : Meta
}

pub fn WildCard::new(meta : Meta) -> WildCard {
  WildCard::{ meta, }
}

pub fn meta(self : WildCard) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- Expr
///
/// ### BNF:
///
/// ```
/// expr ::=
///    if_expr |
///    match_expr |
///    block_expr |
///    binary_expr|
///    apply_expr ;
/// ```
///
/// ### Example:
///
/// - `if true { 1 } else { 2 }`
/// - `match 1 { 1 => 2, _ => 3 }`
/// - `{ 1 + 2 }`
/// - `1 + 2`
/// ---------------------------------------------------
pub enum Expr {
  Binary(BinaryExpr)
  Apply(ApplyExpr)
  Block(BlockExpr)
  Match(MatchExpr)
  IfExpr(IfExpr)
}

pub fn meta(self : Expr) -> Meta {
  match self {
    Binary(b) => b.meta()
    Apply(a) => a.meta()
    Block(b) => b.meta()
    Match(m) => m.meta()
    IfExpr(i) => i.meta()
  }
}

pub fn is_literal(self : Expr) -> Bool {
  match self {
    Apply(a) => a.is_literal()
    _ => false
  }
}

pub fn get_literal(self : Expr) -> Literal? {
  match self {
    Apply(a) => a.get_literal()
    _ => None
  }
}

/// ---------------------------------------------------
/// ## Ast Node --- BinaryExpr
///
/// ### BNF:
///
/// `binary_expr ::= expr op expr ;`
///
/// ### Example:
///
/// - `x + 42`
/// - `x + y * z`
/// ---------------------------------------------------
pub struct BinaryExpr {
  meta : Meta
  op : Operator
  left : Expr
  right : Expr
}

pub fn BinaryExpr::new(
  meta : Meta,
  op : Operator,
  left : Expr,
  right : Expr
) -> BinaryExpr {
  BinaryExpr::{ meta, op, left, right }
}

pub fn meta(self : BinaryExpr) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- ApplyExpr
///
/// ### BNF:
///
/// apply_expr ::= atom_expr | access_expr | call_expr ;
///
/// ### Example:
///
/// - `42`
/// - `point.x`
/// - `arr.get(0)`
/// ---------------------------------------------------
pub enum ApplyExpr {
  Atomic(AtomExpr)
  Access(AccessExpr)
  CallExpr(CallExpr)
}

pub fn meta(self : ApplyExpr) -> Meta {
  match self {
    Atomic(a) => a.meta()
    Access(a) => a.meta()
    CallExpr(c) => c.meta()
  }
}

pub fn is_literal(self : ApplyExpr) -> Bool {
  match self {
    Atomic(a) => a.is_literal()
    _ => false
  }
}

pub fn get_literal(self : ApplyExpr) -> Literal? {
  match self {
    Atomic(a) => a.get_literal()
    _ => None
  }
}

/// ---------------------------------------------------
/// ## Ast Node --- AccessExpr
/// ---------------------------------------------------
pub struct AccessExpr {
  meta : Meta
  apply_expr : ApplyExpr
  accessor : Accessor
}

pub fn meta(self : AccessExpr) -> Meta {
  self.meta
}

pub fn AccessExpr::new(
  meta : Meta,
  apply_expr : ApplyExpr,
  accessor : Accessor
) -> AccessExpr {
  AccessExpr::{ meta, apply_expr, accessor }
}

/// ---------------------------------------------------
/// ## Ast Node --- Accessor
/// ---------------------------------------------------
pub enum Accessor {
  TupleAcc(Literal) // for example: (1, 2).0
  FieldAcc(LowerCase) // for example: Point.x
  ArrayAcc(Expr) // for example: arr[0]
}

pub fn meta(self : Accessor) -> Meta {
  match self {
    TupleAcc(l) => l.meta()
    FieldAcc(l) => l.meta()
    ArrayAcc(e) => e.meta()
  }
}

/// ---------------------------------------------------
/// ## Ast Node --- CallExpr
///
/// ### BNF:
///
/// - `call_expr ::= apply_expr '(' expr* ')' ;`
///
/// ### Example:
///
/// - `add(1, 2)`
/// - `arr.get(0)`
/// - `funcs[0](1, 2)`
/// ---------------------------------------------------
pub struct CallExpr {
  meta : Meta
  apply_expr : ApplyExpr
  args : Array[Expr]
}

pub fn meta(self : CallExpr) -> Meta {
  self.meta
}

pub fn CallExpr::new(
  meta : Meta,
  apply_expr : ApplyExpr,
  args : Array[Expr]
) -> CallExpr {
  CallExpr::{ meta, apply_expr, args }
}

/// ---------------------------------------------------
/// ## Ast Node --- AtomExpr
///
/// ### BNF:
///
/// ```
/// atom_expr ::=
///    literal     |
///    ident       |
///    method_expr |
///    closure     |
///    tuple_expr  |
///    array_expr  ;
/// ```
///
/// ### Note:
///
/// Atom expression is the most basic expression in the language.
/// It define the basic building blocks of the language.
///
/// ### Example:
/// 
/// - `42`
/// - `x`
/// - `Array::new`
/// - `fn inc(x) { x + 1 }`
/// - `(1, 2)`
/// - `[1, 2, 3]`
/// ---------------------------------------------------
pub enum AtomExpr {
  Literal(Literal)
  Ident(LowerCase)
  Paren(Expr)
  Method(MethodExpr) // UpperCase '::' LowerCase
  Closure(Closure) // 'fn' '(' ')'
  Construct(ConstructExpr) // (UpperCase '::')? UpperCase '(' ')'
  StructExpr(StructExpr) // UpperCase '::' '{' '}'
  UnaryExpr(UnaryExpr)
  TupleExpr(TupleExpr)
  ArrayExpr(ArrayExpr)
  Unit(UnitExpr)
}

pub fn meta(self : AtomExpr) -> Meta {
  match self {
    Literal(l) => l.meta()
    Ident(i) => i.meta()
    Paren(p) => p.meta()
    Method(m) => m.meta()
    Closure(c) => c.meta()
    Construct(c) => c.meta()
    StructExpr(s) => s.meta()
    UnaryExpr(u) => u.meta()
    TupleExpr(t) => t.meta()
    ArrayExpr(a) => a.meta()
    Unit(u) => u.meta()
  }
}

pub fn is_literal(self : AtomExpr) -> Bool {
  match self {
    Literal(_) => true
    _ => false
  }
}

pub fn get_literal(self : AtomExpr) -> Literal? {
  match self {
    Literal(l) => Some(l)
    _ => None
  }
}

/// ---------------------------------------------------
/// ## Ast Node --- Literal
/// ---------------------------------------------------
pub struct Literal {
  meta : Meta
  category : LiteralCategory
}

pub enum LiteralCategory {
  Boolean(Bool)
  Double(Double)
  Integer(Int)
  Long(Int64)
  Unsigned(UInt)
  UnsignedLong(UInt64)
  Char(Char)
  String(String)
}

pub fn meta(self : Literal) -> Meta {
  self.meta
}

trait CanBeLiteral {
  to_literal(Self, Meta) -> Literal
}

pub fn Literal::new(meta : Meta, category : LiteralCategory) -> Literal {
  Literal::{ meta, category }
}

impl CanBeLiteral for Bool with to_literal(self, meta : Meta) -> Literal {
  Literal::new(meta, LiteralCategory::Boolean(self))
}

impl CanBeLiteral for Double with to_literal(self, meta : Meta) -> Literal {
  Literal::new(meta, LiteralCategory::Double(self))
}

impl CanBeLiteral for Int with to_literal(self, meta : Meta) -> Literal {
  Literal::new(meta, LiteralCategory::Integer(self))
}

impl CanBeLiteral for Int64 with to_literal(self, meta : Meta) -> Literal {
  Literal::new(meta, LiteralCategory::Long(self))
}

impl CanBeLiteral for UInt with to_literal(self, meta : Meta) -> Literal {
  Literal::new(meta, LiteralCategory::Unsigned(self))
}

impl CanBeLiteral for UInt64 with to_literal(self, meta : Meta) -> Literal {
  Literal::new(meta, LiteralCategory::UnsignedLong(self))
}

impl CanBeLiteral for Char with to_literal(self, meta : Meta) -> Literal {
  Literal::new(meta, LiteralCategory::Char(self))
}

impl CanBeLiteral for String with to_literal(self, meta : Meta) -> Literal {
  Literal::new(meta, LiteralCategory::String(self))
}

/// ---------------------------------------------------
/// ## Ast Node --- MethodExpr
///
/// ### BNF:
///
/// method_expr ::= uppercase "::" lowercase ;
///
/// #### Example:
///
/// - `Array::new`
/// - `Map::default`
/// ---------------------------------------------------
pub struct MethodExpr {
  meta : Meta
  scope : UpperCase
  ident : LowerCase
}

pub fn MethodExpr::new(
  meta : Meta,
  scope : UpperCase,
  ident : LowerCase
) -> MethodExpr {
  MethodExpr::{ meta, scope, ident }
}

pub fn meta(self : MethodExpr) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- ConstructExpr
/// ---------------------------------------------------
pub struct ConstructExpr {
  meta : Meta
  scope : UpperCase?
  ident : UpperCase
}

pub fn ConstructExpr::new(
  meta : Meta,
  scope : UpperCase?,
  ident : UpperCase
) -> ConstructExpr {
  ConstructExpr::{ meta, scope, ident }
}

pub fn meta(self : ConstructExpr) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- StructExpr
/// ---------------------------------------------------
pub struct StructExpr {
  meta : Meta
  name : UpperCase
  fields : Array[(LowerCase, TypeExpr)]
}

pub fn StructExpr::new(
  meta : Meta,
  name : UpperCase,
  fields : Array[(LowerCase, TypeExpr)]
) -> StructExpr {
  StructExpr::{ meta, name, fields }
}

pub fn meta(self : StructExpr) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- UnaryExpr
///
/// ### BNF:
///
/// - `unary_expr ::= unary_op expr ;`
/// ---------------------------------------------------
pub struct UnaryExpr {
  meta : Meta
  uop : Operator
  expr : Expr
}

pub fn UnaryExpr::new(meta : Meta, uop : Operator, expr : Expr) -> UnaryExpr {
  UnaryExpr::{ meta, uop, expr }
}

pub fn meta(self : UnaryExpr) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- TupleExpr
///
/// ### BNF:
///
/// - `tuple_expr ::= "(" expr ("," expr)* ")" ;`
///
/// ### Example:
///
/// - `(1, 2)`
/// - `(1, max(1, 2), "abc")`
/// ---------------------------------------------------
pub struct TupleExpr {
  meta : Meta
  exprs : Array[Expr]
}

pub fn TupleExpr::new(meta : Meta, exprs : Array[Expr]) -> TupleExpr {
  TupleExpr::{ meta, exprs }
}

pub fn meta(self : TupleExpr) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- ArrayExpr
/// ---------------------------------------------------
pub struct ArrayExpr {
  meta : Meta
  exprs : Array[Expr]
}

pub fn ArrayExpr::new(meta : Meta, exprs : Array[Expr]) -> ArrayExpr {
  ArrayExpr::{ meta, exprs }
}

pub fn meta(self : ArrayExpr) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- UnitExpr
/// ---------------------------------------------------
pub struct UnitExpr {
  meta : Meta
}

pub fn meta(self : UnitExpr) -> Meta {
  self.meta
}

pub fn UnitExpr::new(meta : Meta) -> UnitExpr {
  UnitExpr::{ meta, }
}

/// ---------------------------------------------------
/// ## Ast Node --- IfExpr
///
/// ### BNF:
///
/// `if_expr ::= 'if' expr block_expr ('else' if_expr | block_expr) ;`
///
/// ### Note
///
/// Unlike C-like language, `if` is expression, not a statement
/// which means `if` could return a value
///
/// ### Example:
///
/// - `let x = if a > b { a } else { b }`
/// ---------------------------------------------------
pub struct IfExpr {
  meta : Meta
  cond_expr : Expr
  then_expr : BlockExpr
  else_clause : ElseClause?
}

pub fn IfExpr::new(
  meta : Meta,
  cond_expr : Expr,
  then_expr : BlockExpr,
  else_clause : ElseClause?
) -> IfExpr {
  IfExpr::{ meta, cond_expr, then_expr, else_clause }
}

pub fn meta(self : IfExpr) -> Meta {
  self.meta
}

pub enum ElseClause {
  Block(BlockExpr)
  IfExpr(IfExpr)
}

pub fn meta(self : ElseClause) -> Meta {
  match self {
    Block(b) => b.meta()
    IfExpr(i) => i.meta()
  }
}

/// ---------------------------------------------------
/// ## Ast Node --- MatchExpr
///
/// ### BNF:
///
/// - `match_expr ::= "match" expr "{" match_arm* "}" ;`
/// ---------------------------------------------------
pub struct MatchExpr {
  meta : Meta
  expr : Expr
  arms : Array[MatchArm]
}

pub fn MatchExpr::new(
  meta : Meta,
  expr : Expr,
  arms : Array[MatchArm]
) -> MatchExpr {
  MatchExpr::{ meta, expr, arms }
}

pub fn meta(self : MatchExpr) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- MatchArm
///
/// ### BNF:
///
/// - `match_arm ::= pattern guard? "=>" Expr ;`
/// ---------------------------------------------------
pub struct MatchArm {
  meta : Meta
  pat : Pattern
  match_guard : Expr?
  expr : Expr
}

pub fn MatchArm::new(
  meta : Meta,
  pat : Pattern,
  match_guard : Expr?,
  expr : Expr
) -> MatchArm {
  MatchArm::{ meta, pat, match_guard, expr }
}

pub fn meta(self : MatchArm) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- Pattern
///
/// ### BNF:
///
/// - `pattern ::= as_pat | multi_pat ;`
///
/// ### Example
///
/// - `(a, b) as x`
/// - `1 | 2 | 3`
/// ---------------------------------------------------
pub enum Pattern {
  AsPat(AsPat)
  MultiPat(MultiPat)
}

pub fn meta(self : Pattern) -> Meta {
  match self {
    AsPat(a) => a.meta()
    MultiPat(m) => m.meta()
  }
}

/// ---------------------------------------------------
/// ## Ast Node --- AsPat
///
/// ### BNF:
///
/// - `as_pat ::= atom_pat "as" LowerCase ;`
/// ---------------------------------------------------
pub struct AsPat {
  meta : Meta
  atom_pat : AtomPat
  as_ident : LowerCase?
}

pub fn AsPat::new(
  meta : Meta,
  atom_pat : AtomPat,
  as_ident : LowerCase?
) -> AsPat {
  AsPat::{ meta, atom_pat, as_ident }
}

pub fn meta(self : AsPat) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- MultiPat
///
/// ### BNF:
///
/// - `multi_pat ::= atom_pat ("|" atom_pat)* ;`
/// ---------------------------------------------------
pub struct MultiPat {
  meta : Meta
  pats : Array[AtomPat]
}

pub fn MultiPat::new(meta : Meta, pats : Array[AtomPat]) -> MultiPat {
  MultiPat::{ meta, pats }
}

pub fn meta(self : MultiPat) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- AtomPat
///
/// ### BNF:
///
/// ```
/// atom_pat ::=
///     wild_card |
///     ident     |
///     literal   |
///     tuple_pat |
///     array_pat |
///     construct_pat |
///     struct_pat ;
/// ```
/// ---------------------------------------------------
pub enum AtomPat {
  WildCard(WildCard)
  Ident(LowerCase)
  Literal(Literal)
  Tuple(TuplePat)
  // ArrayPat(ArrayPat)
  // Construct(ArrayPat)
  // Struct(ArrayPat)
}

pub fn meta(self : AtomPat) -> Meta {
  match self {
    WildCard(w) => w.meta()
    Ident(i) => i.meta()
    Literal(l) => l.meta()
    Tuple(t) => t.meta()
  }
}

/// ---------------------------------------------------
/// ## Ast Node --- TuplePat
///
/// ### BNF:
///
/// - `tuple_pat ::= "(" as_pat ("," as_pat)* ")" ;`
///
/// ### Example:
///
/// - `(a, b)`
/// ---------------------------------------------------
pub struct TuplePat {
  meta : Meta
  as_pats : Array[AsPat]
}

pub fn TuplePat::new(meta : Meta, as_pats : Array[AsPat]) -> TuplePat {
  TuplePat::{ meta, as_pats }
}

pub fn meta(self : TuplePat) -> Meta {
  self.meta
}

pub fn iter(self : TuplePat) -> Iter[AsPat] {
  self.as_pats.iter()
}

/// ---------------------------------------------------
/// ## Ast Node --- TypeExpr
/// ---------------------------------------------------
pub enum TypeExpr {
  IdentType(UpperCase)
  TupleType(TupleType)
  ApplyType(ApplyType)
  FuncType(FuncType)
}

pub fn meta(self : TypeExpr) -> Meta {
  match self {
    IdentType(i) => i.meta()
    TupleType(t) => t.meta()
    ApplyType(a) => a.meta()
    FuncType(f) => f.meta()
  }
}

/// ---------------------------------------------------
/// ## Ast Node --- TupleType
/// ---------------------------------------------------
pub struct TupleType {
  meta : Meta
  types : Array[TypeExpr]
}

pub fn TupleType::new(meta : Meta, types : Array[TypeExpr]) -> TupleType {
  TupleType::{ meta, types }
}

pub fn meta(self : TupleType) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- ApplyType
/// ---------------------------------------------------
pub struct ApplyType {
  meta : Meta
  type_name : UpperCase
  type_args : Array[TypeExpr]
}

pub fn ApplyType::new(
  meta : Meta,
  type_name : UpperCase,
  type_args : Array[TypeExpr]
) -> ApplyType {
  ApplyType::{ meta, type_name, type_args }
}

pub fn meta(self : ApplyType) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- FuncType
/// ---------------------------------------------------
pub struct FuncType {
  meta : Meta
  arg_type : TypeExpr
  ret_type : TypeExpr
}

pub fn FuncType::new(
  meta : Meta,
  arg_type : TypeExpr,
  ret_type : TypeExpr
) -> FuncType {
  FuncType::{ meta, arg_type, ret_type }
}

pub fn meta(self : FuncType) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- LetExpr
///
/// ### BNF:
///
/// - `let_expr ::= "let" as_pat [":" type_annote] "=" expr ";" ;`
///
/// ### Note
///
/// Usually it should have `mut` keyword.
/// But currently, we do not implement it.
/// Besides, let expression support shadowing mechanism.
///
/// ### Example:
///
/// - `let x = 42`
/// - `let (x, y) as z = (1, 2)` In this case, `x = 1, y = 2, z = (1, 2)`
/// ---------------------------------------------------
pub struct LetExpr {
  meta : Meta
  pat : AsPat
  type_annote : TypeExpr?
  expr : Expr
}

pub fn LetExpr::new(
  meta : Meta,
  pat : AsPat,
  type_annote : TypeExpr?,
  expr : Expr
) -> LetExpr {
  LetExpr::{ meta, pat, type_annote, expr }
}

pub fn meta(self : LetExpr) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- AssignExpr
///
/// ### BNF:
///
/// - `assign_expr ::= apply_expr assign_op expr ;`
///
/// ### Example:
///
/// - `x = 42`
/// - `(x, y) = (1, 2)`
/// - `(a, b) = (b, a)`
/// - `a[b+42] = 42`
/// ---------------------------------------------------
pub struct AssignExpr {
  meta : Meta
  left : ApplyExpr
  assign_op : Operator
  right : Expr
}

pub fn AssignExpr::new(
  meta : Meta,
  left : ApplyExpr,
  assign_op : Operator,
  right : Expr
) -> AssignExpr {
  AssignExpr::{ meta, left, assign_op, right }
}

pub fn meta(self : AssignExpr) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- StateExpr
///
/// ### BNF:
///
/// ```
/// state_expr ::=
///    let_expr      |
///    assign_expr   |
///    while_expr    |
///    break_expr    |
///    continue_expr |
///    expr          ;
/// ```
///
/// ### Note
///
/// Each state expression have type, if it's expr,
/// its type is same as expr, if not, its type is `Unit`
/// ---------------------------------------------------
pub enum StateExpr {
  Let(LetExpr)
  Assign(AssignExpr)
  // For(ForExpr)
  While(WhileExpr)
  Break(BreakExpr)
  Continue(ContinueExpr)
  Return(ReturnExpr)
  Expr(Expr)
  Null(NullExpr)
}

pub fn meta(self : StateExpr) -> Meta {
  match self {
    Let(l) => l.meta()
    Assign(a) => a.meta()
    While(w) => w.meta()
    Break(b) => b.meta()
    Continue(c) => c.meta()
    Return(r) => r.meta()
    Expr(e) => e.meta()
    Null(n) => n.meta()
  }
}

/// ---------------------------------------------------
/// ## Ast Node --- BlockExpr
///
/// ### BNF:
///
/// - `block_expr ::= '{' state_expr* '}' ;`
///
/// ### Note
///
/// Block is an expression, it could have a vlue.
/// Besides, it is unnecessary that each statement have a terminator.
/// The last statement could be a expression without terminator,
/// and the value and type of block is the value and type of last
///
/// ### Example:
///
/// - `let t = { let x = 1; x + 1 }`
/// ---------------------------------------------------
pub struct BlockExpr {
  meta : Meta
  state_exprs : Array[StateExpr]
}

pub fn BlockExpr::new(meta : Meta, state_exprs : Array[StateExpr]) -> BlockExpr {
  BlockExpr::{ meta, state_exprs }
}

pub fn is_empty(self : BlockExpr) -> Bool {
  self.state_exprs.is_empty()
}

pub fn meta(self : BlockExpr) -> Meta {
  self.meta
}

pub fn mark_type(self : BlockExpr, ty : Type) -> Unit {
  self.meta.mark_type(ty)
}

/// ---------------------------------------------------
/// ## Ast Node --- WhileExpr
///
/// ### BNF:
///
/// - `while_expr ::= 'while' expr block_expr ;`
/// ---------------------------------------------------
pub struct WhileExpr {
  meta : Meta
  cond_expr : Expr
  body : BlockExpr
}

pub fn WhileExpr::new(
  meta : Meta,
  cond_expr : Expr,
  body : BlockExpr
) -> WhileExpr {
  WhileExpr::{ meta, cond_expr, body }
}

pub fn meta(self : WhileExpr) -> Meta {
  self.meta
}

//---------------------------------------------------
// ## Ast Node --- ForExpr
// ---------------------------------------------------
// TODO: Implement ForExpr

/// ---------------------------------------------------
/// ## Ast Node --- BreakExpr
/// ---------------------------------------------------
pub struct BreakExpr {
  meta : Meta
}

pub fn BreakExpr::new(meta : Meta) -> BreakExpr {
  BreakExpr::{ meta, }
}

pub fn meta(self : BreakExpr) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- ContinueExpr
/// ---------------------------------------------------
pub struct ContinueExpr {
  meta : Meta
}

pub fn ContinueExpr::new(meta : Meta) -> ContinueExpr {
  ContinueExpr::{ meta, }
}

pub fn meta(self : ContinueExpr) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- ReturnExpr
/// ---------------------------------------------------
pub struct ReturnExpr {
  meta : Meta
  expr : Expr?
}

pub fn ReturnExpr::new(meta : Meta, expr : Expr?) -> ReturnExpr {
  ReturnExpr::{ meta, expr }
}

pub fn meta(self : ReturnExpr) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- NullExpr
/// ---------------------------------------------------
pub struct NullExpr {
  meta : Meta
}

pub fn NullExpr::new(meta : Meta) -> NullExpr {
  NullExpr::{ meta, }
}

pub fn meta(self : NullExpr) -> Meta {
  self.meta
}

/// ---------------------------------------------------
/// ## Ast Node --- Closure
///
/// ### Note
///
/// Closure is a special function, it is defined inside a
/// function. Different with normal function, we allow closure
/// definition lack type annotation for parameters and return type
/// Bisides, closure could be anonymous function
///
/// ### BNF:
///
/// - `closure ::= 'fn' LowerCase? closure_params ret_type block_expr ;`
/// - `closure_params ::= '(' closure_param* ')' ;`
/// - `closure_param ::= LowerCase (':' type_expr)? ;`
/// - `ret_type ::= '->' type_expr ;`
///
/// ### Example:
///
/// - `fn max(a, b) { if a > b { a } else { b } }`
/// - `fn fact(n) { if n == 0 { 1 } else { n * fact(n - 1) } }`
/// - `fn (t) { t + 1 }`
/// ---------------------------------------------------
pub struct Closure {
  meta : Meta
  name : LowerCase?
  params : Array[ClosureParam]
  ret_type : TypeExpr?
  body : BlockExpr
}

pub fn Closure::new(
  meta : Meta,
  name : LowerCase?,
  params : Array[ClosureParam],
  ret_type : TypeExpr?,
  body : BlockExpr
) -> Closure {
  Closure::{ meta, name, params, ret_type, body }
}

pub fn meta(self : Closure) -> Meta {
  self.meta
}

pub fn mark_type(self : Closure, ty : Type) -> Unit {
  self.meta.mark_type(ty)
}

/// ---------------------------------------------------
/// ## Ast Node --- ClosureParam
///
/// ### BNF:
///
/// - `closure_param ::= LowerCase (':' type_expr)? ;`
/// ---------------------------------------------------
pub struct ClosureParam {
  meta : Meta
  ident : LowerCase
  type_annote : TypeExpr?
}

pub fn ClosureParam::new(
  meta : Meta,
  ident : LowerCase,
  type_annote : TypeExpr?
) -> ClosureParam {
  ClosureParam::{ meta, ident, type_annote }
}

pub fn meta(self : ClosureParam) -> Meta {
  self.meta
}

pub fn mark_type(self : ClosureParam, ty : Type) -> Unit {
  self.meta.mark_type(ty)
}

/// ---------------------------------------------------
/// ## Ast Node --- Operator
///
/// ### NotE
///
/// About precedence:
///
/// - `*`, `/`, `%` : 7
/// - `+`, `-` : 6
/// - `==`, `!=`, `<`, `<=`, `>`, `>=` : 4
/// - `&&` : 3
/// - `||` : 2
/// - `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=` : 1
/// - others : 9
/// ---------------------------------------------------
struct Operator {
  meta : Meta
  tok : Token
  sym : String
  prec : Int
}

pub fn Operator::new(meta : Meta, tok : Token) -> Operator {
  let prec = match tok.token_type {
    TokenType::Operator("*") => 7
    TokenType::Operator("/") => 7
    TokenType::Operator("%") => 7
    TokenType::Operator("+") => 6
    TokenType::Operator("-") => 6
    TokenType::Operator("==") => 4
    TokenType::Operator("!=") => 4
    TokenType::Operator("<") => 4
    TokenType::Operator("<=") => 4
    TokenType::Operator(">") => 4
    TokenType::Operator(">=") => 4
    TokenType::Operator("&&") => 3
    TokenType::Operator("||") => 2
    TokenType::Operator("=") => 1
    TokenType::Operator("+=") => 1
    TokenType::Operator("-=") => 1
    TokenType::Operator("*=") => 1
    TokenType::Operator("/=") => 1
    TokenType::Operator("%=") => 1
    TokenType::Operator("&=") => 1
    TokenType::Operator("|=") => 1
    _ => {
      let msg = "Unknown Operator: " + tok.get_token_content()
      tok.write_error(msg)
      tok.throw_err()
      panic()
    }
  }
  Operator::{ meta, tok, sym: tok.get_token_content(), prec }
}

/// ---------------------------------------------------
/// ## UpperCase
///
/// ### Note
///
/// UpperCase is used to represent the name of a type or a module
///
/// ### Example
///
/// - `Int`
/// - `Point`
/// - `Array`
/// ---------------------------------------------------
pub struct UpperCase {
  meta : Meta
  tok : Token
  id : String
}

pub fn UpperCase::new(meta : Meta, tok : Token) -> UpperCase {
  let id = tok.get_token_content()
  UpperCase::{ meta, tok, id }
}

pub fn meta(self : UpperCase) -> Meta {
  self.meta
}

pub fn hash_combine(self : UpperCase, hasher : Hasher) -> Unit {
  hasher.combine_string(self.id)
}

pub fn op_equal(self : UpperCase, other : UpperCase) -> Bool {
  self.id == other.id
}

/// ---------------------------------------------------
/// ## LowerCase
///
/// ### Note
///
/// LowerCase is used to represent the name of a variable or a function
///
/// - `x`
/// - `max`
/// - `Array::make`
/// ---------------------------------------------------
pub struct LowerCase {
  meta : Meta
  tok : Token
  scope : UpperCase?
  id : String
}

pub fn LowerCase::new(
  meta : Meta,
  tok : Token,
  ~scope : UpperCase? = None
) -> LowerCase {
  let tok_content = tok.get_token_content()
  let id = match scope {
    Some(upper) => upper.id + "::" + tok_content
    None => tok_content
  }
  LowerCase::{ meta, tok, scope, id }
}

pub fn meta(self : LowerCase) -> Meta {
  self.meta
}

pub fn hash_combine(self : LowerCase, hasher : Hasher) -> Unit {
  hasher.combine_string(self.id)
}

pub fn op_equal(self : LowerCase, other : LowerCase) -> Bool {
  self.id == other.id
}
