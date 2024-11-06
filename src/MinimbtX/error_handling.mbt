///-------------------------------------------------------------
/// Lexical Error
///
///-------------------------------------------------------------
pub type! LexicalError Lexer

pub fn output(self : LexicalError, logger : Logger) -> Unit {
  let LexicalError(lexer) = self
  logger.write_string(lexer.error_msg)
  lexer.error_msg = ""
}

///-------------------------------------------------------------
/// Parser Error
///
///-------------------------------------------------------------
type! ParseError Parser

pub fn output(self : ParseError, logger : Logger) -> Unit {
  let ParseError(parser) = self
  match parser.get?() {
    Ok(token) => {
      let mut msg = "ParseError: "
      if parser.extra_err_msg != "" {
        msg += parser.extra_err_msg
      }
      let msg = token.get_errmsg(msg)
      parser.extra_err_msg = ""
      logger.write_string(msg)
    }
    Err(_) => {
      let msg = "ParseError: unexpected end of file"
      logger.write_string(msg)
    }
  }
}

///-------------------------------------------------------------
/// Type Analysis Error
///
///-------------------------------------------------------------

///-------------------------------------------------------------
/// Redefinition Function Error
///-------------------------------------------------------------
type! RedefineFunctionError FuncDef

pub fn output(self : RedefineFunctionError, logger : Logger) -> Unit {
  let RedefineFunctionError(func_def) = self
  let tok = func_def.meta().get_head_token()
  let msg = @Color.red_font("Redefinition of function: ")
  let msg = msg + @Color.bold_magenta(func_def.name.id)
  let msg = tok.get_errmsg(msg)
  logger.write_string(msg)
}

///-------------------------------------------------------------
/// Redefinition Main Function Error
///-------------------------------------------------------------
type! ExtraMainFunctionError MainFunc

pub fn output(self : ExtraMainFunctionError, logger : Logger) -> Unit {
  let ExtraMainFunctionError(main_func) = self
  let tok = main_func.meta().get_head_token()
  let msg = @Color.red_font("Extra main function: ")
  let msg = msg + @Color.bold_magenta("main")
  let msg = tok.get_errmsg(msg)
  logger.write_string(msg)
}

///-------------------------------------------------------------
/// Redefinition Struct Error
///-------------------------------------------------------------
type! RedefineStructError StructDef

pub fn output(self : RedefineStructError, logger : Logger) -> Unit {
  let RedefineStructError(struct_def) = self
  let tok = struct_def.meta().get_head_token()
  let msg = @Color.red_font("Redefinition of struct: ")
  let msg = msg + @Color.bold_magenta(struct_def.name.id)
  let msg = tok.get_errmsg(msg)
  logger.write_string(msg)
}

/// -------------------------------------------------------------
/// Redefine Enum Error
/// -------------------------------------------------------------
type! RedefineEnumError EnumDef

pub fn output(self : RedefineEnumError, logger : Logger) -> Unit {
  let RedefineEnumError(enum_def) = self
  let tok = enum_def.meta().get_head_token()
  let msg = @Color.red_font("Redefinition of enum: ")
  let msg = msg + @Color.bold_magenta(enum_def.name.id)
  let msg = tok.get_errmsg(msg)
  logger.write_string(msg)
}

///-------------------------------------------------------------
/// ILegal Left Value
///
/// The following expression may cause illegal left value error
///
/// - `2 = 42`
/// - `foo() = 42`
///-------------------------------------------------------------
type! ILegalLeftValueError ILegalLeftValue

struct ILegalLeftValue {
  apply_expr : ApplyExpr
  msg : String
}

pub fn ILegalLeftValue::new(
  apply_expr : ApplyExpr,
  msg : String
) -> ILegalLeftValue {
  ILegalLeftValue::{ apply_expr, msg }
}

pub fn output(self : ILegalLeftValue, logger : Logger) -> Unit {
  let tok = self.apply_expr.meta().get_head_token()
  let msg = @Color.red_font("Illegal left value: ")
  let msg = msg + @Color.bold_magenta(self.msg)
  let msg = tok.get_errmsg(msg)
  logger.write_string(msg)
}

pub fn output(self : ILegalLeftValueError, logger : Logger) -> Unit {
  let ILegalLeftValueError(illegal_left_value) = self
  illegal_left_value.output(logger)
}

///-------------------------------------------------------------
/// ILegal Let Pattern
///
/// The following let expression may cause illegal let pattern error
///
/// - `let 2 = 42`  pattern could not be a literal
/// - `let (a, b) = 3` pattern mismatch
///-------------------------------------------------------------
type! ILegalLetPatternError ILegalLetPattern

struct ILegalLetPattern {
  as_pat : AsPat
  msg : String
}

pub fn ILegalLetPattern::new(as_pat : AsPat, msg : String) -> ILegalLetPattern {
  ILegalLetPattern::{ as_pat, msg }
}

pub fn output(self : ILegalLetPattern, logger : Logger) -> Unit {
  let tok = self.as_pat.meta().get_head_token()
  let msg = @Color.red_font("Illegal let pattern: ")
  let msg = msg + @Color.bold_magenta(self.msg)
  let msg = tok.get_errmsg(msg)
  logger.write_string(msg)
}

pub fn output(self : ILegalLetPatternError, logger : Logger) -> Unit {
  let ILegalLetPatternError(illegal_let_pattern) = self
  illegal_let_pattern.output(logger)
}

/// -------------------------------------------------------------
/// Call Error
///
/// Case 1: Call a non-function value
///   - `let a = 42; a()`
/// Case 2: arg type and param type mismatch
///   - `let f = fn (a: Int) -> Int { a }; f(42.0)`
///   - `fn max(a, b) -> Int;  max(3)`
/// -------------------------------------------------------------
type! ILegalCallError ILegalCall

struct ILegalCall {
  apply_expr : ApplyExpr
  msg : String
}

pub fn ILegalCall::new(apply_expr : ApplyExpr, msg : String) -> ILegalCall {
  ILegalCall::{ apply_expr, msg }
}

pub fn output(self : ILegalCall, logger : Logger) -> Unit {
  let tok = self.apply_expr.meta().get_head_token()
  let msg = @Color.red_font("Call error: ")
  let msg = msg + @Color.bold_magenta(self.msg)
  let msg = tok.get_errmsg(msg)
  logger.write_string(msg)
}

pub fn output(self : ILegalCallError, logger : Logger) -> Unit {
  let ILegalCallError(illegal_call) = self
  illegal_call.output(logger)
}

type! Unimplemented Token

type! UnimplementedOperation Operator

type! UndefinedVariable LowerCase

type! UndefinedConstrain String

type! TypeConstrainMismatch (Constraint, Type)

type! TypeMismatch (Token, Type, Type) // Err token, expected, got

type! BinaryTypeMismatch (Token, Type, Type)

type! ArrayTypeMismatch (Type, Type)

type! ILegalOperation (Operator, Type)

pub fn output(self : UndefinedVariable, logger : Logger) -> Unit {
  let UndefinedVariable(ident) = self
  let msg = ident.tok.get_errmsg("Undefined variable")
  logger.write_string(msg)
}

pub fn output(self : Unimplemented, logger : Logger) -> Unit {
  let Unimplemented(tok) = self
  let msg = tok.get_errmsg("Unimplemented")
  logger.write_string(msg)
}

pub fn output(self : TypeMismatch, logger : Logger) -> Unit {
  let TypeMismatch((tok, expected, current)) = self
  let msg = @Color.red_font("Expression type mismatch, Expected ")
  let msg = msg + @Color.bold_magenta(expected.to_string())
  let msg = msg + @Color.red_font(", while got ")
  let msg = msg + @Color.bold_magenta(current.to_string())
  let msg = tok.get_errmsg(msg)
  logger.write_string(msg)
}

pub fn output(self : ArrayTypeMismatch, logger : Logger) -> Unit {
  let ArrayTypeMismatch((lty, rty)) = self
  let msg = @Color.red_font("Array type mismatch, previous element is ")
  let lty_msg = @Color.bold_magenta(lty.to_string())
  let mid_msg = @Color.red_font(", while current is ")
  let rty_msg = @Color.bold_magenta(rty.to_string())
  let msg = msg + lty_msg + mid_msg + rty_msg
  logger.write_string(msg)
}

pub fn output(self : BinaryTypeMismatch, logger : Logger) -> Unit {
  let BinaryTypeMismatch((op, lty, rty)) = self
  let msg = @Color.red_font("Binary type mismatch, left is ")
  let lty_msg = @Color.bold_magenta(lty.to_string())
  let mid_msg = @Color.red_font(", while right is ")
  let rty_msg = @Color.bold_magenta(rty.to_string())
  let msg = msg + lty_msg + mid_msg + rty_msg
  let msg = op.get_errmsg(msg)
  logger.write_string(msg)
}

pub fn output(self : ILegalOperation, logger : Logger) -> Unit {
  let ILegalOperation((op, ty)) = self
  let msg = @Color.red_font("Illegal operation ")
  let op_msg = @Color.bold_magenta(op.sym)
  let mid_msg = @Color.red_font(" on type ")
  let ty_msg = @Color.bold_magenta(ty.to_string())
  let msg = msg + op_msg + mid_msg + ty_msg
  let msg = op.tok.get_errmsg(msg)
  logger.write_string(msg)
}
