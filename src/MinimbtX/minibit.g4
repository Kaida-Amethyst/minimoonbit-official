
prog : top_decl *

top_decl :
      func_def
    | struct_def
    | enum_def
    | type_def
    ;
         
func_def :
  'fn' func_name generic_types? parameter ret_type func_body ;

func_name :
    (UpperCase '::')? LowerCase ;

generic_types :
    '[' generic_type (',' generic_type)* ']' ;

generic_type :
    UpperCase (':' constrain)? ;

constrain :
    UpperCase ('+' UpperCase)* ;

parameter :
    '(' (parameter_list)? ')' ;

parameter_list :
    parameter_decl (',' parameter_decl)* ;

parameter_decl :
    LowerCase ':' type ;

ret_type :
    '->' type ;

func_body :
    block_expr ;

struct_def :
    'struct' UpperCase '{' struct_body '}' ;

struct_body :
    struct_field (',' struct_field)* ;

struct_field :
    LowerCase ':' type ;

enum_def :
    'enum' UpperCase '{' enum_body '}' ;

enum_body :
    enum_item (',' enum_item)* ;

enum_item :
    UpperCase '(' type ')' ;

type_def :
    'type' UpperCase type ;

block_expr :
    '{' state_expr (';' state_expr)* '}' ;

state_expr :
      let_expr
    | assign_expr
    | for_expr
    | while_expr
    | "break"
    | "continue"
    | "return" expr
    | expr
    | // (empty)
    ;

expr :
      if_expr
    | match_expr
    | block_expr
    | infix_expr
    ;

infix_expr :
    apply_expr (op apply_expr)* ;

apply_expr :
      atom_expr
    | access_expr
    | call_expr
    ;

access_expr :
    apply_expr accessor  ;

accessor :
      '.' (LowerCase | Integer)
    | '[' expr ']' ;
    ;

call_expr :
    apply_expr '(' (expr (',' expr)*)? ')' ;

atom_expr :
        literal
      | LowerCase
      | '(' expr ')'
      | UpperCase '::' LowerCase
      | '(' expr (',' expr)+ ')'
      | '[' expr (',' expr)* ']'
      | UpperCase '::' '{' struct_field (',' struct_field)* '}'
      | (UpperCase '::' UpperCase)? ( '(' expr ')' )?
      | unary_op expr
      | '_'
      ;

literal :
      Integer
    | Double
    | String
    | Char
    | Boolean
    ;

let_expr :
    'let' non_literal_pattern type_annotation? '=' expr

assign_expr :
    left_value assign_op expr ;

left_value :
      LowerCase
    | access_expr ;
    | '(' left_value (';' left_value)* ')' ;
    ;

for_expr :
    'for' non_literal_pattern 'in' expr block_expr ;

while_expr :
    'while' expr block_expr ;

unary_op : '-'

assign_op : '=' | '+=' | '-=' | '*=' | '/=' 

op : '+' | '-' | '*' | '/' | '%' | '==' | '!=' | '<' | '<=' | '>' | '>=' | '&&' | '||' | '++' | '--'

Lowercase : [a-z][a-zA-Z0-9_]*
UpperCase : [A-Z][a-zA-Z0-9_]*

Integer : [0-9]+
Double : [0-9]+\.[0-9]+
Boolean : 'true' | 'false'
