%{
  open! Batteries
  open! Uref
  open! Ubool
  open! Memo
  open! E
%}

%token EOF AND XOR ZERO ONE UNIFY LPAREN RPAREN
%token<string> VAR

%start<boolean list list> expr_file
%start<ubool * ubool> problem

%left XOR
%left AND

%%

expr_file: expr EOF {$1}

expr: 
  | ZERO {[[ref (BConst (C.zero))]]}
  | ONE {[[ref (BConst (C.one))]]}
  | expr XOR expr {[[ref (BExpr ($1 @ $3))]]}
  | expr AND expr {[[ref (BExpr (mul_basic $1 $3))]]}
  | VAR {memo $1}
  | LPAREN expr RPAREN {$2}

problem: expr UNIFY expr EOF {uref $1, uref $3}
