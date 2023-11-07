%{
  open! Batteries
  open! Uref
  open! Ubool
  open! Memo
  open! E
%}

%token EOF XOR ZERO ONE UNIFY LPAREN RPAREN
%token<string> VAR

%start<t> expr_file
%start<t * t> problem

%left XOR

%%

expr_file: expr EOF {$1}

expr: 
  | expr XOR expr {add_t $1 $3}
  | term {$1}


term: 
  | lit term {mul_t $1 $2}
  | lit {$1}

lit: 
  | ZERO {uexpr []}
  | ONE {uexpr one}
  
  | VAR {memo $1}
  | LPAREN expr RPAREN {$2}

problem: expr UNIFY expr EOF {$1, $3}
