{
  open! Batteries
  open! Lexing

  open Parse
}

let whitespace = ' '+ | ['\r' '\n'] | '\r' '\n' | '\t'

let digit = ['0'-'9']
let sig_digits = ['1'-'9'] digit*

let id_char = ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']
let id_tail = ('-'? id_char)*
let id = ['a'-'z'] id_tail
let cap_id = ['A'-'Z'] id_tail

rule token = parse
  | "eof" {EOF}
  | "$" {EOF}
  | whitespace {token lexbuf}
  
  | "&" {AND}
  | "^" {XOR}
  | cap_id as c {VAR c}
  | '0' {ZERO}
  | '1' {ONE}

  | "(" {LPAREN}
  | ")" {RPAREN}

  | "=?=" {UNIFY}

  | _ as s {failwith (Printf.sprintf "Unexpected character %c" s)}
