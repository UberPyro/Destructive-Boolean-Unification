open! Batteries
open Uref
open Dbu
open Memo
open E

let () = match Sys.argv.(1) with
  | "simplify" -> 
    let lexbuf = Lexing.from_string (Sys.argv.(2) ^ "$") in
    let e = Parse.expr_file Lex.token lexbuf |> uget |> map_expr simp in
    print_anf (uref e);
  | "unify" -> 
    let lexbuf = Lexing.from_string (Sys.argv.(2) ^ "$") in
    let e = Parse.problem Lex.token lexbuf in
    uncurry unify e;
    print_anf (fst e);
  | s -> failwith (Printf.sprintf "[%s] is not a recognized operation" s)
