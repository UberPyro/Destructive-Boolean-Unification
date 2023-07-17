open! Batteries
open Dbu
open Memo
open E

let () = match Sys.argv.(1) with
  | "simplify" -> 
    let lexbuf = Lexing.from_string (Sys.argv.(2) ^ "$") in
    let e = Parse.expr_file Lex.token lexbuf |> simp in
    let out = IO.output_string () in
    pretty_ubool_ out e;
    print_endline (IO.close_out out)
  | "unify" -> 
    let lexbuf = Lexing.from_string (Sys.argv.(2) ^ "$") in
    let e = Parse.problem Lex.token lexbuf in
    uncurry unify e;
    let out = IO.output_string () in
    pretty_ubool out (fst e);
    print_endline (IO.close_out out)
  | s -> failwith (Printf.sprintf "[%s] is not a recognized operation" s)
