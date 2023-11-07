open! Batteries
open Ubool

module C = struct
  type t = bool
  let mul = (&&)
  let add x y = (x || y) && not (x && y)
  let one = true
  let zero = false
  let eq = (=)
  let to_string = function
    | true -> "1"
    | false -> "0"
end

module E = Make(C)
open E

let m : (string, E.t) Hashtbl.t = Hashtbl.create 32
let memo x = 
  Hashtbl.find_option m x
  |> Option.default_delayed @@ fun () -> 
    let nu = fresh () in
    Hashtbl.add m x nu;
    nu
