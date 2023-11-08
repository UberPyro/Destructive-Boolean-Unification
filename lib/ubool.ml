open Batteries
open Uref

module type Constant = sig
  type t

  val add : t -> t -> t
  val mul : t -> t -> t
  val zero : t
  val one : t

  val eq : t -> t -> bool
  val to_string : t -> string
end

module Make(C : Constant) = struct
  
  type t = _t uref
  and _t = 
    | Var of int
    | Expr of (C.t * t list) list
  
  let one = [C.one, []]
  let const c = [c, []]
  let var v = [C.one, [v]]
  
  let uvar i = uref (Var i)
  let uexpr e = uref (Expr e)
  let uconst c = uref (Expr (const c))
  let fresh () = uvar (unique ())
  let getvar u = 
    let[@warning "-8"] (Var i) = uget u in i
  let getexpr u = match uget u with
    | Expr e -> e
    | Var _ -> var u
  
  let mul e1 e2 = 
    List.map
      (fun ((c1, t1), (c2, t2)) -> C.mul c1 c2, t1 @ t2)
      (List.cartesian_product e1 e2)
  
  let add_t u v = match uget u, uget v with
    | Expr [], _ -> v
    | _, Expr [] -> u
    | Var _, Expr e -> uexpr (var u @ e)
    | Expr e, Var _ -> uexpr (var v @ e)
    | Expr e1, Expr e2 -> uexpr (e1 @ e2)
    | Var _, Var _ -> uexpr (var u @ var v)
  
  let mul_t u v = match uget u, uget v with
    | Expr [coeff, []], _ when C.eq coeff C.one -> v
    | _, Expr [coeff, []] when C.eq coeff C.one -> u
    | Var _, Expr e -> 
      uexpr (List.map (Tuple2.map2 (List.cons u)) e)
    | Expr e, Var _ -> 
      uexpr (List.map (Tuple2.map2 (List.cons v)) e)
    | Expr e1, Expr e2 -> uexpr (mul e1 e2)
    | Var _, Var _ -> uexpr [C.one, [u; v]]
  
  let map_expr f = function
    | Var i -> Var i
    | Expr e -> Expr (f e)
  
  let upd_expr f x = uset x (f (uget x)); x
  
  let rec distribute e = 
    List.fold_left (fun expr_acc (coeff, vars) ->  
      let flatvars = List.map (upd_expr (map_expr distribute)) vars in
      let newterms = 
        getexpr (List.fold_left mul_t (uconst coeff) flatvars) in
      newterms @ expr_acc
    ) [] e
  
  let elim = 
    List.map (Tuple2.map2 (List.sort_uniq compare))
    %> List.sort (fun s t -> compare (snd s) (snd t))
    %> List.fold_left (function
      | [] -> List.singleton
      | (c, t) :: ts as acc -> fun (const, vars as term) -> 
        if t = vars then
          let c' = C.add c const in
          if C.eq c' C.zero then ts
          else (c', t) :: ts
        else term :: acc
    ) []
    %> List.filter (fst %> C.eq C.zero %> not)
  
  let simp = distribute %> elim %> List.rev

  open Printf

  let pretty_term_anf out = function
    | coeff, [] when C.eq coeff C.one -> fprintf out "%d" 1
    | coeff, vars -> 
      if not (C.eq coeff C.one) then fprintf out "%s" (C.to_string coeff);
      List.iter (getvar %> fprintf out "[%d]") vars

  let pretty_anf out = uget %> map_expr simp %> function
    | Var i -> fprintf out "[%d]" i
    | Expr [] -> fprintf out "%d" 0
    | Expr (t :: ts) -> 
      pretty_term_anf out t;
      List.iter (fun x -> fprintf out " + "; pretty_term_anf out x) ts
  
  let print_anf u = 
    let out = IO.output_string () in
    pretty_anf out u;
    print_endline (IO.close_out out)
  
  let[@warning "-8"] smallterm (x :: xs) = 
    List.fold_left (fun t t' -> 
      if List.compare_lengths t t' = 1 then t'
      else t
    ) x xs
  
  let select_var = 
    List.map snd %> List.filter (Fun.negate List.is_empty)
    %> smallterm %> List.hd
  
  let factor u = 
    List.partition_map (fun (coeff, vars) -> 
      match[@warning "-8"] List.partition (Uref.equal u) vars with
      | [], full -> Right (coeff, full)
      | [_], part -> Left (coeff, part)
    )
  
  let rec solve e0 = match simp e0 with
    | [] -> ()
    | [_, []] -> failwith "No unifiers."
    | e -> 
      let u = select_var e in
      let t1, t2 = factor u e in
      solve (mul t2 (one @ t1));
      (* printf "Subbing [%d] |-> " (getvar u);
      print_anf (uexpr (simp (t2 @ mul (var (fresh ())) (one @ t1)))); *)
      uset u (Expr (simp (t2 @ mul (var (fresh ())) (one @ t1))))
  
  let unify r = unite ~sel:(curry @@ function
    | Var i, _ | _, Var i -> Var i
    | Expr e1 as x, Expr e2 -> 
      solve (e1 @ e2);
      x
  ) r

end
