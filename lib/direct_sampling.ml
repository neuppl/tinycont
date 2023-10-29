open Core_grammar


(** capture-avoiding substitution e[x/v] *)
let rec subst e x (v: value) : expr = 
  match e with 
  | Ident(id) when id = x -> Value(v)
  | Bind(id, e1, e2) when id != x -> Bind(id, subst e1 x v, subst e2 x v)
  | Bind(_) | Ident(_) | Value(_) | Unif | Flip(_) -> e
  | Observe(e1, e2) -> Observe(subst e1 x v, subst e2 x v)
  | Ite(g, thn, els) -> Ite(subst g x v, subst thn x v, subst els x v)
  | Plus(e1, e2) -> Plus(subst e1 x v, subst e2 x v)
  | Mul(e1, e2) -> Mul(subst e1 x v, subst e2 x v)
  | Lt(e1, e2) -> Lt(subst e1 x v, subst e2 x v)
  | Or(e1, e2) -> Or(subst e1 x v, subst e2 x v)
  | And(e1, e2) -> And(subst e1 x v, subst e2 x v)
  | Not(e) -> Not(subst e x v)
  | Return(e) -> Return(subst e x v)

(** converts a value to a float or panics *)
let v_to_f v = 
  match v with 
  | Real(v) -> v 
  | _ -> failwith "unreachable"

(** converts a value to a Boolean or panics *)
let v_to_b v = 
  match v with 
  | Bool(v) -> v
  | _ -> failwith "unreachable"

(* true if a value is bot *)
let is_bot v = 
  match v with 
  | Bot -> true 
  | _ -> false

(** implement the weighted sampling semantics *)
let rec sample (e: Core_grammar.expr) : value = 
  failwith "implement me"

(* implement the eval semantics: loop until you get an accepted sample *)
let rec eval e : value = 
  failwith "implement me"

(** compute an estimate for the program with n samples*)
let estimate (p: Core_grammar.program) (n: int) =
  let e = ref 0.0 in
  for _ = 0 to n do
    let s = eval p.body in 
    if v_to_b s then e := !e +. 1.0;
  done;
  !e /. (float_of_int n)
