open Core
let ( |?> ) x (p, f, g) = if (p x) then (f x) else (g x);;

(* Truth tables for logical expressions.
 *
 * Define predicates
 *   not, and, or, nand, nor, xor, eq
 * which succeed or fail according to the result of their respective
 * operations; e.g. and(A,B) will succeed, iff both A and B are true.
*)
exception Variable_Unbound
module Expr : sig
  type t =
    | And   of t * t
    | Or    of t * t
    | Not   of t
    | Var   of string
    | Const of bool
  type ctxt   = (string * bool) list
  type result = bool
  val eval : t -> (string * bool) list -> bool
  val simplify : t -> t
  val find_vars : t -> string list
  val gen_truth_table : t -> (ctxt * result) list
  val print_truth_table : t -> unit
end = struct
  type t =
    | And   of t * t
    | Or    of t * t
    | Not   of t
    | Var   of string
    | Const of bool
    (*      | Nand  of t * t
            | Nor   of t * t
            | Xor   of t * t
            | Eq    of t * t *)

  type ctxt   = (string * bool) list
  type result = bool

  let fst e = match e with
    | And (x, _) -> Some x
    | Or (x, _) -> Some x
    | _ -> None

  let snd e = match e with
    | And (_, y) -> Some y
    | Or (_, y) -> Some y
    | _ -> None

  exception Bad_expr_application
  let exn e f = match (f e) with Some x -> x | None -> raise Bad_expr_application
  let fst_exn e = exn e fst
  let snd_exn e = exn e snd

  (* TODO *)
  let simplify e = e

  let find_vars expr =
    let open String.Set in
    let rec aux expr vars = match expr with
      | Var   x -> add vars x
      | Const x -> vars
      | Not x -> aux x vars
      | And _ | Or _ as x -> union (aux (fst_exn x) vars) (aux (snd_exn x) vars)
    in to_list (aux expr empty)

  let gen_ctxts vars =
    let push ctxt v b = List.map ctxt ~f:(fun x -> (v, b) :: x) in
    let rec aux vs = match vs with
      | [] -> []
      | [v] -> [[v, true]; [v, false]]
      | v :: vs ->
        let vs' = aux vs in
        (push vs' v true) @ (push vs' v false)
    in aux vars

  let eval expr ctxt =
    let unbind x = match List.Assoc.find ctxt ~equal:String.equal x with
      | Some b -> b
      | None -> raise Variable_Unbound in
    let rec eval' expr = match expr with
      | And   (x, y) -> eval' x && eval' y
      | Or    (x, y) -> eval' x || eval' y
      | Not   x      -> not (eval' x)
      | Var   x      -> unbind x
      | Const x      -> x
    in eval' expr

  let ctxt_to_string = List.fold ~init:"" ~f:(fun s (v, b) -> s ^ v ^ ": " ^ Bool.to_string b ^ " ")

  let gen_truth_table expr =
    find_vars expr
    |> gen_ctxts
    |> List.map ~f:(fun c -> (c, eval expr c))

  let print_truth_table expr =
    find_vars expr
    |> gen_ctxts
    |?> ( List.is_empty
        , (fun _ -> ["NO VARS " ^ (Bool.to_string (eval expr []))])
        , List.map ~f:(fun c -> (ctxt_to_string c) ^"=> "^  (Bool.to_string (eval expr c)))
        )
    |> List.iter ~f:print_endline

end

(* Gray code.
 *
 * An n-bit Gray code is a sequence of n-bit strings constructed
 * according to certain rules.
 *
 * gray_code 1 = ["0";"1"]
 * gray_code 2 = ["00";"01";"11";"10"]
 * gray_code 3 = ["000";"001";"011";"010";"110";"111";"101";"100"]
*)
let gray_code n =
  let pref_all p l = List.map l ~f:(fun a -> p ^ a) in
  let rec aux n = match n with
    | 1 -> ["0";"1"]
    | _ ->
      let l = aux (n - 1) in
      let l2 = List.rev l in
      (pref_all "0" l) @ (pref_all "1" l2)
  in aux n

(* TODO, in results of n we duplciate data of results of n - 1.
 * Can we store less in results and do some simple work to avoid storing
 * duplicated data?
 *
 * Maybe only store n mod (4 | 8 ?) = 0 keys and work from there?
 *
 * TODO Benchmark and analyse word usage
 *)
let gray_code_fn_gen () =
  let results = Hashtbl.create ~hashable:(Int.hashable) ?size:(Some 64) () in
  let pref_all p l = List.map l ~f:(fun a -> p ^ a) in
  let rec memo n =
    match Hashtbl.find results n with
    | Some result -> result
    | None ->
      let r = aux n in
      Hashtbl.add_exn results ~key:n ~data:r;
      r
  and aux n =
    match n with
    | 1 -> ["0";"1"]
    | _ ->
      let l = memo (n - 1) in
      let l2 = List.rev l in
      (pref_all "0" l) @ (pref_all "1" l2)
  in (fun n -> memo n)
