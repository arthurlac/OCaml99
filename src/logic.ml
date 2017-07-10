open Core

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
    val eval : t -> (string * bool) list -> bool
    val simplify : t -> t
    val find_vars : t -> string list
    val gen_truth_table : t -> bool list
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

    (* TODO Benchmark set vs list impl *)
    (* TODO Return list or set? *)
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

    (* TODO Memoify? *)
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

    (* TODO Pretty print? *)
    let gen_truth_table expr = find_vars expr |> gen_ctxts |> List.map ~f:(fun ctxt -> eval expr ctxt)
end

(* Gray code.
 *
 * An n-bit Gray code is a sequence of n-bit strings constructed
 * according to certain rules.
 *
 * gray_code 1 = ["0";"1"]
 * gray_code 2 = ["00";"01";"11";"10"]
 * gray_code 3 = ["000";"001";"011";"010";"110";"111";"101";"100"]
 *
 * TODO apply result caching !!!!
let memo f =
  let results = Hashtbl.create 256 in
  (fun input ->
     match Hashtbl.find results input with
     | None ->
        let result = f input in
        Hashtbl.add results ~key:input ~data:result;
        result
     | Some result -> result)
 *)
(* TODO Return array? *)
let gray_code n =
    let pref_all p l = List.map l ~f:(fun a -> p ^ a) in
    let rec aux n = match n with
        | 1 -> ["0";"1"]
        | _ ->
            let l = aux (n - 1) in
            let l2 = List.rev l in
            (pref_all "0" l) @ (pref_all "1" l2)
    in aux n
