(* Truth tables for logical expressions.
 *
 * Define predicates
 *   not, and, or, nand, nor, xor, eq
 * which succeed or fail according to the result of their respective
 * operations; e.g. and(A,B) will succeed, iff both A and B are true.
 *)
exception Variable_Unbound
type expr =
    | And   of expr * expr
    | Or    of expr * expr
    | Not   of expr
    | Var   of string
    | Const of bool
(*  | Nand  of expr * expr
 *  | Nor   of expr * expr
 *  | Xor   of expr * expr
 *  | Eq    of expr * expr *)

(* TODO Benchmark set vs list impl *)
(* TODO Return list or set? *)
let find_vars expr =
    let open String.Set in
    let rec aux expr vars = match expr with
        | Var   x -> add vars x
        | Const x -> vars
        | And _ | Or _ as (x, y) -> union (aux x vars) (aux y vars)
        | Not x -> aux vars x
    in aux expr empty

let gen_ctxts vars = 

let eval expr ctxt =
    let unbind x = match List.Assoc.find ctxt ~equal:String.equal x with
        | Some b -> b
        | None -> raise Variable_Unbound in
    let rec eval' expr = match expr with
        | And   (x, y) -> eval x && eval y
        | Or    (x, y) -> eval x || eval y
        | Not   x      -> not (eval x)
        | Var   x      -> unbind x
        | Const x      -> x
    in eval' expr

let gen_truth_table expr = find_vars expr
    |> gen_ctxts
    |> List.map ~f:(ctxt -> eval expr ctxt) (* TODO Memoify? *)

(* Gray code.
 *
 * An n-bit Gray code is a sequence of n-bit strings constructed
 * according to certain rules.
 *
 * gray_code 1 = ["0";"1"]
 * gray_code 2 = ["00";"01";"11";"10"]
 * gray_code 3 = ["000";"001";"011";"010";´110´;´111´;´101´;´100´]
 *
 * TODO apply result caching
 *)
let gray_code n =

(* Huffman code *)
let huff s =
