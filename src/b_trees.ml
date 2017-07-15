(* Binary trees *)
open Core

type 'a t = Empty | Node of 'a * 'a t * 'a t

let car   = function Empty -> None | Node (v, _, _) -> Some v
let left  = function Empty -> None | Node (_, l, _) -> Some l
let right = function Empty -> None | Node (_, _, r) -> Some r

open Option.Monad_infix
let lcar t = left t  >>= car
let rcar t = right t >>= car

(* Symmetric binary trees. *)
exception Break
let is_sym t =
    let rec aux t = match t with
        | Empty -> true
        | Node (v, l, r) -> if (car l) = (car r) then
            (aux l) && (aux r)
        else raise Break (* Short circuits as we found ineq *)
    in try (aux t) with Break -> false

(* Count the leaves of a binary tree. *)
let count_leaves t =
    let rec aux = function
        | Empty -> 0
        | Node (_, Empty, Empty) -> 1
        | Node (_, l, r) -> (aux r) + (aux l)
    in aux t

(* Collect the leaves of a binary tree in a list. *)
let leaves t =
    let rec aux = function
        | Empty -> []
        | Node (x, _, Empty) | Node (x, Empty, _) -> [x]
        | Node (_, l, r) -> (aux r) @ (aux l)
    in aux t

(* Collect the internal nodes of a binary tree in a list. *)
let internal_vals t =
    let rec aux = function
        | Empty | Node (_, _, Empty) | Node (_, Empty, _) -> []
        | Node (v, l, r) -> v :: (aux r) @ (aux l)
    in aux t

(* Collect the nodes at a given level in a list. *)
let nodes_at_depth t ~depth =
    let rec aux t c = if depth = c then [car t] else match t with
        | Empty -> []
        | Node (_, l, r) -> (aux l (c + 1)) @ (aux r (c + 1))
    in aux t 0 |> List.fold ~f:(fun acc x ->
        match x with None -> acc | Some x -> x :: acc)
