(* Binary trees *)
open Option.Monad_infix

type 'a t =
    | Leaf of 'a
    | Node of 'a * 'a t * 'a t

(* Sample data for debugging *)
let t1  = Node (1, Empty, Empty);;
let t2  = Node (2, t1, t1);;
let t3  = Node (3, t2, t2);;
let t23 = Node (4, t2, t3);;

let car   = function Empty -> None | Node (v, _, _) -> Some v
let left  = function Empty -> None | Node (_, l, _) -> Some l
let right = function Empty -> None | Node (_, _, r) -> Some r

let lcar t = left t  >>= car
let rcar t = right t >>= car

(* Construct completely balanced binary trees. *)
(*let cons n v = *)

(* Symmetric binary trees. *)
exception Break

(* TODO A more tail recursive version *)
(* flat t |> is_palindrome *)
let is_sym t =
    let rec aux t = match t with
        | Empty -> true
        | Node (v, l, r) -> if (car l) = (car r) then
            (aux l) && (aux r)
        else raise Break (* Short circuits as we found ineq *)
    in try (aux t) with Break -> false

(* Binary search trees (dictionaries).
 * Construct a binary search tree from a list of integer numbers.
 *)
(* let cons l = *)

(* Generate-and-test paradigm. *)

(* Construct height-balanced binary trees. *)

(* Construct height-balanced binary trees with a given number of nodes. *)

(* Count the leaves of a binary tree. *)
let count_leaves t =
    let rec aux = function
        | Leaf of x -> 1
        | Node (_, l, r) -> (aux r) + (aux l)
    in aux t

(* Collect the leaves of a binary tree in a list. *)
let leaves t =
    let rec aux = function
        | Leaf of x -> [x]
        | Node (_, l, r) -> (aux r) @ (aux l)
    in aux t

(* Collect the internal nodes of a binary tree in a list. *)

(* Collect the nodes at a given level in a list. *)

(* Construct a complete binary tree. *)

(* Layout a binary tree. *)

(* A string representation of binary trees. *)

(* Preorder and inorder Osequences of binary trees. *)

(* Dotstring representation of binary trees. *)

