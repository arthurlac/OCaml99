(* Binary trees *)
open Option.Monad_infix

type 'a t = Empty | Node of 'a * 'a t * 'a t

(* Sample data for debugging *)
let t1  = Node (1, Empty, Empty);;
let t2  = Node (2, t1, t1);;
let t3  = Node (3, t2, t2);;
let t23 = Node (4, t2, t3);;

let car   = function Empty -> None | Node (v, _, _) -> v
let left  = function Empty -> None | Node (_, l, _) -> Some l
let right = function Empty -> None | Node (_, _, r) -> Some r

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

(* Construct a binary search tree from a list of integer numbers.  *)
let from_list l =
    let (open Option) in (* for bind *)
    let split l = List.foldi ~init:([],[]) ~f:(fun ix (l, r) x ->
        if ix mod 2 = 0 then (x :: l, r) else (l, x :: r)) in
    let rec aux l = match l with
        | [] -> None
        | [x] -> Leaf x
        | x :: xs ->
            let l, r = split l in
            Node (x, , xs >>= aux)
    in aux l 

(* Construct height-balanced binary trees with a given number of nodes. *)

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
        | Node (x, Empty, Empty) -> [x]
        | Node (_, l, r) -> (aux r) @ (aux l)
    in aux t

(* Collect the internal nodes of a binary tree in a list. *)
let internal_vals t =
    let rec aux = function
        | Empty | Node (_, Empty, Empty) -> []
        | Node (v, l, r) -> v :: (aux r) @ (aux l)
    in aux t

(* Collect the nodes at a given level in a list. *)
let nodes_at_depth t ~depth =
    let rec aux t d c = if d = c then [car t] else match t with
        | Empty -> []
        | Node (_, l, r) -> (aux l (c + 1)) @ (aux r (c + 1)) in
    let res = aux t depth 0 in
    if List.length res = then None else Some res

(* Construct a complete binary tree. *)

(* Layout a binary tree. *)

(* A string representation of binary trees. *)

(* Preorder and inorder Osequences of binary trees. *)

(* Dotstring representation of binary trees. *)

