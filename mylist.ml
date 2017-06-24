(*  Find the last but one box of a list.  *)
let penultimate = function
    | Empty -> None
    | Cons (hd, Cons (tl, Empty)) -> Some (hd, tl)
    | Cons (_, tl) -> penultimate tl


(* Find the K'th element of a list. *)
let pick l k =
    let rec pick_aux l k i = match l with
        | Empty -> None
        | Cons (hd, tl) -> if k = i then Some hd else pick_aux tl k (i + 1)
    in pick_aux l k 0


(* Find the number of elements of a list. *)
let len l = fold l ~init:0 ~f:(fun acc _ -> acc + 1);;


(* Reverse a list. *)
let rev l =
    let rev_aux new_l old_l = match old_l with
        | Empty -> new_l
        | Cons (hd, tl) -> rev_aux (Cons (hd, new_l)) tl
    in rev_aux Empty l


(* Find out whether a list is a palindrome. *)
let is_palindrome l = (=) l (rev l)


(* Eliminate consecutive duplicates of list elements. *)
let rec destutter l =
    let match_head ~elem ~l = match
        | car l with None -> false
        | Some x -> x = elem in
    match l with
    | Empty -> Empty
    | Cons (hd, tl) -> if match_head ~elem:hd ~l:tl
        then destutter tl
        else Cons (hd, destutter tl)

(* Run-length encoding of a list. *)
(* Decode a run-length encoded list. *)
(* Replicate the elements of a list a given number of times. *)
(* Drop every N'th element from a list. *)
(* Split a list into two parts; the length of the first part is given. *)
(* Extract a slice from a list. *)
(* Rotate a list N places to the left. *)
(* Remove the K'th element from a list. *)
(* Insert an element at a given position into a list. *)
(* Create a list containing all integers within a given range. *)
(*  Extract a given number of randomly selected elements from a list. *)
(* Lotto: Draw N different random numbers from the set 1..M. *)
(* Generate a random permutation of the elements of a list. *)
(* Generate the combinations of K distinct objects chosen from the N elements of a list *)
(* Group the elements of a set into disjoint subsets. *)
