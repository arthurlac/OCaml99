open Core

type 'a t =
    | Empty
    | Cons of 'a * 'a t

let car = function
    | Empty -> None
    | Cons (hd, _) -> Some hd

let cdr = function
    | Empty -> None
    | Cons (_, tl) -> Some tl

let match_head ~elem ~l = match car l with
    | None -> false
    | Some elem2 -> elem = elem2

let filter l ~pred = match l with
    | Empty -> Empty
    | Cons (hd, tl) -> if pred hd
        then Cons (hd, filter tl ~pred)
        else fiter tl ~pred

let append l1 l2 =
    let rl1 = reverse l1 in
    fold rl1 ~init:l2 ~f:(fun elem ~acc -> Cons (elem, acc))

let partition_tf l ~pred =
    let rec partition_aux ~pred l l1 l2 = match l with
        | Empty -> (l1, l2)
        | Cons (hd, tl) -> if pred hd
            then partition_aux ~pred tl (Cons (hd, l1)) l2
            else partition_aux ~pred tl l1 (Cons (hd, l2))
    in partition_aux ~pred l Empty Empty

let map l ~f = match l with
    | Empty -> Empty
    | Cons (hd, tl) -> Cons (f hd, map tl ~f)
    (* TODO Tail recursion possible? *)
    (* Hold results in tail rec list, then cons all?*)

let fold l ~init ~f = 
    let rec fold_aux l acc ~f = match l with
        | Empty -> acc
        | Cons (hd, tl) -> fold_aux tl (f hd ~acc) ~f
    in fold_aux l init ~f

let to_my_list = function
    | [] -> Empty
    | x :: xs -> Cons (x, to_my_list xs)

let zip l1 l2 = 
    let zip_auxmatch (l1, l2) with
    | (Empty, Empty) -> Some zipl
    | (Empty, _) | (_, Empty) -> None
    | (Cons (x, xs), Cons (y, ys) -> Cons ((x, y), zip_aux xs ys)

(* quicksort *)
(* uneffecient mess currently *)
let rec sort_ints l = match l with
    | Empty -> Empty
    | Cons (hd, tl) ->
        let pred = fun y -> hd >= y in
        let lt, gte = partition_tf tl ~pred  in
        append (sort_ints lt) (Cons (hd, sort_ints gte))

(* Find the last box of a list. *)
let rec last = function
    | Empty -> None
    | Cons (hd, Empty) -> Some hd
    | Cons (_, tl) -> last tl


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
