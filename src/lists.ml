open Core

type 'a t = Empty | Cons of 'a * 'a t

exception Empty_list_car
let car = function Empty -> None | Cons (hd, _) -> Some hd
let car_exn = function Empty -> raise Empty_list_car | Cons (hd, _) -> hd

(* TODO should make some/none? *)
let cdr = function
    | Empty -> Empty
    | Cons (_, tl) -> tl

let rec filter l ~pred = match l with
    | Empty -> Empty
    | Cons (hd, tl) -> if pred hd
        then Cons (hd, filter tl ~pred)
        else filter tl ~pred

let reverse l =
    let rec rev_aux new_l old_l = match old_l with
        | Empty -> new_l
        | Cons (hd, tl) -> rev_aux (Cons (hd, new_l)) tl
    in rev_aux Empty l

let partition_tf l ~pred =
    let rec partition_aux ~pred l l1 l2 = match l with
        | Empty -> (l1, l2)
        | Cons (hd, tl) -> if pred hd
            then partition_aux ~pred tl (Cons (hd, l1)) l2
            else partition_aux ~pred tl l1 (Cons (hd, l2))
    in partition_aux ~pred l Empty Empty

let rec map l ~f = match l with
    | Empty -> Empty
    | Cons (hd, tl) -> Cons (f hd, map tl ~f)

let fold l ~init ~f = 
    let rec fold_aux l acc ~f = match l with
        | Empty -> acc
        | Cons (hd, tl) -> fold_aux tl (f acc hd) ~f
    in fold_aux l init ~f

let append l1 l2 =
    let rl1 = reverse l1 in
    fold rl1 ~init:l2 ~f:(fun acc elt -> Cons (elt, acc))

let zip l1 l2 =
    let rec zip_aux zipped l1 l2 = match (car l1, car l2) with
        | (None, None) -> Some zipped
        | (None, _) | (_, None) -> None
        | (Some ll, Some lr) -> zip_aux (Cons ((ll, lr), zipped)) (cdr l1) (cdr l2)
    in zip_aux Empty l1 l2

(* Find the last but one box of a list.  *)
let rec penultimate = function
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
let len l = fold l ~init:0 ~f:(fun acc _ -> acc + 1)


(* Reverse a list. see reverse *)

(* Find out whether a list is a palindrome. *)
let is_palindrome l = (=) l (reverse l)


(* Eliminate consecutive duplicates of list elements. *)
let match_head ~elem ~l = match car l with
    | None -> false
    | Some x -> x = elem

let rec destutter l = match l with
    | Empty -> Empty
    | Cons (hd, tl) -> if match_head ~elem:hd ~l:tl
        then destutter tl
        else Cons (hd, destutter tl)

(* Run-length encoding of a list. *)
let pack l ~equal =
    let inc counts elt = 
        let c = match List.Assoc.find counts elt ~equal with
            | None -> 0
            | Some x -> x
        in List.Assoc.add ~equal counts elt (c + 1)
    in
    let rec aux l counts = match l with
        | Empty -> counts
        | Cons (hd, tl) -> aux tl (inc counts hd)
    in aux l []

(* Decode a run-length encoded list. *)
let prep_n_times ~n ~elt ~tl =
    let rec prep_aux c elt tl = match c with
        | 0 -> tl
        | _ -> prep_aux (c - 1) elt (Cons (elt, tl))
    in prep_aux n elt tl

let unpack counts =
    let rec aux acc (elt, c) = prep_n_times ~n:c ~elt ~tl:acc in
    fold counts ~init:Empty ~f:aux

(* Duplicate the elements of a list. *)
let dupli l =
    fold l ~init:Empty ~f:(fun acc hd -> prep_n_times ~n:2 ~elt:hd ~tl:acc)
    |> reverse

(* Replicate the elements of a list a given number of times. *)
let repli l n =
    fold l ~init:Empty ~f:(
        fun acc hd -> prep_n_times ~n ~elt:hd ~tl:acc
    ) |> reverse

(* Drop every N'th element from a list. *)
let drop_every_nth l ~n =
    let prep_or_drop ix hd tl = if n mod ix = 0 then tl else Cons (hd, tl) in
    let rec drop_aux l ix nl = match l with
        | Empty -> l
        | Cons (hd, tl) -> drop_aux tl (ix + 1) (prep_or_drop ix hd nl)
    in drop_aux (reverse l) 0 Empty

(* Split a list into two parts; the length of the first part is given. *)
let split l ~n =
    let rec aux left right ix = if ix = n then (left, right) else
        match right with
            | Empty -> (left, right)
            | Cons (hd, tl) -> aux (Cons (hd, left)) tl (ix + 1)
    in aux Empty l 0

(* Extract a slice from a list. *)
let slice l ~lb ~ub =
    split l ~n:lb |> snd |> split ~n:ub |> fst

(* Rotate a list N places to the left. *)
(* TODO check for off by one err *)
(* TODO bounds check *)
let rotate l ~n =
    let (left, right) = split l ~n in
    append right left

(* Remove the K'th element from a list. *)
exception Out_Of_Bounds
let drop_at_nth_exn l ~n =
    let rec aux l ix r = if ix = n then (car_exn l, append l (cdr r)) else
        match r with
            | Empty -> raise Out_Of_Bounds
            | Cons (hd, tl) -> aux (Cons (hd, l)) (ix + 1) tl
    in if n < 0 then raise Out_Of_Bounds else aux Empty 0 l

(* Insert an element at a given position into a list. *)
let insert_at_nth_exn l ~n ~elem =
    let rec aux l ix r = if ix = n then append l (Cons (elem, r)) else
        match r with
            | Empty -> raise Out_Of_Bounds
            | Cons (hd, tl) -> aux (Cons (hd, l)) (ix + 1) tl
    in if n < 0 then raise Out_Of_Bounds else aux Empty 0 l

(* Create a list containing all integers within a given range. *)
let seq lb ub =
    let rec aux lb ub l = if lb = ub then l else
        aux lb (ub - 1) (Cons (ub, l))
    in if lb <= ub then aux lb ub Empty else aux ub lb Empty

(* Extract a given number of randomly selected elements from a list. *)
let rndm_extract l ~count =
    let rec aux count l selected = if count <= 0 then (selected, l) else
        let rnd_ix = Random.int (len l) in
        let (pick, nl) = drop_at_nth_exn l ~n:rnd_ix in
        aux (count - 1) nl (Cons (pick, selected)) in
    aux count l Empty

(* Lotto: Draw N different random numbers from the set 1..M. *)
exception Bad_call
let lotto ~count ~ub = if count > ub then raise Bad_call else
    let (picks, _) = rndm_extract (seq 1 ub) ~count in
    picks

(* Generate a random permutation of the elements of a list. *)
let permu l =
    let rnd_ix = Random.int (len l) in
    let rec aux acc to_extract_from perms_left =
        if perms_left = 0 then acc else
            let selection, rem = rndm_extract ~count:1 to_extract_from in
            aux (Cons (selection, acc)) rem (perms_left - 1)
    in aux Empty l rnd_ix

(* Generate the combinations of K distinct objects chosen from the N elements of a list *)

(* Group the elements of a set into disjoint subsets. *)

