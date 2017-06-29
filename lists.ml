(* Find the last but one box of a list.  *)
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
let len l = Llist.fold l ~init:0 ~f:(fun acc _ -> acc + 1)


(* Reverse a list. *)
let rev l =
    let rev_aux new_l old_l = match old_l with
        | Empty -> new_l
        | Cons (hd, tl) -> rev_aux (Cons (hd, new_l)) tl
    in rev_aux Empty l


(* Find out whether a list is a palindrome. *)
let is_palindrome l = (=) l (Llist.rev l)


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
(* TODO *)
let pack l =
    let pack_aux l counts = in

(* Decode a run-length encoded list. *)
(* TODO *)
let unpack counts =

(* Duplicate the elements of a list. *)
let prep_n_times ~n hd tl =
    let rec prep_aux c hd tl = match c with
        | 0 -> tl
        | _ -> prep_aux (c - 1) hd (Cons (hd, tl))
    in prep_aux n hd tl

let dupli l =
    Llist.fold_right l ~init:Empty ~f:(fun hd ~acc -> prep_n_times ~n:2 hd acc)

(* Replicate the elements of a list a given number of times. *)
let repli l n =
    Llist.fold_right l ~init:Empty ~f:(fun hd ~acc -> prep_n_times ~n hd acc)

(* Drop every N'th element from a list. *)
let drop_every_nth l ~n =
    let prep_or_drop ix hd tl = if n mod ix = 0 then tl else Cons (hd, tl)
    let rec drop_aux l ix nl = match l with
        | Empty -> l
        | Cons (hd, tl) -> drop_aux tl (ix + 1) (prep_or_drop ix hd nl) in
    drop_aux (reverse l) 0 Empty

(* Split a list into two parts; the length of the first part is given. *)
(* TODO Clean *)
let split l ~n =
    let rec split_aux left right dec = if dec <= 0
        then ((reverse left), right) else match right with
            | Empty -> ((reverse left), right)
            | Cons (hd, tl) -> split_aux (Cons (hd, left)) tl (dec - 1)
    in split_aux Empty l n

(* Extract a slice from a list. *)
(* TODO *)
let slice l ~lb ~ub =
    let slice_aux l lb ub ix = if ix <
        if ix = ub then Some l else match l with
            | Empty -> None
            | Cons (hd, tl) ->

(* Rotate a list N places to the left. *)
(* TODO check for off by one err *)
(* TODO bounds check *)
let rotate l ~n =
    let (left, right) = split l ~n in
    Llist.append right left

(* Remove the K'th element from a list. *)
(* TODO *)
let drop_at_nth l ~n =
    let aux l ix r = if ix = n then (car r, append l r) else
        match 

(* Insert an element at a given position into a list. *)
(* TODO append at end if n > len l ? insert_at_nth_exn *)
(* TODO see if an be built in order to not need rev *)
(* TODO check n => 0 *)
let insert_at_nth l ~n ~elem =
    let rec aux l ix r = if ix = n then append l (Cons (elem, r)) else
        match l with
            | Empty -> Cons (elem, r)
            | Cons (hd, tl) -> aux tl (ix + 1) (Cons (hd, r))
    in rev (aux l 0 Empty)
  (*in      aux Empty 0 l XXX see todo no2 *)


(* Create a list containing all integers within a given range. *)
let seq ~lb ~ub =
    let rec seq_aux lb ub l = if lb = ub then l else
        seq_aux lb (ub - 1) (Cons (ub, l))
    in if lb =< ub then seq_aux lb ub Empty else seq_aux ub lb Empty

(* Extract a given number of randomly selected elements from a list. *)
let rndm_extract l ~count =
    (* TODO Make what the return is more explicit *)
    let rec aux count l selected = if count =< 0 then (selected, l) else
        let rnd_ix = Random.int (len l) in
        let (pick, nl) = drop_at_nth l ~n:rnd_ix in
        lotto_aux (count - 1) nl (Cons (pick, selected)) in
    aux count l Empty

(* Lotto: Draw N different random numbers from the set 1..M. *)
let lotto ~count ~ub = 
    let (picks, _) = rndm_extract (seq ~lb:1 ~ub) ~count in
    picks

(* Generate a random permutation of the elements of a list. *)
let permu l =

(* Generate the combinations of K distinct objects chosen from the N elements of a list *)

(* Group the elements of a set into disjoint subsets. *)

