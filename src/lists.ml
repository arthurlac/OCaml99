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
let pack l ~equal =
    let inc counts elt = 
        let c = match List.Assoc.find counts elt ~equal with
            | None -> 0
            | Some x -> x
        in List.Assoc.add ~equal counts elt (c + 1)
    let aux l counts = match l with
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
    let aux acc (elt, c) = prep_n_times ~n:c ~elt ~tl:acc
        let elt_l = 
    in List.fold_right counts ~init:Empty ~f:aux

(* Duplicate the elements of a list. *)
let dupli l =
    Llist.fold_right l ~init:Empty ~f:(
        fun hd ~acc -> prep_n_times ~n:2 ~elt:hd ~tl:acc
    )

(* Replicate the elements of a list a given number of times. *)
let repli l n =
    Llist.fold_right l ~init:Empty ~f:(
        fun hd ~acc -> prep_n_times ~n ~elt:hd ~tl:acc
    )

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
    Llist.append right left

(* Remove the K'th element from a list. *)
exception Out_Of_Bounds
let drop_at_nth_exn l ~n =
    let rec aux l ix r = if ix = n then append l (cdr r) else
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
    let rec seq_aux lb ub l = if lb = ub then l else
        seq_aux lb (ub - 1) (Cons (ub, l))
    in if lb =< ub then seq_aux lb ub Empty else seq_aux ub lb Empty

(* Extract a given number of randomly selected elements from a list. *)
let rndm_extract l ~count =
    (* TODO Make what the return is more explicit *)
    let rec aux count l selected = if count =< 0 then (selected, l) else
        let rnd_ix = Random.int (len l) in
        let (pick, nl) = drop_at_nth l ~n:rnd_ix in
        aux (count - 1) nl (Cons (pick, selected)) in
    aux count l Empty

(* Lotto: Draw N different random numbers from the set 1..M. *)
let lotto ~count ~ub = 
    let (picks, _) = rndm_extract (seq 1 ub) ~count in
    picks

(* Generate a random permutation of the elements of a list. *)
let permu l =
    let rnd_ix = Random.int (len l) in
    let rec aux acc to_extract_from perms_left =
        if perms_left = 0 then acc else
            let selction, rem = rndm_extract to_extract_from in
            aux (selection :: acc) rem (perms_left - 1)
    in aux [] l rnd_ix

(* Generate the combinations of K distinct objects chosen from the N elements of a list *)

(* Group the elements of a set into disjoint subsets. *)

