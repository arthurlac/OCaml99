open Core

let car = function
    | Empty -> None
    | Cons (hd, _) -> Some hd

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
    (* TODO Tail recursion possible? *)
    (* Hold results in tail rec list, then cons all?*)

let fold l ~init ~f = 
    let rec fold_aux l acc ~f = match l with
        | Empty -> acc
        | Cons (hd, tl) -> fold_aux tl (f hd ~acc) ~f
    in fold_aux l init ~f

let append l1 l2 =
    let rl1 = reverse l1 in
    fold rl1 ~init:l2 ~f:(fun elem ~acc -> Cons (elem, acc))

let zip l1 l2 =
    let rec zip_aux zipped l1 l2 = match (car l1, car l2) with
        | (None, None) -> Some zipped
        | (None, _) | (_, None) -> None
        | (Some ll, Some lr) -> zip_aux (Cons ((ll, lr), zipped)) (cdr l1) (cdr l2)
    in zip_aux Empty l1 l2

(* quicksort TODO review
let rec sort_ints l = match l with
    | Empty -> Empty
    | Cons (hd, tl) ->
        let pred = fun y -> hd >= y in
        let lt, gte = partition_tf tl ~pred  in
        append (sort_ints lt) (Cons (hd, sort_ints gte))
*)
