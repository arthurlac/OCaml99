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
    let zip_aux = match (l1, l2) with
        | (Empty, Empty) -> Some zipl
        | (Empty, _) | (_, Empty) -> None
        | (Cons (x, xs), Cons (y, ys) -> Cons ((x, y), zip_aux xs ys) in


(* quicksort *)
(* uneffecient mess currently *)
let rec sort_ints l = match l with
    | Empty -> Empty
    | Cons (hd, tl) ->
        let pred = fun y -> hd >= y in
        let lt, gte = partition_tf tl ~pred  in
        append (sort_ints lt) (Cons (hd, sort_ints gte))
