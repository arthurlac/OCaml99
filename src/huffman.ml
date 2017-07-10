open Core

module Huffman : sig
    val encode     : ('a * int) list -> ('a * string) list
    val encode_str : string -> (char * string) list
end = struct
    module HT : sig
        type 'a tree = Leaf of 'a * int | Node of int * 'a tree * 'a tree
        val sum   : 'a tree -> int
        val leaf  : 'a * int -> 'a tree
        val node  : 'a * int -> 'a * int -> 'a tree
        val merge : 'a tree -> 'a tree -> 'a tree
    end = struct
        type 'a tree = Leaf of 'a * int |  Node of int * 'a tree * 'a tree
        let sum t = match t with Leaf (_, s) -> s | Node (s, _, _) -> s
        let leaf p = Leaf (fst p, snd p)
        let node p p' = Node (snd p + snd p', leaf p, leaf p')
        let merge t t' = let s = sum t + sum t' in
            if sum t < sum t' then Node (s, t', t) else Node (s, t, t')
    end

    let gen_tree pairs =
        let rec aux acc ps = match ps with
            | hd :: nxt :: tl -> aux (HT.merge acc (HT.node hd nxt)) tl
            | hd :: tl -> HT.merge (HT.leaf hd) acc
            | [] -> assert false
        in match pairs with
            | p :: p' :: rest -> aux (HT.node p p') rest
            | _ -> assert false

    let gen_encoding tree =
        let rec aux acc = function
            | HT.Leaf (c, _) -> [(c, acc)]
            | HT.Node (_, l, r) -> aux (acc ^ "0") r @ aux (acc ^ "1") l
    in aux "" tree

    let str_to_freq_pairs str =
        let equal = Char.equal in
        let inc counts elt =
            let c = match List.Assoc.find counts elt ~equal with
                | None -> 0
                | Some x -> x
            in List.Assoc.add ~equal counts elt (c + 1)
    in String.fold str ~init:[] ~f:inc

    (* TODO *)
    let order_pairs ps = ps

    let encode ps      =                          order_pairs ps |> gen_tree |> gen_encoding
    let encode_str str = str_to_freq_pairs str |> order_pairs    |> gen_tree |> gen_encoding

end

open Huffman
