open Core (* For Heap *)
let fs = [ ("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5) ];;

(* # Huffman.encode ["a", 10;  "b", 15;  "c", 30;  "d", 16;  "e", 29];;
 * - : (string * string) list = [("d", "00"); ("a", "010"); ("b", "011"); ("e", "10"); ("c", "11")]
 *)

(* TODO Make functor with elt : match sig of comparable *)
module Huffman = struct

    type t = List.Assoc.t

    type tree =

    (* Simple qsort of pairs based on count as snd
     * # order_pairs [("a", 4); ("b", 7); ("c", 5); ("d", 2)];;
     * - : (string * int) list = [("d", 2); ("a", 4); ("c", 5); ("b", 7)]
     *)
    let rec order_pairs fp = match fp with
        | [] -> []
        | (s, c) :: tl -> let pred = fun (s', c') -> c >= c' in
            let lt, gte = List.partition_tf tl ~f:pred in
            (order_pairs lt) @ ((s, c) :: order_pairs gte)

    let gen_pq ord_pairs =
        let h = Heap.create () ~cmp:String.compare in
        List.fold pairs ~init:[] ~f:(
        fun (symbol, count) ->
        

    let gen_encoding pq =

    (* TODO Check *) (* TODO Better name *)
    let gen_pairs content ~fold ~equal =
        let inc counts elt =
            let c = match List.Assoc.find counts elt ~equal with
                | None -> 0
                | Some x -> x
            in List.Assoc.add ~equal counts elt (c + 1)
    in fold ~init:[] ~f:inc content

    let encode_pairs ps = order_pairs ps |> gen_pq |> gen_encoding

    let str_to_freq_pairs str = gen_pairs str ~fold:String.fold ~equal:String.equal

    let encode_str str = str_to_freq_pairs str |> encode

end
