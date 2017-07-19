open Core

(* Usage
  module G = Adjc_list(Int);;
  let g' = G.from_edge_list [1,2; 2,3; 2,4; 5,6];;
  G.nodes g';;
  - : int list = [6; 5; 4; 3; 2; 1]
  G.path g' 3 2;;
  - : bool = true
  G.path g' 3 5;;
  - : bool = false
*)

module type Node = sig
    type t
    include Hashable.S with type t := t

    val equal   : t -> t -> bool
    val compare : t -> t -> int
end

(* List all edges, an edge being a pair of nodes *)
module Edge_list (N : Node) = struct
  type node = N.t
  type t = (node * node) list

  let equal = N.equal

  let nodes t = List.fold t ~init:[]
      ~f:(fun acc (x, _) -> if (List.mem acc x ~equal) then acc else x :: acc)

  let from_edge_list el = el
  let to_edge_list el = el
end

(* Graph term form: nodes and edges *)
module Graph_term (N : Node) = struct
    module El = Edge_list(N)
    type node = El.node
    type t = { nodes : node list; edges : El.t }

    let nodes t = t.nodes

    let from_edge_list el =
      let edges = El.from_edge_list el in
      let nodes = El.nodes edges in
      { nodes ; edges }

    let to_edge_list t = El.to_edge_list t.edges
end

(* List of nodes and the nodes which they share an edge with *)
module Adjc_list (N : Node) : sig
  type node = N.t
  type t

  val nodes : t -> node list
  val from_edge_list : (node * node) list -> t
  val to_edge_list : t -> (node * node) list

  val is_connected : t -> node -> node -> bool
  val path         : t -> node -> node -> node list option

  val cycle : t -> node -> node -> node list option

  val degree : t -> node -> int option

  val dfs_traverse : t -> node -> node -> node list option

  val split_unconnected : t -> t list

  val is_bipartite : t -> node -> bool

  val gen_reg : int -> t

end = struct
    type node = N.t
    type t = (node, node list) Hashtbl.t

    let nodes = Hashtbl.keys

    let nodes_mem l n = List.mem ~equal:(N.equal) l n

    let from_edge_list el =
      let tbl = Hashtbl.create ~hashable:(N.hashable) () in
      (* TODO Comm *) (* TODO Adapt for directed graph *)
      let add_edge f t = (* f : from , t : to *)
          let neighbours = match Hashtbl.find tbl f with
            | None -> [] | Some l -> l
          in if nodes_mem neighbours t then () else
          Hashtbl.set tbl ~key:f ~data:(t :: neighbours)
      in
      List.iter el ~f:(fun (x, y) -> add_edge x y; add_edge y x);
      tbl

    let pair_eq (a, b) (c, d) =
      if a = c && b = d then true else
      if a = d && b = c then true else false

    let to_edge_list t =
      (* for each node find its edges *)
      Hashtbl.fold t ~init:[] ~f:(fun ~key ~data acc ->
        (* add NEW edges to acc *)
        List.fold data ~init:acc ~f:(fun acc x ->
            if List.mem ~equal:pair_eq acc (key, x)
            then acc
            else (x, key) :: acc
            )
        )

    (* TODO Clean *) (* TODO Review *)
    let is_connected g origin target =
      let rec bfs visited curn =
        if nodes_mem visited curn then false else
        match Hashtbl.find g curn with
          | None -> false
          | Some nodes ->
            if nodes_mem nodes target then true else
              let v' = curn :: visited in
              List.fold nodes ~init:false
                ~f:(fun acc n -> (bfs v' n) || acc)
      in bfs [] origin

    exception Ret of node list option

    let path g origin target =
      let rec bfs visited path curn =
        if nodes_mem visited curn then None else
        match Hashtbl.find g curn with
          | None -> None
          | Some nodes ->
            let p' = curn :: path in
            if nodes_mem nodes target then
              raise (Ret (Some (List.rev (target :: p'))))
            else
              let v' = curn :: visited in
              List.fold nodes ~init:None
                ~f:(fun _ n -> bfs v' p' n)
      in try bfs [] [] origin with Ret a -> a

    let cycle g n = failwith "uninmplemented"

    let degree g n =
      if not (Hashtbl.mem g n) then None else
        match Hashtbl.find g n with
        | None -> Some 0
        | Some ns -> Some (List.length ns)

    let dfs_traverse g n = failwith "uninmplemented"

    let split_unconnected g = failwith "uninmplemented"

    let is_bipartite g n = failwith "uninmplemented"

    let gen_reg k = failwith "uninmplemented"

end
