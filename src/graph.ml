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

  val nodes      : t -> node list
  val neighbours : t -> node -> node list option

  val from_edge_list : (node * node) list -> t
  val to_edge_list   : t -> (node * node) list

  val is_connected : t -> node -> node -> bool
  val path         : t -> node -> node -> node list option

  val cycle : t -> node -> bool

  val s_tree : t -> node list
  val mst    : t -> node list

  val isomorphism : t -> t -> bool

  val degree       : t -> node -> int option
  val node_degrees : t -> int list

  val dfs_traverse : t -> node -> node -> node list option

  val split_unconnected : t -> t list

  val is_bipartite : t -> node -> bool

  val gen_reg : int -> t

end = struct
    type node = N.t
    type t = (node, node list) Hashtbl.t

    exception Ret of (node list option)

    let nodes          = Hashtbl.keys
    let neighbours g n = Hashtbl.find g n

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

    (* For a cycle we need to rediscover a node via a new edge *)
    let node_cycle g origin =
      let merge a b ~compare =
        a @ b |> List.dedup ~compare in
      let make_edges node neighbours =
        List.map neighbours ~f:(fun n -> (node, n)) in
      let is_back_edge curn curn_neighbours edges_known visited =
        (* See if we are revisiting a node *)
        if List.exists curn_neighbours ~f:(fun n -> nodes_mem visited n)
        (* If we are check it's not via a known edge *)
        then List.exists curn_neighbours ~f:(fun n ->
            not List.mem edges_known (curn, n) ~equal:pair_eq)
        else false in
      let rec aux visited_nodes edges_known to_visit =
        match to_visit with [] -> false | curn :: t_v' ->
          let ns = match Hashtbl.find g curn with
            | None -> []
            | Some ns -> ns in
          if is_back_edge edges_known then true
          else aux
              (merge ns visited_nodes)
              (merge edges_known (make_edges curn ns))
              (merge to_visit ns)
      in aux [] [] [origin]

    let cycle g = nodes g |> List.exists ~f:(fun n -> node_cycle g n)

    let s_tree g = failwith "uninmplemented"

    let mst g = failwith "uninmplemented"

    let isomorphism g h = failwith "unimplemented"

    let degree g n = match Hashtbl.find g n with
      | None -> None
      | Some ns -> Some (List.length ns)

    let node_degrees g = nodes g
      |> List.map ~f:(fun n -> degree g n)
      |> List.fold ~init:[] ~f:(fun acc n_d ->
          match n_d with
          | Some d -> (d :: acc)
          | None -> assert false)

    let dfs g origin target =
      (* Set up a stack to hold nodes to visit *)
      let stack = Stack.create () in
      let _ = Stack.push stack origin in
      (* Recurse on that stack *)
      let rec search visited path curn =
        if nodes_mem visited curn
        then vis_next visited path
        else match Hashtbl.find g curn with
          | None -> None (* Deadend *)
          | Some ns ->
            let p' = curn :: path in
            (* Find target or move thru stack *)
            if nodes_mem ns target then Some (List.rev (target :: p'))
            else begin
              Stack.pop stack;
              List.iter ns ~f:(fun n -> Stack.push stack n);
              let v' = curn :: visited in
              vis_next v' p'
            end
      and vis_next visited path =
        match Stack.pop stack with
        | None -> None
        | Some n -> search visited path n
      in search [] [] origin

    (* Could short circuit, less terse *)
    let known sets n = List.exists ~f:(fun s -> Set.mem s n)

    let split_unconnected g =
      (* Try to place node in a set or create new set *)
      let rec aux sets node =
        if known sets n then acc else (* find_set_or_create sets n *)
          (* If we don't know it then look at neighbours
           * and match neighbours to known sets. Make sure to look at all
           * neighbours and merge sets when ness.
           *)
          match Hashtbl.find g n with
          (* This case only possible in directed graph *)
          | None -> assert fals
          | Some ns ->
            List.fold ~init ~f:(fun acc x ->
              if

      in nodes g |> List.fold ~init:[] ~f:aux |> reconnect_sets

    let is_bipartite g n = failwith "uninmplemented"

    let gen_reg k = failwith "uninmplemented"

end
