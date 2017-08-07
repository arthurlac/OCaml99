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
  val edges      : t -> node -> (node * node) list option

  val first_node : t -> node option

  val empty : unit -> t
  val from_edge_list : (node * node) list -> t
  val to_edge_list   : t -> (node * node) list

  val has_node   : t -> node -> bool
  val add_edge   : t -> node -> node -> dir:bool -> unit

  val is_connected : t -> node -> node -> bool
  val path         : t -> node -> node -> node list option

  val node_cycle : t -> node -> bool

  val cycle : t -> bool

  val s_tree : t -> node -> t option

  val isomorphism : t -> t -> bool

  val degree       : t -> node -> int option

  val dfs : t -> node -> node -> node list option

  val split_unconnected : t -> t list

  val is_bipartite : t -> bool

  val gen_reg : int -> t

end = struct
    type node = N.t
    type t = (node, node list) Hashtbl.t

    exception Ret of (node list option)
    exception No_neighbours

    let nodes      = Hashtbl.keys
    let neighbours g n = Hashtbl.find g n
    let edges g n =
      match Hashtbl.find g n with
      | None -> None
      | Some ns -> Some (
          List.map ns ~f:(fun n' -> (n, n')))
    let edges_exn g n =
      match Hashtbl.find g n with
      | None -> raise No_neighbours
      | Some ns -> List.map ns ~f:(fun n' -> (n, n'))

    let first_node g = Hashtbl.keys g |> List.hd

    let has_node = Hashtbl.mem

    let nodes_mem l n = List.mem ~equal:(N.equal) l n

    let rec add_edge g f t ~dir = (* f : from , t : to *)
      if not dir then
        let ns = match neighbours g f with
          | None -> [] | Some l -> l
        in
        if nodes_mem ns t
        then ()
        else Hashtbl.set g ~key:f ~data:(t :: ns)
      else begin
        add_edge g f t ~dir:false;
        add_edge g t f ~dir:false
      end

    let from_edge_list el =
      let tbl = Hashtbl.create ~hashable:(N.hashable) () in
      (* TODO Comm *) (* TODO Adapt for directed graph *)
      List.iter el ~f:(fun (x, y) -> add_edge ~dir:false tbl y x);
      tbl

    let empty () = from_edge_list []

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
      (* TODO Handle cycles *)
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
      (* TOOD Dedup *)
      let merge a b = a @ b in
      let make_edges node neighbours =
        List.map neighbours ~f:(fun n -> (node, n))
      in
      let is_back_edge curn curn_neighbours edges_known visited =
        (* See if we are revisiting a node *)
        if List.exists curn_neighbours ~f:(fun n -> nodes_mem visited n)
        (* If we are check it's not via a known edge *)
        then List.exists curn_neighbours ~f:(fun n ->
            not (List.mem edges_known (curn, n) ~equal:pair_eq))
        else false
      in
      let rec aux visited_nodes edges_known to_visit =
        match to_visit with [] -> false | curn :: t_v' ->
          let ns = match Hashtbl.find g curn with
            | None -> []
            | Some ns -> ns in
          if is_back_edge curn ns edges_known visited_nodes
          then true
          else aux
              (merge ns visited_nodes)
              (merge edges_known (make_edges curn ns))
              (merge to_visit ns)
      in aux [] [] [origin]

    let cycle g = nodes g |> List.exists ~f:(fun n -> node_cycle g n)

    let s_tree g node =
      let g' = empty () in
      let rec bfs to_vis =
        match to_vis with
        | [] -> g'
        | (curn, next) :: rest ->
          if has_node g' next
          then bfs rest
          else
            let _ = add_edge ~dir:false g' curn next in
            let es = edges_exn g next in
            bfs (rest @ es)
      in
      match neighbours g node with
      | None | Some [] -> None
      | Some (n :: _) -> Some (bfs [(node, n)])

    let isomorphism g h =

      let rec



      let g_n = nodes g in
      let h_n = nodes h in
      let open Hashtbl in
      (* find a mismatch *)
      let aux () =  not List.exists2 g_n h_n ~f:(fun gx hx ->
          let gxn, hxn = find_exn g gx, find_exn h hx =
      ) in try aux () with Invalid_argument _ -> false

    let degree g n = match Hashtbl.find g n with
      | None -> None
      | Some ns -> Some (List.length ns)

    let set_add_list s l = List.fold l ~init:s ~f:(fun s nd -> Set.add s nd)

    let dfs g origin target =
      (* Set up a stack to hold nodes to visit *)
      let stack = Stack.create () in
      let _ = Stack.push stack origin in
      (* Recurse on that stack *)
      let rec search visited path curn =
        if nodes_mem visited curn
        then vis_next visited path
        else match Hashtbl.find g curn with
          | None -> vis_next visited path
          | Some ns ->
            let p' = curn :: path in
            (* Find target or move thru stack *)
            if nodes_mem ns target then Some (List.rev (target :: p'))
            else begin
              let _ = Stack.pop stack in
              List.iter ns ~f:(fun n -> Stack.push stack n);
              let v' = curn :: visited in
              vis_next v' p'
            end
      and vis_next visited path =
        match Stack.pop stack with
        | None -> None
        | Some n -> search visited path n
      in search [] [] origin

    let known sets n = List.exists ~f:(fun s -> Set.mem s n)

    (* Finds membership of node in sets
     * Return None
     * or Some S::R
     *   where S is the set containing node
     *   and R is the other sets.
     * *)
    let which node sets =
      let rec aux sl acc = match sl with
        | [] -> None
        | s' :: r -> if Set.mem s' node
          then Some (s' :: (r @ acc))
          else aux r (s' :: acc)
      in aux sets []

    (* Finds membership of nodes in sets
     * same return as which *)
    let which_of_many nodes sets =
      let rec aux ns = match ns with
        | [] -> None
        | n :: r -> match which n sets with
          | None -> aux r
          | Some x -> Some x
      in aux nodes

    let split_unconnected g = failwith "unimpl"
    (*
    let split_unconnected g =
      let new_set_from_list sets l =
        let s = Set.empty ~comparator:N.comparator in
        let new_set = set_add_list s l in
        new_set :: sets
      in
      (* TODO *)
      (* When we add new nodes to a set it may cause them to
       * link to each other thus we should merge them. *)
      (* This is O(n^2) only run when sets r modified. I.e. case 1/2 *)
      let coalsce sets =






        let rec aux is sets acc = match to_check with
          | [] -> acc
          | s :: r ->
            if empty_intersect s is
            then s :: acc
            else (merge s sx) :: acc


      in
      let rec aux sets n =
        (* Find neighbours, exn as we only come from known nodes *)
        let ns = Hashtbl.find_exn g n in
        match which n sets with
        (* Case 0: Node is in known set. Push neighbours and coalesce *)
        (* new connections may result from adding neighbours. *)
        | Some (set :: rest) -> let set' = set_add_list set ns in (set' :: rest) |> coalesce
        | Some [] -> assert false (* Can only find matching set if it has > 1 elt *)
        | None ->
          match which_of_many ns sets with
          (* Case 1: Node neighbour is in known set *)
          (* We can treat this similarly to case 0 and save some work *)
       (* | Some neighbour -> aux sets neighbour *)
          | Some (set :: rest) -> let set' = set_add_list set ns in (set' :: rest) |> coalesce
          (* Case 2: Node and neighbours are not in any set already, create new set *)
          | None -> (new_set_from_list (n :: ns) ) :: sets
      in nodes g |> List.fold ~init:[] ~f:aux
           *)

    let is_bipartite g = match first_node g with
      | None -> false
      | Some n ->
        let disj a b = Set.diff a b |> Set.length |> (fun l -> l > 0) in
        let to_set l = Set.empty ~comparator:N.comparator |> set_add_list in
        let init_u = [n] |> to_set in (* contains first node *)
        let init_v = neighbours g n |> to_set in (* contains first nodes neighbours *)
        (* Try to place a neighbour we can use to join *)
        let place n ns u v =
          (* Return aux fn *)
          let ret to_add_n to_add_ns =
            ( Set.add      to_add_n  n
            , set_add_list to_add_ns ns
            )
          in
          let rec p_aux to_check = match to_check with
            | [] -> None
            | hd :: tl -> match Set.mem u hd, Set.mem v hd with
              (* we can not keep the sets disjoint *)
              | true, true -> None
              (* no links, try next neighbour *)
              | false, false -> p_aux tl
              (* IF we can link a neighbour of n to set alpha and not beta
               * THEN n must be in set beta and and neighbours in alpha.
               * Afterwards we need to check alpha' and beta'
               * are disjoint where alpha' and beta' include the addition
               * of ns and n respectively.
               *)
              | true, false -> Some (ret u v)
              | false, true -> Some (ret v u)
          in p_aux ns
        in
        let rec check u v to_check = match to_check with
          | [] -> true
          | hd :: tail ->
            match Set.mem u hd, Set.mem v hd with
            (* Node in both u & v, thus not bipartite *)
            | true, true -> false
            (* Node already in u |X| v, place next node *)
            | true, false | false, true -> check u v tail
            (* place the node in u or v *)
            | false, false ->
              let ns = neighbours g hd in
              match place hd ns u v with
              (* no possible placement, not bipartite *)
              | None -> false
              (* next node to check if disjoint sets can be maintained *)
              | Some (u', v') -> if disj u v then false else check u v tail
        in check init_u init_v nodes

    let gen_reg k = failwith "uninmplemented"

end
