type 'a t = Empty | Node of 'a * 'a t * 'a t                                                                                                                                      val t1 : int t = Node (1, Empty, Empty)                                                                                                                                           val t2 : int t = Node (2, Node (1, Empty, Empty), Node (1, Empty, Empty))

val car : 'a t -> 'a option
val left : 'a t -> 'a t option
val right : 'a t -> 'a t option
val lcar : 'a t -> 'a option
val rcar : 'a t -> 'a option

val is_sym : 'a t -> bool
val count_leaves : 'a t -> int
val leaves : 'a t -> 'a list
val internal_vals : 'a t -> 'a list
val nodes_at_depth : 'a t -> depth:int -> init:'a list -> 'a list
