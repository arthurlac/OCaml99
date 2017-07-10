exception Out_Of_Bounds
exception Bad_call

type 'a t = Empty | Cons of 'a * 'a t                                                                                                                                             exception Empty_list_car                                                                                                                                                          val car : 'a t -> 'a option

val car_exn : 'a t -> 'a
val cdr : 'a t -> 'a t
val filter : 'a t -> pred:('a -> bool) -> 'a t
val reverse : 'a t -> 'a t
val partition_tf : 'a t -> pred:('a -> bool) -> 'a t * 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val append : 'a t -> 'a t -> 'a t
val zip : 'a t -> 'b t -> ('a * 'b) t option
val penultimate : 'a t -> ('a * 'a) option
val pick : 'a t -> int -> 'a option
val len : 'a t -> int
val is_palindrome : 'a t -> bool
val match_head : elem:'a -> l:'a t -> bool
val destutter : 'a t -> 'a t
val pack : 'a t -> equal:('a -> 'a -> bool) -> ('a, int) Core.List.Assoc.t
val prep_n_times : n:int -> elt:'a -> tl:'a t -> 'a t
val unpack : ('a * int) t -> 'a t
val dupli : 'a t -> 'a t
val repli : 'a t -> int -> 'a t
val drop_every_nth : 'a t -> n:int -> 'a t
val split : 'a t -> n:int -> 'a t * 'a t
val slice : 'a t -> lb:int -> ub:int -> 'a t
val rotate : 'a t -> n:int -> 'a t
val drop_at_nth_exn : 'a t -> n:int -> 'a * 'a t
val insert_at_nth_exn : 'a t -> n:int -> elem:'a -> 'a t
val seq : int -> int -> int t
val rndm_extract : 'a t -> count:int -> 'a t * 'a t
val lotto : count:int -> ub:int -> int t
val permu : 'a t -> 'a t t
