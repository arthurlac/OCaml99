type 'a t = Empty | Cons of 'a * 'a t                                                                                                                                             val car : 'a t -> 'a option = <fun>                                                                                                                                               val cdr : 'a t -> 'a t = <fun>
val filter : 'a t -> pred:('a -> bool) -> 'a t = <fun>
val reverse : 'a t -> 'a t = <fun>
val partition_tf : 'a t -> pred:('a -> bool) -> 'a t * 'a t = <fun>
val map : 'a t -> f:('a -> 'b) -> 'b t = <fun>
val fold : 'a t -> init:'b -> f:('a -> acc:'b -> 'b) -> 'b = <fun>
val append : 'a t -> 'a t -> 'a t = <fun>
val zip : 'a t -> 'b t -> ('a * 'b) t option = <fun>
