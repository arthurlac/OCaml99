# ~99 problems in OCaml

Based on [this problem set](http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html) originally for lisp.

## Lists
```ocaml
(* Base type and functions *)
type 'a t = Empty | Cons of 'a * 'a t
val car : 'a t -> 'a option
val car_exn : 'a t -> 'a
val cdr : 'a t -> 'a t
val filter : 'a t -> pred:('a -> bool) -> 'a t
val reverse : 'a t -> 'a t
val partition_tf : 'a t -> pred:('a -> bool) -> 'a t * 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val append : 'a t -> 'a t -> 'a t
val zip : 'a t -> 'b t -> ('a * 'b) t option

(* Find the last box of a list. *)
val last : 'a -> 'a option

(* Find the last but one box of a list.  *)
val penultimate : 'a t -> ('a * 'a) option

(* Find the K'th element of a list. *)
val pick : 'a t -> int -> 'a option

(* Find the number of elements of a list. *)
val len : 'a t -> int

(* Reverse a list. *)
val reverse : 'a t -> 'a t

(* Find out whether a list is a palindrome. *)
val is_palindrome : 'a t -> bool

(* Eliminate consecutive duplicates of list elements. *)
val destutter : 'a t -> 'a t

(* Run-length encoding of a list. *)
val pack : 'a t -> equal:('a -> 'a -> bool) -> ('a, int) List.Assoc.t

(* Decode a run-length encoded list. *)
val unpack : ('a * int) t -> 'a t

(* Replicate the elements of a list a given number of times. *)
val repli : 'a t -> int -> 'a t

(* Drop every N'th element from a list. *)
val drop_every_nth : 'a t -> n:int -> 'a t

(* Split a list into two parts; the length of the first part is given. *)
val split : 'a t -> n:int -> 'a t * 'a t

(* Extract a slice from a list. *)
val slice : 'a t -> lb:int -> ub:int -> 'a t

(* Rotate a list N places to the left. *)
val rotate : 'a t -> n:int -> 'a t

(* Remove the K'th element from a list. *)
val drop_at_nth_exn : 'a t -> n:int -> 'a * 'a t

(* Insert an element at a given position into a list. *)
val insert_at_nth_exn : 'a t -> n:int -> elem:'a -> 'a t

(* Create a list containing all integers within a given range. *)
val seq : int -> int -> int t

(* Extract a given number of randomly selected elements from a list. *)
val rndm_extract : 'a t -> count:int -> 'a t * 'a t

(* Lotto: Draw N different random numbers from the set 1..M. *)
val lotto : count:int -> ub:int -> int t

(* Generate a random permutation of the elements of a list.*)
val permu :

(* Generate the combinations of K distinct objects chosen from the N elements of a list*)
(* Group the elements of a set into disjoint subsets.*)
```


## Arith
```ocaml
(* Is prime predicate *)
val is_prime : int -> bool

(* Prime sequence generator *)
val primes_seq : int -> int list

(* Greatest common denominator *)
val gcd : int -> int -> int

(* Coprime predicate *)
val coprime : int -> int -> bool

(* Euler's totient function *)
val phi : int -> int

(* Prime factorisation *)
val prime_factors : int -> int list
val prime_factor_pairs : int -> (int, int) List.Assoc.t

(* Improved phi function. See phi benchmark file. *)
val phi' : int -> int

(* Goldbachs conjecture *)
val goldbach          : int -> int * int
val goldbach_w_primes : int -> int list -> int * int
val goldbach_list     : int -> int -> (int * int) list
```


## Logic
Boolean expr language
```ocaml
module Expr : sig
    type t =
      | And of t * t
      | Or of t * t
      | Not of t
      | Var of string
      | Const of bool
    type ctxt = (string * bool) list
    type result = bool
    val eval : t -> (string * result) list -> result
    val simplify : t -> t
    val find_vars : t -> string list
    val gen_truth_table : t -> (ctxt * result) list
    val print_truth_table : t -> unit
end
```

Gray code and memoised gray code
```ocaml
val gray_code        :         int -> string list
(* returns a function which memoises previous work*)
val gray_code_fn_gen : unit -> int -> string list
```

Huffman encoding
```ocaml
module Huffman :sig
    val encode : ('a * int) list -> ('a * string) list
    val encode_str : string -> (char * string) list
end
```


## Binary trees

## Graph
