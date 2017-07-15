open Core

val is_prime : int -> bool

val primes_seq : int -> int list

val gcd : int -> int -> int

val coprime : int -> int -> bool

val seq : int -> int -> int list

val phi : int -> int

val prime_factors : int -> int list
val prime_factor_pairs : int -> (int, int) List.Assoc.t

val phi' : int -> int

val goldbach          : int -> int * int
val goldbach_w_primes : int -> int list -> int * int
val goldbach_list     : int -> int -> (int * int) list
