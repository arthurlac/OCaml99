(* Determine whether a given integer number is prime. *)
exception Short_circuit
let is_prime n = if n <= 1 then false else if n <= 3 then true else
   if n mod 2 = 0 || n mod 3 = 0 then false else
   try 

(* Determine the greatest common divisor of two positive integer numbers. *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

(* Determine whether two positive integer numbers are coprime. *)
let coprime a b = (gcd a b) = 1

(* Calculate Euler's totient function phi(m). *)
let seq ~lb ~ub =
    let rec aux lb ub = if lb > ub then [] else lb :: aux (lb + 1) ub in
    aux lb ub

let phi n  = seq ~lb:1 ~ub:(n - 1)
    |> List.filter ~f:(fun x -> coprime x n)
    |> List.length

(* Determine the prime factors of a given positive integer. *)
let prime_factors n = 

(* Determine the prime factors of a given positive integer (2). *)
let prime_factors_mult_list n = 

(* A list of prime numbers. *)
let prime_list ~lb ~ub =

(*  Goldbach's conjecture says that every positive even number greater than 2
 *  is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the
 *  most famous facts in number theory that has not been proved to be correct
 *  in the general case. It has been numerically confirmed up to very large
 *  numbers (much larger than we can go). Write a predicate to find the two
 *  prime numbers that sum up to a given even integer.
 *  *)
let goldbach n m =
