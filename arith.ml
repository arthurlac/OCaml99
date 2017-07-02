(* Determine whether a given integer number is prime. *)
(* TODO *)
exception Short_circuit
let is_prime n = if n <= 1 then false else if n <= 3 then true else
   if n mod 2 = 0 || n mod 3 = 0 then false else
   try 

(* Determine the greatest common divisor of two positive integer numbers. *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

(* Determine whether two positive integer numbers are coprime. *)
let coprime a b = (gcd a b) = 1

(* Calculate Euler's totient function phi(n). *)
let seq lb ub =
    let rec aux lb ub = if lb > ub then [] else lb :: aux (lb + 1) ub in
    aux lb ub

let phi n  = seq 1 (n - 1)
    |> List.filter ~f:(fun x -> coprime x n)
    |> List.length

(* Determine the prime factors of a given positive integer. *)
(* TODO Clean *)
let prime_factors n =
    let primes = primes_seq n in
    let rec aux cur_n ps fs =
        (* TODO Clean up this pred both may not be ness idk *)
        (* len predicate rm may need no (hd|tl)_exns used below *)
        if List.length ps < 1 || cur_n <= 1 then fs
        else if cur_n mod div = 0 then
            aux (cur_n / div) ps (List.hd_exn ps :: fs)
        else
            aux cur_n (List.tail_exn ps) fs
    in aux n (List.rev primes) []

(* Determine the prime factors of a given positive integer (2). *)
(* TODO I feel like this could be more clean *)
let pack l =
    let equal = Int.equal in
    let inc counts elt =
        let c = match List.Assoc.find counts elt ~equal with
            | None -> 0
            | Some x -> x
        in List.Assoc.add ~equal counts elt (c + 1)
    in List.fold l ~init:[] ~f:inc

let prime_factor_pairs n = prime_factors n
    |> pack

(* Calculate Euler's totient function phi(n) (improved).
 * If the list of the prime factors of a number m is known in the form
 * of [(p, e); ...] where p is the prime factor and e is the exponent
 * then the function phi(n) can be efficiently calculated as follows:
 *  phi n = (((p1 - 1) * p1) ** (e1 - 1)) + ...
 *)
let int_exp x y = (float_of_int x) ** (float_of_int y) |> int_of_float
let phi' n = prime_factor_pairs n
    |> List.fold ~init:0 ~f:(fun acc (p, e) ->
        acc + (int_exp ((p - 1) * p) (e - 1))
    )

(* A list of prime numbers. *)
(* TODO *)
let primes_seq ub =
    let rec aux div primes = if div = 666 then primes else
        List.filter primes ~f:(fun x -> x <> div && x mod div <> 0)
        |> aux (div + 1)
    in aux 2 (seq ~lb:2 ~ub)

(*  Goldbach's conjecture says that every positive even number greater than 2
 *  is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the
 *  most famous facts in number theory that has not been proved to be correct
 *  in the general case. It has been numerically confirmed up to very large
 *  numbers (much larger than we can go). Write a predicate to find the two
 *  prime numbers that sum up to a given even integer.
 *  *)
let goldbach n =

(* A list of Goldbach compositions. *)
let goldbach_list ~lb ~ub = seq ~lb ~ub
    |> List.map ~f:(fun x -> goldbach x)
