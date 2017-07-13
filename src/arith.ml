open Core

(* Determine whether a given integer number is prime. *)
let int_sqrt x = Float.iround_towards_zero_exn (sqrt (float_of_int x))

(* TODO Clean *)
let is_prime n =
    let rec aux div = if div * div <= n then true else
        if n mod div = 0 || n mod (div + 2) = 0 then false
        else aux (div + 6)
    in
    if n <= 1                     then false else
    if n <= 3                     then true  else
    if n mod 2 = 0 || n mod 3 = 0 then false else aux 5

(* A list of prime numbers. *)
(* TODO comm. *)
let is_prime_array n =
    (* Let ints be an array of Boolean values, indexed by integers
     * 0 to n, initially all set to true. Of course 0 and 1 are not
     * primes and as such must be set to false. *)
    let ints = Array.create (n + 1) true in
    ints.(0) <- false;
    ints.(1) <- false;
    let lim = int_sqrt n in
    for i = 2 to lim do
        if ints.(i) then
            let multiple = ref (i * i) in
            while (!multiple <= n) do
                ints.(!multiple) <- false;
                multiple := !multiple + i (* next multiple *)
            done
    done;
    ints

let primes_seq n = is_prime_array n
    |> Array.foldi ~init:[] ~f:(fun ix acc is_prime ->
        if is_prime then (ix :: acc) else acc)
    |> List.rev (* TODO Maybe faster to rev array than list ? *)

(* TODO Benchmark
 * seq 1 n |> List.filter ~f:is_prime
 * vs
 * primes_seq n
 *)

(* Determine the greatest common divisor of two positive integer numbers. *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

(* Determine whether two positive integer numbers are coprime. *)
let coprime a b = (gcd a b) = 1

(* Calculate Euler's totient function phi(n). *)
let seq lb ub =
    let rec aux lb ub = if lb > ub then [] else lb :: aux (lb + 1) ub in
    if lb < ub then aux lb ub else aux ub lb

let phi n = seq 1 (n - 1)
    |> List.filter ~f:(fun x -> coprime x n)
    |> List.length

(* Determine the prime factors of a given positive integer. *)
(* Note this can't handle large numbers due to needing lists, consider array ver *)
let prime_factors n =
    let primes = primes_seq n in (* TODO Assert list in asc order *)
    let rec aux cur ps fs = match cur with (* TODO rm unrolling, is nesc? *)
        | 0 | 1 -> fs
        | 2 | 3 | 5 -> cur :: fs
        | 4 -> 2 :: 2 :: fs
        | _ -> match ps with
            | [] -> assert false (* BAD *) (* TODO rm this *)
            | hd :: tl -> if cur mod hd <> 0
                then aux cur tl fs
                else aux (cur / hd) ps (hd :: fs)
    in aux n (List.rev primes) []

(* Determine the prime factors of a given positive integer (2). *)
let pack l =
    let equal = Int.equal in
    let inc counts elt =
        let c = match List.Assoc.find ~equal counts elt with
            | None -> 0
            | Some x -> x
        in List.Assoc.add ~equal counts elt (c + 1)
    in List.fold l ~init:[] ~f:inc

let prime_factor_pairs n = prime_factors n |> pack

(* Calculate Euler's totient function phi(n) (improved).
 * If the list of the prime factors of a number m is known in the form
 * of [(p, e); ...] where p is the prime factor and e is the exponent
 * then the function phi(n) can be efficiently calculated as follows:
 *  phi n = (((p1 - 1) * p1) ** (e1 - 1)) + ...
 *)
let int_exp x y = (float_of_int x) ** (float_of_int y) |> int_of_float
let phi' n =
    let phi_fn acc (p, e) = acc * ((p - 1) * (int_exp p (e - 1))) in
    prime_factor_pairs n |> List.fold ~init:1 ~f:phi_fn

(*  Goldbach's conjecture says that every positive even number greater than 2
 *  is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the
 *  most famous facts in number theory that has not been proved to be correct
 *  in the general case. It has been numerically confirmed up to very large
 *  numbers (much larger than we can go). Write a function to find the two
 *  prime numbers that sum up to a given even integer.
 *) (* TODO Clean *)
exception Break2 of int * int
let goldbach n = if n mod 2 <> 0 || n < 2 then assert false else
    let primes = primes_seq n in
    let rec aux ps = match ps with
        | [] -> assert false (* u just disproved the golbach conjecture congrats *)
        | hd :: tl -> List.iter primes ~f:(fun x ->
            if hd + x = n then raise (Break2 (hd, x)) else ());
        aux tl
    in try aux (List.rev primes) with Break2 (a, b) -> (a, b)


(* A list of Goldbach compositions. *)
(* TODO Memoify ? *) (* TODO feed primes list into goldbach *)
let goldbach_list lb ub = seq lb ub
    |> List.filter ~f:(fun x -> x mod 2 = 0 && x > 2) (* Rewrite seq to avoid filter ? *)
    |> List.map ~f:(fun x -> goldbach x)
