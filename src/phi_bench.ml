(* Compare the two methods of calculating Euler's totient function. *)
open Core
open Core_bench

let iter ~f ~min ~max : unit =
    let rec aux cur = if cur = max then () else let _ = f cur in aux (cur + 1)
    in aux min

let tests ~min ~max =
  let test name f = Bench.Test.create f ~name in
  [ test "phi"   (fun () -> iter ~f:Arith.phi  ~min ~max)
  ; test "phi'"  (fun () -> iter ~f:Arith.phi' ~min ~max)
  ]

let () =
  tests ~min:10000 ~max:20000
  |> Bench.make_command
  |> Command.run

(* Results
 * ┌──────┬──────────┬──────────┬──────────┬──────────┬────────────┐
 * │ Name │ Time/Run │  mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
 * ├──────┼──────────┼──────────┼──────────┼──────────┼────────────┤
 * │ phi  │   23.78s │ 997.20Mw │ 121.47Mw │ 121.47Mw │    100.00% │
 * │ phi' │    2.47s │ 158.85Mw │ 152.87Mw │   2.85Mw │     10.38% │
 * └──────┴──────────┴──────────┴──────────┴──────────┴────────────┘
 * Benchmarks that take 1ns to 100ms can be estimated precisely. For more reliable
 * estimates, redesign your benchmark to have a shorter execution time.
 *)
