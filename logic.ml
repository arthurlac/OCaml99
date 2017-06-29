(* Truth tables for logical expressions.
 *
 * Define predicates
 *   not, and, or, nand, nor, xor, eq
 * which succeed or fail according to the result of their respective
 * operations; e.g. and(A,B) will succeed, iff both A and B succeed.
 *)
let truth_table exp =


(* Gray code.
 *
 * An n-bit Gray code is a sequence of n-bit strings constructed
 * according to certain rules.
 *
 * gray_code 1 = ["0";"1"]
 * gray_code 2 = ["00";"01";"11";"10"]
 * gray_code 3 = ["000";"001";"011";"010";´110´;´111´;´101´;´100´]
 *
 * TODO apply result caching
 *)
let gray_code n =

(* Huffman code *)
let huff s =
