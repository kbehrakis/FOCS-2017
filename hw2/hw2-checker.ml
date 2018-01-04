
(* To use this file:
 * 
 * (1) start a _new_ OCaml shell
 * (2) load your homework submission #use "homework2.ml";;
 * (3) load this file  #use "hw2-checker.ml";;
 * 
 * If there are _any_ errors, go back and fix your homework. 
 *
 * The only errors this will check for are:
 * - function not defined or with syntax or type errors
 * - functions not having the required type
 *
 *)

(* Q1 *)
let a:bool = try inS 1 [1] with Failure _ -> true in let _ = a in
let a:bool = try inS "a" ["a"]  with Failure _ -> true in let _ = a in
let a:bool = try subsetS [1] [1] with Failure _ -> true in let _ = a in
let a:bool = try subsetS ["a"] ["a"] with Failure _ -> true in let _ = a in
let a:bool = try equalS [1] [1] with Failure _ -> true in let _ = a in
let a:bool = try equalS ["a"] ["a"] with Failure _ -> true in let _ = a in
let a:int list = try unionS [1] [1] with Failure _ -> [] in let _ = a in
let a:string list = try unionS ["a"] ["a"] with Failure _ -> [] in let _ = a in
let a:int list = try interS [1] [1] with Failure _ -> [] in let _ = a in
let a:string list = try interS ["a"] ["a"] with Failure _ -> [] in let _ = a in
let a:int = try sizeS [1] with Failure _ -> 0 in let _ = a in
let a:int = try sizeS ["a"] with Failure _ -> 0 in let _ = a in
(* Q2 *)
let a:string list = try atMost 1 ["a"] with Failure _ -> [""] in let _ = a in
let a:string list = try unionL 1 ["a"] ["a"] with Failure _ -> [""] in let _ = a in
let a:string list = try concatL 1 ["a"] ["a"] with Failure _ -> [""] in let _ = a in
let a:string list = try starL 1 ["a"] with Failure _ -> [""] in let _ = a in
(* Q3 *)
let a:string = try regexp_a with Failure _ -> "" in let _ = a in
let a:string = try regexp_b with Failure _ -> "" in let _ = a in
let a:string = try regexp_c with Failure _ -> "" in let _ = a in
let a:string = try regexp_d with Failure _ -> "" in let _ = a in
let a:string = try regexp_e with Failure _ -> "" in let _ = a in
  print_string "Types all OK.\n"

