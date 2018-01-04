
(* To use this file:
 * 
 * (1) start a _new_ OCaml shell
 * (2) load your homework submission #use "homework1.ml";;
 * (3) load this file  #use "hw1-checker.ml";;
 * 
 * If there are _any_ errors, go back and fix your homework. 
 *
 * The only errors this will check for are:
 * - function not defined or with syntax or type errors
 * - functions not having the required type
 *
 *)

(* Q1 *)
let a:int = try expt 1 1 with Failure _ -> 0 in let _ = a in
let a:int = try fastexpt 1 1 with Failure _ -> 0 in let _ = a in
let a:int = try tetra 1 1  with Failure _ -> 0 in let _ = a in
let a:int = try choose 1 1 with Failure _ -> 0 in let _ = a in
(* Q2 *)
let a:int list = try doubleUp [1] with Failure _ -> [] in let _ = a in
let a:string list = try doubleUp ["a"] with Failure _ -> [] in let _ = a in
let a:int list = try everyOther [1] with Failure _ -> [] in let _ = a in
let a:string list = try everyOther ["a"] with Failure _ -> [] in let _ = a in
let a:int list = try concatenate [1] [2]  with Failure _ -> [] in let _ = a in
let a:string list = try concatenate ["a"] ["b"] with Failure _ -> [] in let _ = a in
let a:int list = try concatenateAll [[1]] with Failure _ -> [] in let _ = a in
let a:string list = try concatenateAll [["a"]] with Failure _ -> [] in let _ = a in
let a:int = try nth 0 [1;2;3] with Failure _ -> 0 in let _ = a in
let a:string = try nth 0 ["a";"b";"c"] with Failure _ -> "" in let _ = a in
let a:int = try last [1;2;3] with Failure _ -> 0 in let _ = a in
let a:string = try last ["a";"b";"c"] with Failure _ -> "" in let _ = a in
(* Q3 *)
let a:int list = try addV [1] [2]  with Failure _ -> [] in let _ = a in
let a:int list = try scaleV 1 [2]  with Failure _ -> [] in let _ = a in
let a:int = try inner [1] [2]  with Failure _ -> 0 in let _ = a in
let a:int list list = try outer [1] [2]  with Failure _ -> [] in let _ = a in
(* Q4 *)
let a:int list list = try addM [[1]] [[2]]  with Failure _ -> [] in let _ = a in
let a:int list list = try scaleM 1 [[2]]  with Failure _ -> [] in let _ = a in
let a:int list list = try multM [[1]] [[2]]  with Failure _ -> [] in let _ = a in
  print_string "Types all OK.\n"

