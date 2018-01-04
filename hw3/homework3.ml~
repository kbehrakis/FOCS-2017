(* 

HOMEWORK 3

Name: 

Email:

Remarks, if any:

*)


(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * PLEASE DO NOT CHANGE THE TYPES IN THE STUBS I GIVE YOU. 
 * Doing so will make it impossible to test your code.
 *
 * Always make sure you can #use this file before submitting it.
 * It has to load without any errors.
 *
 *)



(* 
 * String <-> characters utility functions:
 *
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode cs = 
  List.fold_right (fun a r -> (String.make 1 a)^r) cs ""



(*
 *  The type of a finite automaton
 * 
 *)

type 'a fa = { states: 'a list;
               alphabet: char list;
               delta: ('a * char * 'a) list;
               start : 'a;
               final : 'a list }


(* 
 * Sample FAs
 *
 * The first accepts the language of all strings over {a,b} 
 * with a multiple-of-3 number of a's.
 *
 * The second accepts the language of all strings over {a,b,c} 
 * whose last three symbols are b's.
 *
 * Notes that states can be of any type -- the first example
 * uses strings as states, while the second uses integers.
 *
 *)

let faThreeA = { 
  states = ["start";"one";"two"];
  alphabet = ['a';'b'];
  delta = [ ("start",'a',"one");
	    ("one",'a',"two");
	    ("two",'a',"start");
	    ("start",'b',"start");
	    ("one",'b',"one");
	    ("two",'b',"two") ];
  start = "start";
  final = ["start"]
} 

let faLastThreeB = {
  states = [0;1;2;3];
  alphabet = ['a';'b';'c'];
  delta = [ (0,'a',0);
	    (0,'b',0);
	    (0,'c',0);
	    (0,'b',1);
	    (1,'b',2);
	    (2,'b',3); ];
  start = 0;
  final = [3]
} 





(* QUESTION 1 *)


let rec hasFinal (m:'a fa) (qs:'a list):bool = 
  failwith "hasFinal not implemented"


let rec reachableStates (m:'a fa) (q:'a) (a:char):'a list = 
  failwith "reachableStates not implemented"


let rec follow (m:'a fa) (qs:'a list) (a:char):'a list = 
  failwith "follow not implemented"


let rec followAll (m:'a fa) (qs:'a list) (syms:char list):'a list = 
  failwith "followAll not implemented"


let accept (m:'a fa) (input:string):bool = 
  failwith "accept not implemented"




(* QUESTION 2 *)

(* Right now, these are dummy finite automata -- replace by your own *)

let dummy = { states = [[]];
			alphabet = [];
			delta = [];
			start = [];
			final = []}

let fa_q2_a : 'a fa = dummy


let fa_q2_b : 'a fa = dummy


let fa_q2_c : 'a fa = dummy


let fa_q2_d : 'a fa = dummy


let fa_q2_e : 'a fa = dummy




(* QUESTION 3 *)


(*
 *  Matrix multiplication (from HW1)
 * 
 *  A matrix is an int list list
 *
 *)

let multM m n = 

  let rec addV v w =
    match v with
    | [] -> []
    | x::xs -> (match w with
      | [] -> []
      | y::ys -> (x+y)::(addV xs ys))  in 
  let rec scaleV a v =
    match v with
    | [] -> []
    | x::xs -> (a*x)::(scaleV a xs)  in
  let rec mult1M v m = 
    match v with
    | [] -> []
    | x::xs -> (match m with
      | [] -> []   (* shouldn't happen *)
      | [ys] -> scaleV x ys
      | ys::yss -> addV (scaleV x ys) (mult1M xs yss))  in
  let rec multM' m n = 
    match m with 
    | [] -> []
    | xs::xss -> (mult1M xs n)::(multM' xss n)  in
  multM' m n



let matTrans (m:'a fa) (a:char):int list list =
  failwith "matTrans not implemented"


let matInit (m:'a fa):int list list =
  failwith "matInit not implemented"


let matFinal (m:'a fa):int list list =
  failwith "matFinal not implemented"


let matAccept (m:'a fa) (str:string):bool =
  failwith "matAccept not implemented"





(* This function is the base function that basically loops through all
 * strings  
 * of length up to n, and prints those that are accepted by the
 * finite automaton.
 *
 * This is being way too clever to try to not blow the stack 
 * while enumerating all strings up to a given length. Basically.
 * we enumerate all integer, convert them to base K (where K is the
 * size of the alphabet) and then replace every digit base K by the
 * letter of the alphabet at the corresponding index in the alphabet. 
 *
 * The key is that we can enumerate integers super easily
 *
 *)

let lang_ accept m n = 

  let rec expt a n = if n <= 0 then 1 else a*(expt a (n-1)) in
  
  let rec take n default l = 
    if n <= 0 then []
    else (match l with
          | [] -> default::(take (n-1) default l)
          | x::xs -> x::(take (n-1) default xs)) in
  
  let to_base_n base size n = 
    let rec loop n = 
      if n <= 0 then []
      else if n mod base = 0 then 0::(loop (n / base))
      else (n mod base)::(loop ((n - n mod base) / base))  in
    take size 0 (loop n)  in
  
  let to_string alphabet size n = 
    let base = List.length alphabet in
    let num_base = to_base_n base size n in
    implode (List.map (fun i -> List.nth alphabet i) num_base) in
  
    if n < 0 then ()
    else
      let print_str s = if s = "" then print_string "  <epsilon>\n"
  	              else print_string ("  "^s^"\n")  in
      let rec loop i = 
        if i <= n then 
  	  let ts = to_string m.alphabet i  in
  	  let bound = expt (List.length m.alphabet) i in
  	  let rec loop2 j = 
  	    if j < bound then (if accept m (ts j) 
                                 then print_str (ts j)
                               else ();
  			       loop2 (j+1))
  	    else ()  in
  	  (loop2 0; loop (i+1))
        else ()  in
      loop 0

(* 
 * Tester functions that dump the language accepted by a
 * finite automaton for Q1 and Q3
 *
 *)
 
let lang m n = lang_ accept m n
let matLang m n = lang_ matAccept m n
