(* 

HOMEWORK 3

Name: Kristen Behrakis

Email: kristen.behrakis@students.olin.edu

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

           (************************** PART A ************************)
(* Helper Function: Returns true is element is in the set, false otherwise*)
let rec inS e xs =
  match xs with
    [] -> false    (* Case with an empty list *)
    | firstElement::rest ->
     (* Check if entered element equals an element in list *)
           (firstElement = e)||(inS e rest)

 (* Returns true if one of the states in qa is a final state *)
let rec hasFinal (m:'a fa) (qs:'a list):bool = 
      match qs with
      [] -> false
      | head::tail -> 
             if (inS head m.final)
               then true
             else
               (hasFinal m tail)


           (************************** PART B ************************)
(* Helper Function: Searches the delta and returns the correct list *)
let rec searchDelta listOfTransitions desiredState desiredTransition  =
   match listOfTransitions with
      [] -> []
      | head::tail ->
         (* Decompose the first n-tuple *)
          let (startPosition,transition,endPosition) = head in
             (* Check if the state and transition match *)
             if ((desiredState=startPosition) && (desiredTransition = transition))
               then endPosition::(searchDelta tail desiredState desiredTransition)
             else
               (searchDelta tail desiredState desiredTransition)


(* Returns list of states that can be reached from transition a to state q *)
let rec reachableStates (m:'a fa) (q:'a) (a:char):'a list =
     (* Calls to a helper that takes in just the list of deltas.  
      * This way, m doesn't have to be reconstructed *)
     (searchDelta m.delta q a)


           (************************** PART C ************************)
(* Helper Function: removes duplicates from a list *)
let rec removeDuplicates xs =
    match xs with
      [] -> []
      | head::tail -> 
             if (inS head tail)  (* If first element is repeated later on *)
               then (removeDuplicates tail)
             else
               head::(removeDuplicates tail)

(* Helper Function: Returns the union of xs and ys *)
let rec unionS xs ys =
  let rec helperUnion xs ys =
    match xs with
    [] -> ys          (* Once all elements of xs are added, then add ys *)
    | firstElementXS::restXS -> firstElementXS::(unionS restXS ys)
  in

  (removeDuplicates (helperUnion xs ys))


(* Returns a list of states reachable by following transition a from states in qs *)
let rec follow (m:'a fa) (qs:'a list) (a:char):'a list = 
  match qs with
      [] -> []
      | head::tail -> unionS (reachableStates m head a) (follow m tail a)


           (************************** PART D ************************)
(* Returns the list of states reachable by following a given sequency of symbols *)
let rec followAll (m:'a fa) (qs:'a list) (syms:char list):'a list =
  (* Do follow with first element of syms, then do followAll w/ returned reachable list
   * and the next element of syms *)
  match syms with
      [] -> qs  (* Can start from any element in qs, so all are reachable *)
      | head::tail -> removeDuplicates (followAll m (follow m qs head) tail)


           (************************** PART E ************************)
(* Helper Function - Returns the intersection of xs and ys *)
let rec interS xs ys = 
    match xs with
    [] -> []  
    | firstElementXS::restXS ->
       (* See which elements of xs are in ys, then add to the new set *)
       if (inS firstElementXS ys)
         then firstElementXS::(interS restXS ys)
       else 
         (interS restXS ys)

(* Helper Function - Returns the number of distinct elements in xs *)
let rec sizeS xs =
  (* Remove duplicates then get length *)
    match (removeDuplicates xs) with
      [] -> 0
      | head::tail -> 1 + (sizeS tail)

(* Returns true if and only if m accepts the strings s *)
let accept (m:'a fa) (input:string):bool = 
  (* Explode the string - put it into list of chars *)
  let explodedString = (explode input) in
  
  (* Call followAll with the exploded string as input. 
   * If the returned string (reachable strings) includes final state, then true*)
   let reachableStrings = (followAll m [m.start] explodedString) in
      if (sizeS (interS m.final reachableStrings) > 0)
         then true
      else 
         false


(* QUESTION 2 *)

(* Right now, these are dummy finite automata -- replace by your own *)

let dummy = { states = [[]];
			alphabet = [];
			delta = [];
			start = [];
			final = []}

(* Third from end is always f *)
let fa_q2_a : 'a fa = { states = [0;1;2;3];
                        alphabet = ['d';'e';'f'];
                        delta = [ (0,'d',0);
	                          (0,'e',0);
	                          (0,'f',1);
	                          (1,'f',1);
                                  (1,'d',2);
                                  (1,'e',2);
                                  (1,'f',2);
                                  (2,'d',3);
                                  (2,'e',3);
	                          (2,'f',3); ];
                                  start = 0;
                                  final = [3]
                       } 

(* Every f is followed by an f *)
let fa_q2_b : 'a fa = { states = [0;1;2];
                        alphabet = ['d';'e';'f'];
                        delta = [ (0,'d',0);
	                          (0,'e',0);
	                          (0,'f',1);
	                          (1,'f',2);
                                  (2,'f',2);
                                  (0,'f',2); ];
                                  start = 0;
                                  final = [2]
                       } 

(* No more than two consecutive d's or e's *)
let fa_q2_c : 'a fa = { states = [0;1;2;3;4;5];
                        alphabet = ['d';'e';'f'];
                        delta = [ (0,'f',0);
                                  (0,'d',1);
	                          (1,'f',1);
	                          (1,'d',3);
	                          (3,'e',2);
	                          (3,'f',2);
                                  (1,'e',2);
                                  (5,'d',1);
                                  (5,'f',1);
                                  (0,'e',2);
                                  (2,'d',1);
                                  (2,'e',5);
                                  (2,'f',2);
                                  (0,'d',4);
                                  (0,'e',4);
                                  (0,'f',4);
                                  (2,'d',3);
                                  (2,'f',3);
                                  (1,'e',5);
                                  (1,'f',5);
                                  (5,'d',3);
                                  (5,'f',3);
                                  (3,'e',5);
                                  (3,'f',5);
                                  (4,'f',4);];
                                  start = 0;
                                  final = [3;4;5]
                       }

(* Even number of d's and odd number of e's *)
let fa_q2_d : 'a fa =  { states = [0;1;2;3];
                        alphabet = ['d';'e';'f'];
                        delta = [ (0,'d',1);
	                          (1,'d',0);
	                          (0,'f',0);
	                          (1,'f',1);
                                  (1,'e',2);
                                  (2,'e',1);
                                  (2,'f',2);
                                  (2,'d',3);
                                  (3,'d',2);
	                          (3,'f',3); 
                                  (3,'e',0);
                                  (0,'e',3);];
                                  start = 0;
                                  final = [3]
                       } 


let fa_q2_e : 'a fa = { states = [0;1;2;3;4;5;6;7;8;9;10;11;12;13];
                        alphabet = ['d';'e';'f'];
                        delta = [ (0,'d',1);
	                          (1,'d',0);
                                  (1,'e',2);
                                  (2,'e',1);
                                  (2,'d',3);
                                  (3,'d',2);
                                  (3,'e',0);
                                  (0,'e',3);
                               (* NEXT LAYER *)
                                  (3,'f',12);
                                  (12,'f',11);
                                  (5,'f',9);
                                  (1,'f',13);
                                  (13,'f',9);
                                  (6,'f',10);
                                  (3,'f',7);
                                  (1,'f',5);
                                  (2,'f',6);
                                  (0,'f',4);
                                  (4,'d',5);
	                          (5,'d',4);
                                  (5,'e',6);
                                  (6,'e',5);
                                  (6,'d',7);
                                  (7,'d',6);
                                  (7,'e',4);
                                  (4,'e',7);
                               (* NEXT LAYER *)
                                  (7,'f',11);
                                  (4,'f',8);
                                  (8,'d',9);
	                          (9,'d',8);
                                  (9,'e',10);
                                  (10,'e',9);
                                  (10,'d',11);
                                  (11,'d',10);
                                  (11,'e',8); 
                                  (8,'e',11);];
                                  start = 0;
                                  final = [11]
                       } 





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



           (************************** PART A ************************)
(* Helper Function - computes one row of the final matrix *)
let rec getOneRow (m:'a fa) startState endStates a =
    match endStates with
    | [] -> []
    | head::tail ->
           (* If the value is reachable - reachableStates gets all states reachable via a *)
           if (inS head (reachableStates m startState a))
              then 1::(getOneRow m startState tail a)
           else
              0::(getOneRow m startState tail a)

(* Helper Function - gets each row and creates a matrix *)
let rec getAllRows (m:'a fa) a listStates =
   match listStates with
    | [] -> []
    | firstState::restStates ->(getOneRow m firstState m.states a)::(getAllRows m a restStates)

let matTrans (m:'a fa) (a:char):int list list =
   (getAllRows m a m.states)


          (************************** PART B ************************)

(* Helper Function - Allows recursive calls *)
let rec recInit(m:'a fa) listStates =
   match listStates with
    | [] -> []
    | firstState::restStates ->
                    (* If the current state is a start state, put 1 *)
                     if (firstState = m.start)
                         then 1::(recInit m restStates)
                     else
                         0::(recInit m restStates)

let matInit (m:'a fa):int list list =
  [(recInit m m.states)]


           (************************** PART C ************************)
(* Helper Function - Check if state is in list of final states *)
let rec getOneFinal (m:'a fa) listStates =
   match listStates with
    | [] -> []
    | firstState::restStates ->
                    (* If the current state is a final state, put 1 *)
                     if (inS firstState m.final)
                         then [1]::(getOneFinal m restStates)
                     else
                         [0]::(getOneFinal m restStates)

let matFinal (m:'a fa):int list list =
  (getOneFinal m m.states)


           (************************** PART D ************************)

(* Helper Function - Returns the number of elements in xs *)
let rec numElements xs =
  (* Remove duplicates then get length *)
    match xs with
      [] -> 0
      | head::tail -> 1 + (sizeS tail)


(* Helper Function - Multiply (Ta through Tk) *)
let rec multAllT m listChars =
   match listChars with
    | [] -> [[]]
    | firstChar::restChars -> (* We never want to multiply with an empty matrix *)
                              if ((numElements restChars) > 0)
                                 then (multM (matTrans m firstChar)(multAllT m restChars))
                              else
                                 (matTrans m firstChar)

let matAccept (m:'a fa) (str:string):bool =
  (* Explode the string - put it into list of chars *)
  let explodedString = (explode str) in
  
  (* Call followAll with the exploded string as input. 
   * If the returned string (reachable strings) includes final state, then true *)
     let matrixMultiplication = (multM (multM (matInit m) (multAllT m explodedString)) (matFinal m)) in
           if (matrixMultiplication <= [[0]])
              then false
           else 
              true





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
