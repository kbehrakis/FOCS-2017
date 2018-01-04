(* 

HOMEWORK 1

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
 * Always make sure you can #use this file before submitting it.
 * It has to load without any errors.
 *
 *)



(* Question 1 *)

(* Recursive function to calculate a raised to the b *)
let rec expt a b =
  (*Function is only defined when b is >= 0*)
  if (b < 0)
     then failwith "expt: is only defined when b is greater than or equal to 0"
  else
     if (b == 0)         (* Exponent is 0 - final case in the recursion*)
        then 1
     else
        a * expt a (b-1)  (* Recursive call, updating the b value *)


(* Alternative way to compute exponential *)
let rec fastexpt a b =
  if (b == 0)
    then 1
  else
    if (b mod 2 == 0)  (* If b is even *)
       then let evenAnswer = (fastexpt a (b/2)) in evenAnswer * evenAnswer
    else
        a * fastexpt a (b-1)  (* Recursive call, updating the b value *)


(* Computes tetration of a by b *)
let rec tetra a b =
  if (b == 0)         (* Base case, all b's have been accounted for *)
    then 1
  else
    fastexpt a (tetra a (b-1))  (* Calling tetra again in the exponent - update b *)


(* Computes binomial coefficient *)
let rec choose n k =
  if (k == 0 || k == n) (* When k is 1 or n, n! cancels *)  
    then 1
  else if (k > 0 && k < n)  (* k cannot be negative (b/c of the factorials) *)
    then choose (n-1) (k-1) + choose (n-1) k
  else
    0  (* k isn't positive *)




(* Question 2 *)

(* Takes a list and duplicates each element *)
let rec doubleUp xs =
  match xs with
    [] -> []  (* Case with an empty list *)
    | firstElement::rest -> firstElement::firstElement::(doubleUp rest) 


(* Returns every other element in a list *)
let rec everyOther xs =
    match xs with
    [] -> []  (* Case with an empty list *)
    | firstElement::rest -> 

    (* Take away element - the first of the rest of the list *)
    match rest with
    [] -> firstElement::[]  (* Case with just one element *)
    | firstElementRest::restRest -> firstElement::everyOther(restRest) 


(* Concatenates two lists *)
let rec concatenate xs ys =
   (* Split up list xs *)
    match xs with
    [] -> ys          (* Once all elements of xs are added, then add ys *)
    | firstElementXS::restXS -> firstElementXS::(concatenate restXS ys)


(* Takes a list of lists and combines into one list*)
let rec concatenateAll xss =
    match xss with
    [] -> []                     (* Once all lists are accounted for *)
    | firstList::restLists ->    (* Extract the first list *)

    (* Extract the first element of the list and reconstruct a list of lsits w/o first element *)
    match firstList with
    [] -> concatenateAll restLists   
    | firstElement::restElements -> firstElement::concatenateAll (restElements::restLists)
    

(* Returns the nth element of a list *)
let rec nth n xs =
  (* Helper function to return the length of a list *)
  let rec length l = 
    match l with
      [] -> 0
      | head::tail -> 1 + (length tail)
  in

  match xs with
    [] -> (failwith "nth failure")         
    | firstElement::rest ->

  (* Catch exception for when n is out of range *)
  if ((n < 0)|| (n > (length xs)))
    then failwith "Index out of bounds"

  (* If index has been reached *)
  else if (n == 0)
    then firstElement

  (* If index has not been reached *)
  else
    (nth (n-1) rest)
  

(* Returns the last element of a list *)
let rec last xs = 
  (* Helper function to return the length of a list *)
  let rec length l = 
    match l with
      [] -> 0
      | head::tail -> 1 + (length tail)
  in

  match xs with
    [] -> (failwith "Failed: no element in list")         
    | firstElement::rest ->

  (* When the length is 1 - only one element in list i.e. the last element *)
  if ((length xs )== 1)
    then firstElement

  (* If last element not yet reached *)
  else
    (last rest)



(* QUESTION 3 *)

(* Returns vector with sum of v1 + v2 *)
let rec addV v w =
 (* Helper function to return the length of a list *)
  let rec length l = 
    match l with
      [] -> 0
      | head::tail -> 1 + (length tail)
  in
   
(* Check if vector lengths are equal *)
  if ((length v) != (length w))
     then failwith "Vectors are not of equal length"
  else 
     match v with
      [] -> []              
      | firstV::restV ->    

     match w with
      [] -> []            (* Sum the first element of the vector *)
      | firstW::restW -> (firstV + firstW)::(addV restV restW)


(* Computes scalar multiplication a*v *)
let rec scaleV a v =
     match v with
      [] -> []           (* Multiply the first element, then recursive call*)
      | firstV::restV -> (a*firstV)::(scaleV a restV)


(*Computes the inner product, i.e. dot product, of two vectors *)
let rec inner v w =
  (* Helper function to return the length of a list *)
  let rec length l = 
    match l with
      [] -> 0
      | head::tail -> 1 + (length tail)
  in
   
(* Check if vector lengths are equal *)
  if ((length v) != (length w))
     then failwith "Vectors are not of equal length"
  else 
     match v with
      [] -> 0             
      | firstV::restV ->    

     match w with
      [] -> 0            (* Multiply the first element of vectors *)
      | firstW::restW -> (firstV*firstW)+(inner restV restW)


(* Computes the outer product of two vectors *)
let rec outer v w =
   (* Helper function to compute scaler multiple *) 
    let rec scale a v =
      match v with
        [] -> []           (* Multiply the first element, then recursive call*)
        | firstV::restV -> (a*firstV)::(scale a restV)
    in

   (* Take each element in v and multiply by first element in w *)
   match w with
      [] -> []           
      | firstW::restW ->    

   match v with
      [] -> []
      | firstV::restV -> (scale firstW v)::(outer v restW)



(* QUESTION 4 *)

(* Adds two matrices *) 
let rec addM m n =
    let rec addV v w =
    (* Helper function to return the length of a list *)
      let rec length l = 
        match l with
          [] -> 0
          | head::tail -> 1 + (length tail)
      in

    (* Check if vector lengths are equal *)
      if ((length v) != (length  w))
         then failwith "Incorrect dimensions"
      else 
         match v with
          [] -> []              
          | firstV::restV ->    

         match w with
          [] -> []            (* Sum the first element of the vector *)
          | firstW::restW -> (firstV + firstW)::(addV restV restW)
      in

  (* MAIN FUNCTION *)
   (* Get first list of M *)
   match m with
      [] -> []          
      | firstMList::restMLists ->           

   (* Get first list of N *)
   match n with
      [] -> []                            
      | firstNList::restNLists ->  addV firstMList firstNList:: (addM restMLists restNLists)


(* Scales a matrix by a *)
let rec scaleM a m =
  (* Get first list of M *)
   match m with
      [] -> []          
      | firstList::restLists -> scaleV a firstList::(scaleM a restLists)

(* Multiplies two matricies *)
let rec multM m n =
    (* Helper function to delete the first column of a matrix - not used, could be useful as an extension later on*)
 (*   let rec deleteColumn matrix =
     match matrix with
      [] -> []              
      | firstRow::restRows ->    

     match firstRow with
      [] -> []            
      | firstElement::restElements -> restElements::(deleteColumn restRows)
    in 
  *)

   (* Helper function to compute row vector vM *)
   let rec mult1M row n = 
     match (row,n) with
      ([row],[n]) -> scaleV row n
      | (x::x',[]) -> failwith "Wrong dimensions"
      | ([],y::ys') -> failwith "Wrong dimensions"
      | (firstRow::restRow,firstRowN::restRowsN) -> (addV (scaleV firstRow firstRowN) (mult1M restRow restRowsN))
      |  _ -> assert false
    in 

   (* MAIN FUNCTION *)
     (* Helper function to return the length of a list *)
      let rec length l = 
        match l with
          [] -> 0
          | head::tail -> 1 + (length tail)
      in
     
      (* Returns the number of rows in a matrix *)
      let rec getNumRows matrix =
         match matrix with
          [] -> 0              
          | firstRow::restRows ->    

         match firstRow with
          [] -> 0            
          | firstElement::restElements -> 1 + (getNumRows restRows)
      in 


       match m with
          [] -> []         
          | mFirstRow::mRestRows ->  
 
       match n with
          [] -> []
          | nFirstRow:: nRestRows -> 

       match nFirstRow with
          [] -> []
          | nFirstElement::nRestElements ->
                 (* Check if vector lengths are equal *)
                    if ((length mFirstRow) != (getNumRows n))
                        then failwith "Failure: Wrong dimensions"
                    else 
                       (mult1M mFirstRow n)::(multM mRestRows n)

