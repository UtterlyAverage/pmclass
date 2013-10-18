(* Homework 2, Coursera Programming Languages, Fall 2013
 * Allen McPherson   Los Alamos, New Mexico *)

(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(*  WARNING -- no null, hd, tl, isSome, valOf, or #   *)

(* put your solutions for problem 1 here *)

(*----------------- Problem 1 -----------------*)

(* Return NONE if string s not in l, or a list (SOME) identical to l but without s *)
fun all_except_option(s, l) =
    case l of
        []     => NONE
      | h::t   => if same_string(s, h) then SOME t
                  else h :: all_except_option(s, t)
    

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*----------------- Problem 2 -----------------*)
