(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(*  WARNING -- no null, hd, tl, isSome, valOf, or #   *)

(* put your solutions for problem 1 here *)
(* use "hw2-1.sml"; *)

(* Return NONE if s not in l, or a list (SOME) identical to l but without s *)
fun all_except_option(s: string, l: list) =
    case l of
        []       => NONE
      | h::[]  => if not same_string(s, h) then SOME h else NONE
      | h::t   => if not same_string(s, h) 
                    then h :: all_except_option(s,t)
                    else all_except_option(s,t)
        
    

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* use "hw2-2.sml"; *)
