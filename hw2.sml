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

fun all_except_option (s, l) =
    case l of
        []     => NONE
      | h::t   => if same_string(s, h) 
                  then SOME t
                  else case all_except_option(s, t) of
                           NONE      => NONE
                        |  SOME rest => SOME (h :: rest)


fun get_substitutions1 (subs, s) =
    case subs of
        []      => []
      | h::t    => case all_except_option(s, h) of
                       NONE   => get_substitutions1(t, s)
                     | SOME l => l @ get_substitutions1(t, s)


fun get_substitutions2 (subs, s) =
    let fun aux (subs, s, acc) =
            case subs of
                []      => acc
              | h::t    => case all_except_option(s, h) of
                               NONE   => aux(t, s, acc)
                             | SOME l => aux(t, s, acc @ l)
    in
        aux(subs, s, [])
    end


fun similar_names (subs, {first=f, last=l, middle=m}) =
    let fun sub_first (lst) =
            case lst of
                []   => []
              | h::t => {first=h, last=l, middle=m} :: sub_first(t)
    in
        case get_substitutions1(subs, f) of
            []  => [{first=f, last=l, middle=m}]
         |  lst => {first=f, last=l, middle=m} :: sub_first(lst)
    end




(* You may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*----------------- Problem 2 -----------------*)

fun card_color c =
    case c of
        (Clubs,_)    => Black
      | (Diamonds,_) => Red
      | (Hearts,_)   => Red
      | (Spades,_)   => Black

fun card_value c =
    case c of
        (_,Ace)   => 11
     |  (_,King)  => 10     
     |  (_,Queen) => 10     
     |  (_,Jack)  => 10     
     |  (_,Num v) => v

fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
     |  h::t => if h = c
                then t
                else case remove_card(t, c, e) of
                         []   => [h]
                       | lst  => h :: lst

fun all_same_color cs =
    case cs of
        []                 => true
      | x::[]              => true
      | head::(neck::rest) => (card_color(head) = card_color(neck) andalso
                               all_same_color(neck::rest))

fun sum_cards cs =
    let fun aux (cs, acc) =
            case cs of
                []     => acc
              | c::cs' => aux(cs', acc + card_value(c))
    in
        aux (cs, 0)
    end

fun score (cs, goal) =
    let 
        val sum = sum_cards(cs)
        val prelim =
            if sum > goal then 3 * (sum-goal)
            else goal-sum
    in
        if all_same_color(cs) then prelim div 2
        else prelim
    end                     
                   
fun officiate (cards, moves, goal) =
    let
        fun play (cards, moves, goal, held) =
            case moves of
                [] => score(held, goal)
              | Discard c::t => play(cards, t, goal, remove_card(held, c, IllegalMove))
              | Draw::t => case cards of 
                               [] => score(held, goal)
                             | draw::cards' => if sum_cards(draw::held) > goal
                                               then score(draw::held, goal)
                                               else play(cards, t, goal, draw::held)
    in
        play (cards, moves, goal, [])
    end
