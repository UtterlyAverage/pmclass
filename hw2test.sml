(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2.sml";

val string1to3 = ["one", "two", "three"];

val allen      = ["Allen", "Al", "BWMF", "Bhagwan"]
val theresa    = ["Theresa", "Terry", "Tiwi", "T"]
val rick       = ["Rick", "Ricky", "Mr. Earl", "He who knows no difference"]
val marlon     = ["Marlon", "Boo"]
val alan       = ["Alan", "Al", "Boo"]
val subs       = [allen, theresa, rick, marlon, alan];

val boo = {first="Boo", last="McPherson", middle="C"}
val al  = {first="Al",  last="McPherson", middle="L"}
val t   = {first="T",   last="McPherson", middle="M"}


val test_all_except_option =
    all_except_option("three", string1to3)  = SOME ["one", "two"]    andalso
    all_except_option("one",   string1to3)  = SOME ["two", "three"]  andalso
    all_except_option("two",   string1to3)  = SOME ["one", "three"]  andalso
    all_except_option("zero",  string1to3)  = NONE                   andalso
    all_except_option("string", ["string"]) = SOME []

val test_get_substitutions1 =
    get_substitutions1(subs, "Boo") = ["Marlon", "Alan", "Al"]       andalso
    get_substitutions1(subs, "Al")  = ["Allen", "BWMF", "Bhagwan", 
                                       "Alan", "Boo"]                andalso
    get_substitutions1(subs, "T")   = ["Theresa", "Terry", "Tiwi"]

val test_get_substitutions2 =
    get_substitutions2(subs, "Boo") = ["Marlon", "Alan", "Al"]       andalso
    get_substitutions2(subs, "Al")  = ["Allen", "BWMF", "Bhagwan", 
                                       "Alan", "Boo"]                andalso
    get_substitutions2(subs, "T")   = ["Theresa", "Terry", "Tiwi"]

val test_similar_names =
    similar_names(subs, boo) 
       = [{first="Boo",     last="McPherson", middle="C"},
          {first="Marlon",  last="McPherson", middle="C"},
          {first="Alan",    last="McPherson", middle="C"},
          {first="Al",      last="McPherson", middle="C"}]       andalso
    similar_names(subs, al) 
       = [{first="Al",      last="McPherson", middle="L"},
          {first="Allen",   last="McPherson", middle="L"},
          {first="BWMF",    last="McPherson", middle="L"},
          {first="Bhagwan", last="McPherson", middle="L"},
          {first="Alan",    last="McPherson", middle="L"},
          {first="Boo",     last="McPherson", middle="L"}]       andalso
    similar_names(subs, t) 
       = [{first="T",       last="McPherson", middle="M"},
          {first="Theresa", last="McPherson", middle="M"},
          {first="Terry",   last="McPherson", middle="M"},
          {first="Tiwi",    last="McPherson", middle="M"}]

val test_card_color =
    card_color((Spades,   Queen))  = Black         andalso
    card_color((Diamonds, Jack))   = Red           andalso
    card_color((Clubs,    Num 10)) = Black         andalso
    card_color((Hearts,   Num 1))  = Red

val test_card_value =
    card_value((Spades, Ace))      = 11            andalso
    card_value((Clubs,  King))     = 10            andalso
    card_value((Hearts, Queen))    = 10            andalso
    card_value((Diamonds, Jack))   = 10            andalso
    card_value((Spades, Num 1))    = 1             andalso
    card_value((Diamonds, Num 10)) = 10            andalso
    card_value((Hearts, Num 7))    = 7             andalso
    card_value((Clubs, Num 3))     = 3           

val hand1 = [(Clubs, Num 8), (Hearts, Num 8), (Spades, Ace),
             (Clubs, Ace),   (Hearts, Num 8), (Diamonds, Queen)];
val hand2 = [(Clubs, Num 8), (Hearts, Num 8),    (* no Ace of Spades *)
             (Clubs, Ace),   (Hearts, Num 8), (Diamonds, Queen)];
val hand3 = [(Clubs, Num 8), (Spades, Ace),      (* minus one 8 of Hearts *)
             (Clubs, Ace),   (Hearts, Num 8), (Diamonds, Queen)];
val hand4 = [(Hearts, Num 8), (Spades, Ace),     (* minus front *)
             (Clubs, Ace),   (Hearts, Num 8), (Diamonds, Queen)];
val hand5 = [(Clubs, Num 8), (Hearts, Num 8), (Spades, Ace),
             (Clubs, Ace), (Hearts, Num 8)];     (* minus tail *)
val handR = [(Hearts, Ace), (Hearts, Num 2), (Hearts, Queen)] (* all red *)
val handM = [(Hearts, Ace), (Hearts, Num 2), (Clubs, Queen)]  (* mixed   *)

val test_remove_card =
    remove_card(hand1, (Spades, Ace), IllegalMove)     = hand2      andalso
    remove_card(hand1, (Hearts, Num 8), IllegalMove)   = hand3      andalso
    remove_card(hand1, (Clubs,  Num 8), IllegalMove)   = hand4      andalso
    remove_card(hand1, (Diamonds, Queen), IllegalMove) = hand5      andalso
    remove_card(hand1, (Diamonds, Ace), IllegalMove)   
         = hand1 handle IllegalMove => true 

val test_all_same_color =
    all_same_color(handR) = true       andalso
    all_same_color(handM) = false

val test_sum_cards =
    sum_cards(hand1) = 56       andalso
    sum_cards([])    = 0        andalso
    sum_cards(handR) = 23

val test_score =
    score(hand1, 55) = 3        andalso
    score(hand1, 57) = 1        andalso
    score(handR, 24) = 0



val test2 = get_substitutions1([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2([["foo"],["there"]], "foo") = []

val test4 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color((Clubs, Num 2)) = Black

val test6 = card_value((Clubs, Num 2)) = 2

val test7 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true

val test9 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4

val test10 = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4


val test11 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
