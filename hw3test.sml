(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3.sml";





val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"              andalso
            longest_string1 ["aaa", "bbb", "cc"] = "aaa"

val test3 = longest_string2 ["A","bc","C"] = "bc"              andalso
            longest_string2 ["aaa", "bbb", "cc"] = "bbb"

val test4a= longest_string3 ["A","bc","C"] = "bc"              andalso
            longest_string3 ["aaa", "bbb", "cc"] = "aaa"

val test4b= longest_string4 ["A","B","C"] = "C"                andalso
            longest_string4 ["aaa", "bbb", "cc"] = "bbb"

val test5 = longest_capitalized ["A","bc","C"] = "A"           andalso
            longest_capitalized ["Abcd", "d", "Defg"] = "Abcd"

val test6 = rev_string "abc" = "cba";

val test7 = first_answer(fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] =
            4 andalso
            first_answer(fn x => if x = 5 then SOME x else NONE) [1,2,3,4,5] = 5

val test8 = all_answers(fn x => if x = 1 then SOME [x] else NONE
                       ) [2,3,4,5,6,7] = NONE andalso
            all_answers(fn x => if x > 1 then SOME [x] else NONE) [] = SOME []

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1   andalso
             count_wild_and_variable_lengths (Variable("abc")) = 3

val test9c = count_some_var ("x", Variable("x")) = 1;

val test10 = check_pat (Variable("x")) = true

(*

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

*)

