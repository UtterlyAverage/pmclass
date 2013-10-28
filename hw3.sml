(* Homework 3, Coursera Programming Languages, Fall 2013
 * Allen McPherson   Los Alamos, New Mexico *)

(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(*----------------- Problem 1 -----------------*)

fun only_capitals xs: string list = 
    List.filter(fn s => Char.isUpper(String.sub(s,0)))  xs

(*----------------- Problem 2 -----------------*)

fun longest_string1 xs =
    List.foldl(fn (x,y) => if String.size(x) > String.size(y) then x else y) "" xs

(*----------------- Problem 3 -----------------*)

fun longest_string2 xs =
    List.foldl(fn (x,y) => if String.size(x) >= String.size(y) then x else y) "" xs

(*----------------- Problem 4 -----------------*)

fun longest_string_helper f xs =
    List.foldl(fn (x,y) => if f(String.size(x), String.size(y)) then x else y) "" xs

val longest_string3 =
    longest_string_helper(fn (x,y) => x > y)

val longest_string4 =
    longest_string_helper(fn (x,y) => x >= y)

(*----------------- Problem 5 -----------------*)

val longest_capitalized =
    longest_string3 o only_capitals

(*----------------- Problem 6 -----------------*)

val rev_string =
    String.implode o rev o String.explode

(*----------------- Problem 7 -----------------*)

fun first_answer f ans =
    case ans of
        []      => raise NoAnswer
      | a::ans' => case f a of
                      SOME a' => a'
                    | NONE    => first_answer f ans'

(*----------------- Problem 8 -----------------*)

fun all_answers f ans =
    let fun helper (f, ans, acc) = 
            case ans of
                [] => SOME acc
              | a::ans' => case f a of
                               NONE => NONE
                             | SOME a' => helper(f, ans', a' @ acc)
    in
        helper(f, ans, [])
    end

(*----------------- Problem 9 -----------------*)

fun count_wildcards pat =
    g(fn () => 1) (fn x => 0) pat

fun count_wild_and_variable_lengths pat =
    g(fn () => 1) (fn x => String.size x) pat

fun count_some_var (str, pat) =
    g(fn () => 0) (fn x => if x = str then 1 else 0) pat

(*----------------- Problem 10 -----------------*)

fun check_pat pat =
    let
        fun extract_vars pat =
            case pat of
                Variable x => [x]
              | TupleP ps  => List.foldl (fn (v,vs) => vs @ extract_vars(v)) [] ps
              | ConstructorP(_,pat) => extract_vars(pat)
              | _ => []
        fun has_repeats xs =
            case xs of
                [] => true
              | x::xs' => if List.exists (fn a => a = x) xs then false else has_repeats(xs')
    in
        (not o has_repeats o extract_vars) pat
    end
