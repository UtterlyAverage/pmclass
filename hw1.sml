(* Homework 1, Coursera Programming Languages, Fall 2013
 * Allen McPherson   Los Alamos, New Mexico *)

(* This homework deals with operations on dates.
 * Dates are of type int*int*int
 * As function parameters, I'll use 'd' to denote a date. E.g. d1, d2, etc. *)

(* Helper values and functions
 *          Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec   *)
val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

(* Return sum of fisrt 'n' elements in 'reasonable' list
 * - no check for errors: going past end of list, negative n, etc. *)
fun sum_n (n: int, l: int list) =
    if n = 0 then 0
    else hd l + sum_n(n-1, tl l)

(* Return "day of year" as required in assignment *)
fun day_of_year (d: int*int*int) =
    sum_n(#2 d, days) + (#3 d)

fun day_in_year (d1: int*int*int) =
    (* Sum days in prior months *)
    (* Add the day of this month *)
    (* Ignore the year *)
    true


(* Problem 1: return true if d1 older than d2, false otherwise *)
fun is_older (d1: int*int*int, d2: int*int*int) =
    if #3 d1 > #3 d2
    then true
    else false
