(* Homework 1, Coursera Programming Languages, Fall 2013 *)
(* Allen McPherson   Los Alamos, New Mexico *)

(* This homework deals with operations on dates. *)
(* Dates are of type int*int*int *)
(* As function parameters, I'll use 'd' to denote a date. E.g. d1, d2, etc. *)

(* Helper values and functions *)
(*          Ja  F   Mr  Ap  My  Jn  Jy  Au  S   O   N   D   *)
val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

(* Sum elements in list of inetgers *)
fun sum (l: int list) =
    if null l
    then 0
    else hd l + sum(tl l)

(* Return a list 'f' of the first 'c' elements in integer list 'l' *)
fun front (c: int, l: int list, f: int list) =
    if c = 0
    then f
    else front(c-1, tl l, hd::f)


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
