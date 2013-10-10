(* Homework 1, Coursera Programming Languages, Fall 2013
 * Allen McPherson   Los Alamos, New Mexico *)

(* This homework deals with operations on dates.
 * Dates are of type int*int*int
 * As function parameters, I'll use 'd' to denote a date. E.g. d1, d2, etc. *)

(* Return "day of year" as required in assignment
 * - using let here means we don't have to check for pathalogical cases *)
fun day_of_year (d: int*int*int) =
    let
                 (* Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec *)
        val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        fun sum_n (n: int, l: int list) =
            if n = 0 then 0
            else hd l + sum_n(n-1, tl l)
    in           
        sum_n(#2 d - 1, days) + #3 d  (* Sum to month BEFORE day *)
    end


(* Problem 1: return true if d1 earlier than d2, false otherwise *)
fun is_older (d1: int*int*int, d2: int*int*int) =
    #1 d1 < #1 d2  orelse  #1 d1 = #1 d2  andalso  day_of_year(d1) < day_of_year(d2)


(* Problem 2: return NUMBER of dates in list that occur in provided month *)
fun number_in_month (l: (int*int*int) list, m: int) =
    if null l then 0
    else 
        if #2 (hd l) = m
        then 1 + number_in_month(tl l, m)
        else number_in_month(tl l, m)

(* Problem 3: like problem 2, but check for any dates in a LIST of months *)
fun number_in_months (l: (int*int*int) list, m: int list) =
    if null m then 0
    else number_in_month(l, hd m) + number_in_months(l, tl m)

(* Problem 4: return LIST of dates in list that occur in provided month *)
fun dates_in_month (l: (int*int*int) list, m: int) =
    if null l then []
    else 
        if #2 (hd l) = m
        then hd l :: dates_in_month(tl l, m)
        else dates_in_month(tl l, m)

(* Problem 5: like problem 4, but check for ANY dates in a LIST of months *)
fun dates_in_months (l: (int*int*int) list, m: int list) =
    if null m then []
    else dates_in_month(l, hd m) @ dates_in_months(l, tl m)


