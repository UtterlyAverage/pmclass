(* Homework 1, Coursera Programming Languages, Fall 2013
 * Allen McPherson   Los Alamos, New Mexico *)

(* This homework deals with operations on dates.
 * Dates are of type int*int*int
 * As function parameters, I'll use 'd' to denote a date. E.g. d1, d2, etc. 
 * Short comments at begining of functions are just to remind me what they're
 * doing--for full details see the assignment sheet *)


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


(* Problem 3: like problem 2, but check for any dates in a LIST of months
 * - no repetition in list of months *)
fun number_in_months (l: (int*int*int) list, m: int list) =
    if null m then 0
    else number_in_month(l, hd m) + number_in_months(l, tl m)


(* Problem 4: return LIST of dates in list that occur in provided month 
 * - returned list of dates must be in order originally given *)
fun dates_in_month (l: (int*int*int) list, m: int) =
    if null l then []
    else 
        if #2 (hd l) = m
        then hd l :: dates_in_month(tl l, m)
        else dates_in_month(tl l, m)


(* Problem 5: like problem 4, but check for ANY dates in a LIST of months 
 * - no repetion in the list of months 
 * - no statement that the returned list must be in original order *)
fun dates_in_months (l: (int*int*int) list, m: int list) =
    if null m then []
    else dates_in_month(l, hd m) @ dates_in_months(l, tl m)


(* Problem 6: return the nth element in a list of strings *)
fun get_nth (l: string list, n: int) =
    if n = 1 then hd l
    else get_nth(tl l, n-1)


(* Problem 7: return a string of the form "Month day, year" for passed date *)
fun date_to_string (d: int*int*int) =
    let
        val months = ["January", "February", "March", "April", "May", "June",
                      "July", "August", "September", "October", "November",
                      "December"]
    in
        get_nth(months, #2 d) ^ " " ^ 
        Int.toString(#3 d) ^ ", " ^ 
        Int.toString(#1 d)
    end


(* Problem 8: sequentially sum elements of list, returning index of the
 * last list element at which the total sum was less that the value
 * of passed in integer.
 * - This may be a weird way of doing it, but the prof did mntion in his
 *   slides that functions could be used in let expressions. 
 * - I redefine (shadow) number_before_matching_sum to add an index paramter
 *   that keeps track of the position in the list. *
 * - I'm aware (from class forum) that there are other ways to solve this
 *   without a helper function. However, as we will soon see, pattern matching
 *   function definitions do exactly this--and may be more idiomatic. *)
fun number_before_reaching_sum (n: int, l: int list) =
    let 
        fun number_before_reaching_sum (n: int, l: int list, index: int) =
            if n <= 0 then index  (* original n WILL BE positive *)
            else number_before_reaching_sum(n-hd l, tl l, index+1)
    in
        number_before_reaching_sum(n-hd l, tl l, 0)
    end


(* Problem 9: takes a day of year (1-365) and returns the month it fall in
   (1-12)  *)
fun what_month (d: int) =
    let
                 (* Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec *)
        val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(d, days) + 1   (* nbms returns month BEFORE *)
    end


(* Problem 10: like problem 9 but range of months between (inclusive)
   of two dates *)
fun month_range (d1: int, d2: int) =
    if (d1 > d2) then []
    else what_month(d1) :: month_range(d1+1, d2)


(* Problem 11: return oldest date in list (SOME) or (NONE) if list empty 
 * - doing it like better_max in notes 
 * - at this point in the class I feel it cleaner than better_max2 *)
fun oldest (l: (int*int*int) list) =
    if null l then NONE
    else
        let val older = oldest(tl l)
        in
            if isSome older andalso is_older(valOf older, hd l)
            then older
            else SOME (hd l)
        end

