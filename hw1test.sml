(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not 
 * guarantee that your code will pass the actual homework grader.
 * To run the test, add a new line to the top of this 
 * file: use "homeworkname.sml";
 * All the tests should evaluate to true. For example, the REPL 
 * should say: val test1 = true : bool *)

use "hw1.sml";


val al     = (1955,  7, 27)
val boo    = (1990,  7, 24)
val tiwi   = (1961, 12, 11)
val fourth = (1776,  7,  4)
val moon   = (1969,  7, 21)
val pearl  = (1941, 12,  7)
val vj     = (1945,  9,  2)
val ve     = (1945,  5,  8)
val nine11 = (2001,  9, 11)

val dates  = [al,boo,tiwi,fourth,moon,pearl,vj,ve,nine11]

val test_is_older = 
    is_older((1,2,3),(2,3,4)) = true  andalso
    is_older((3,2,3),(2,3,4)) = false andalso
    is_older((1,1,1),(1,1,1)) = false andalso
    is_older((1,2,2),(1,2,3)) = true  andalso
    is_older((1,2,2),(1,2,1)) = false andalso
    is_older(al,tiwi)         = true  andalso
    is_older(boo,al)          = false andalso
    is_older(moon,moon)       = false

val test_number_in_month =
    number_in_month([(1,2,3),(2,2,4),(1,1,3),(3,2,6)],2) = 3 andalso
    number_in_month([(1,2,3),(2,2,4),(1,1,3),(3,2,6)],1) = 1 andalso
    number_in_month([(1,2,3),(2,2,4),(1,1,3),(3,2,6)],4) = 0 andalso
    number_in_month(dates,7)                             = 4 andalso
    number_in_month(dates,12)                            = 2 andalso
    number_in_month(dates,9)                             = 2 andalso
    number_in_month([],9)                                = 0 andalso
    number_in_month(dates,6)                             = 0

val test_number_in_months =
    number_in_months([(1,2,3),(2,2,4),(1,1,3),(3,2,6)],[2,6]) = 3 andalso
    number_in_months([(1,2,3),(2,2,4),(1,1,3),(3,2,6)],[])    = 0 andalso
    number_in_months([(1,2,3),(2,2,4),(1,1,3),(3,2,6)],[1,2]) = 4 andalso
    number_in_months([(1,2,3),(2,2,4),(1,1,3),(3,2,6)],[4])   = 0 andalso
    number_in_months(dates,[7,12,11])                         = 6 andalso
    number_in_months(dates,[9,5,12,7])                        = 9 andalso
    number_in_months(dates,[7,12,5,9])                        = 9 andalso
    number_in_months(dates,[])                                = 0 andalso
    number_in_months(dates,[6])                               = 0 andalso
    number_in_months([],[])                                   = 0 andalso
    number_in_months([],[12])                                 = 0

val test_dates_in_month =
    dates_in_month([(1,2,3),(2,2,4),(1,1,3),(3,2,6)],2)              = 
                   [(1,2,3),(2,2,4),        (3,2,6)]                 andalso
    dates_in_month([(1,2,3),(2,2,4),(1,1,3),(3,2,6)],1)              =
                   [(1,1,3)]                                         andalso
    dates_in_month([(1,2,3),(2,2,4),(1,1,3),(3,2,6)],4)              =
                   []                                                andalso
    dates_in_month(dates,7)                                          = 
                   [(1955,7,27),(1990,7,24),(1776,7,4),(1969,7,21)]  andalso
    dates_in_month(dates,12)                                         = 
                   [(1961,12,11),(1941,12,7)]                        andalso
    dates_in_month(dates,9)                                          =
                   [(1945,9,2),(2001,9,11)]                          andalso
    dates_in_month([],9)                                             =
                   []                                                andalso
    dates_in_month(dates,6)                                          =
                   []

val test_dates_in_months =
    dates_in_months([(1,2,3),(2,2,4),(1,1,3),(3,2,6)],[2])              = 
                    [(1,2,3),(2,2,4),        (3,2,6)]                   andalso
    dates_in_months([(1,2,3),(2,2,4),(1,1,3),(3,2,6)],[1])              =
                    [                (1,1,3)        ]                   andalso
    dates_in_months([(1,2,3),(2,2,4),(1,1,3),(3,2,6)],[1,2])            =
                    [(1,2,3),(2,2,4),(1,1,3),(3,2,6)]                   andalso
    dates_in_months([(1,2,3),(2,2,4),(1,1,3),(3,2,6)],[2,1])            =
                    [(1,2,3),(2,2,4),(1,1,3),(3,2,6)]                   andalso
    dates_in_months(dates,[7])                                          = 
                    [(1955,7,27),(1990,7,24),(1776,7,4),(1969,7,21)]    andalso
    dates_in_months(dates,[9,12])                                       = 
                    [(1945,9,2),(2001,9,11),(1961,12,11),(1941,12,7)]   andalso
    dates_in_months(dates,[12,9])                                       =
                    [(1945,9,2),(2001,9,11),(1961,12,11),(1941,12,7)]   andalso
    dates_in_months([],[9])                                             =
                    []                                                  andalso
    dates_in_months(dates,[1,2,3,4])                                    =
                    []                                                  andalso
    dates_in_months(dates,[4,3,2,1])                                    =
                    []                                                  andalso
    dates_in_months(dates,[])                                           =
                    []                                                  andalso
    dates_in_months([],[])                                              =
                    []

val test_all =
    test_is_older         = true andalso
    test_number_in_month  = true andalso
    test_number_in_months = true andalso
    test_dates_in_month   = true andalso
    test_dates_in_months  = true

(*
val test2 = number_in_month([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months([(2012,2,28),(2013,12,1),
                              (2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],
                            [2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string((2013, 6, 1)) = "June 1, 2013"

val test8 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3

val test9 = what_month(70) = 3

val test10 = month_range(31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
*)