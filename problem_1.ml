
(* 
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, 
 * we get 3, 5, 6 and 9. The sum of these multiples is 23. Find the sum of 
 * all the multiple s of 3 or 5 below 1000. 
 *)

#load "str.cma";;

let sum_multiples (n: int) : int =
  let rec inner (n: int) (acc: int): int = 
    if n < 3 
      then acc
    else if (n mod 3 == 0) || (n mod 5 == 0)
      then inner (n-1) (acc + n)
    else 
      inner (n-1) acc
  in 
  inner (n-1) 0 
in

print_endline (string_of_int (sum_multiples 1000))