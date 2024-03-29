(* 
 * Each new term in the Fibonacci sequence is generated by adding the 
 * previous two terms. By starting with 1 and 2, the first 10 terms will be:
 *    1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
 * By considering the terms in the Fibonacci sequence whose values do not 
 * exceed four million, find the sum of the even-valued terms.
 *)

#load "str.cma";;

let rec sum_even_fibs prev_1 prev_2 sum max =
  let current = prev_1 + prev_2 in
  if current > max
    then sum
  else if (current mod 2 == 0)
    then sum_even_fibs current prev_1 (sum + current) max
  else 
    sum_even_fibs current prev_1 sum max
in

print_endline (string_of_int (sum_even_fibs 1 1 0 4000000))