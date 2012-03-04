(*
 * The sum of the squares of the first ten natural numbers is,
 *   12 + 22 + ... + 102 = 385
 * The square of the sum of the first ten natural numbers is,
 *   (1 + 2 + ... + 10) ^ 2 = 552 ^ 2 = 3025
 * 
 * Hence the difference between the sum of the squares of the first ten 
 * natural numbers and the square of the sum is 3025  385 = 2640.
 *
 * Find the difference between the sum of the squares of the first 
 * one hundred natural numbers and the square of the sum.
*)

let square (n: int) : int = 
	int_of_float ((float_of_int n) ** 2.0)
in

let difference (n: int) : int =
	let rec inner (n: int) (sum: int) (squared_sum: int) : int list = 
		if n = 0 
		then [sum; squared_sum]
		else inner (n - 1) (sum + n) (squared_sum + (square n))
	in
	match (inner n 0 0) with 
		sum :: squared_sum ::[] -> (square sum) - squared_sum
	 | x -> -1

in print_endline (string_of_int (difference 100))	

