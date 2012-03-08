(*
 * A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 *
 * a^2 + b^2 = c^2
 * For example, 32 + 42 = 9 + 16 = 25 = 5^2.
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product abc.
 * 
 * From Wikipedia, http://en.wikipedia.org/wiki/Pythagorean_triple
 * given arbitrary positive integers m, n where n < m can generate a 
 * pythagorean triple a, b, c where
 *   a = m^2 - n^2
 *   b = 2mn
 *   c = m^2 + n^2
 *)
#load "util.cmo";;
open Util;;


let find_triplet n m : (int * int * int) =
	let a = (pow m 2) - (pow n 2) in
	let b = 2 * m * n in
	let c = (pow m 2) + (pow n 2) in
	(a, b, c)
in

let rec search_triplets n m : (int * int * int) =
	match (find_triplet n m) with
  	(a, b, c) -> if (a + b + c = 1000) then (a, b, c)
							   else if m = n then search_triplets (n + 1) 333
						   	 else search_triplets n (m - 1)
in

let triple = (search_triplets 1 333) in
let () = print_triplet triple in
match triple with (a, b, c) -> 
	print_endline (string_of_int(a * b * c));;
 

