(*
 * The following iterative sequence is defined for the set of positive integers:
 *
 *   n -> n/2 (n is even)
 *   n -> 3n + 1 (n is odd)
 *
 * Using the rule above and starting with 13, we generate the following sequence:
 *
 *   13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
 *
 * It can be seen that this sequence (starting at 13 and finishing at 1) 
 * contains 10 terms. Although it has not been proved yet (Collatz Problem), 
 * it is thought that all starting numbers finish at 1.
 *
 * Which starting number, under one million, produces the longest chain?
 *
 * NOTE: Once the chain starts the terms are allowed to go above one million.
 *)

#load "unix.cma";;
#load "util.cmo";;
open Util;;

let term_map = Hashtbl.create 1000 in

let rec seq start n count : int =
	if Hashtbl.mem term_map n then
		(Hashtbl.find term_map n) + count
	else if n = 1 then 
	  let () = Hashtbl.add term_map start (count + 1) in
	  (count + 1)
	else 
		let next = if n mod 2 = 0 
							 then n / 2
		  				 else (3 * n) + 1 in
		seq start next (count + 1)
in

let rec search n best_n max : (int * int) =
	let () = print_endline (string_of_int n) in
	if n >= 1000000 then (best_n, max) 
	else
		let count = seq n n 0 in
		if count > max 
		then search (n + 1) n count
		else search (n + 1) best_n max
in

let start = Unix.time() in
let () = match (search 2 2 0) with (x,y) -> print_point x y in
print_endline ("completed in " ^ (string_of_float (Unix.time() -. start)) ^ " seconds")

(* 837799 *)

