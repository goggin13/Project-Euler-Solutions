(*
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 * Find the sum of all the primes below two million.
 *)

#load "unix.cma";;

let is_prime v : bool =
  let min: int = int_of_float (ceil (sqrt (float_of_int v))) in
  
  let rec is_prime_inner (candidate: int) : bool =
    if candidate > min then true
    else if (v mod candidate == 0) then false
    else is_prime_inner (candidate + 2)
  in 
  is_prime_inner 3
in

let sum_primes n : int = 
	let rec _sum_primes c sum : int =
		if c > n then sum
		else if (is_prime c) then _sum_primes (c + 2) (sum + c)
		else _sum_primes (c + 2) sum
	in
	_sum_primes 3 2
in

let start = Unix.time() in
let () = print_endline (string_of_int (sum_primes 2000000)) in
print_endline ("completed in " ^ (string_of_float (Unix.time() -. start)) ^ " seconds")