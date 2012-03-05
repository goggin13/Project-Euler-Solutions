(*
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, 
 * we can see that the 6th prime is 13.
 *
 * What is the 10,001st prime number?
 *)
#load "unix.cma";;


(* the nth prime number *)
let nth_prime n : int =
  
  (* true if v is divisible by any integers given in primes *)
  let is_prime v primes : bool =
    (* only check up to the square root *)
    let min: int = int_of_float (ceil (sqrt (float_of_int v))) in
    let rec is_prime_inner primes: bool =
      match primes with
         [] -> true
       | candidate :: tail ->
          if candidate > min then true
          else if (v mod candidate == 0) then false
          else is_prime_inner tail
    in 
    is_prime_inner primes
  in
  
  (* we will start at 0, and build up an array of primes that we will
   * use to determine if later numbers are prime *)
  let rec nth_prime_inner (i: int) (primes: int list) (length: int): int =
    if length == (n + 1)
    then List.nth primes (n)
    else if (is_prime i primes)
    then 
      nth_prime_inner (i + 2) (List.append primes [i]) (length + 1)
    else nth_prime_inner (i + 2) (primes) length
  in
  
  nth_prime_inner 3 [2; 3] 2
in

let start = Unix.time() in
let () = print_endline (string_of_int (nth_prime 10001)) in
print_endline ("completed in " ^ (string_of_float (Unix.time() -. start)) ^ " seconds")


