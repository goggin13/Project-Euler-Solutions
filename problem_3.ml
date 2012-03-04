(* 
 * The prime factors of 13195 are 5, 7, 13 and 29.
 * What is the largest prime factor of the number 600851475143 ?
 *)

let prime_factors (n: int) : int list =
  let rec inner number candidate factors: int list =
    if number <= 1 
      then factors
    else if (number mod candidate == 0) 
      then inner (number / candidate) (candidate + 1) (candidate :: factors)
    else
      inner number (candidate + 1) factors
  in
  inner n 2 []
in

let factors : int list = prime_factors(600851475143) in
print_endline (string_of_int (List.hd factors));