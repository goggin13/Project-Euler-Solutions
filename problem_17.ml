(* 
 * If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
 * then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 * 
 * If all the numbers from 1 to 1000 (one thousand) inclusive were written out 
 * in words, how many letters would be used?
 * 
 * NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) 
 * contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The 
 * use of "and" when writing out numbers is in compliance with British usage.
 *)

let one_to_word n : string =
	match n with 
	  1. -> "one"
	| 2. -> "two"
	| 3. -> "three"
	| 4. -> "four"
	| 5. -> "five"
	| 6. -> "six"
	| 7. -> "seven"
	| 8. -> "eight"
	| 9. -> "nine"
	| 9. -> "nine"
	| 10. -> "ten"
	| 11. -> "eleven"
	| 12. -> "twelve"
	| 13. -> "thirteen"
	| 14. -> "fourteen"
	| 15. -> "fifteen"
	| 16. -> "sixteen"
	| 17. -> "seventeen"
	| 18. -> "eighteen"
	| 19. -> "nineteen"											
	| 0. -> ""
	| _ -> "NAN"
in

let ten_to_word n : string =
	if n = 1. then "ten"
	else if n = 2. then "twenty"
  else if n = 3. then "thirty"
  else if n = 4. then "forty"
  else if n = 5. then "fifty"
	else if n = 8. then "eighty"
  else (one_to_word n) ^ "ty"
in

let first_digit (n: float) : float =
	if n < 10. then n
	else if n < 100. then (floor (n /. 10.))
  else (floor (n /. 100.))
in

let rec as_word (n: float) : string = 
  if n < 20. then (one_to_word n)
  else if n < 100. then 
    (ten_to_word (first_digit n)) ^ "" ^ (as_word (mod_float n 10.))
  else if n = 1000. then "onethousand"
  else
	  let rem = (mod_float n 100.) in
	  (one_to_word (first_digit n)) ^ "hundred" ^ (if rem > 0. then "and"  ^ (as_word rem) else "")
in

let numbers : string array = Array.init 1000 (fun n -> (as_word (float_of_int (n + 1)))) in
let process acc s : int =
	let () = print_endline s in
	acc + (String.length s)
in
let sum = Array.fold_left process 0 numbers
in
print_endline (string_of_int sum);;

(* 21124 *)
