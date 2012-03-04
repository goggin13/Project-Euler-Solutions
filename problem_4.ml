(*
 * A palindromic number reads the same both ways. 
 * The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.
 * Find the largest palindrome made from the product of two 3-digit numbers.
 *)

let is_palindrome n : bool =
 let rec reverse n rev : int =
   if n == 0 then rev
   else 
     let dig = n mod 10 in
     reverse (n/10) (rev * 10 + dig)
  in
  (reverse n 0) == n
in

let rec find_palindrome x y max : int =
  if x <= 100 && y < 100
  then max
  else 
    let product = x * y in
    let new_max = if (is_palindrome product) && product > max
                  then 
                    let s : string = "(" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ") - " ^ (string_of_int product) in
                    let () = print_endline s in
                    product
                  else max in
    if x > 100 
    then find_palindrome (x-1) y new_max
    else find_palindrome (y-1) (y-1) new_max
in


print_endline (string_of_int (find_palindrome 999 999 0))

