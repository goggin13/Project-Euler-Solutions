(* 
 * 2520 is the smallest number that can be divided by each of the 
 * numbers from 1 to 10 without any remainder. 
 *
 * What is the smallest positive number that is evenly divisible 
 * by all of the numbers from 1 to 20?
 *)

let is_divisible n under : bool =
  let rec inner n test : bool =
    if test == 0 
      then true
    else if (n mod test == 0)
      then inner n (test - 1)
    else
      let () = print_endline ((string_of_int n) ^ " is not divisble by " ^ (string_of_int test)) in
      false
  in inner n under
in

let find_smallest under : int =
  let rec inner test under : int =
    let () = print_endline (string_of_int test) in
    if is_divisible test under 
    then test
    else inner (test + under) under
  in
  inner under under
in 

print_endline (string_of_int (find_smallest 20))