
let pow (n: int) (p: int) = 
	int_of_float ((float_of_int n) ** (float_of_int p))
	
let square (n: int) : int = pow n 2

let print_triplet (a, b, c) : unit =
	let s = "(" ^ (string_of_int a) ^ ", "
							^ (string_of_int b) ^ ", "
							^ (string_of_int c) ^ ")"
	in print_endline s

let print_point x y : unit =
	let s = "(" ^ (string_of_int x) ^ ", "
							^ (string_of_int y) ^ ")"
	in print_endline s
	

let factor n : int list =
	let s : int = int_of_float (floor (sqrt (float_of_int n))) in
  let ls = Array.init s (fun x -> x + 1) in
  let collect acc c = 
	  if n mod c = 0 then c :: (n / c) :: acc
	  else acc
	in
  Array.fold_left collect [] ls
