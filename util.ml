
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