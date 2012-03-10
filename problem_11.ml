(*
 * In the 20 x 20 grid below, four numbers along a diagonal line have been marked in red.
 *
 *     08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
 *     49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
 *     81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
 *     52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
 *     22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
 *     24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
 *     32 98 81 28 64 23 67 10 (26) 38 40 67 59 54 70 66 18 38 64 70
 *     67 26 20 68 02 62 12 20 95 (63) 94 39 63 08 40 91 66 49 94 21
 *     24 55 58 05 66 73 99 26 97 17 (78) 78 96 83 14 88 34 89 63 72
 *     21 36 23 09 75 00 76 44 20 45 35 (14) 00 61 33 97 34 31 33 95
 *     78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
 *     16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
 *     86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
 *     19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
 *     04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
 *     88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
 *     04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
 *     20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
 *     20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
 *     01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
 *
 *     The product of these numbers is 26 x 63 x 78 x 14 = 1788696.
 * 
 *  What is the greatest product of four adjacent numbers in any direction 
 *  (up, down, left, right, or diagonally) in the 20 x 20 grid?
*)
#load "util.cmo";;
open Util;;

type pos = (int * int);;
type row = int list;;
type grid = row list;;
type direction = (int * int);;

let print_pos (p: pos) : unit =
	match p with (x, y) -> print_point x y

let value (p: pos) (g: grid) : int option =
	match p with (x, y) ->
		if y < 0 or x < 0 or (List.length g) <= y then None
		else
			let row = List.nth g y in
			if List.length row <= x then None
			else Some (List.nth row x)
;;


let directions : direction list =
	[(0, 1); (0, -1);
	 (1, 0); (1, 1); (1, -1);
	 (-1, 0); (-1, 1); (-1, -1)]
;;

let prod (p: pos) (g: grid) (d: direction) : int =
	match d with (dx, dy) ->
	match p with (x, y) ->
	
	let rec inner distance prod x1 y1 : int =
		if distance = 4 then prod
		else match value (x1, y1) g with
			 Some(v) -> 
				inner (distance + 1) (prod * v) (x1 + dx) (y1 + dy)
		 | None -> prod
	in 
	
	inner 0 1 x y
;;

let max_prod (p: pos) (g: grid): int =
	let rec inner ls max_prod : int =
		match ls with
		  d :: ds -> 
				let product = (prod p g d) in
				if product > max_prod then inner ds product
				else inner ds max_prod
		| [] -> max_prod
	in inner directions 0
;;

let fold_grid (f: int -> pos -> int) (a: int) (g: grid) : int =	
	
	let rec inner row_num row_accum g: int =		
		match g with
		   row :: rows -> 
			   let rec process_row col_num a_row col_accum : int = 
			   	 match a_row with
				      col :: cols -> 
					      let new_accum = f col_accum (row_num, col_num) in
								process_row (col_num + 1) cols new_accum
					  | [] ->	col_accum
				 in 
				 let new_accum = process_row 0 row row_accum in
			 	 inner (row_num + 1) new_accum rows
				 
		 | [] -> row_accum
	in
	inner 0 a g
;;

let g : grid = 
 [[08; 02; 22; 97; 38; 15; 00; 40; 00; 75; 04; 05; 07; 78; 52; 12; 50; 77; 91; 08];
  [49; 49; 99; 40; 17; 81; 18; 57; 60; 87; 17; 40; 98; 43; 69; 48; 04; 56; 62; 00];
  [81; 49; 31; 73; 55; 79; 14; 29; 93; 71; 40; 67; 53; 88; 30; 03; 49; 13; 36; 65];
  [52; 70; 95; 23; 04; 60; 11; 42; 69; 24; 68; 56; 01; 32; 56; 71; 37; 02; 36; 91];
  [22; 31; 16; 71; 51; 67; 63; 89; 41; 92; 36; 54; 22; 40; 40; 28; 66; 33; 13; 80];
  [24; 47; 32; 60; 99; 03; 45; 02; 44; 75; 33; 53; 78; 36; 84; 20; 35; 17; 12; 50];
  [32; 98; 81; 28; 64; 23; 67; 10; 26; 38; 40; 67; 59; 54; 70; 66; 18; 38; 64; 70];
  [67; 26; 20; 68; 02; 62; 12; 20; 95; 63; 94; 39; 63; 08; 40; 91; 66; 49; 94; 21];
  [24; 55; 58; 05; 66; 73; 99; 26; 97; 17; 78; 78; 96; 83; 14; 88; 34; 89; 63; 72];
  [21; 36; 23; 09; 75; 00; 76; 44; 20; 45; 35; 14; 00; 61; 33; 97; 34; 31; 33; 95];
  [78; 17; 53; 28; 22; 75; 31; 67; 15; 94; 03; 80; 04; 62; 16; 14; 09; 53; 56; 92];
  [16; 39; 05; 42; 96; 35; 31; 47; 55; 58; 88; 24; 00; 17; 54; 24; 36; 29; 85; 57];
  [86; 56; 00; 48; 35; 71; 89; 07; 05; 44; 44; 37; 44; 60; 21; 58; 51; 54; 17; 58];
  [19; 80; 81; 68; 05; 94; 47; 69; 28; 73; 92; 13; 86; 52; 17; 77; 04; 89; 55; 40];
  [04; 52; 08; 83; 97; 35; 99; 16; 07; 97; 57; 32; 16; 26; 26; 79; 33; 27; 98; 66];
  [88; 36; 68; 87; 57; 62; 20; 72; 03; 46; 33; 67; 46; 55; 12; 32; 63; 93; 53; 69];
  [04; 42; 16; 73; 38; 25; 39; 11; 24; 94; 72; 18; 08; 46; 29; 32; 40; 62; 76; 36];
  [20; 69; 36; 41; 72; 30; 23; 88; 34; 62; 99; 69; 82; 67; 59; 85; 74; 04; 36; 16];
  [20; 73; 35; 29; 78; 31; 90; 01; 74; 31; 49; 71; 48; 86; 81; 16; 23; 57; 05; 54];
  [01; 70; 54; 71; 83; 51; 54; 69; 16; 92; 33; 48; 61; 43; 52; 01; 89; 19; 67; 48]]
in

let f acc p =
	let m_prod = max_prod p g in
	if (m_prod) > acc then m_prod
	else acc
in

let max = fold_grid f 0 g in

print_endline (string_of_int (max));;
(* 70600674 *)

