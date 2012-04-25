(*
 * By starting at the top of the triangle below and moving to adjacent 
 * numbers on the row below, the maximum total from top to bottom is 23.
 *
 *       3
 *      7 4
 *     2 4 6
 *    8 5 9 3
 *
 *  That is, 3 + 7 + 4 + 9 = 23.
 *
 * Find the maximum total from top to bottom of the triangle below:
 * (see problem 18.txt)
 *)

type triangle = (int, int array array);;

let triangle_from_file file : int array array = 
	