(*
 * CSE 130 FALL 2014
 * misc.ml
 * PROF. SORIN LERNER
 * ASSIGNMENT 1
 * XIAOQING LI || A97071753
 *)

let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* sumList : int list -> int 
 * 
 * This sumList function takes an argument l, and if it empty, returns
 * 0; otherwise, it adds the value in the head element
 * to a recursive call on the tail.
 *)

let rec sumList l = match l with
	| []	-> 0
	| h::t	-> h + sumList t

(* digitsOfInt : int -> int list 
 *
 * This function takes an int n input and outputs the digits of that
 * integer in order as a list. To do this, it uses a recursive helper
 * function because it relies on recursion using an accumulator "ret".
 * The last digit of the int is calculated using n%10, which always
 * returns the ones digit. The result is appended to the accumulator.
 * Then, the process repeats with the next digit, which is found using
 * n/10. The output list is already in order because the process begins
 * at the end of the integer.
 *)

let digitsOfInt n =
	let rec helper n ret =
		if n < 10 then n::ret
		else helper (n/10) ((n mod 10)::ret)
	in helper n []

(* listReverse : 'a list -> 'a list 
 * 
 * The listReverse function recursively reverses a given list. It
 * simply takes a list and appends the head to a new empty list,
 * then calls listReverse on the tail and appends the result to that
 * list. Thus, in each iteration, the head of the previous tail is
 * appended to the output, reversing the list.
 *)
	

let rec listReverse l = match l with
	| []	-> []
	| h::t	-> listReverse t @ (h::[])
	

(* let rec helper l2 ret = match l2 with
		| []	-> []
		| h::t 	-> h::ret helper t ret in
		
		match l with
		| []	-> []
		| h::t	-> *)

(* digitalRoot : int -> int
 * 
 * The digitalRoot function calculates the digital root of a given
 * integer by calling the digitsOfInt function on it to generate a list,
 * summing that list using sumList, and then repeating the process
 * until a number with one digit (i.e. <10) is reached.
 *)

let rec digitalRoot n =
	if n < 10 then n
	else digitalRoot (sumList(digitsOfInt(n)))

(* additivePersistence : int -> int 
 * 
 * The additivePersistence function calculates the number of summations
 * needed to reach a digitalRoot by using a helper function with an
 * increasing index "ret" which increases each time a summation is
 * computed. Otherwise, it is exactly the same as digitalRoot.
 *)

let additivePersistence n =
	let rec helper a ret =
		if a < 10 then ret
		else helpT Friday 10/17 at 3 Per (sumList(digitsOfInt(a))) (ret+1)
	in helper n 0

(* palindrome : string -> bool 
 * 
 * The palindrome function uses the given explode function to
 * generate a list from the input string. Then, it uses listReverse
 * on that string and checks it against the original list. If they
 * are equal, it returns true.
 *)

let palindrome w =
	if (explode w) = (listReverse (explode w)) then true
	else false


