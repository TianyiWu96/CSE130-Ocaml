(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)

(* assoc: 'a * 'b * ('b * 'a) list -> 'a
 * assoc takes the triple (d, k, l); if k exists in l, then it
 * returns the value associated with k, else d. The function takes
 * the inputs and uses the base case [(key, value)], which checks
 * the last member of l; in each other recursive case, it checks the
 * head element of the list then calls assoc on the tail. This function
 * is tail-recursive beacuse as soon as there is a match, the value is
 * returned and the function ends, and everything disappears; only while
 * there is no match does the recursive stack thing keep growing, but
 * nothing depends on the return value of a recursive call.
 *)
let rec assoc (d,k,l) = match l with
	| [] -> d
	| [(key, value)] -> if key = k then value else d
	| (key, value)::t -> if key = k then value else assoc (d, k, t)

(* removeDuplicates: 'a list -> 'a list
 * removeDuplicates takes a list l and removes the duplicate values
 * in it by creating a temporary list "seen" which holds all the unique
 * values seen thus far, checking each new value against the list using
 * List.mem, and then recursively calling itself on the remainder of the
 * list. This function is tail-recursive because nothing depends on the
 * value of the recursive call, the function always reaches the base case
 * and immediately returns thereafter.
 *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
      | [] -> seen
      | h::t -> 
        let seen' = if List.mem h seen then seen else h::seen in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))


(* wwhile: ('a -> 'a * bool) * 'a -> 'a
 * wwhile takes the tuple (function, value of 'a). The fuction must evaluate
 * to a tuple ('a, bool). wwhile evaluates function(value), and if the
 * boolean condition is true, then it evaluates the equivalent of
 * function(function(value)), and so forth. This function is tail-
 * recursive because the only thing that follows the recursive call is
 * the else statement which (I assume) will not be seen because it would
 * not be used if the recursive call happens.
 *)
let rec wwhile (f,b) =
	let (b', cond) = f b in
	if cond then wwhile (f,b') else b'

(* jesus i was literally stuck on this for like 3 hours ggggggg 
 *
 * fixpoint: ('a -> 'a) * -a -> 'a
 * fixpoint does this:
 * let b' = f b in if b = b' then b' else fixpoint(f,b')
 * but uses wwhile instead. Basically a temporary function f' is
 * defined to create the needed tuple for wwhile and then is passed
 * to wwhile. Why? Who knows.
 *
 * This is probably tail-recursive. I don't really want to think about
 * this function any more ;-;
 *)
 let fixpoint (f,b) =
	wwhile(let f' b' = (f b', b' <> f b') in f', b)


 (* fixpoint is sux ^__^*)


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
