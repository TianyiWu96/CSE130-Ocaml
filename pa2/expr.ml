(*
 * expr.ml
 * cse130
 * based on code by Chris Stone
 *)

(* Please do not modify the names or types of any of the following
 * type constructors, or predefined functions, unless EXPLICITLY
 * asked to. You will loose points if you do.
 *)


(* REMEMBER TO DOCUMENT ALL FUNCTIONS THAT YOU WRITE OR COMPLETE *)

type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Thresh   of expr * expr * expr * expr
  | MagicCat of expr
  | MagicDog of expr * expr * expr

let rec exprToString e = match e with
  | VarX		-> "x"
  | VarY		-> "y"
  | Sine(e')		-> "sin(pi*" ^ exprToString e' ^ ")"
  | Cosine(e')		-> "cos(pi*" ^ exprToString e' ^ ")"
  | Average(e',f')	-> "((" ^ exprToString e' ^"+"^ exprToString f' ^ ")/2)"
  | Times(e',f')	-> exprToString e' ^ "*" ^ exprToString f'
  | Thresh(e',f',g',h')	-> "(" ^ exprToString e' ^ "<" ^ exprToString f' ^
  			   "?" ^ exprToString g' ^ ":" ^ exprToString h' ^ ")"
  (* catuses are lazy, (and so am i) so their function is as short as possible.
   * doguses are tryhards. I'm not though, so their function also sucks. Sorry
   * ^___^
   *)
  | MagicCat(e')  	-> "(" ^ exprToString e' ^ "/322)"
  | MagicDog(e',f',g')  -> "(" ^ exprToString e' ^ "*" ^ exprToString f' ^ "/"
			   ^ exprToString g' ^ ")"

(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
let buildMagicCat(e1)		   = MagicCat(e1)
let buildMagicDog(e1,e2,e3)        = MagicDog(e1,e2,e3)


let pi = 4.0 *. atan 1.0

let rec eval (e,x,y) = match e with
  | VarX		-> x
  | VarY		-> y
  | Sine(e')		-> sin(pi *. eval(e',x,y))
  | Cosine(e')		-> cos(pi *. eval(e',x,y))
  | Average(e',f')	-> (eval(e',x,y) +. eval(f',x,y)) /. 2.0
  | Times(e',f')	-> (eval(e',x,y) *. eval(f',x,y))
  | Thresh(e',f',g',h')	-> if (eval(e',x,y) < eval(f',x,y))
  				then eval(g',x,y) else eval(h',x,y)
  | MagicCat(e')        -> (eval(e',x,y)) /. 322.0
  | MagicDog(e',f',g')  -> eval(e',x,y) *. eval(f',x,y) /. eval(g',x,y)

(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(************** Add Testing Code Here ***************)
