exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

let lookup (x,evn) = let temp = listAssoc (x, evn) in
  match temp with
    | None	-> raise (MLFailure("Variable not bound: " ^ x))
    | Some x'	-> x'

let rec eval (evn,e) = match e with
  | Const c		-> Int c
  | Var v		-> lookup (v, evn)
  | True		-> Bool true
  | False		-> Bool false
  | App (f, n)		-> (let arg = eval(evn, n) in
  			     (match eval(evn,f) with
			      	| Closure(new_evn, None, x, e) -> eval((x,
						arg)::new_evn@evn, e)
				| Closure(new_evn, Some y, x, e) as f ->
				eval((y, arg)::(x,f)::new_evn@evn, e)))
  | Fun (f, n)		-> Closure(evn, None, f, n)
  | If (e1, e2, e3)	-> (let x = eval(evn, e1) in
  			     match x with
			       | Bool b	-> if b then eval (evn, e2) else eval
			       (evn, e3)
			       | _	-> raise (MLFailure ("Invalid if
						       statement")))
  | Let (s, e2, e3)	-> (let new_evn = (s, eval(evn, e2))::evn in eval
				(new_evn, e3))
  | Letrec (s, e2, e3)	-> (let new_evn = (s, eval(evn, e2))::evn in eval
				(new_evn, e3))
(* we use the values to define which operators to check for, rather than the
 other way around *)
  | Bin(e1, op, e2)	-> (let x = eval(evn, e1) in
  			   let y = eval(evn, e2) in
			   match (x, y) with
			     (* handle integer ops first *)
			     | (Int i, Int j)	-> (match op with
			       | Plus	-> Int (i+j)
			       | Minus	-> Int (i-j)
			       | Mul	-> Int (i*j)
			       | Div	-> (match j with
			         | 0	-> raise (MLFailure("Err: divide by 0"))
				 | _	-> Int(i/j))
			       | Lt	-> Bool (i<j)
			       | Le	-> Bool (i<=j)
			       | Eq	-> Bool (i=j)
			       | Ne	-> Bool (i!=j)
			       | _ 	-> raise (MLFailure("Err: invalid
						       input")))
			     | (Bool b1, Bool b2) -> (match op with
			       | Or	-> Bool (b1||b2)
			       | And	-> Bool (b1&&b2)
			       | Eq	-> Bool (b1=b2)
			       | Ne	-> Bool (b1!=b2)
			       | _	-> raise (MLFailure("Err: invalid
						       input")))
			     | _	-> raise (MLFailure("Err: invalid
					     input")))

  | _			-> raise (MLFailure("invalid input"))




(**********************     Testing Code  ******************************)
