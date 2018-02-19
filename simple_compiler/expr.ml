(*
A suite of functions for handling arithmetical expressions

Expressions are represented like this

    Num N
    Var A
    Add (E1, E2)
    Mul (E1, E2)

where N is a number, A is a character
and E1, E2 are themselves expressions,
*)

type expr =
    Num of int
  | Var of char
  | Add of expr*expr
  | Mul of expr*expr
  | Cond of expr*expr*expr
  | Let of expr*expr*expr

let expr1 : expr = Add (Var 'a',Mul (Num 2,Var 'b'))

let expr2 : expr =
  Add (Mul (Num 1, Var 'b'),
       Mul (Add (Mul (Num 2, Var 'b') ,Mul (Num 1, Var 'b') ), Num 0))

let expr3 : expr =
  Add (Num 1000, Cond (Num 1, Num 10, Num 1))

let expr4 : expr =
  Add (Num 1000, Cond (Num 0, Num 10, Num 1))

let expr5 : expr =
  Let (Var 't', Num 5, Add (Var 't', Num 10))

(*
Printing

Turn an expression into a string, so that
  Add (Var 'a', Mul (Num 2, Var 'b'))
  {add,{var,a},{mul,{num,2},{var,b}}
is turned into
  "(a+(2*b))"
*)

(*
print : expr -> string
*)

let rec print (e : expr) : string =
  match e with
  | Num n -> string_of_int n
  | Var v -> String.make 1 v
  | Add (e1,e2) -> String.concat "" ["("; print e1  ; "+"  ; print e2  ;")"]
  | Mul (e1,e2) -> String.concat "" ["("; print e1  ; "*"  ; print e2  ;")"]
  | Let  (e1,e2,e3) -> String.concat "" ["("; print e1  ; "="  ; print e2 ; ":" ; print e3 ; ")"]
  | Cond (e1,e2,e3) -> String.concat "" ["("; print e1 ; "?" ; print e2 ; ":" ; print e3 ; ")"]

(*
%
% parsing
%

% recognise expressions
% deterministic, recursive descent, parser.

% the function returns two things
%   - an expression recognised at the beginning of the string
%     (in fact, the longers such expression)
%   - whatever of the string is left
%
% for example, parse("(-55*eeee)+1111)") is
%   {{mul,{num,-55},{var,eeee}} , "+1111)"}
*)

(*
% recgnise a fully-bracketed expression, with no spaces etc.
*)

(*
First some auxiliary functions to get a sequence of characters.
Used in parsing an integer.
*)


(*
is_digit : char -> bool
*)

let is_digit (ch : char) : bool =
  '0' <= ch && ch <= '9'

  (*
  get the longest initial sequence from the list where
  each element satisfies the predicate
  *)

  (*
  get_while : ('a -> bool) -> 'a list -> 'a list * 'a list
  *)

let rec get_while (p : ('a -> bool)) (xs : 'a list) : ('a list * 'a list) = 
  match xs with
    | (y::ys) ->
      if p y
      then
        let (zs,rest) = get_while p ys in
        (y::zs,rest)
      else
        ([],y::ys)
    | [] -> ([],[])

(*
get_digits : char list -> char list * char list
*)

let get_digits : (char list -> char list * char list) =
  get_while is_digit

(*
recognise an integer, a sequence of digits
with an optional '-' sign at the start
*)

let get_int (xs : char list) : (char list * char list) =
  match xs with
  | [] -> ([],[])
  | (y::ys) ->
    if y='-'
    then
      let (n,rest) = get_digits ys in
      (y::n,rest)
    else
      get_digits (y::ys)

(*
convert a list of digits (with optional -) into
an int
*)

let rec list_to_int (chars : char list) : int =
  let rec worker (digs : char list) : int =
    match digs with
    | [] -> 0
    | (d::ds) -> (int_of_char d - int_of_char '0') + 10 * (worker ds)
  in
  match chars with
  | [] -> 0
  | ('-'::rest) -> (-1)* worker (List.rev rest)
  | xs -> worker (List.rev xs)

(*
parse : char list -> expr * char list
*)

let rec parse (input : char list) : expr * char list =
  match input with
  | ('('::rest) ->
    let (e1,rest1)   = parse rest in
    let (op::rest2)  = rest1 in
    (* TODO: instead of matching with ? vs _, match for lookahead length? *)
    (match op with
        | '=' -> let (e2, rest3)  = parse rest2 in
                 let (':'::rest4) = rest3 in
                 let (e3, rest5)  = parse rest4 in
                 let (')'::rest6) = rest5 in
                 (Let (e1, e2, e3), rest6)
        | '?' -> let (e2, rest3)  = parse rest2 in
                 let (':'::rest4) = rest3 in
                 let (e3, rest5)  = parse rest4 in
                 let (')'::rest6) = rest5 in
                 (Cond (e1, e2, e3), rest6)
        | _   -> let (e2,rest3)   = parse rest2 in
                 let (')'::rest4) = rest3 in
                 (match op with
                     | '+' -> Add (e1,e2), rest4
                     | '*' -> Mul (e1,e2), rest4
                 )
    )
  | (ch::rest) ->
    if ('a'<= ch && ch <= 'z')
    then (Var ch, rest)
    else
      let (ns,rest1) = get_int (ch::rest) in
      (Num (list_to_int ns), rest1)

(*
Rewriting the parse function to work over Ocaml strings
rather than lists of Char.
*)

(*
get_int takes a string and starting position and finds the
longest digit sequence starting at that position, returning
the integer value of that sequence paired with the position
of the first non-digit. For example
   get_nat "1234xx" 2 = (34, 4)
*)

(*
get_nat : string -> int -> int * int
*)

let get_nat (input : string) (pos : int) : int*int =
  let rec gn (pos : int) : int =
    let curr = String.get input pos in
    if ('0' <= curr && curr <= '9')
    then
      gn (pos+1)
    else
      pos
  in
  let rec conv (st : int) (en : int) (acc : int) : int =
    if en >= st then
      conv (st+1) en (10*acc + (int_of_char(String.get input st) - int_of_char '0'))
    else
      acc
  in
  let next = gn pos in
  (conv pos (next - 1) 0 , next)

(*
Parses the largest expression at the beginning of the string.
*)

(*
fast_parse : string -> expr
*)

let fast_parse (input : string) : expr =
  let rec fparse (pos : int) : expr*int =
    match String.get input pos with
    | '(' ->
      let (e1,n1) = fparse (pos+1) in
      let (op,n2) = (String.get input n1, n1+1) in
      let (e2,n3) = fparse n2 in
      let (')',n4) = (String.get input n3, n3+1) in
      ((match op with
          | '+' -> Add (e1,e2)
          | '*' -> Mul (e1,e2)),
       n4)
    | '-' ->
      let (n,pos1) = get_nat input (pos+1) in
      ( Num ((-1)*n),pos1)
    | ch ->
      if ('a'<= ch && ch <= 'z')
      then
        (Var ch, pos+1)
      else
      let (n,pos1) = get_nat input pos in
      ( Num n,pos1)
  in
  fst (fparse 0)

(*
Evaluate an expression in an environment.
*)

type env = (char*int) list

let env1 = [ ('a',12); ('b', -11) ]

(*
lookup : char -> (char * int) list -> int
*)

let rec lookup v env =
  match env with
  | [] -> 0
  | ((x,n)::ps) -> if x=v then n else lookup v ps

let build_local_env (ik, iv) env =
  if (List.exists (fun (k, v) -> k = ik) env) then
      List.map (fun (k, v) -> if k = ik then (k, iv) else (k, v)) env
  else
      env@[(ik, iv)]
                          

(*
eval : (char * int) list -> expr -> int
*)

let rec eval (env : env) (e: expr) : int =
  match e with
  | Num n -> n
  | Var v -> lookup v env
  | Add (e1,e2) -> eval env e1 + eval env e2
  | Mul (e1,e2) -> eval env e1 * eval env e2
  | Let (e1,e2,e3)  -> match e1 with
                          | Var ch -> eval (build_local_env (ch, (eval env e2)) env) e3
  | Cond (e1,e2,e3) -> let r = eval env e1 in
                       if r = 1 then eval env e2 else eval env e3 

(*
Compiler and virtual machine
*)

(*
Instructions
   Push N - push integer N onto the stack
   Fetch A - lookup value of variable a and push the result onto the stack
   Add2 - pop the top two elements of the stack, add, and push the result
   Mul2 - pop the top two elements of the stack, multiply, and push the result
*)

type instr =
  | Push of int
  | Fetch of char
  | Put2
  | Add2
  | Mul2
  | Cond3
  | Let3

type program = instr list

type stack = int list

(*
compiler
*)

(*
compile : expr -> instr list
*)

let rec compile e =
  match e with
  | Num n -> [Push n]
  | Var v -> [Fetch v]
  | Add (e1,e2) -> compile e2 @ compile e1 @ [Add2]
  | Mul (e1,e2) -> compile e2 @ compile e1 @ [Mul2]
  | Let (e1,e2,e3)  -> compile e2 @ [Let3] @ compile e1 @ compile e3
  | Cond (e1,e2,e3) -> compile e3 @ compile e2 @ compile e1 @ [Cond3]

(*
execute an instruction, and when the code is exhausted,
return the top of the stack as result.
classic tail recursion
*)

(*
run : instr list -> (char * int) list -> int list -> int
*)

let rec run prog env stack =
  match prog with
  | (Push n :: cont) ->
    run cont env (n::stack)
  | (Fetch v :: cont) ->
    run cont env (lookup v env :: stack)
  | (Add2 :: cont) ->
    (match stack with
      (n1::n2::rest) -> run cont env (n1+n2::rest))
  | (Mul2 :: cont) ->
    (match stack with
       (n1::n2::rest) -> run cont env (n1*n2::rest))
  | (Cond3 :: cont) ->
    (match stack with
        (n1::n2::n3::rest) -> if n1 = 1 then run cont env (n2::rest) else run cont env (n3::rest))
  | (Let3 :: Fetch ch :: cont) ->
    (match stack with
        (n1::rest) -> run cont (build_local_env (ch, n1) env) (rest))
  | [] ->
    match stack with
    (n::_) -> n

(*
compile then run … should be the same as eval
*)

(*
execute : (char * int) list -> expr -> int
*)

let rec execute env e =
  run (compile e) env []

let rec simplify exp =
  match exp with
  | Add (e1,Num 0) -> simplify e1
  | Add (Num 0,e2) -> simplify e2
  | Mul (e1,Num 1) -> simplify e1
  | Mul (Num 1,e2) -> simplify e2
  | Mul (_,Num 0)  -> Num 0
  | Mul (Num 0,_)  -> Num 0
  | Add (e1,e2)    ->
    let s1 = simplify e1 in
    let s2 = simplify e2 in
    if not(s1=e1 && s2=e2)
    then
      simplify (Add (s1,s2))
    else
      Add (s1,s2)
  | Mul (e1,e2)    ->
    let s1 = simplify e1 in
    let s2 = simplify e2 in
    if not(s1=e1 && s2=e2)
    then
      simplify (Mul (s1,s2))
    else
      Mul (s1,s2)
  | e -> e


(*
Unused utility
*)


(*
convert a string to a char list
*)

(*
string_to_list : string -> char list
*)


let rec string_to_list (str : string) : char list =
  match String.length str with
  | 0 -> []
  | _ -> (String.get str 0) :: (string_to_list (String.sub str 1 ((String.length str) -1)))
