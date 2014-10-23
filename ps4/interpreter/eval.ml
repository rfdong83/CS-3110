open Ast

type builtin = value list -> environment -> value

and procedure =
  | ProcBuiltin of builtin
  | ProcLambda of variable list * environment * expression list

and value =
  | ValDatum of datum
  | ValProcedure of procedure

and binding = value ref Environment.binding
and environment = value ref Environment.environment

(* Parses a datum into an expression. *)
let rec read_expression (input : datum) : expression =
  let rec f (data : datum) : expression list =
      match data with
      | Cons (d1, Nil) -> [read_expression d1]
      | Cons (d1, d2) -> (read_expression d1) :: (f d2)
      | _ -> failwith "gfy" in
  match input with
  | Atom (Identifier id) when Identifier.is_valid_variable id ->
      ExprVariable (Identifier.variable_of_identifier id)
<<<<<<< HEAD
  | Atom (Identifier id) ->
     (* Above match case didn't succeed, so id is not a valid variable. *)
     failwith "Hiya"
  | Atom (Integer x) ->
      ExprSelfEvaluating (SEInteger x)
  | Atom (Boolean tf) ->
      ExprSelfEvaluating (SEBoolean tf)
  | _ ->
      ExprQuote Nil
=======
  | Atom (Boolean tf) -> 
      ExprSelfEvaluating (SEBoolean tf)
  | Atom (Integer n) -> 
      ExprSelfEvaluating (SEInteger n)
  | Cons (Atom(Identifier id),d2) when (Identifier.string_of_identifier id) = "quote" ->
      ExprQuote d2
  | Cons (Atom(Identifier id), Cons (d1, Cons (d2, Cons (d3 , Nil))))
     when (Identifier.string_of_identifier id) = "if" -> 
        ExprIf ((read_expression d1), (read_expression d2), (read_expression d3))
  | Cons (d1,d2) ->
      let temp = f (Cons (d1,d2)) in
      ExprProcCall (List.hd (temp), List.tl (temp))
  | Nil ->
      ExprQuote Nil
  | _ -> failwith "Everything else"

>>>>>>> 8be7a63199bc46cc2275dbc6b62415c090cdb041

(* Parses a datum into a toplevel input. *)
let read_toplevel (input : datum) : toplevel =
  match input with
  | _ -> ToplevelExpression (read_expression input)

(* This function returns an initial environment with any built-in
   bound variables. *)
let rec initial_environment () : environment =
  let variable_of_string s = 
    Identifier.variable_of_identifier (Identifier.identifier_of_string s) in 
  let ans = ref Environment.empty_environment in 
  let car l env = 
    match l with 
    | [ValDatum (Cons (a,b))] -> ValDatum a
    | _ -> failwith "fix later" in 
  let cdr l env =
    match l with 
    | [ValDatum (Cons (a,b))] -> ValDatum b
    | _ -> failwith "fix later" in 
  let cons l env =
    match l with 
    | [ValDatum d1; ValDatum d2] -> ValDatum (Cons (d1,d2))
    | _ -> failwith "fix later" in
  let ( + ) l env =
    let ans = List.fold_left (fun a x -> match x with 
              |ValDatum (Atom (Integer x)) -> a + x
              | _ -> failwith "not an int") 0 l in
    ValDatum (Atom (Integer ans)) in
  let ( * ) l env =
    let ans = List.fold_left (fun a x -> match x with  
              |ValDatum (Atom (Integer x)) -> a * x
              | _ -> failwith "not an int") 1 l in
    ValDatum (Atom (Integer ans)) in
  let equal l env =
    match l with
    | [ValDatum d1; ValDatum d2] -> ValDatum (Atom (Boolean (d1=d2)))
    | _ -> failwith "not equal" in
  let eval l env =
    match l with
    | [ValDatum d] -> eval (read_expression d) !ans
    | _ -> failwith "idk?" in


  ans := Environment.add_binding !ans ((variable_of_string "course"), 
                                        ref (ValDatum (Atom(Integer 3110)))
                                      );
  ans := Environment.add_binding !ans ((variable_of_string "car"),
                                        ref (ValProcedure (ProcBuiltin car))
                                      );
  ans := Environment.add_binding !ans ((variable_of_string "cdr"),
                                        ref (ValProcedure (ProcBuiltin cdr))
                                      );
  ans := Environment.add_binding !ans ((variable_of_string "cons"),
                                        ref (ValProcedure (ProcBuiltin cons))
                                      );
  ans := Environment.add_binding !ans ((variable_of_string "+"),
                                        ref (ValProcedure (ProcBuiltin ( + )))
                                      );
  ans := Environment.add_binding !ans ((variable_of_string "*"),
                                        ref (ValProcedure (ProcBuiltin ( * )))
                                      );
  ans := Environment.add_binding !ans ((variable_of_string "equal?"),
                                        ref (ValProcedure (ProcBuiltin equal))
                                      );
  ans := Environment.add_binding !ans ((variable_of_string "eval"),
                                        ref (ValProcedure (ProcBuiltin eval))
                                      );
  !ans


(* Evaluates an expression down to a value in a given environment. *)
(* You may want to add helper functions to make this function more
   readable, because it will get pretty long!  A good rule of thumb
   would be a helper function for each pattern in the match
   statement. *)
and eval (expression : expression) (env : environment) : value =
  match expression with
  | ExprSelfEvaluating (SEInteger x) ->
      ValDatum (Atom (Integer x))
  | ExprSelfEvaluating (SEBoolean tf) ->
      ValDatum (Atom (Boolean tf))
  | ExprVariable var ->
      if (Environment.is_bound env var) then
          !(Environment.get_binding env var)
      else
          failwith "Variable"
  | ExprQuote (Cons (q, _)) ->
      ValDatum q
  | ExprLambda (_, _) ->
      failwith "SAYY"
  | ExprProcCall (e1, lst) ->
      let vlst = List.rev (List.fold_left (fun a x -> (eval x env)::a) [] lst) in
      (match (eval e1 env) with
      | ValProcedure (ProcBuiltin f) -> f vlst env
      | _ -> failwith "not a function")
  | ExprIf (e1, e2, e3) ->
      if e1 = ExprSelfEvaluating (SEBoolean false) then 
          eval e3 env
      else
          eval e2 env

  | ExprAssignment (_, _) ->
     failwith "Say something funny, Rower!"
  | ExprLet (_, _)
  | ExprLetStar (_, _)
  | ExprLetRec (_, _)     ->
     failwith "Ahahaha!  That is classic Rower."
  | _ -> failwith "missing stuff"

(* Evaluates a toplevel input down to a value and an output environment in a
   given environment. *)
let eval_toplevel (toplevel : toplevel) (env : environment) :
      value * environment =
  match toplevel with
  | ToplevelExpression expression -> (eval expression env, env)
  | ToplevelDefinition (_, _)     ->
     failwith "I couldn't have done it without the Rower!"

let rec string_of_value value =
  let rec string_of_datum datum =
    match datum with
    | Atom (Boolean b) -> if b then "#t" else "#f"
    | Atom (Integer n) -> string_of_int n
    | Atom (Identifier id) -> Identifier.string_of_identifier id
    | Nil -> "()"
    | Cons (car, cdr) -> string_of_cons car cdr

  and string_of_cons car cdr =
    let rec strings_of_cons cdr =
      match cdr with
      | Nil -> []
      | Cons (car, cdr) -> (string_of_datum car) :: (strings_of_cons cdr)
      | _ -> ["."; string_of_datum cdr;] in
    let string_list = (string_of_datum car) :: (strings_of_cons cdr) in
    "(" ^ (String.concat " " string_list) ^ ")" in
  
  match value with
  | ValDatum (datum) -> string_of_datum datum
  | ValProcedure (ProcBuiltin p) -> "#<builtin>"
  | ValProcedure (ProcLambda (_, _, _)) -> "#<lambda>"
