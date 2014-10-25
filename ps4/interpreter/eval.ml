open Ast

exception Error

type builtin = value list -> environment -> value

and procedure =
  | ProcBuiltin of builtin
  | ProcLambda of variable list * environment * expression list

and value =
  | ValDatum of datum
  | ValProcedure of procedure

and binding = value ref Environment.binding
and environment = value ref Environment.environment

exception Error

(* Parses a datum into an expression. *)
let rec read_expression (input : datum) : expression =

  (* Helper Function: takes in Scheme3110 list and applies read to each datum
   *  and returns the expression list. *)
  let rec eval_list (data: datum): expression list =
      match data with
      | Cons (d1, Nil) -> [read_expression d1]
      | Cons (d1, d2) -> (read_expression d1) :: (eval_list d2)
      | _ -> failwith "gfy" in

  (* Helper Function: takes in an expression list and runs read_expression*)
  let elist_to_varlist (lst: expression list): variable list =
    List.rev (List.fold_left 
     (fun a x -> match x with
                |ExprVariable v -> v::a
                |_ -> a)
     [] lst) in

  let expr_to_var (expr: expression): variable = 
    match expr with 
    | ExprVariable v -> v
    | _ -> failwith "That isn't going to be a variable..." in

  let rec lst_to_bind (lst: datum): let_binding list =
    match lst with
    | Cons (Cons(v, e), Nil) -> 
        [((expr_to_var (read_expression v)), (read_expression e))]
    | Cons (Cons (v1, e1), rest) -> 
        (expr_to_var((read_expression v1)), (read_expression e1))::(lst_to_bind rest)
    | _ -> failwith "That wasn't a Scheme list.." in 

  match input with
  (* Dealing with Variables *)
  | Atom (Identifier id) when Identifier.is_valid_variable id ->
      ExprVariable (Identifier.variable_of_identifier id)
    (* Dealing with Booleans *)
  | Atom (Boolean tf) 
  | Cons (Atom (Boolean tf), Nil)-> 
      ExprSelfEvaluating (SEBoolean tf)
  (* Dealing with Integers *)
  | Atom (Integer n)
  | Cons (Atom (Integer n), Nil) -> 
      ExprSelfEvaluating (SEInteger n)
  (* Dealing with Quotes*)
  | Cons (Atom(Identifier id),d2) 
      when (Identifier.string_of_identifier id) = "quote" ->
        ExprQuote d2
  (* Dealing with Ifs *)
  | Cons (Atom(Identifier id), Cons (d1, Cons (d2, Cons (d3 , Nil))))
     when (Identifier.string_of_identifier id) = "if" -> 
        ExprIf ((read_expression d1),(read_expression d2),(read_expression d3))
  (* Dealing with Lambdas *)
  | Cons (Atom(Identifier id), Cons (list1, list2))
      when (Identifier.string_of_identifier id) = "lambda" ->
        ExprLambda ((elist_to_varlist (eval_list list1)), (eval_list list2))
  (* Dealing with Assignments *)
  | Cons (Atom(Identifier id), Cons (Atom(Identifier var), Cons (expr, Nil)))
      when (Identifier.string_of_identifier id) = "set!" ->
        ExprAssignment 
        ((Identifier.variable_of_identifier var),(read_expression expr))
  (* Dealing with LetStars *)
  | Cons (Atom(Identifier id), Cons (lst, elist))
    when (Identifier.string_of_identifier id) = "let*" ->
        ExprLetStar ((lst_to_bind lst),(eval_list elist))

  (* Dealing with Lets *)
  | Cons (Atom(Identifier id), Cons (lst, elist))
    when (Identifier.string_of_identifier id) = "let" ->
        ExprLet ((lst_to_bind lst),(eval_list elist))

  (* Dealing with LetRecs *)
  | Cons (Atom(Identifier id), Cons (lst, elist))
    when (Identifier.string_of_identifier id) = "letrec" ->
        ExprLet ((lst_to_bind lst),(eval_list elist))

  (* Dealing with Procedures*)
  | Cons (d1,d2) ->
      let temp = eval_list (Cons (d1,d2)) in
      ExprProcCall (List.hd (temp), List.tl (temp))

  (* Dealing with Nil *)
  | Nil ->
      ExprQuote Nil
  | _ -> failwith "Everything else"

(* Parses a datum into a toplevel input. *)
let read_toplevel (input : datum) : toplevel =
  let datum_to_var input =
    match (read_expression input) with
    | ExprVariable x -> x
    | _ -> failwith "Wasn't going to be a variable" in

  match input with
  | Cons (Atom (Identifier id), Cons (var, Cons(expr, Nil))) 
    when (Identifier.string_of_identifier id) = "define" -> 
      ToplevelDefinition ((datum_to_var var), (read_expression expr))
  | _ -> ToplevelExpression (read_expression input)


(* This function returns an initial environment with any built-in
   bound variables. *)
let rec initial_environment () : environment =
  let ans = ref Environment.empty_environment in 
  (* Helper Function: takes a string and returns a variable with that name*)
  let variable_of_string s = 
    Identifier.variable_of_identifier (Identifier.identifier_of_string s) in 

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
  let evaluate l env =
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
                                        ref (ValProcedure (ProcBuiltin evaluate))
                                      );
  !ans


(* Evaluates an expression down to a value in a given environment. *)
(* You may want to add helper functions to make this function more
   readable, because it will get pretty long!  A good rule of thumb
   would be a helper function for each pattern in the match
   statement. *)
and eval (expression : expression) (env : environment) : value =
  (* Helper Function: returns true if l contains all unique elements,
   * false otherwise *)
  let rec unique l = 
    match l with
    | [] -> true 
    | [x] -> true
    | h::t -> (not (List.mem h t)) && (unique t) in 
  (* Helper Function: takes an expression list and runs eval on each element
   * returning a value list *)
  let elist_to_vlist lst =
    List.rev (List.fold_left (fun a x -> (eval x env)::a)
        [] lst) in

  let self_evaluating_helper (se: self_evaluating) (env: environment) : value =
    match se with
    | SEInteger x -> ValDatum (Atom (Integer x))
    | SEBoolean tf -> ValDatum (Atom (Boolean tf)) in

  let variable_helper (var: variable) (env: environment) : value =
      if (Environment.is_bound env var) then
          !(Environment.get_binding env var)
      else
          failwith "variable helper" in

  let quote_helper (quote: datum) (env: environment) : value =
      match quote with
      | Atom a -> ValDatum (Atom a)
      | Cons (q, _) -> ValDatum q
      | Nil -> ValDatum Nil in

  let lambda_helper (vlist, elist : variable list * expression list) 
                    (env: environment) : value =
      if unique vlist then 
          ValProcedure (ProcLambda (vlist, env, elist))
        else
          failwith "lambda helper" in


  let procedure_helper (funct, inputs : expression * expression list)
                       (env: environment) : value =

      match (eval funct env) with
      | ValProcedure (ProcBuiltin f) -> f (elist_to_vlist inputs) env
      | ValProcedure (ProcLambda (parameters, env, h::t)) -> 
          (* Making sure that there are same number of inputs and parameters *)
          if List.length parameters = List.length inputs then
              (* Create temporary call frame with new bindings to evaluate in *)
              let env' = List.fold_left2 
                (fun a x1 x2 -> Environment.add_binding a (x1,ref x2))
                env parameters (elist_to_vlist inputs) in 
              (* Evaluate the rest of the call in this newly created call frame *)
              eval (ExprProcCall (h, t)) env'
          else
              (ValDatum (Atom (Integer (List.length parameters))))
      | ValDatum data ->
          eval (read_expression data) env
      | _ -> failwith "procedure error 2" in

  let if_helper (e1, e2, e3 : expression * expression * expression)
                (env: environment) : value =
      if e1 = ExprSelfEvaluating (SEBoolean false) then 
            eval e3 env
        else
            eval e2 env in

  let assignment_helper (var,expr) env =
    if (Environment.is_bound env var) then
      let bind = Environment.get_binding env var in 
      bind := (eval expr env);
      ValDatum Nil
    else
      failwith "assignment error" in 

  let let_helper (blist, elist) (env: environment): value = 
      (*let env' = List.fold_left 
          (fun a x -> match x with 
                      | (v,e) -> let ans = eval e a in
                                  if Environment.is_bound a v then
                                    let thing = (Environment.get_binding a v) in
                                    thing := ans; 
                                    a
                                 else
                                  Environment.add_binding a ((v, ref ans)))
         env blist in
         eval (ExprProcCall ((List.hd elist), (List.tl elist))) env' in *)
      let tuple = List.fold_left (fun a x -> match x with 
                                  | (v, h::t) -> 
                                  (v::fst(a),e::snd(a))) ([],[]) blist in
      eval (ExprProcCall (ExprLambda ((fst tuple), t) , (snd tuple))) env in
    
  let letstar_helper (blist,elist) (env: environment): value = 
      let env' = ref env in
      let tuple = List.fold_left (fun a x -> match x with 
                                  | (v,e) -> 
                                  env' := Environment.add_binding !env' 
                                          (v, ref (eval e !env')); 
                                  (v::fst(a),e::snd(a))) ([],[]) blist in
      eval (ExprProcCall (ExprLambda ((fst tuple), elist) , (snd tuple))) !env' in   


  let letrec_helper (blist, elist) env =
      let env' = ref env in
      let tuple = List.fold_left (fun a x -> match x with 
                                  | (v,e) -> 
                                  env' := Environment.add_binding !env'
                                          (v, ref (eval (read_expression Nil) !env'));
                                  Environment.get_binding !env' v := (eval e !env'); 
                                  (v::fst(a),e::snd(a))) ([],[]) blist in
      eval (ExprProcCall (ExprLambda ((fst tuple), elist) , (snd tuple))) !env' in      


  match expression with
  | ExprSelfEvaluating se -> self_evaluating_helper se env
  | ExprVariable var -> variable_helper var env
  | ExprQuote quote -> quote_helper quote env
  | ExprLambda (vlist, elist) -> lambda_helper (vlist, elist) env
  | ExprProcCall (e1, exprlist) -> procedure_helper (e1, exprlist) env
  | ExprIf (e1, e2, e3) -> if_helper (e1, e2, e3) env
  | ExprAssignment (var, expr) -> assignment_helper (var, expr) env
  | ExprLetStar (blist, elist) -> letstar_helper (blist, elist) env
  | ExprLet (blist,elist) -> let_helper (blist, elist) env
  | ExprLetRec (blist, elist) -> letrec_helper (blist, elist) env

(* Evaluates a toplevel input down to a value and an output environment in a
   given environment. *)
let eval_toplevel (toplevel : toplevel) (env : environment) :
      value * environment =
  match toplevel with
  | ToplevelExpression expression -> (eval expression env, env)
  | ToplevelDefinition (var, expr) ->
    if Environment.is_bound env var then 
      let tmp = (Environment.get_binding env var) in 
      tmp := (eval expr env);
      (ValDatum Nil, env)
    else
      let env' = Environment.add_binding env 
                  (var, ref (eval (read_expression Nil) env)) in
      let value = eval expr env' in 
      Environment.get_binding env' var := value;
      (ValDatum Nil, env')

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