open Assertions
open Identifier
open Ast
open Eval
open Environment

(* <<<<<<<<<<<<<<<<<<<<<<<<<<<< EVAL >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> *)

(* HELPERS*)

let variable_of_string s = 
  variable_of_identifier (identifier_of_string s)

(* Initializing some stuff for testing: *)
let quote = Atom (Identifier (identifier_of_string "quote"))
let lambda = Atom (Identifier (identifier_of_string "lambda"))
let iff = Atom (Identifier (identifier_of_string "if"))
let sets = Atom (Identifier (identifier_of_string "set!"))
let lets = Atom (Identifier (identifier_of_string "let"))
let letstar = Atom (Identifier (identifier_of_string "let*"))
let letrec = Atom (Identifier (identifier_of_string "letrec"))
let define = Atom (Identifier (identifier_of_string "define"))
let x = Atom (Identifier (identifier_of_string "x"))
let y = Atom (Identifier (identifier_of_string "y"))
let two = Atom (Integer 2)
let one = Atom (Integer 1)
let plus = Atom (Identifier (identifier_of_string "+"))


let vx = (variable_of_string "x")

(* ----------------------- Testing read_expression ------------------*)

TEST_UNIT "read_expression_test1" = assert_true (
  read_expression x = 
  ExprVariable vx
)

TEST_UNIT "read_expression_test2" = assert_true (
  read_expression (Atom (Boolean true)) =
  ExprSelfEvaluating (SEBoolean true)
)

TEST_UNIT "read_expression_test3" = assert_true (
  read_expression (Atom (Boolean false)) =
  ExprSelfEvaluating (SEBoolean false)
)

TEST_UNIT "read_expression_test4" = assert_true (
  read_expression (Cons(quote, x)) = ExprQuote x
)

TEST_UNIT "read_expression_test5" = assert_true (
  read_expression (Cons (quote, (Cons (x, Cons (y, Nil))))) = 
  ExprQuote ((Cons (x, Cons (y, Nil))))
)

TEST_UNIT "read_expression_test6" = assert_true (
  read_expression (Cons (iff, Cons(Atom (Integer 1), 
    Cons (Atom (Integer 2), Cons (Atom (Integer 3), Nil))))) = 
  ExprIf (ExprSelfEvaluating (SEInteger 1), 
          ExprSelfEvaluating (SEInteger 2),      
          ExprSelfEvaluating (SEInteger 3))
)

TEST_UNIT "read_expression_test7" = assert_true (
  read_expression (Cons (lambda, Cons (Cons (x, Nil),Cons (x, Nil)))) =
  ExprLambda ([vx], [ExprVariable vx]) 
)

TEST_UNIT "read_expression_test8" = assert_true (
  read_expression (Cons (sets, Cons (x, Cons (Atom (Integer 1), Nil)))) = 
  ExprAssignment (vx, ExprSelfEvaluating (SEInteger 1))
)

TEST_UNIT "read_expression_test9" = assert_true (
  read_expression 
    (Cons (lets, Cons(Cons (Cons (x, Cons (two, Nil)), Nil),
      Cons (Cons (x, Nil), Nil)))) = 
  (ExprLet ([(vx, ExprSelfEvaluating (SEInteger 2))], 
    [ExprProcCall (ExprVariable vx, [])]))
)

TEST_UNIT "read_expression_test10" = assert_true (
    read_expression 
    (Cons (letstar, Cons(Cons (Cons (x, Cons (two, Nil)), Nil),
      Cons (Cons (x, Nil), Nil)))) = 
  (ExprLetStar ([(vx, ExprSelfEvaluating (SEInteger 2))], 
    [ExprProcCall (ExprVariable vx, [])]))
)

TEST_UNIT "read_expression_test10" = assert_true (
    read_expression 
    (Cons (letrec, Cons(Cons (Cons (x, Cons (two, Nil)), Nil),
      Cons (Cons (x, Nil), Nil)))) = 
  (ExprLetRec ([(vx, ExprSelfEvaluating (SEInteger 2))], 
    [ExprProcCall (ExprVariable vx, [])]))
)

TEST_UNIT "read_expression_test11" = assert_true (
  read_expression (Cons (plus, Cons (one, Cons (two, Nil)))) = 
  ExprProcCall (ExprVariable (variable_of_string "+"),
    [ExprSelfEvaluating (SEInteger 1); ExprSelfEvaluating (SEInteger 2)])
)

(* ------------------------ Testing read_toplevel ----------------------- *)

TEST_UNIT "read_toplevel_test1" = assert_true (
  read_toplevel x = 
  ToplevelExpression (ExprVariable vx)
)

TEST_UNIT "read_toplevel_test2" = assert_true (
  read_toplevel (Atom (Boolean true)) =
  ToplevelExpression (ExprSelfEvaluating (SEBoolean true))
)

TEST_UNIT "read_toplevel_test3" = assert_true (
  read_toplevel (Atom (Boolean false)) =
  ToplevelExpression (ExprSelfEvaluating (SEBoolean false))
)

TEST_UNIT "read_toplevel_test4" = assert_true (
  read_toplevel (Cons(quote, x)) = ToplevelExpression (ExprQuote x)
)

TEST_UNIT "read_toplevel_test5" = assert_true (
  read_toplevel (Cons (quote, (Cons (x, Cons (y, Nil))))) = 
  ToplevelExpression (ExprQuote ((Cons (x, Cons (y, Nil)))))
)

TEST_UNIT "read_toplevel_test6" = assert_true (
  read_toplevel (Cons (iff, Cons(Atom (Integer 1), 
    Cons (Atom (Integer 2), Cons (Atom (Integer 3), Nil))))) = 
  ToplevelExpression (ExprIf (ExprSelfEvaluating (SEInteger 1), 
          ExprSelfEvaluating (SEInteger 2),      
          ExprSelfEvaluating (SEInteger 3)))
)

TEST_UNIT "read_toplevel_test7" = assert_true (
  read_toplevel (Cons (lambda, Cons (Cons (x, Nil),Cons (x, Nil)))) =
  ToplevelExpression (ExprLambda ([vx], [ExprVariable vx]))
)

TEST_UNIT "read_toplevel_test8" = assert_true (
  read_toplevel (Cons (sets, Cons (x, Cons (Atom (Integer 1), Nil)))) = 
  ToplevelExpression (ExprAssignment (vx, ExprSelfEvaluating (SEInteger 1)))
)

TEST_UNIT "read_toplevel_test9" = assert_true (
  read_toplevel 
    (Cons (lets, Cons(Cons (Cons (x, Cons (two, Nil)), Nil),
      Cons (Cons (x, Nil), Nil)))) = 
  ToplevelExpression (ExprLet ([(vx, ExprSelfEvaluating (SEInteger 2))], 
    [ExprProcCall (ExprVariable vx, [])]))
)

TEST_UNIT "read_toplevel_test10" = assert_true (
    read_toplevel 
    (Cons (letstar, Cons(Cons (Cons (x, Cons (two, Nil)), Nil),
      Cons (Cons (x, Nil), Nil)))) = 
  ToplevelExpression (ExprLetStar ([(vx, ExprSelfEvaluating (SEInteger 2))], 
    [ExprProcCall (ExprVariable vx, [])]))
)

TEST_UNIT "read_toplevel_test10" = assert_true (
    read_toplevel 
    (Cons (letrec, Cons(Cons (Cons (x, Cons (two, Nil)), Nil),
      Cons (Cons (x, Nil), Nil)))) = 
  ToplevelExpression (ExprLetRec ([(vx, ExprSelfEvaluating (SEInteger 2))], 
    [ExprProcCall (ExprVariable vx, [])]))
)

TEST_UNIT "read_toplevel_test11" = assert_true (
  read_toplevel (Cons (plus, Cons (one, Cons (two, Nil)))) = 
  ToplevelExpression (ExprProcCall (ExprVariable (variable_of_string "+"),
    [ExprSelfEvaluating (SEInteger 1); ExprSelfEvaluating (SEInteger 2)]))
)