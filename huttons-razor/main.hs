
module HuttonsRazor where



e1 = Add (Lit 1) (Lit 9001)
e2 = Add (Add e1 (Lit 2)) (Lit 20001)




data Expression =
  Lit Integer |
  Add Expression Expression



-- Functions

eval :: Expression -> Integer
eval (Lit int) = int
eval (Add e1 e2) = (+) (eval e1) (eval e2)

printExpression :: Expression -> String
printExpression (Lit int) = show int
printExpression (Add e1 e2) =
     printExpression e1
  ++ " + "
  ++ printExpression e2
