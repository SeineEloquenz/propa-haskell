module Ast where

data Exp t = Var t
            | Const Integer
            | Sum (Exp t) (Exp t)
            | Less (Exp t) (Exp t)
            | And (Exp t) (Exp t)
            | Not (Exp t)
            | IfThenElse (Exp t) (Exp t) (Exp t)
instance Show (Exp t) where
  show (Var t) = "var"
  show (Const t) = show t
  show (Sum e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Less e1 e2) = "(" ++ show e1 ++ " < " ++ show e2 ++ ")"
  show (And e1 e2) = "(" ++ show e1 ++ " && " ++ show e2 ++ ")"
  show (Not e) = "(!" ++ show e ++ ")"
type Env t = t -> Integer

eval :: Env t -> Exp t -> Integer
eval env (Var t) = env t
eval env (Const t) = t
eval env (Sum e1 e2) = eval env e1 + eval env e2
eval env (Less e1 e2) = if eval env e1 < eval env e2 then 1 else 0
eval env (And e1 e2) = if eval env e1 /= 0 && eval env e2 /= 0 then 1 else 0
eval env (Not e) = if eval env e == 0 then 1 else 0