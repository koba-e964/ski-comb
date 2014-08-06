module Common where

data LExpr = -- lambda expression
   LAbst String LExpr
   |LApp LExpr LExpr
   |LName String deriving (Eq)

instance Show LExpr where
  show (LAbst x y) = "\\" ++ x ++ "." ++ show y
  show (LApp x y)  = "(" ++ show x ++ " " ++ show y ++ ")"
  show (LName x)   = x

