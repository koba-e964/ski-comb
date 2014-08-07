module SKICC where

import Common
import SKI
import Control.Monad.State
import Data.List (foldl', union)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Debug.Trace

type Env = Map String ([String], LExpr2) -- pair of bound variables and expression

type M = State (Int,Env)
runM :: M a -> (a,Env)
runM x = let (a, s) = runState x (0, Map.empty) in (a, snd s)

remove :: Eq a => a -> [a] -> [a]
remove x = filter (/=x)

data LExpr2 = 
  L2Comb String
  | L2Name String
  | L2App LExpr2 LExpr2

infixl 2 `L2App`

instance Show LExpr2 where
  show (L2Comb str) = "@" ++ str
  show (L2Name str) = str
  show (L2App x y) = "(" ++ show x ++ " " ++ show y ++ ")"

-- reference : http://www.tatapa.org/~takuo/kotori_ski/
compile :: LExpr -> Expr
compile z = let
  (l2, env) = runM $ convertToLExpr2 z
  in
  convertToSKI env l2


newCombName :: M String
newCombName = do
  (a,_) <- get
  modify (\(x,y) -> (x+1,y))
  return $ "M" ++ show a

newComb :: M LExpr2
newComb = fmap L2Comb newCombName

-- return a new combinator
addComb :: [String] -> LExpr2 -> M LExpr2
addComb vs expr = do
  name <- newCombName
  modify (\(x,y) -> (x, Map.insert name (vs, expr) y))
  return $ L2Comb name

convertToLExpr2 :: LExpr -> M LExpr2
convertToLExpr2 le@(LAbst v expr) = do
  sub <- convertToLExpr2 expr
  let vs = variables le
  ret <- addComb (vs ++ [v]) sub
  return $ foldl' L2App ret (map L2Name vs) -- let vs = [v1,v2,..] in ret v1 v2 ...
convertToLExpr2 (LApp x y) = do
  mx <- convertToLExpr2 x
  my <- convertToLExpr2 y
  return $ L2App mx my
convertToLExpr2 (LName x) = return $ L2Name x

variables :: LExpr -> ([String])
variables (LName x) = ([x])
variables (LApp x y) = variables x `union` variables y
variables (LAbst v expr) = remove v (variables expr)

-- | eliminate y expr eliminates y in expr
-- example:
-- elimination of y from...
--   x ====> K x
--   y ====> I
--   x y =====> S (K x) I

eliminate :: Env -> String -> LExpr2 -> LExpr2
eliminate env v l@(L2Comb name)
  | Map.member name env = let
      (var, expr) = fromMaybe (error $ "no such combinator: " ++ name) $ Map.lookup name env
    in L2App (L2Comb "K") $ eliminateAll env var expr
  | otherwise           = L2App (L2Comb "K") l
eliminate _   v (L2Name x)
    | v == x = L2Comb "I"
    | otherwise = L2App (L2Comb "K") (L2Name x)
eliminate env v (L2App x (L2Name name)) | name == v && isConstant x -- special case
  = x where
    isConstant (L2Comb _) = True
    isConstant (L2Name _) = False
    isConstant (L2App a b) = isConstant a || isConstant b
eliminate env v (L2App x y) = L2App (L2Comb "S" `L2App` eliminate env v x) (eliminate env v y)

eliminateAll :: Env -> [String] -> LExpr2 -> LExpr2
eliminateAll env vs expr = foldr (eliminate env) expr vs 

convertToSKI :: Env -> LExpr2 -> Expr
convertToSKI env (L2App x y) = convertToSKI env x :*: convertToSKI env y
convertToSKI _ (L2Comb "S") = S
convertToSKI _ (L2Comb "K") = K
convertToSKI _ (L2Comb "I") = I
convertToSKI env (L2Comb name)
  |  Map.member name env = let
      (var, expr) = fromMaybe (error "(>_<)") $ Map.lookup name env
    in convertToSKI env $ eliminateAll env var expr
convertToSKI _ x            = error $ "invalid expr :" ++ show x
