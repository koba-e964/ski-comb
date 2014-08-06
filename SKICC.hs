module SKICC where

import Common
import SKI
import Control.Monad.State
import Data.List (foldl', union)
import Data.Map (Map)
import qualified Data.Map as Map

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

instance Show LExpr2 where
  show (L2Comb str) = "@" ++ str
  show (L2Name str) = str
  show (L2App x y) = "(" ++ show x ++ " " ++ show y ++ ")"

-- reference : http://www.tatapa.org/~takuo/kotori_ski/
compile :: LExpr -> Expr
compile z = undefined


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

