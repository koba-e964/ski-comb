module SKI where

data Expr = S | K | I | Expr :*: Expr | Label String deriving (Eq, Show)

infixl 2 :*:

evalFully :: Expr -> Expr
evalFully e
  | evaluable e = evalFully (eval e)
  | otherwise   = e

evaluable :: Expr -> Bool
evaluable (S :*: _ :*: _ :*: _) = True
evaluable (S :*: K :*: _) = True
evaluable (S :*: (K :*: I)) = True
evaluable (S :*: (K :*: _) :*: I) = True
evaluable (K :*: _ :*: _) = True
evaluable (I :*: _) = True
evaluable (x :*: y) = evaluable x || evaluable y
evaluable _ = False

eval :: Expr -> Expr
eval (S :*: x :*: y :*: z) = (x :*: z) :*: (y :*: z)
eval (S :*: K) = K :*: I
eval (S :*: (K :*: I)) = I
eval (S :*: (K :*: m) :*: I) = m
eval (K :*: x :*: _) = x
eval (I :*: x) = x
eval (x :*: y) = eval x :*: eval y
eval x = x

evalTimes :: Int -> Expr -> Expr
evalTimes 0 x = x
evalTimes n x
 | n > 0 = evalTimes (n-1) (eval x) 
 | otherwise = x

{- standard functions -}

comp :: Expr
comp = S :*: (K :*: S) :*: K

-- Usual Church encoding. n |-> \f x. f^n x
seInt :: Int -> Expr

seInt 0 = S :*: K
seInt n
  | n < 0 = undefined
  | otherwise = SKI.succ :*: seInt (n-1)

exprToInt :: Expr -> Int
exprToInt exp = destruct 0 $ evalFully $ exp :*: Label "undefinedF" :*: Label "undefinedX"
  where
    destruct x (Label "undefinedX") = x
    destruct x (_ :*: t) = destruct (x + 1) t


selfApp :: Expr
selfApp = S :*: I :*: I

-- fapp x y = y x
fapp :: Expr
fapp = S :*: (K :*: (S :*: I)) :*: K


-- flip x y z = x z y

flip :: Expr
flip = evalFully $ S :*: (K :*: (S :*: (K :*: (S :*: I)) :*: K :*: K)) :*: (S :*: (K :*: S) :*: (S :*: (K :*: K) :*: S))

iota :: Expr
iota = evalFully $ comp :*: (fapp :*: K) :*: (fapp :*: S)

yComb :: Expr
yComb = S :*: (K :*: selfApp) :*: (S :*: (S :*: (K :*: S) :*: K) :*: (K :*: selfApp))

{- boolean -}
true :: Expr
false :: Expr

true = K
false = K :*: I

bnot :: Expr
bnot = S :*: (S :*: I :*: (K :*: false)) :*: (K :*: true)

boolToExpr :: Bool -> Expr
boolToExpr t = if t then true else false

exprToBool :: Expr -> Bool
exprToBool expr = case evalFully (expr :*: K :*: I) of
  K -> True
  I -> False
  _ -> error $ "not a bool: " ++ show expr


band :: Expr
bor :: Expr

band = S :*: S :*: (K :*: (K :*: false))
bor  = S :*: I :*: (K :*: true)


{- natural number -}
-- succ n = \f x -> f (n f x)
succ :: Expr
succ = S :*: (S :*: (K :*: S) :*: K)

-- pred x = x zero id TODO very buggy
pred :: Expr
pred = comp :*: (fapp :*: I) :*: (fapp :*: K)

