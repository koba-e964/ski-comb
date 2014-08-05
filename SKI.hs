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
evaluable (K :*: _ :*: _) = True
evaluable (I :*: _) = True
evaluable (x :*: y) = evaluable x || evaluable y
evaluable _ = False

eval :: Expr -> Expr
eval (S :*: x :*: y :*: z) = (x :*: z) :*: (y :*: z)
eval (S :*: K) = K :*: I
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

seInt :: Int -> Expr

seInt 0 = K
seInt n
  | n < 0 = undefined
  | otherwise = K :*: (S :*: I :*: (K :*: seInt (n-1)))

exprToInt :: Expr -> Int
exprToInt exp = sub 0 exp
  where
    sub x y
      | x `seq` y `seq` False                     = undefined
      | evaluable $ y :*: Label "undefined" :*: I = sub (x+1) $ evalFully $ y :*: Label "undefined" :*: I
      | y == Label "undefined"                    = x - 1
      | otherwise                                 = error $ "stopped evaluation: " ++ show y


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
-- succ x = \a b -> b x
succ :: Expr
succ = comp :*: K :*: (comp :*: (S :*: I) :*: K)

-- pred x = x zero id
pred :: Expr
pred = comp :*: (fapp :*: I) :*: (fapp :*: K)

