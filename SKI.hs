module SKI where

data Expr = S | K | I | Expr :*: Expr | Label String deriving (Eq, Show)

infixl 2 :*:

evalFully :: Expr -> Expr
evalFully e
  | evaluable e = evalFully (eval e)
  | otherwise   = e

evaluable :: Expr -> Bool
evaluable (S :*: _ :*: _ :*: _) = True
evaluable (K :*: _ :*: _) = True
evaluable (I :*: _) = True
evaluable (x :*: y) = evaluable x || evaluable y
evaluable _ = False

eval :: Expr -> Expr
eval (S :*: x :*: y :*: z) = (x :*: z) :*: (y :*: z)
eval (K :*: x :*: _) = x
eval (I :*: x) = x
eval (x :*: y) = eval x :*: eval y
eval x = x



{- standard functions -}

comp :: Expr
comp = S :*: (K :*: S) :*: K

seInt :: Int -> Expr

seInt 0 = K
seInt n
  | n < 0 = undefined
  | otherwise = K :*: (fapp :*: seInt (n-1))

selfApp :: Expr
selfApp = S :*: I :*: I

-- fapp x y = y x
fapp :: Expr
fapp = S :*: (K :*: (S :*: I)) :*: K


-- flip x y z = x z y

flip :: Expr
flip = S :*: (K :*: (S :*: (K :*: (S :*: I)) :*: K :*: K)) :*: (S :*: (K :*: S) :*: (S :*: (K :*: K) :*: S))


