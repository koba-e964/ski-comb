module Binary where

import SKI
import qualified Data.ByteString as ByteString
import Data.ByteString (append, singleton, ByteString)

size :: Expr -> Int
depth :: Expr -> Int

size K = 1
size S = 1
size I = 1
size (x :*: y) = size x + size y
depth K = 1
depth S = 1
depth I = 1
depth (x :*: y) = max (size x) (size y) + 1

pack :: Expr -> ByteString

pack S = singleton 1
pack K = singleton 2
pack I = singleton 3
pack (x :*: y) = let
  px = pack x
  py = pack y
  lpx = ByteString.length px
  lpy = ByteString.length py
   in
  conv4 lpx `append` conv4 lpy `append` px `append` py
  


conv4 :: Int -> ByteString
conv4 val = ByteString.pack $ take 4 $ map (fromIntegral .(`mod` 256)) $ iterate (`div` 256) val

