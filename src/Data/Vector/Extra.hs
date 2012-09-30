module Data.Vector.Extra
( updateAt
, updateAt'
) where

--------------------------------------------------------------------------------
import qualified Data.Vector as V
--------------------------------------------------------------------------------

updateAt :: (a -> a) -> Int -> V.Vector a -> V.Vector a
updateAt f n old = V.accum (\x _ -> f x) old [(n, ())]

updateAt' :: (a -> (a, b)) -> Int -> V.Vector a -> (V.Vector a, b)
updateAt' f n old = (old V.// [(n, updated)], res)
    where
      (updated, res) = f (old V.! n)

