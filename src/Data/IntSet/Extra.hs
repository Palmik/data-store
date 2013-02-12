module Data.IntSet.Extra
( foldrM
) where

--------------------------------------------------------------------------------
import qualified Data.IntSet
--------------------------------------------------------------------------------

foldrM :: Monad f => (Int -> b -> f b) -> b -> Data.IntSet.IntSet -> f b
foldrM go start = Data.IntSet.foldr (\i acc -> acc >>= go i) (return start)

