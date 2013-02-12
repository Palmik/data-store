module Data.IntSet.Extra
( foldrM
) where

--------------------------------------------------------------------------------
import qualified Data.IntSet
--------------------------------------------------------------------------------

-- | Monadic fold over the elements of a `Data.IntSet.IntSet`,
-- associating to the right, i.e. from right to left. 
foldrM :: Monad f => (Int -> b -> f b) -> b -> Data.IntSet.IntSet -> f b
foldrM go start = Data.IntSet.foldr (\i acc -> acc >>= go i) (return start)
{-# INLINE foldrM #-}

