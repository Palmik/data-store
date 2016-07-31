module Data.IntSet.Extra
( foldrM
, foldrM'
, foldlM
, foldlM'
) where

--------------------------------------------------------------------------------
import qualified Data.IntSet
--------------------------------------------------------------------------------

-- TODO: Measure INLINE/SPECIALIZE tradeoff.

-- | Monadic fold over the elements of a `Data.IntSet.IntSet`,
-- associating to the right, i.e. from right to left.
foldrM :: Monad f => (Int -> b -> f b) -> b -> Data.IntSet.IntSet -> f b
foldrM go start = Data.IntSet.foldr (\i acc -> acc >>= go i) (return start)
{-# INLINE foldrM #-}
--{-# SPECIALIZE foldrM :: (Int -> b -> Maybe b) -> b -> Data.IntSet.IntSet -> Maybe b #-}

-- | Strict monadic fold over the elements of a `Data.IntSet.IntSet`,
-- associating to the right, i.e. from right to left.
foldrM' :: Monad f => (Int -> b -> f b) -> b -> Data.IntSet.IntSet -> f b
foldrM' go start = Data.IntSet.foldr' (\i acc -> acc >>= go i) (return start)
{-# INLINE foldrM' #-}
--{-# SPECIALIZE foldrM' :: (Int -> b -> Maybe b) -> b -> Data.IntSet.IntSet -> Maybe b #-}

-- | Monadic fold over the elements of a `Data.IntSet.IntSet`,
-- associating to the left, i.e. from left to right.
foldlM :: Monad f => (b -> Int -> f b) -> b -> Data.IntSet.IntSet -> f b
foldlM go start = Data.IntSet.foldl (\acc i -> acc >>= flip go i) (return start)
{-# INLINE foldlM #-}
--{-# SPECIALIZE foldlM :: (b -> Int -> Maybe b) -> b -> Data.IntSet.IntSet -> Maybe b #-}

-- | Monadic fold over the elements of a `Data.IntSet.IntSet`,
-- associating to the left, i.e. from left to right.
foldlM' :: Monad f => (b -> Int -> f b) -> b -> Data.IntSet.IntSet -> f b
foldlM' go start = Data.IntSet.foldl' (\acc i -> acc >>= flip go i) (return start)
{-# INLINE foldlM' #-}
--{-# SPECIALIZE foldlM' :: (b -> Int -> Maybe b) -> b -> Data.IntSet.IntSet -> Maybe b #-}

