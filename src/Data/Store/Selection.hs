{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

module Data.Store.Selection
( (.<)
, (.<=)
, (.>)
, (.>=)
, (./=)
, (.==)
, (.&&)
, (.||)
, not
, all
, all1D
, any
, any1D
, IsSelection(..)
, Selection
) where

--------------------------------------------------------------------------------
import           Prelude hiding (not, all, any)
--------------------------------------------------------------------------------
import           Data.Monoid ((<>))
import qualified Data.IntSet
import qualified Data.List
import qualified Data.IntMap.Strict as Data.IntMap
import qualified Data.Map.Strict    as Data.Map
--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Type as I
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Store.Selection"

-- INTERFACE

infix  4  .==, ./=, .<, .<=, .>=, .>
infixr 3  .&&
infixr 2  .||

-- | The expression (@not' sel@) is a selection that includes all values
-- except those that match the selection @sel@. 
not :: IsSelection sel => sel tag krs irs ts -> Selection tag krs irs ts
not = SelectionNot
{-# INLINE not #-}

-- | Selection that matches the intersection of all the selections in the
-- list or everything if the list is empty.
all :: [Selection tag krs irs ts] -> Selection tag krs irs ts
all []  = error $ moduleName <> ".all: empty list."
all [s] = s
all (s:rest) = Data.List.foldl' (.&&) s rest -- this way we do not have to intersect with "everything"
{-# INLINE all #-}

-- | The expression (@'Data.Store.Selection.all1D' d ss@) is equivalent to (@'Data.Store.Selection.all'' $ map ($ d) ss@).
all1D :: (tag, n) -> [(tag, n) -> Selection tag krs irs ts] -> Selection tag krs irs ts
all1D _ [] = error $ moduleName <> ".all1D: empty list."
all1D d [h] = h d
all1D d (h:rest) = Data.List.foldl' (\acc f -> acc .&& f d) (h d) rest -- this way we do not have to intersect with "everything"
{-# INLINE all1D #-}

-- | Selection that matches the union of all the selections in the
-- list or nothing if the list is empty.
any :: [Selection tag krs irs ts] -> Selection tag krs irs ts
any [] = error $ moduleName <> ".any: empty list."
any (x:xs) = Data.List.foldl' (.||) x xs
{-# INLINE any #-}

-- | The expression (@'Data.Store.Selection.any1D' d ss@) is equivalent to (@'Data.Store.Selection.any'' $ map ($ d) ss@).
any1D :: (tag, n) -> [(tag, n) -> Selection tag krs irs ts] -> Selection tag krs irs ts
any1D _ [] = error $ moduleName <> ".any1D: empty list."
any1D d (x:xs) = Data.List.foldl' (\acc f -> acc .|| f d) (x d) xs
{-# INLINE any1D #-}

-- | The expression (@sDim .< c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k < c@.
--
-- Complexity of @'Data.Store.Selection.resolve'@: /O(log n + k)/
(.<) :: I.GetDimension n (I.Index irs ts) => (tag, n) -> I.DimensionType n irs ts -> Selection tag krs irs ts
(.<) (_, n) = SelectionType . SelectionDimension n (Condition True False False)
{-# INLINE (.<) #-}

-- | The expression (@sDim .<= c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k <= c@.
--
-- Complexity of @'Data.Store.Selection.resolve'@: /O(log n + k)/
(.<=) :: I.GetDimension n (I.Index irs ts) => (tag, n) -> I.DimensionType n irs ts -> Selection tag krs irs ts
(.<=) (_, n) = SelectionType . SelectionDimension n (Condition True True False)
{-# INLINE (.<=) #-}

-- | The expression (@sDim .> c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k > c@.
--
-- Complexity of @'Data.Store.Selection.resolve'@: /O(log n + k)/
(.>) :: I.GetDimension n (I.Index irs ts) => (tag, n) -> I.DimensionType n irs ts -> Selection tag krs irs ts
(.>) (_, n) = SelectionType . SelectionDimension n (Condition False False True)
{-# INLINE (.>) #-}

-- | The expression (@sDim .>= c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k >= c@.
--
-- Complexity of @'Data.Store.Selection.resolve'@: /O(log n + k)/
(.>=) :: I.GetDimension n (I.Index irs ts) => (tag, n) -> I.DimensionType n irs ts -> Selection tag krs irs ts
(.>=) (_, n) = SelectionType . SelectionDimension n (Condition False True True)
{-# INLINE (.>=) #-}

-- | The expression (@sDim ./= c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k /= c@.
--
-- Complexity of @'Data.Store.Selection.resolve'@: /O(n)/
(./=) :: I.GetDimension n (I.Index irs ts) => (tag, n) -> I.DimensionType n irs ts -> Selection tag krs irs ts
(./=) (_, n) = SelectionType . SelectionDimension n (Condition True False True)
{-# INLINE (./=) #-}

-- | The expression (@sDim .== c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k == c@.
--
-- Complexity of @'Data.Store.Selection.resolve'@: /O(log n)/
(.==) :: I.GetDimension n (I.Index irs ts) => (tag, n) -> I.DimensionType n irs ts -> Selection tag krs irs ts
(.==) (_, n) = SelectionType . SelectionDimension n (Condition False True False)
{-# INLINE (.==) #-}

-- | The expression (@s1 .&& s2@) is a selection that includes the
-- intersection of the selections @s1@ and @s2@.
--
-- Complexity of @'Data.Store.Selection.resolve'@: /O(c(s1) + c(s2) + s(s1) + s(s2)/
(.&&) :: (IsSelection s1, IsSelection s2)
      => s1 tag krs irs ts -> s2 tag krs irs ts -> Selection tag krs irs ts
(.&&) = SelectionA
{-# INLINE (.&&) #-}

-- | The expression (@s1 .|| s2@) is a selection that includes the
-- union of the selections @s1@ and @s2@.
--
-- Complexity of @'Data.Store.Selection.resolve'@: /O(c(s1) + c(s2) + s(s1) + s(s2)/
(.||) :: (IsSelection s1, IsSelection s2)
      => s1 tag krs irs ts -> s2 tag krs irs ts -> Selection tag krs irs ts
(.||) = SelectionO
{-# INLINE (.||) #-}

-- IMPLEMENTATION

instance IsSelection Selection where
    resolve (SelectionType sel) s = resolve sel s
    resolve (SelectionA s1 s2) s = Data.IntSet.intersection (resolve s1 s) (resolve s2 s)
    resolve (SelectionO s1 s2) s = Data.IntSet.union (resolve s1 s) (resolve s2 s)
    resolve (SelectionNot sel) s@(I.Store vs _ _) =
        Data.IntSet.difference (Data.IntMap.keysSet vs) (resolve sel s)
    {-# INLINE resolve #-}

instance IsSelection (SelectionDimension n) where
    resolve = resolveSD
    {-# INLINE resolve #-}

resolveSD :: forall tag n krs irs ts v . SelectionDimension n tag krs irs ts 
          -> I.Store tag krs irs ts v
          -> Data.IntSet.IntSet
resolveSD (SelectionDimension _ (Condition False False False) _) _ = Data.IntSet.empty
resolveSD (SelectionDimension _ (Condition True True True) _) (I.Store vs _ _) = Data.IntSet.fromList $ Data.IntMap.keys vs
resolveSD (SelectionDimension n (Condition lt eq gt) v) (I.Store _ ix _) =
    go $ I.getDimension n ix
    where
      go (I.IndexDimensionO m) = case Data.Map.splitLookup v m of
          (lk, ek, gk) -> (if lt then trO lk else Data.IntSet.empty) <>
                          (if eq then trMaybeO ek else Data.IntSet.empty) <>
                          (if gt then trO gk else Data.IntSet.empty)                         
      go (I.IndexDimensionM m) = case Data.Map.splitLookup v m of
          (lk, ek, gk) -> (if lt then trM lk else Data.IntSet.empty) <>
                          (if eq then trMaybeM ek else Data.IntSet.empty) <>
                          (if gt then trM gk else Data.IntSet.empty)
      {-# INLINEABLE go #-}

      trO :: Data.Map.Map k Int -> Data.IntSet.IntSet
      trO = Data.Map.foldr Data.IntSet.insert Data.IntSet.empty
      {-# INLINE trO #-}

      trMaybeO :: Maybe Int -> Data.IntSet.IntSet
      trMaybeO (Just x) = Data.IntSet.singleton x
      trMaybeO _ = Data.IntSet.empty
      {-# INLINE trMaybeO #-}

      trM :: Data.Map.Map k Data.IntSet.IntSet -> Data.IntSet.IntSet
      trM = Data.Map.foldr (<>) Data.IntSet.empty
      {-# INLINE trM #-}

      trMaybeM :: Maybe Data.IntSet.IntSet -> Data.IntSet.IntSet
      trMaybeM (Just x) = x
      trMaybeM _ = Data.IntSet.empty
      {-# INLINE trMaybeM #-}
{-# INLINE resolveSD #-}

-- | TYPE

data SelectionDimension n tag krs irs ts where
    SelectionDimension :: I.GetDimension n (I.Index irs ts)
                       => n
                       -> Condition
                       -> I.DimensionType n irs ts
                       -> SelectionDimension n tag krs irs ts

data Selection tag krs irs ts where
    SelectionType :: IsSelection sel => sel tag krs irs ts -> Selection tag krs irs ts    

    SelectionA :: (IsSelection s1, IsSelection s2)
               => s1 tag krs irs ts -> s2 tag krs irs ts -> Selection tag krs irs ts

    SelectionO :: (IsSelection s1, IsSelection s2)
               => s1 tag krs irs ts -> s2 tag krs irs ts -> Selection tag krs irs ts

    SelectionNot :: IsSelection sel => sel tag krs irs ts -> Selection tag krs irs ts

data Condition = Condition !Bool !Bool !Bool

class IsSelection sel where
    resolve :: sel tag krs irs ts -> I.Store tag krs irs ts v -> Data.IntSet.IntSet
   
