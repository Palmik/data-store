{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Store.Selection
( (.<)
, (.<=)
, (.>)
, (.>=)
, (./=)
, (.==)
, (.&&)
, (.||)
, IsSelection(..)
, Selection
) where

--------------------------------------------------------------------------------
import           Control.Applicative
--------------------------------------------------------------------------------
import           Data.Maybe
import           Data.Monoid ((<>))
import qualified Data.IntSet
import qualified Data.IntSet.Extra
import qualified Data.IntMap
import qualified Data.Map
--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Type     as I
import qualified Data.Store.Internal.Function as I
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Store.Selection"

-- INTERFACE

-- | The expression (@sDim .< c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k < c@.
(.<) :: I.GetDimension n (I.Index irs ts) => n -> I.DimensionType n irs ts -> Selection krs irs ts
(.<)  n = SelectionSingle . SelectionDimension n (Condition True False False)

-- | The expression (@sDim .<= c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k <= c@.
(.<=) :: I.GetDimension n (I.Index irs ts) => n -> I.DimensionType n irs ts -> Selection krs irs ts
(.<=) n = SelectionSingle . SelectionDimension n (Condition True True False)

-- | The expression (@sDim .> c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k > c@.
(.>) :: I.GetDimension n (I.Index irs ts) => n -> I.DimensionType n irs ts -> Selection krs irs ts
(.>)  n = SelectionSingle . SelectionDimension n (Condition False False True)

-- | The expression (@sDim .>= c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k >= c@.
(.>=) :: I.GetDimension n (I.Index irs ts) => n -> I.DimensionType n irs ts -> Selection krs irs ts
(.>=) n = SelectionSingle . SelectionDimension n (Condition False True True)

-- | The expression (@sDim ./= c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k /= c@.
(./=) :: I.GetDimension n (I.Index irs ts) => n -> I.DimensionType n irs ts -> Selection krs irs ts
(./=) n = SelectionSingle . SelectionDimension n (Condition True False True)

-- | The expression (@sDim .== c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k == c@.
(.==) :: I.GetDimension n (I.Index irs ts) => n -> I.DimensionType n irs ts -> Selection krs irs ts
(.==) n = SelectionSingle . SelectionDimension n (Condition False True False)

-- | The expression (@s1 .&& s2@) is a selection that includes the
-- intersection of the selections @s1@ and @s2@.
(.&&) :: IsSelection sel => sel krs irs ts -> sel krs irs ts -> Selection krs irs ts
(.&&) = SelectionAnd

-- | The expression (@s1 .|| s2@) is a selection that includes the
-- union of the selections @s1@ and @s2@.
(.||) :: IsSelection sel => sel krs irs ts -> sel krs irs ts -> Selection krs irs ts
(.||) = SelectionOr

-- IMPLEMENTATION

instance IsSelection Selection where
    resolve (SelectionSingle sel) s = resolve sel s
    resolve (SelectionAnd sel1 sel2) s = Data.IntSet.intersection (resolve sel1 s) (resolve sel2 s)
    resolve (SelectionOr sel1 sel2) s = Data.IntSet.union (resolve sel1 s) (resolve sel2 s)
    
    lookup sel s = genericLookup (resolve sel s) s
    {-# INLINE lookup #-}    

    update tr sel s = genericUpdate tr (resolve sel s) s
    {-# INLINE update #-}    

    updateValues tr sel s = genericUpdateValues tr (resolve sel s) s
    {-# INLINE updateValues #-}    

    delete sel s = genericDelete (resolve sel s) s
    {-# INLINE delete #-}

instance IsSelection (SelectionDimension n) where
    resolve = resolveSD
    {-# INLINE resolve #-}

    lookup sel s = genericLookup (resolve sel s) s
    {-# INLINE lookup #-}    
    
    update tr sel s = genericUpdate tr (resolve sel s) s
    {-# INLINE update #-}    
    
    updateValues tr sel s = genericUpdateValues tr (resolve sel s) s
    {-# INLINE updateValues #-}    
    
    delete sel s = genericDelete (resolve sel s) s
    {-# INLINE delete #-}

resolveSD :: forall n krs irs ts v . SelectionDimension n krs irs ts 
          -> I.Store krs irs ts v
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
      {-# INLINE go #-}

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

genericLookup :: Data.IntSet.IntSet -> I.Store krs irs ts v -> [(I.RawKeyType krs ts, v)]
genericLookup ids (I.Store vs _ _) =
    Data.IntSet.foldr (\i acc -> case Data.IntMap.lookup i vs of
                                   Just (ik, _, v) -> (I.keyInternalToRaw ik, v) : acc
                                   _ -> acc
                      ) [] ids
{-# INLINE genericLookup #-}

genericUpdate :: (v -> Maybe (v, Maybe (I.Key krs ts)))
              -> Data.IntSet.IntSet
              -> I.Store krs irs ts v
              -> Maybe (I.Store krs irs ts v)
genericUpdate tr ids old = Data.IntSet.Extra.foldrM accum old ids
    where
      accum i store@(I.Store vs ix nid) =
          case Data.IntMap.lookup i vs of
            Just (ik, k, v) ->
                case tr v of
                  -- Update the value & key.
                  Just (nv, Just nk) -> (\nix -> I.Store
                    { I.storeV = Data.IntMap.insert i (ik, k, nv) vs
                    , I.storeI = nix
                    , I.storeNID = nid
                    }) <$> I.indexInsertID (mkik nk ik) i (I.indexDeleteID ik i ix)

                  -- Update the value.
                  Just (nv, Nothing) -> Just I.Store
                    { I.storeV = Data.IntMap.insert i (ik, k, nv) vs
                    , I.storeI = ix
                    , I.storeNID = nid
                    }

                  -- Delete.
                  Nothing -> Just I.Store
                    { I.storeV = Data.IntMap.delete i vs
                    , I.storeI = I.indexDeleteID ik i ix
                    , I.storeNID = nid
                    }
            _ -> Just store
      {-# INLINEABLE accum #-}

      mkik :: I.Key krs ts -> I.IKey krs ts -> I.IKey krs ts
      mkik (I.K1 (I.KeyDimensionO d))   (I.K1 _)   = I.K1 (I.IKeyDimensionO d)
      mkik (I.K1 (I.KeyDimensionM d))   (I.K1 _)   = I.K1 (I.IKeyDimensionM d)
      mkik (I.KN (I.KeyDimensionO d) s) (I.KN _ is) = I.KN (I.IKeyDimensionO d) $ mkik s is
      mkik (I.KN (I.KeyDimensionM d) s) (I.KN _ is) = I.KN (I.IKeyDimensionM d) $ mkik s is
      mkik _ _ = error $ moduleName <> ".genericUpdate.mkik: The impossible happened."
      {-# INLINEABLE mkik #-}

genericUpdateValues :: (v -> Maybe v) -> Data.IntSet.IntSet -> I.Store krs irs ts v -> I.Store krs irs ts v
genericUpdateValues tr sel s = fromJust $ genericUpdate (maybe Nothing (\v -> Just (v, Nothing)) . tr) sel s
{-# INLINE genericUpdateValues #-}

genericDelete :: Data.IntSet.IntSet -> I.Store krs irs ts v -> I.Store krs irs ts v 
genericDelete sel s = fromJust $ genericUpdate (const Nothing) sel s
{-# INLINE genericDelete #-}

-- | TYPE

data SelectionDimension n krs irs ts where
    SelectionDimension :: I.GetDimension n (I.Index irs ts)
                       => n
                       -> Condition
                       -> I.DimensionType n irs ts
                       -> SelectionDimension n krs irs ts

data Selection krs irs ts where
    SelectionSingle :: IsSelection sel => sel krs irs ts -> Selection krs irs ts
    SelectionAnd    :: IsSelection sel => sel krs irs ts -> sel krs irs ts -> Selection krs irs ts
    SelectionOr     :: IsSelection sel => sel krs irs ts -> sel krs irs ts -> Selection krs irs ts

data Condition = Condition Bool Bool Bool

class IsSelection sel where
    resolve :: sel krs irs ts -> I.Store krs irs ts v -> Data.IntSet.IntSet
   
    -- | The expression (@'Data.Store.Selection.lookup' sel store@) is
    -- list of (raw-key)-value pairs that match the selection.
    lookup :: sel krs irs ts -> I.Store krs irs ts v -> [(I.RawKeyType krs ts, v)]
    
    -- | The expression (@'Data.Store.Selection.update' tr sel old@) is
    -- (@Just new@) store that updates all values @x@ of @old@ that match the
    -- selection using the function @tr@ or @Nothing@ if the update would
    -- cause collisions.
    --
    -- The update process follows these rules:
    --
    -- * If (@tr x@) is Nothing, @x@ is removed.
    --
    -- * If (@tr x@) is (@Just (x', Nothing)@) the value @x@ is replaced by @x'@.
    --
    -- * If (@tr x@) is (@Just (x', Just k')@) the value @x@ is replaced by
    -- @x'@ and is reindexed under the new key @k'@.
    update :: (v -> Maybe (v, Maybe (I.Key krs ts)))
           -> sel krs irs ts
           -> I.Store krs irs ts v
           -> Maybe (I.Store krs irs ts v)

    -- | The expression (@'Data.Store.Selection.updateValues' tr sel old@) is
    -- equivalent to
    -- (@'Data.Maybe.fromJust' $ 'Data.Store.Selection.update' ('Data.Maybe.maybe' Nothing (\v -> Just (v, Nothing)) . tr) sel old@). 
    updateValues :: (v -> Maybe v)
                 -> sel krs irs ts
                 -> I.Store krs irs ts v
                 -> I.Store krs irs ts v

    -- | The expression (@'Data.Store.Selection.delete' sel old@) is
    -- equivalent to
    -- (@'Data.Store.fromJust' $ 'Data.Store.Selection.update' (const Nothing) sel old@).
    delete :: sel krs irs ts
           -> I.Store krs irs ts v
           -> I.Store krs irs ts v

