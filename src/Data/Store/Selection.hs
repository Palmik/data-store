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
, not'
, all'
, all1D
, any'
, any1D
, everything
, nothing
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
import qualified Data.List
--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Type     as I
import qualified Data.Store.Internal.Function as I
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Store.Selection"

-- INTERFACE

infix  4  .==, ./=, .<, .<=, .>=, .>
infixr 3  .&&
infixr 2  .||

-- | Selection that matches all values. Useful as a base for folding.
everything :: Selection krs irs ts
everything = SelectionEverything
{-# INLINE everything #-}

-- | Selection that does not match any values. Useful as a base for folding.
nothing :: Selection krs irs ts
nothing = SelectionNothing
{-# INLINE nothing #-}

-- | The expression (@not' sel@) is a selection that includes all values
-- except those that match the selection @sel@. 
not' :: IsSelection sel => sel krs irs ts -> Selection krs irs ts
not' = SelectionNot
{-# INLINE not' #-}

-- | Selection that matches the intersection of all the selections in the
-- list or everything if the list is empty.
all' :: [Selection krs irs ts] -> Selection krs irs ts
all' []  = everything
all' [s] = s
all' (s:rest) = Data.List.foldl' (.&&) s rest -- this way we do not have to intersect with "everything"
{-# INLINE all' #-}

-- | The expression (@'Data.Store.Selection.all1D' d ss@) is equivalent to (@'Data.Store.Selection.all'' $ map ($ d) ss@).
all1D :: n -> [n -> Selection krs irs ts] -> Selection krs irs ts
all1D _ [] = everything
all1D d [h] = h d
all1D d (h:rest) = Data.List.foldl' (\acc f -> acc .&& f d) (h d) rest -- this way we do not have to intersect with "everything"
{-# INLINE all1D #-}

-- | Selection that matches the union of all the selections in the
-- list or nothing if the list is empty.
any' :: [Selection krs irs ts] -> Selection krs irs ts
any' = Data.List.foldl' (.||) nothing
{-# INLINE any' #-}

-- | The expression (@'Data.Store.Selection.any1D' d ss@) is equivalent to (@'Data.Store.Selection.any'' $ map ($ d) ss@).
any1D :: n -> [n -> Selection krs irs ts] -> Selection krs irs ts
any1D d = Data.List.foldl' (\acc f -> acc .|| f d) nothing
{-# INLINE any1D #-}

-- | The expression (@sDim .< c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k < c@.
(.<) :: I.GetDimension n (I.Index irs ts) => n -> I.DimensionType n irs ts -> Selection krs irs ts
(.<)  n = SelectionWrap . SelectionDimension n (Condition True False False)
{-# INLINE (.<) #-}

-- | The expression (@sDim .<= c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k <= c@.
(.<=) :: I.GetDimension n (I.Index irs ts) => n -> I.DimensionType n irs ts -> Selection krs irs ts
(.<=) n = SelectionWrap . SelectionDimension n (Condition True True False)
{-# INLINE (.<=) #-}

-- | The expression (@sDim .> c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k > c@.
(.>) :: I.GetDimension n (I.Index irs ts) => n -> I.DimensionType n irs ts -> Selection krs irs ts
(.>)  n = SelectionWrap . SelectionDimension n (Condition False False True)
{-# INLINE (.>) #-}

-- | The expression (@sDim .>= c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k >= c@.
(.>=) :: I.GetDimension n (I.Index irs ts) => n -> I.DimensionType n irs ts -> Selection krs irs ts
(.>=) n = SelectionWrap . SelectionDimension n (Condition False True True)
{-# INLINE (.>=) #-}

-- | The expression (@sDim ./= c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k /= c@.
(./=) :: I.GetDimension n (I.Index irs ts) => n -> I.DimensionType n irs ts -> Selection krs irs ts
(./=) n = SelectionWrap . SelectionDimension n (Condition True False True)
{-# INLINE (./=) #-}

-- | The expression (@sDim .== c@) is a selection that includes value
-- @x@ if and only if it is indexed in the @sDim@ dimension with a key @k@
-- such that @k == c@.
(.==) :: I.GetDimension n (I.Index irs ts) => n -> I.DimensionType n irs ts -> Selection krs irs ts
(.==) n = SelectionWrap . SelectionDimension n (Condition False True False)
{-# INLINE (.==) #-}

-- | The expression (@s1 .&& s2@) is a selection that includes the
-- intersection of the selections @s1@ and @s2@.
(.&&) :: IsSelection sel => sel krs irs ts -> sel krs irs ts -> Selection krs irs ts
(.&&) = SelectionAnd
{-# INLINE (.&&) #-}

-- | The expression (@s1 .|| s2@) is a selection that includes the
-- union of the selections @s1@ and @s2@.
(.||) :: IsSelection sel => sel krs irs ts -> sel krs irs ts -> Selection krs irs ts
(.||) = SelectionOr
{-# INLINE (.||) #-}

-- IMPLEMENTATION

instance IsSelection Selection where
    resolve SelectionEverything (I.Store vs _ _) = Data.IntMap.keysSet vs
    resolve SelectionNothing _ = Data.IntSet.empty
    resolve (SelectionWrap sel) s = resolve sel s
    resolve (SelectionAnd sel1 sel2) s = Data.IntSet.intersection (resolve sel1 s) (resolve sel2 s)
    resolve (SelectionOr sel1 sel2) s = Data.IntSet.union (resolve sel1 s) (resolve sel2 s)
    resolve (SelectionNot sel) s@(I.Store vs _ _) =
        Data.IntSet.difference (Data.IntMap.keysSet vs) (resolve sel s)

    lookup sel s = genericLookup (resolve sel s) s
    {-# INLINE lookup #-}    

    updateWithKey tr sel s = genericUpdateWithKey tr (resolve sel s) s
    {-# INLINE updateWithKey #-}    

    delete sel s = genericDelete (resolve sel s) s
    {-# INLINE delete #-}

instance IsSelection (SelectionDimension n) where
    resolve = resolveSD
    {-# INLINE resolve #-}

    lookup sel s = genericLookup (resolve sel s) s
    {-# INLINE lookup #-}    
    
    updateWithKey tr sel s = genericUpdateWithKey tr (resolve sel s) s
    {-# INLINE updateWithKey #-}    
    
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

genericLookup :: Data.IntSet.IntSet -> I.Store krs irs ts v -> [(I.RawKey krs ts, v)]
genericLookup ids (I.Store vs _ _) =
    Data.IntSet.foldr (\i acc -> case Data.IntMap.lookup i vs of
                                   Just (ik, v) -> (I.keyInternalToRaw ik, v) : acc
                                   _ -> acc
                      ) [] ids
{-# INLINE genericLookup #-}

genericUpdateWithKey :: (I.RawKey krs ts -> v -> Maybe (v, Maybe (I.Key krs ts)))
                      -> Data.IntSet.IntSet
                      -> I.Store krs irs ts v
                      -> Maybe (I.Store krs irs ts v)
genericUpdateWithKey tr ids old = Data.IntSet.Extra.foldrM accum old ids
    where
      accum i store@(I.Store vs ix nid) =
          case Data.IntMap.lookup i vs of
            Just (ik, v) ->
                case tr (I.keyInternalToRaw ik) v of
                  -- Update the value & key.
                  Just (nv, Just nk) -> (\nix -> I.Store
                    { I.storeV = Data.IntMap.insert i (ik, nv) vs
                    , I.storeI = nix
                    , I.storeNID = nid
                    }) <$> I.indexInsertID (mkik nk ik) i (I.indexDeleteID ik i ix)

                  -- Update the value.
                  Just (nv, Nothing) -> Just I.Store
                    { I.storeV = Data.IntMap.insert i (ik, nv) vs
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

--genericUpdateValues :: (v -> Maybe v) -> Data.IntSet.IntSet -> I.Store krs irs ts v -> I.Store krs irs ts v
--genericUpdateValues tr sel s = fromJust $ genericUpdate (maybe Nothing (\v -> Just (v, Nothing)) . tr) sel s
--{-# INLINE genericUpdateValues #-}

genericDelete :: Data.IntSet.IntSet -> I.Store krs irs ts v -> I.Store krs irs ts v 
genericDelete sel s = fromJust $ genericUpdateWithKey (\_ _ -> Nothing) sel s
{-# INLINE genericDelete #-}

-- | TYPE

data SelectionDimension n krs irs ts where
    SelectionDimension :: I.GetDimension n (I.Index irs ts)
                       => n
                       -> Condition
                       -> I.DimensionType n irs ts
                       -> SelectionDimension n krs irs ts

data Selection krs irs ts where
    SelectionWrap :: IsSelection sel => sel krs irs ts -> Selection krs irs ts
    SelectionAnd  :: IsSelection sel => sel krs irs ts -> sel krs irs ts -> Selection krs irs ts
    SelectionOr   :: IsSelection sel => sel krs irs ts -> sel krs irs ts -> Selection krs irs ts
    SelectionNot  :: IsSelection sel => sel krs irs ts -> Selection krs irs ts
    SelectionNothing :: Selection krs irs ts
    SelectionEverything :: Selection krs irs ts

data Condition = Condition Bool Bool Bool

class IsSelection sel where
    resolve :: sel krs irs ts -> I.Store krs irs ts v -> Data.IntSet.IntSet
   
    -- | The expression (@'Data.Store.Selection.lookup' sel store@) is
    -- list of (raw-key)-value pairs that match the selection.
    lookup :: sel krs irs ts -> I.Store krs irs ts v -> [(I.RawKey krs ts, v)]
   
    -- | The expression (@'Data.Store.Selection.updateWithKeys' tr sel old@) is
    -- either a) (@Just new@), where @new@ is store with all values @x@ of @old@ that
    -- match the selection updated using the function @tr@; 
    -- or b) @Nothing@ if the update would cause collisions.
    --
    -- Let @(rk, v)@ be any of the pairs of raw key, key and value
    -- that match the selection, then the update process follows these rules:
    --
    -- * If (@tr rk x@) is Nothing, @x@ is deleted.
    --
    -- * If (@tr rk x@) is (@Just (x', Nothing)@) the value @x@ is replaced by @x'@.
    --
    -- * If (@tr rk x@) is (@Just (x', Just k')@) the value @x@ is replaced by
    -- @x'@ and the value is reindexed under the new key @k'@.
    updateWithKey :: (I.RawKey krs ts -> v -> Maybe (v, Maybe (I.Key krs ts)))
                  -> sel krs irs ts
                  -> I.Store krs irs ts v
                  -> Maybe (I.Store krs irs ts v)

    -- | The expression (@'Data.Store.Selection.delete' sel old@) is
    -- equivalent to
    -- (@'Data.Store.fromJust' $ 'Data.Store.Selection.update' (const Nothing) sel old@).
    delete :: sel krs irs ts
           -> I.Store krs irs ts v
           -> I.Store krs irs ts v

