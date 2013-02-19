{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.Store.Internal.Function
where

--------------------------------------------------------------------------------
import           Control.Applicative
--------------------------------------------------------------------------------
import           Data.Monoid ((<>))
import           Data.Maybe
import qualified Data.Foldable as F 
import qualified Data.Map
import qualified Data.Map.Extra
import qualified Data.IntSet
--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Type as I
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Store.Internal.Function"

keyInternalToRaw :: I.IKey krs ts -> I.RawKey krs ts
keyInternalToRaw (I.K1 (I.IKeyDimensionO x)) = x
keyInternalToRaw (I.K1 (I.IKeyDimensionM x)) = x
keyInternalToRaw (I.KN (I.IKeyDimensionO x) s) = x I.:. keyInternalToRaw s
keyInternalToRaw (I.KN (I.IKeyDimensionM x) s) = x I.:. keyInternalToRaw s
{-# INLINE keyInternalToRaw #-}

keyFromInternal :: I.IKey krs ts -> I.Key krs ts
keyFromInternal (I.K1 (I.IKeyDimensionO x)) = I.K1 (I.KeyDimensionO x)
keyFromInternal (I.K1 (I.IKeyDimensionM x)) = I.K1 (I.KeyDimensionM x)
keyFromInternal (I.KN (I.IKeyDimensionO x) s) = I.KN (I.KeyDimensionO x) (keyFromInternal s)
keyFromInternal (I.KN (I.IKeyDimensionM x) s) = I.KN (I.KeyDimensionM x) (keyFromInternal s)
{-# INLINE keyFromInternal #-}


indexInsertID :: I.IKey krs ts
              -> Int
              -> I.Index irs ts
              -> Maybe (I.Index irs ts)
indexInsertID ik i = zipDimensions (zipInsert i) ik 
{-# INLINE indexInsertID #-}

zipInsert :: Ord t => Int -> I.IKeyDimension kr t -> I.IndexDimension ir t -> Maybe (I.IndexDimension ir t)
zipInsert val key index =
    case (index, key) of
      (I.IndexDimensionO m, I.IKeyDimensionO k)  -> I.IndexDimensionO <$> goO k val m
      (I.IndexDimensionO m, I.IKeyDimensionM ks) -> I.IndexDimensionO <$> F.foldrM (\k acc -> goO k val acc) m ks
      (I.IndexDimensionM m, I.IKeyDimensionO k)  -> Just . I.IndexDimensionM $ goM k val m
      (I.IndexDimensionM m, I.IKeyDimensionM ks) -> Just . I.IndexDimensionM $ F.foldr (\k acc -> goM k val acc) m ks
    where
      goO :: Ord k => k -> Int -> Data.Map.Map k Int -> Maybe (Data.Map.Map k Int)
      goO = Data.Map.Extra.insertUnique
      {-# INLINE goO #-}

      goM :: Ord k => k -> Int -> Data.Map.Map k Data.IntSet.IntSet -> Data.Map.Map k Data.IntSet.IntSet
      goM k v = Data.Map.insertWith (\_ s -> Data.IntSet.insert v s) k (Data.IntSet.singleton v)
      {-# INLINE goM #-}
{-# INLINEABLE zipInsert #-}

indexDeleteID :: I.IKey krs ts
              -> Int
              -> I.Index irs ts
              -> I.Index irs ts
indexDeleteID ik i ix = fromJust $ zipDimensions (zipDelete i) ik ix
{-# INLINE indexDeleteID #-}

zipDelete :: Ord t => Int -> I.IKeyDimension kr t -> I.IndexDimension ir t -> Maybe (I.IndexDimension ir t)
zipDelete val key index = Just $
    case (index, key) of
      (I.IndexDimensionO m, I.IKeyDimensionO k)  -> I.IndexDimensionO $ goO k val m
      (I.IndexDimensionO m, I.IKeyDimensionM ks) -> I.IndexDimensionO $ F.foldr (\k acc -> goO k val acc) m ks
      (I.IndexDimensionM m, I.IKeyDimensionO k)  -> I.IndexDimensionM $ goM k val m
      (I.IndexDimensionM m, I.IKeyDimensionM ks) -> I.IndexDimensionM $ F.foldr (\k acc -> goM k val acc) m ks
    where
      goO :: Ord k => k -> Int -> Data.Map.Map k Int -> Data.Map.Map k Int
      goO k v = Data.Map.update (\i -> if i == v then Nothing else Just i) k
      {-# INLINE goO #-}

      goM :: Ord k => k -> Int -> Data.Map.Map k Data.IntSet.IntSet -> Data.Map.Map k Data.IntSet.IntSet
      goM k v = Data.Map.update
          (\ids -> let nids = Data.IntSet.delete v ids in
                    if Data.IntSet.null nids
                       then Nothing
                       else Just nids
          ) k
      {-# INLINE goM #-}
{-# INLINEABLE zipDelete #-}

zipDimensions :: (forall ir kr t . Ord t => I.IKeyDimension kr t -> I.IndexDimension ir t -> Maybe (I.IndexDimension ir t))
              -> I.IKey  krs ts
              -> I.Index irs ts
              -> Maybe (I.Index irs ts)
zipDimensions combine (I.K1 kd) (I.I1 ixd) = I.I1 <$> combine kd ixd
zipDimensions combine (I.KN kd kt) (I.IN ixd it) = I.IN <$> combine kd ixd <*> zipDimensions combine kt it
zipDimensions _ _ _ = error $ moduleName <> ".zipDimensions: The impossible happened."
{-# INLINEABLE zipDimensions #-}

