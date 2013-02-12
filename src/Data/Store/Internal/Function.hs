{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.Store.Internal.Function
where

--------------------------------------------------------------------------------
import           Control.Applicative
--------------------------------------------------------------------------------
import           Data.Maybe
import qualified Data.Foldable as F 
import qualified Data.Map
import qualified Data.Map.Extra
import qualified Data.IntSet
--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Type as I
--------------------------------------------------------------------------------

keyInternalToRaw :: I.IKey krs ts -> I.RawKeyType krs ts
keyInternalToRaw (I.K1 (I.IKeyDimensionO x)) = x
keyInternalToRaw (I.K1 (I.IKeyDimensionM x)) = x
keyInternalToRaw (I.KN (I.IKeyDimensionO x) s) = x I.:. keyInternalToRaw s
keyInternalToRaw (I.KN (I.IKeyDimensionM x) s) = x I.:. keyInternalToRaw s
{-# INLINE keyInternalToRaw #-}

indexInsertID :: I.IKey krs ts
              -> Int
              -> I.Index irs ts
              -> Maybe (I.Index irs ts)
indexInsertID ik i ix = zipDimensions (zipInsert i) ik ix 
{-# INLINE indexInsertID #-}

zipInsert :: Ord t => Int -> I.IKeyDimension kr t -> I.IndexDimension ir t -> Maybe (I.IndexDimension ir t)
zipInsert v key index =
    case (index, key) of
      (I.IndexDimensionO m, I.IKeyDimensionO k)  -> I.IndexDimensionO <$> goO k v m
      (I.IndexDimensionO m, I.IKeyDimensionM ks) -> I.IndexDimensionO <$> F.foldrM (\k acc -> goO k v acc) m ks
      (I.IndexDimensionM m, I.IKeyDimensionO k)  -> Just . I.IndexDimensionM $ goM k v m
      (I.IndexDimensionM m, I.IKeyDimensionM ks) -> Just . I.IndexDimensionM $ F.foldr (\k acc -> goM k v acc) m ks
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
zipDelete v key index = Just $
    case (index, key) of
      (I.IndexDimensionO m, I.IKeyDimensionO k)  -> I.IndexDimensionO $ goO k v m
      (I.IndexDimensionO m, I.IKeyDimensionM ks) -> I.IndexDimensionO $ F.foldr (\k acc -> goO k v acc) m ks
      (I.IndexDimensionM m, I.IKeyDimensionO k)  -> I.IndexDimensionM $ goM k v m
      (I.IndexDimensionM m, I.IKeyDimensionM ks) -> I.IndexDimensionM $ F.foldr (\k acc -> goM k v acc) m ks
    where
      goO :: Ord k => k -> Int -> Data.Map.Map k Int -> Data.Map.Map k Int
      goO k _ = Data.Map.delete k
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
zipDimensions combine (I.K1 kd) (I.I1 id) = I.I1 <$> combine kd id
zipDimensions combine (I.KN kd kt) (I.IN id it) = I.IN <$> combine kd id <*> zipDimensions combine kt it
{-# INLINEABLE zipDimensions #-}

