{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

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

keyInternalToRaw :: I.IKey sk -> I.RawKeyType sk
keyInternalToRaw (I.K1 (I.IKeyDimensionO x)) = x
keyInternalToRaw (I.K1 (I.IKeyDimensionM x)) = x
keyInternalToRaw (I.KN (I.IKeyDimensionO x) s) = x I.:. keyInternalToRaw s
keyInternalToRaw (I.KN (I.IKeyDimensionM x) s) = x I.:. keyInternalToRaw s

indexInsertID :: I.ZipDimensions si sk
              => I.IKey sk
              -> Int
              -> I.Index si
              -> Maybe (I.Index si)
indexInsertID ik i ix = I.zipDimensions (zipInsert i) ix ik 

zipInsert :: Ord a => Int -> I.IndexDimension ti a -> I.IKeyDimension tk a -> Maybe (I.IndexDimension ti a)
zipInsert v index key =
    case (index, key) of
      (I.IndexDimensionO m mmax, I.IKeyDimensionO k)  -> flip I.IndexDimensionO (newMax mmax k) <$> goO k v m
      (I.IndexDimensionO m mmax, I.IKeyDimensionM ks) -> flip I.IndexDimensionO mmax <$> F.foldrM (\k acc -> goO k v acc) m ks
      (I.IndexDimensionM m, I.IKeyDimensionO k)  -> Just . I.IndexDimensionM $ goM k v m
      (I.IndexDimensionM m, I.IKeyDimensionM ks) -> Just . I.IndexDimensionM $ F.foldr (\k acc -> goM k v acc) m ks
    where
      goO :: Ord k => k -> Int -> Data.Map.Map k Int -> Maybe (Data.Map.Map k Int)
      goO = Data.Map.Extra.insertUnique

      goM :: Ord k => k -> Int -> Data.Map.Map k Data.IntSet.IntSet -> Data.Map.Map k Data.IntSet.IntSet
      goM k v = Data.Map.insertWith (\_ s -> Data.IntSet.insert v s) k (Data.IntSet.singleton v)

      newMax :: Ord k => Maybe k -> k -> Maybe k
      newMax (Just x) y = Just $ max x y 
      newMax _ y = Just y      

indexDeleteID :: I.ZipDimensions si sk
              => I.IKey sk
              -> Int
              -> I.Index si
              -> I.Index si
indexDeleteID ik i ix = fromJust $ I.zipDimensions (zipDelete i) ix ik

zipDelete :: Ord a => Int -> I.IndexDimension ti a -> I.IKeyDimension tk a -> Maybe (I.IndexDimension ti a)
zipDelete v index key = Just $
    case (index, key) of
      (I.IndexDimensionO m mmax, I.IKeyDimensionO k)  -> flip I.IndexDimensionO mmax $ goO k v m
      (I.IndexDimensionO m mmax, I.IKeyDimensionM ks) -> flip I.IndexDimensionO mmax $ F.foldr (\k acc -> goO k v acc) m ks
      (I.IndexDimensionM m, I.IKeyDimensionO k)  -> I.IndexDimensionM $ goM k v m
      (I.IndexDimensionM m, I.IKeyDimensionM ks) -> I.IndexDimensionM $ F.foldr (\k acc -> goM k v acc) m ks
    where
      goO :: Ord k => k -> Int -> Data.Map.Map k Int -> Data.Map.Map k Int
      goO k _ = Data.Map.delete k

      goM :: Ord k => k -> Int -> Data.Map.Map k Data.IntSet.IntSet -> Data.Map.Map k Data.IntSet.IntSet
      goM k v = Data.Map.update (\ids -> let nids = Data.IntSet.delete v ids in
                                         if Data.IntSet.null nids
                                            then Nothing
                                            else Just nids
                                ) k

