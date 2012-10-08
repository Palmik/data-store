{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Store.Internal.DimensionIndex
( DimensionIndex(..)
, insert
, split
, empty
) where

--------------------------------------------------------------------------------
import qualified Unsafe.Coerce as Unsafe
--------------------------------------------------------------------------------
import qualified Data.IntSet as IS
import qualified Data.Map    as M
import qualified Data.List   as L
--------------------------------------------------------------------------------
import qualified Data.Store.Key          as I
import qualified Data.Store.Internal.Key as I
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Store.Internal.DimensionIndex"

empty :: forall a d . I.Dimension a d -> DimensionIndex
empty (I.Dimension _) =  DimensionIndex     (M.empty :: M.Map a IS.IntSet)
empty I.DimensionAuto =  DimensionIndexAuto (M.empty :: M.Map a IS.IntSet) I.initValue

data DimensionIndex where
    DimensionIndex
        :: Ord k
        => M.Map k IS.IntSet
        -> DimensionIndex
    
    DimensionIndexAuto
        :: (Ord k, I.Auto k)
        => M.Map k IS.IntSet
        -> k
        -> DimensionIndex

split :: Ord k => k -> DimensionIndex -> (IS.IntSet, IS.IntSet)
split dkey dindex =
    case dindex of
        (DimensionIndex m) -> go m
        (DimensionIndexAuto m _) -> go m
    where
      go :: Ord k => M.Map k IS.IntSet -> (IS.IntSet, IS.IntSet)
      go m = mapTuple (IS.unions . map snd . M.toList) $ M.split (Unsafe.unsafeCoerce dkey) m

      -- | join (***)
      mapTuple :: (a -> b) -> (a, a) -> (b, b)
      mapTuple f (x, y) = (f x, f y) 

insert :: I.Dimension a dt
       -> Int            
       -> DimensionIndex
       -> (DimensionIndex, I.DimensionInternal a dt)
insert (I.Dimension ks) oid (DimensionIndex imap) =
    (DimensionIndex $ L.foldl' go imap ks, I.IDimension ks)
    where
      go acc k = M.insertWith IS.union (Unsafe.unsafeCoerce k) (IS.singleton oid) acc
insert (I.Dimension _) _ _ = error (moduleName ++ ".insert: non-matching dimension and dimension index constructors.") 
insert I.DimensionAuto oid (DimensionIndexAuto imap inext) = 
    (DimensionIndexAuto inew $ I.nextValue inext, I.IDimensionAuto $ Unsafe.unsafeCoerce inext)
    where
      inew  = M.insertWith IS.union (Unsafe.unsafeCoerce inext) (IS.singleton oid) imap
insert I.DimensionAuto _ _ = error (moduleName ++ ".insert: non-matching dimension and dimension index constructors.") 
