{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Store.Internal.DimensionIndex
( DimensionIndex(..)
, insert
, empty
) where

--------------------------------------------------------------------------------
import qualified Unsafe.Coerce as Unsafe
--------------------------------------------------------------------------------
import qualified Data.IntSet as IS
import qualified Data.Map    as M
import qualified Data.List   as L
--------------------------------------------------------------------------------
import qualified Data.Store.Key as I
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

insert :: I.Dimension k d
       -> Int            
       -> DimensionIndex
       -> (DimensionIndex, I.DimensionInsertResult (I.Dimension k d))
insert (I.Dimension ks) oid (DimensionIndex imap) =
    (DimensionIndex $ L.foldl' go imap ks, ())
    where
      go acc k = M.insertWith IS.union (Unsafe.unsafeCoerce k) (IS.singleton oid) acc
insert (I.Dimension _) _ _ = error (moduleName ++ ".insert: non-matching dimension and dimension index constructors.") 
insert I.DimensionAuto oid (DimensionIndexAuto imap inext) = 
    (DimensionIndexAuto inew $ I.nextValue inext, Unsafe.unsafeCoerce inext)
    where
      inew  = M.insertWith IS.union (Unsafe.unsafeCoerce inext) (IS.singleton oid) imap
insert I.DimensionAuto _ _ = error (moduleName ++ ".insert: non-matching dimension and dimension index constructors.") 
