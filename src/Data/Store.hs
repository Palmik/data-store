{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Data.Store
( I.Store(..)

  -- * Store Operations
  -- ** Creation
, empty

  -- ** Insertion
, insert
) where

--------------------------------------------------------------------------------
import           Control.Arrow
--------------------------------------------------------------------------------
import qualified Data.IntMap        as IM
import qualified Data.Vector        as V
import qualified Data.Vector.Extra  as V
--------------------------------------------------------------------------------
import qualified Data.Store.Key                     as I
import qualified Data.Store.Internal                as I
import qualified Data.Store.Internal.DimensionIndex as I
--------------------------------------------------------------------------------

-- | Creates an empty 'Store'.
--
-- TODO: Find a way to remove the 'CEmptyKey' context.
empty :: forall tag kh kd kt v . (I.CEmptyKey (I.Key kh kd kt))
      => I.Store tag (I.Key kh kd kt) v
empty = I.Store
    { I.storeValues = IM.empty
    , I.storeIndex  = emptyStoreIndex ekey
    , I.storeNextID = 0
    }
    where
      ekey :: I.Key kh kd kt
      ekey = I.emptyKey

      emptyStoreIndex :: forall kh1 kd1 kt1 . I.Key kh1 kd1 kt1 -> I.StoreIndex
      emptyStoreIndex (I.K1 d)   = V.singleton $ I.empty d
      emptyStoreIndex (I.KN d r) = I.empty d `V.cons` emptyStoreIndex r 

-- | Inserts the given element into existing 'Store'.
insert :: I.Key kh kd kt                  
       -> v                           
       -> I.Store tag (I.Key kh kd kt) v
       -> (I.Store tag (I.Key kh kd kt) v, I.KeyInsertResult (I.Key kh kd kt))
insert key v I.Store{..} = (I.Store
    { I.storeValues = IM.insert storeNextID v storeValues
    , I.storeIndex  = newStoreIndex 
    , I.storeNextID = succ storeNextID 
    }, result)
    where
      (newStoreIndex, result) = insertToIndex 0 key storeIndex

      -- | Recursively inserts the new ID under indices of every dimension
      -- of the key.
      insertToIndex :: Int -- ^ The position of the dimension of the head of the key in the store index vector.
                    -> I.Key kh kd kt -- ^ The key.
                    -> I.StoreIndex -- ^ The store index.
                    -> (I.StoreIndex, I.KeyInsertResult (I.Key kh kd kt))
      -- Standard dimension, 1-dimensional key.
      insertToIndex d (I.K1 kh@(I.Dimension _)) index =
          second (const ()) $ indexUpdate kh d index
      -- Auto-increment dimension, 1-dimensional key.
      insertToIndex d (I.K1 kh@I.DimensionAuto) index =
          second (I.:.  ()) $ indexUpdate kh d index
      -- Standard dimension, (n + 1)-dimensional key.
      insertToIndex d (I.KN kh@(I.Dimension _) kt) index =
          let (nindex, _) = indexUpdate kh d index
          in  insertToIndex (d + 1) kt nindex
      -- Auto-increment dimension (n + 1)-dimensional key.
      insertToIndex d (I.KN kh@I.DimensionAuto kt) index =
          let (nindex, res) = indexUpdate kh d index
          in  second (res I.:.) $ insertToIndex (d + 1) kt nindex    

      -- | Inserts the new ID under indices of the given dimension.
      indexUpdate :: I.Dimension a d -- ^ The dimension to be inserted.
                  -> Int -- ^ The position of the dimension in the store index vector.
                  -> I.StoreIndex -- The store index.
                  -> (I.StoreIndex, I.DimensionInsertResult (I.Dimension a d))
      indexUpdate d = V.updateAt' (I.insert d storeNextID)


