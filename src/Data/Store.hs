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
import qualified Data.Store.Internal.Key            as I
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
       -> (I.Store tag (I.Key kh kd kt) v, I.KeyInsertResult (I.ToKeyInternal (I.Key kh kd kt)))
insert key value I.Store{..} = (I.Store
    { I.storeValues = IM.insert storeNextID (value, keyInternal) storeValues
    , I.storeIndex  = newStoreIndex 
    , I.storeNextID = succ storeNextID 
    }, toInsertResult keyInternal)
    where
      (newStoreIndex, keyInternal) = insertToIndex 0 key storeIndex

      -- | Recursively inserts the new ID under indices of every dimension
      -- of the key.
      insertToIndex :: Int            -- ^ The position of the dimension of the head of the key in the store index vector.
                    -> I.Key kh kd kt -- ^ The key.
                    -> I.StoreIndex   -- ^ The store index.
                    -> (I.StoreIndex, I.ToKeyInternal (I.Key kh kd kt))
      -- Standard dimension, 1-dimensional key.
      insertToIndex d (I.K1 kh@(I.Dimension _)) index =
          second I.K1 $ indexUpdate kh d index

      -- Auto-increment dimension, 1-dimensional key.
      insertToIndex d (I.K1 kh@I.DimensionAuto) index =
          second I.K1 $ indexUpdate kh d index

      -- Standard dimension, (n + 1)-dimensional key.
      insertToIndex d (I.KN kh@(I.Dimension _) kt) index =
          let (nindex, res) = indexUpdate kh d index
          in  second (I.KN res) $ insertToIndex (d + 1) kt nindex    

      -- Auto-increment dimension (n + 1)-dimensional key.
      insertToIndex d (I.KN kh@I.DimensionAuto kt) index =
          let (nindex, res) = indexUpdate kh d index
          in  second (I.KN res) $ insertToIndex (d + 1) kt nindex    

      -- | Inserts the new ID under indices of the given dimension.
      indexUpdate :: I.Dimension a d -- ^ The dimension to be inserted.
                  -> Int             -- ^ The position of the dimension in the store index vector.
                  -> I.StoreIndex    -- ^ The store index.
                  -> (I.StoreIndex, I.DimensionInternal a d)
      indexUpdate d = V.updateAt' (I.insert d storeNextID)

      toInsertResult :: I.KeyInternal kh kd kt
                     -> I.KeyInsertResult (I.KeyInternal kh kd kt)
      toInsertResult (I.K1 (I.IDimensionAuto v)) = v I.:. ()
      toInsertResult (I.K1 (I.IDimension _))     = ()
      toInsertResult (I.KN (I.IDimensionAuto v) kt) = v I.:. toInsertResult kt
      toInsertResult (I.KN (I.IDimension _) kt)     = toInsertResult kt

