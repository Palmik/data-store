{-# LANGUAGE GADTs #-}

module Data.Store.Internal
( Store(..)
, StoreIndex
) where

--------------------------------------------------------------------------------
import qualified Data.IntMap as IM
import qualified Data.Vector as V
--------------------------------------------------------------------------------
import qualified Data.Store.Internal.DimensionIndex as I
--------------------------------------------------------------------------------

type StoreIndex = V.Vector I.DimensionIndex

data Store tag k v = Store
    { storeValues :: IM.IntMap v -- ^ Map from ID to value.
    , storeIndex  :: StoreIndex -- ^ Vector of maps from key to IDs. Each map represents a single dimension of the key.
    , storeNextID :: Int -- ^ The next ID.
    }  

