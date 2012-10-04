{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs        #-}

module Data.Store.Internal
( Store(..)
, StoreIndex

, Query(..)
, Selection(..)
) where

--------------------------------------------------------------------------------
import           Control.Monad.Reader
--------------------------------------------------------------------------------
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import           Data.Proxy
--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Key            as I
import qualified Data.Store.Internal.DimensionIndex as I
--------------------------------------------------------------------------------

type StoreIndex = V.Vector I.DimensionIndex

data Store tag k v = Store
    { storeValues :: IM.IntMap (v, I.ToKeyInternal k) -- ^ Map from ID to (value, key).
    , storeIndex  :: StoreIndex                       -- ^ Vector of maps from key to IDs. Each map represents a single dimension of the key.
    , storeNextID :: Int                              -- ^ The next ID.
    }  

newtype Query tag k v a = Query { unQuery :: Reader (Store tag k v) a }

data Selection tag k where
    SelectGT :: Proxy n -> I.DimensionType k n -> Selection tag k
    SelectLT :: Proxy n -> I.DimensionType k n -> Selection tag k
    SelectUnion :: Selection tag k -> Selection tag k -> Selection tag k
    SelectIntersection :: Selection tag k -> Selection tag k -> Selection tag k

