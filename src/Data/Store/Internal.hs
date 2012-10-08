{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Store.Internal
( Store(..)
, StoreIndex

, Query(..)
, Selection(..)
) where

--------------------------------------------------------------------------------
import           Control.Applicative
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

newtype Query tag k v a = Query
    { unQuery :: Reader (Store tag k v) a
    }

instance Functor (Query tag k v) where
    fmap f = Query . fmap f . unQuery

instance Applicative (Query tag k v) where
    pure = Query . pure
    (Query f) <*> (Query r) = Query (f <*> r)
    (Query r1) *> (Query r2) = Query (r1 *> r2)
    (Query r1) <* (Query r2) = Query (r1 <* r2)

instance Monad (Query tag k v) where
    return = Query . return
    (Query r1) >>= f = Query (r1 >>= (unQuery . f))
    (Query r1) >> (Query r2) = Query (r1 >> r2)

data Selection tag k where
    SelectGT :: (I.ToInt n, Ord (I.DimensionType k n))
             => Proxy n -> I.DimensionType k n -> Selection tag k
    SelectLT :: (I.ToInt n, Ord (I.DimensionType k n))
             => Proxy n -> I.DimensionType k n -> Selection tag k
    SelectUnion :: Selection tag k -> Selection tag k -> Selection tag k
    SelectIntersection :: Selection tag k -> Selection tag k -> Selection tag k

