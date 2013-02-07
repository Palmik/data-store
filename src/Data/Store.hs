{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Store
( I.Store
) where

--------------------------------------------------------------------------------
import           Prelude hiding (lookup)
--------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Applicative hiding (empty)
--------------------------------------------------------------------------------
import           Data.Maybe
import           Data.Monoid ((<>))
import qualified Data.Map    
import qualified Data.Map.Extra
import qualified Data.Set    
import qualified Data.IntMap
import qualified Data.IntSet
import qualified Data.Foldable as F
--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Type     as I
import qualified Data.Store.Internal.Function as I
import qualified Data.Store.Selection         as Selection
--------------------------------------------------------------------------------

-- | The name of this module.
moduleName :: String
moduleName = "Data.Store"

empty :: I.EmptyIndex (I.IndexSpec s) => I.Store s v
empty = I.Store
    { I.storeV = Data.IntMap.empty
    , I.storeI = I.emptyIndex
    , I.storeNID = minBound :: Int
    }

insert :: I.ZipDimensions (I.IndexSpec s) (I.KeySpec s)
       => I.Key (I.KeySpec s)
       -> v
       -> I.Store s v
       -> Maybe (I.RawKeyType (I.KeySpec s), I.Store s v)
insert k v (I.Store vs ix nid) =
    (\res -> (I.keyInternalToRaw internal, mk res)) <$> I.indexInsertID internal nid ix
    where
      mk ix' = I.Store
        { I.storeV = Data.IntMap.insert nid (internal, k, v) vs
        , I.storeI = ix'
        , I.storeNID = nid + 1
        }
      
      internal = toInternal ix k

      toInternal :: I.Index si -> I.Key sk -> I.IKey sk
      toInternal _ (I.K1 (I.KeyDimensionO x)) = I.K1 (I.IKeyDimensionO x) 
      toInternal _ (I.K1 (I.KeyDimensionM x)) = I.K1 (I.IKeyDimensionM x) 
      toInternal (I.IN _ ix) (I.KN (I.KeyDimensionO x) s) = I.KN (I.IKeyDimensionO x) $ toInternal ix s
      toInternal (I.IN _ ix) (I.KN (I.KeyDimensionM x) s) = I.KN (I.IKeyDimensionM x) $ toInternal ix s 
      toInternal _ _ = error $ moduleName <> ".insert.toInternal: Impossible happened."

showIndex :: Show (I.Index (I.IndexSpec s)) => I.Store s v -> String
showIndex (I.Store _ i _) = show i

printIndex :: Show (I.Index (I.IndexSpec s)) => I.Store s v -> IO ()
printIndex = putStrLn . showIndex

-- | TEST

type Account = Int
type MyStoreSpec = (I.DimensionOneMany, Double) I.:. (I.DimensionManyMany, String)
type MyStore = I.Store MyStoreSpec Account 

type MyStoreKey = I.Key (I.KeySpec MyStoreSpec)

type AccountBalance = I.N0
type AccountName = I.N1

makeMyStoreKey :: Double -> [String] -> MyStoreKey
makeMyStoreKey d1 d2 = I.KN (I.KeyDimensionO d1) $ I.K1 (I.KeyDimensionM d2)

sAccountBalance :: AccountBalance
sAccountBalance = I.Z

sAccountName :: AccountName
sAccountName = I.S I.Z

myStore0 :: MyStore
myStore0 = empty

myStore1 :: MyStore
myStore1 = snd . fromJust $ insert (makeMyStoreKey 1.5 ["aa", "bb", "cc"]) 0 myStore0 

myStore2 :: MyStore
myStore2 = snd . fromJust $ insert (makeMyStoreKey 3.5 ["aaa", "bbb", "ccc"]) 1 myStore1 

-- print myStore0
-- printIndex myStore0
-- print myStore1
-- printIndex myStore1
-- print myStore2
-- printIndex myStore2

