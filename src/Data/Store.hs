{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
--------------------------------------------------------------------------------
-- |
--
-- Module    : Data.Store
-- Copyright : (c) Petr Pilar 2012
-- License   : BSD-style
--
-- Easy to use in-memory data store with multidimensional keys and
-- type-safe interface.
--
-- These modules are intended to be imported qualified to avoid name
-- clashes with prelude, e.g.:
--
-- > import qualified Data.Store as DS
--
-- Throughout out the documentation, the examples will be based on this
-- code:
--
-- > -- | TYPES
-- > 
-- > -- | Simple ADT representing an article.
-- > data Article = Article
-- >     { articleName :: TS.Text
-- >     , articleBody :: TS.Text
-- >     , articleTags :: [TS.Text]
-- >     } deriving (Eq, Ord, Show)
-- > 
-- > newtype ArticleID = ArticleID Int deriving (Eq, Ord, Show)
-- > 
-- > instance DS.Auto ArticleID where
-- >     initValue = ArticleID 1
-- >     nextValue (ArticleID n) = ArticleID $ n + 1
-- > 
-- > --------------------------------------------------------------------------------
-- > -- | BOILERPLATE 
-- > 
-- > -- | Type synonym for key key specification.
-- > type ArticleKeySpec =
-- >     (  (ArticleID, DS.DimAuto)
-- >     :. (TS.Text, DS.Dim)
-- >     :. (TS.Text, DS.Dim)
-- >     :. (TS.Text, DS.Dim) :. DS.K0)
-- > 
-- > data ArticleStoreTag 
-- > type ArticleStore    = DS.Store ArticleStoreTag ArticleKeySpec Article
-- > 
-- > type ArticleSelection = DS.Selection ArticleStoreTag ArticleKeySpec
-- > type ArticleKey = DS.Key ArticleKeySpec
-- > 
-- > articleKey :: Article -> ArticleKey
-- > articleKey (Article n b ts) = DS.dimA
-- >                            .: DS.dimN [n]
-- >                            .: DS.dimN [b]
-- >                            .: DS.K1 (DS.dimN ts)
-- > 
-- > -- | Shortcut for selecting on article ID.
-- > sArticleID :: Proxy (ArticleStoreTag, DS.N0)
-- > sArticleID = Proxy
-- > 
-- > -- | Shortcut for selecting on article name.
-- > sArticleName :: Proxy (ArticleStoreTag, DS.N1)
-- > sArticleName = Proxy
-- > 
-- > -- | Shortcut for selecting on article body.
-- > sArticleBody :: Proxy (ArticleStoreTag, DS.N2)
-- > sArticleBody = Proxy
-- > 
-- > -- | Shortcut for selecting on article tags.
-- > sArticleTag :: Proxy (ArticleStoreTag, DS.N3)
-- > sArticleTag = Proxy
-- > 
-- > -- | BOILERPLATE 
-- > --------------------------------------------------------------------------------
-- 
-- See the 'examples' directory for more complete examples.
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
import qualified Data.Map    
import qualified Data.Map.Extra
import qualified Data.Set    
import qualified Data.IntMap
import qualified Data.IntSet
import qualified Data.Foldable as F
--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Type as I
--------------------------------------------------------------------------------

-- | The name of this module.
moduleName :: String
moduleName = "Data.Store"

empty :: I.EmptyIndex (I.IndexSpec s) => I.Store s v
empty = I.Store
    { I.storeV = Data.IntMap.empty
    , I.storeK = Data.IntMap.empty
    , I.storeI = I.emptyIndex
    , I.storeNID = minBound :: Int
    }

insert :: I.ZipDimensions (I.IndexSpec s) (I.KeySpec s)
       => I.Key (I.KeySpec s)
       -> v
       -> I.Store s v
       -> Maybe (I.Store s v)
insert k v (I.Store vs ks ix nid) = mk <$> I.zipDimensions (zipInsert nid) ix k 
    where
      mk ix' = I.Store
        { I.storeV = Data.IntMap.insert nid v vs
        , I.storeK = Data.IntMap.insert nid k ks
        , I.storeI = ix'
        , I.storeNID = nid + 1
        }

zipInsert :: Ord a => Int -> I.IndexDimension ti a -> I.KeyDimension tk a -> Maybe (I.IndexDimension ti a)
zipInsert v index key =
    case (index, key) of
      (I.IndexDimensionO m, I.KeyDimensionO k)  -> I.IndexDimensionO <$> goO k v m
      (I.IndexDimensionO m, I.KeyDimensionM ks) -> I.IndexDimensionO <$> F.foldrM (\k acc -> goO k v acc) m ks
      (I.IndexDimensionM m, I.KeyDimensionO k)  -> Just . I.IndexDimensionM $ goM k v m
      (I.IndexDimensionM m, I.KeyDimensionM ks) -> Just . I.IndexDimensionM $ F.foldr (\k acc -> goM k v acc) m ks
    where
      goO :: Ord k => k -> Int -> Data.Map.Map k Int -> Maybe (Data.Map.Map k Int)
      goO = Data.Map.Extra.insertUnique

      goM :: Ord k => k -> Int -> Data.Map.Map k Data.IntSet.IntSet -> Data.Map.Map k Data.IntSet.IntSet
      goM k v = Data.Map.insertWith (\_ s -> Data.IntSet.insert v s) k (Data.IntSet.singleton v)

showIndex :: Show (I.Index (I.IndexSpec s)) => I.Store s v -> String
showIndex (I.Store _ _ i _) = show i

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
myStore1 = fromJust $ insert (makeMyStoreKey 1.5 ["aa", "bb", "cc"]) 0 myStore0 

myStore2 :: MyStore
myStore2 = fromJust $ insert (makeMyStoreKey 3.5 ["aaa", "bbb", "ccc"]) 1 myStore1 

-- print myStore0
-- printIndex myStore0
-- print myStore1
-- printIndex myStore1
-- print myStore2
-- printIndex myStore2

