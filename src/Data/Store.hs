{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} 
--------------------------------------------------------------------------------
-- |
--
-- Module : Data.Store
-- Copyright : (c) Petr Pilar 2012
-- License : BSD-style
--
-- Multi-key multi-value store with type-safe interface.
--
-- These modules are intended to be imported qualified to avoid name
-- clashes with prelude, e.g.:
--
-- > import qualified Data.Store as S
-- > import           Data.Store (M, O, (.:), (.:.), (:.), (.<), (.<=), (.>), (.>=), (./=), (.==), (.&&), (.||))
--
-- Throughout out the documentation, the examples will be based on this
-- code:
--
-- > {-# LANGUAGE TypeOperators #-}
-- > 
-- > module Example01
-- > where
-- > 
-- > import qualified Data.Store as S
-- > import           Data.Store (M, O, (.:), (.:.), (:.), (.<), (.<=), (.>), (.>=), (./=), (.==), (.&&), (.||))
-- > 
-- > data Content = Content
-- >     { contentName :: String  
-- >     , contentBody :: String  
-- >     , contentTags :: [String]
-- >     , contentRating :: Double
-- >     }
-- > 
-- > type ContentID = Int
-- > 
-- > -- Content has one ID, only one content can have a given ID.
-- > -- Content has one name, only one content can have a given name.
-- > -- Content has one body, many contents can have the same content.
-- > -- Content has many tags, many contents can have the same tag.
-- > -- Content has one rating, many contents can have the same rating.
-- > 
-- > -- BOILERPLATE
-- > 
-- > type ContentStoreTS  = ContentID :. String :. String :. String :. Double
-- > type ContentStoreKRS = O         :. O      :. O      :. M      :. O
-- > type ContentStoreIRS = O         :. O      :. M      :. M      :. M
-- > type ContentStore = S.Store ContentStoreKRS ContentStoreIRS ContentStoreTS
-- > type ContentStoreKey = S.Key ContentStoreKRS ContentStoreTS
-- > 
-- > sContentID :: S.N0
-- > sContentID = S.n0
-- > 
-- > sContentName :: S.N1
-- > sContentName = S.n1
-- > 
-- > sContentBody :: S.N2
-- > sContentBody = S.n2
-- > 
-- > sContentTag :: S.N3
-- > sContentTag = S.n3
-- > 
-- > sContentRating :: S.N4
-- > sContentRating = S.n4
-- > 
-- > -- BOILERPLATE
-- > 
-- > makeKey :: Content -> ContentStoreKey
-- > makeKey (Content cn cb ct cr) =
-- >    S.dimA .: S.dimO cn .: S.dimO cb .: S.dimM ct .:. S.dimO cr
-- > 
module Data.Store
( I.Store
, I.Key
, I.KeyDimension
, I.M
, I.O
, (I.:.)(..)
, I.Auto

  -- * Creating
, empty
, singleton

  -- * Updating
, insert
, update
, updateValues
, delete

  -- * Querying
, size
, lookup

  -- * Selection
, Selection
, (.<)
, (.<=)
, (.>)
, (.>=)
, (./=)
, (.==)
, (.&&)
, (.||)

  -- * Constructing Key
, dimA
, dimO
, dimM
, (.:)
, (.:.)

  -- * Utility
, I.S(..)
, I.Z
, I.N0
, I.N1
, I.N2
, I.N3
, I.N4
, I.N5
, I.N6
, I.N7
, I.N8
, I.N9
, I.N10
, I.n0
, I.n1
, I.n2
, I.n3
, I.n4
, I.n5
, I.n6
, I.n7
, I.n8
, I.n9
, I.n10

, showIndex
, printIndex
) where

--------------------------------------------------------------------------------
import           Prelude hiding (lookup, map)
--------------------------------------------------------------------------------
import           Control.Applicative hiding (empty)
--------------------------------------------------------------------------------
import           Data.Maybe
import           Data.Monoid ((<>))
import qualified Data.Map    
import qualified Data.IntMap
--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Type     as I
import qualified Data.Store.Internal.Function as I
import           Data.Store.Selection 
--------------------------------------------------------------------------------

-- | The name of this module.
moduleName :: String
moduleName = "Data.Store"
{-# INLINE moduleName #-}

-- INTERFACE

dimA :: I.Auto t => I.KeyDimension I.O t
dimA = I.KeyDimensionA
{-# INLINE dimA #-}

dimO :: Ord t => t -> I.KeyDimension I.O t
dimO = I.KeyDimensionO
{-# INLINE dimO #-}

dimM :: Ord t => [t] -> I.KeyDimension I.M t
dimM = I.KeyDimensionM
{-# INLINE dimM #-}

(.:) :: dim r t
     -> I.GenericKey dim rs1 ts1 
     -> I.GenericKey dim (r I.:. rs1) (t I.:. ts1)
(.:) = I.KN
{-# INLINE (.:) #-}
infixr 3 .:

(.:.) :: dim r1 t1
      -> dim r2 t2
      -> I.GenericKey dim (r1 I.:. r2) (t1 I.:. t2)
(.:.) d1 d2 = I.KN d1 (I.K1 d2)
{-# INLINE (.:.) #-}
infixr 3 .:.

-- | The expression @'Data.Store.empty'@ is empty store.
empty :: I.Empty (I.Index irs ts) => I.Store krs irs ts v
empty = I.Store
    { I.storeV = Data.IntMap.empty
    , I.storeI = I.empty
    , I.storeNID = minBound
    }
{-# INLINE empty #-}

-- | The expression (@'Data.Store.singleton' k v@) is store that contains
-- only the @(k, v)@ as a key-value pair.
singleton :: I.Empty (I.Index irs ts)
          => I.Key krs ts -> v -> I.Store krs irs ts v
singleton k v = snd . fromJust $ insert k v empty
{-# INLINE singleton #-}

-- | The expression (@'Data.Store.insert' k v old@) is either
-- @Nothing@ if inserting the @(k, v)@ key-value pair would cause
-- a collision or (@Just rk new@) where @rk@ is the raw key of
-- @k@ and @new@ is store containing the same key-value pairs as @old@ plus
-- @(k, v)@. 
--
-- Examples:
--
-- > TODO
--
-- See also:
--
-- * @'Data.Store.Internal.Type.RawKey'@
insert :: I.Key krs ts
       -> v
       -> I.Store krs irs ts v
       -> Maybe (I.RawKeyType krs ts, I.Store krs irs ts v)
insert k v (I.Store values index nid) =
    (\res -> (I.keyInternalToRaw internal, mk res)) <$> I.indexInsertID internal nid index
    where
      mk ix = I.Store
        { I.storeV = Data.IntMap.insert nid (internal, k, v) values
        , I.storeI = ix
        , I.storeNID = nid + 1
        }
      {-# INLINE mk #-}
      
      internal = toInternal index k
      {-# INLINE internal #-}

      toInternal :: I.Index irs ts -> I.Key krs ts -> I.IKey krs ts
      toInternal (I.I1 ix) (I.K1 I.KeyDimensionA) = I.K1 (I.IKeyDimensionO $! nextKey ix) 
      toInternal (I.I1 _) (I.K1 (I.KeyDimensionO x)) = I.K1 (I.IKeyDimensionO x) 
      toInternal (I.I1 _) (I.K1 (I.KeyDimensionM x)) = I.K1 (I.IKeyDimensionM x) 
      toInternal (I.IN ix is) (I.KN I.KeyDimensionA s) = I.KN (I.IKeyDimensionO $! nextKey ix) $ toInternal is s
      toInternal (I.IN _ is) (I.KN (I.KeyDimensionO x) s) = I.KN (I.IKeyDimensionO x) $ toInternal is s
      toInternal (I.IN _ is) (I.KN (I.KeyDimensionM x) s) = I.KN (I.IKeyDimensionM x) $ toInternal is s 
      toInternal _ _ = error $ moduleName <> ".insert.toInternal: Impossible happened."
      {-# INLINE toInternal #-}

      nextKey :: I.Auto t => I.IndexDimension r t -> t
      nextKey i =
        case i of
            (I.IndexDimensionM m) -> nextKey' m
            (I.IndexDimensionO m) -> nextKey' m
        where
          nextKey' m = if Data.Map.null m
                          then minBound
                          else succ . fst $! Data.Map.findMax m
          {-# INLINE nextKey' #-}
      {-# INLINE nextKey #-}

-- | The expression @('Data.Store.map' tr old@) is store where every value of
-- @old@ was transformed using the function @tr@.
map :: (v1 -> v2) -> I.Store krs irs ts v1 -> I.Store krs irs ts v2
map tr store@(I.Store vs _ _) = store
    { I.storeV = Data.IntMap.map (\(ik, k, v) -> (ik, k, tr v)) vs
    }
{-# INLINE map #-}

-- | The expression (@'Data.Store.size' store@) is the number of elements
-- in @store@. 
size :: I.Store krs irs ts v -> Int
size (I.Store vs _ _) = Data.IntMap.size vs
{-# INLINE size #-}

-- INSTANCES

instance Functor (I.Store krs irs ts) where
    fmap = map
    {-# INLINE fmap #-}

-- UTILITY

showIndex :: Show (I.Index irs ts) => I.Store krs irs ts v -> String
showIndex (I.Store _ i _) = show i
{-# INLINE showIndex #-}

printIndex :: Show (I.Index irs ts) => I.Store krs irs ts v -> IO ()
printIndex = putStrLn . showIndex
{-# INLINE printIndex #-}

-- TEST

{-
type Account = Int
type MyKRS = I.O    I.:. I.M
type MyIRS = I.M    I.:. I.M
type MyTS  = Double I.:. String
type MyStore = I.Store MyKRS MyIRS MyTS Account 

type MyStoreKey = I.Key MyKRS MyTS

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

#-}

