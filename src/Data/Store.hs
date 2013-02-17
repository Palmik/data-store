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
module Data.Store
(
  -- * Types
  I.Store
, I.Key
, I.KeyDimension
, I.M
, I.O
, (I.:.)(..)
, I.Auto

  -- * Creating
, empty
, singleton

  -- * Inserting
, insert

  -- * Updating
, update
, updateValues
, updateWithKey
, updateWithRawKey
, updateWithKeys
, delete

  -- * Traversing
, map

  -- * Folding
, foldr
, foldrWithKey
, foldrWithRawKey
, foldrWithKeys

, foldl
, foldlWithKey
, foldlWithRawKey
, foldlWithKeys

  -- * List
, toList
, values
, keys
, fromList

  -- * Querying
, size
, lookup

  -- ** Selection
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
  -- 
  -- $constructing-key
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

  -- * Debugging
, showIndex
, printIndex
) where

--------------------------------------------------------------------------------
import           Prelude hiding (lookup, map, foldr, foldl)
--------------------------------------------------------------------------------
import           Control.Applicative hiding (empty)
--------------------------------------------------------------------------------
import           Data.Maybe
import           Data.Monoid ((<>))
import qualified Data.Map    
import qualified Data.IntMap
import qualified Data.List
import qualified Data.Foldable
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

-- $constructing-key
-- These functions are used to create a key for your store. Function for
-- creating a key for our @Content@ data type could look like this:
--
-- > makeContentKey :: ContentID -> String -> String -> [String] -> Double -> ContentStoreKey
-- > makeContentKey cid cn cb cts cr =
-- >    S.dimO cid .: S.dimO cn .: S.dimO cb .: S.dimM cts .:. S.dimO cr
--
-- Notice that this function allows you to specify all the dimensions of
-- the key, including the ID dimension. Usually we do not need this level
-- of flexibility a would use function like this instead:
--
-- > contentKey :: Content -> ContentStoreKey
-- > contentKey (Content cn cb cts cr) =
-- >    S.dimA .: S.dimO cn .: S.dimO cb .: S.dimM cts .:. S.dimO cr
--
-- This function creates a key for given value of type @Content@, the ID
-- dimension is "automatic", which means that the assigned ID will be @succ
-- max@ where @max@ is the value of the maximum ID in the store when
-- inserting.

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

-- CREATING

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

-- INSERTING

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
-- * 'Data.Store.Internal.Type.RawKey'
insert :: I.Key krs ts
       -> v
       -> I.Store krs irs ts v
       -> Maybe (I.RawKey krs ts, I.Store krs irs ts v)
insert k v (I.Store vals index nid) =
    (\res -> (I.keyInternalToRaw internal, mk res)) <$> I.indexInsertID internal nid index
    where
      mk ix = I.Store
        { I.storeV = Data.IntMap.insert nid (internal, k, v) vals
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

-- TRAVERSING

-- | The expression @('Data.Store.map' tr old@) is store where every value of
-- @old@ was transformed using the function @tr@.
map :: (v1 -> v2) -> I.Store krs irs ts v1 -> I.Store krs irs ts v2
map tr store@(I.Store vs _ _) = store
    { I.storeV = Data.IntMap.map (\(ik, k, v) -> (ik, k, tr v)) vs
    }
{-# INLINE map #-}

-- QUERYING

-- | The expression (@'Data.Store.size' store@) is the number of elements
-- in @store@. 
size :: I.Store krs irs ts v -> Int
size (I.Store vs _ _) = Data.IntMap.size vs
{-# INLINE size #-}

-- UPDATING

-- | The expression (@'Data.Store.updateWithKey' tr sel s@) is equivalent
-- to (@'Data.Store.Selection.updateWithKeys' tr' sel s@) where
-- (@tr' = (\_ k v -> tr k v) = const tr@).
updateWithKey :: IsSelection sel
              => (I.Key krs ts -> v -> Maybe (v, Maybe (I.Key krs ts)))
              -> sel krs irs ts
              -> I.Store krs irs ts v
              -> Maybe (I.Store krs irs ts v)
updateWithKey tr = updateWithKeys (\_ -> tr)
{-# INLINE updateWithKey #-}

-- | The expression (@'Data.Store.updateWithRawKey' tr sel s@) is equivalent
-- to (@'Data.Store.Selection.updateWithKeys' tr' sel s@) where
-- (@tr' = (\rk _ v -> tr rk v)@).
updateWithRawKey :: IsSelection sel
                 => (I.RawKey krs ts -> v -> Maybe (v, Maybe (I.Key krs ts)))
                 -> sel krs irs ts
                 -> I.Store krs irs ts v
                 -> Maybe (I.Store krs irs ts v)
updateWithRawKey tr = updateWithKeys (\rk _ -> tr rk)
{-# INLINE updateWithRawKey #-}

-- | The expression (@'Data.Store.update' tr sel s@) is equivalent
-- to (@'Data.Store.Selection.updateWithKeys' tr' sel s@) where
-- (@tr' = (\_ _ v -> tr v) = const . const tr@).
update :: IsSelection sel
       => (v -> Maybe (v, Maybe (I.Key krs ts)))
       -> sel krs irs ts
       -> I.Store krs irs ts v
       -> Maybe (I.Store krs irs ts v)
update tr = updateWithKeys (\_ _ -> tr)
{-# INLINE update #-}

-- | The expression (@'Data.Store.updateValues' tr sel s@) is equivalent
-- to (@'Data.Store.Selection.updateWithKeys' tr' sel s@) where
-- (@tr' = (\_ _ -> maybe Nothing (\v -> Just (v, Nothing)) . tr)@).
updateValues :: IsSelection sel
             => (v -> Maybe v)
             -> sel krs irs ts
             -> I.Store krs irs ts v
             -> I.Store krs irs ts v
updateValues tr sel s = fromJust $ update (maybe Nothing (\v -> Just (v, Nothing)) . tr) sel s
{-# INLINE updateValues #-}             

-- FOLDING

-- | The expression (@'Data.Store.foldrWithKeys' f z s@) folds the store
-- using the given right-associative operator.
foldrWithKeys :: (I.RawKey krs ts -> I.Key krs ts -> v -> b -> b)
              -> b
              -> I.Store krs irs ts v
              -> b
foldrWithKeys accum start (I.Store vs _ _) =
    Data.IntMap.foldr (\(ik, k, v) b -> accum (I.keyInternalToRaw ik) k v b) start vs
{-# INLINE foldrWithKeys #-}

-- | The expression (@'Data.Store.foldrWithKey' f z s@) folds the store
-- using the given right-associative operator.
foldrWithKey :: (I.Key krs ts -> v -> b -> b)
             -> b
             -> I.Store krs irs ts v
             -> b
foldrWithKey accum start (I.Store vs _ _) =
    Data.IntMap.foldr (\(_, k, v) b -> accum k v b) start vs
{-# INLINE foldrWithKey #-}

-- | The expression (@'Data.Store.foldrWithRawKey' f z s@) folds the store
-- using the given right-associative operator.
foldrWithRawKey :: (I.RawKey krs ts -> v -> b -> b)
                -> b
                -> I.Store krs irs ts v
                -> b
foldrWithRawKey accum start (I.Store vs _ _) =
    Data.IntMap.foldr (\(ik, _, v) b -> accum (I.keyInternalToRaw ik) v b) start vs
{-# INLINE foldrWithRawKey #-}

-- | The expression (@'Data.Store.foldr' f z s@) folds the store
-- using the given right-associative binary operator.
foldr :: (v -> b -> b)
      -> b
      -> I.Store krs irs ts v
      -> b
foldr accum start (I.Store vs _ _) =
    Data.IntMap.foldr (\(_, _, v) b -> accum v b) start vs
{-# INLINE foldr #-}

-- | The expression (@'Data.Store.foldlWithKeys' f z s@) folds the store
-- using the given left-associative operator.
foldlWithKeys :: (b -> I.RawKey krs ts -> I.Key krs ts -> v -> b)
              -> b
              -> I.Store krs irs ts v
              -> b
foldlWithKeys accum start (I.Store vs _ _) =
    Data.IntMap.foldl (\b (ik, k, v) -> accum b (I.keyInternalToRaw ik) k v) start vs
{-# INLINE foldlWithKeys #-}

-- | The expression (@'Data.Store.foldlWithKey' f z s@) folds the store
-- using the given left-associative operator.
foldlWithKey :: (b -> I.Key krs ts -> v -> b)
             -> b
             -> I.Store krs irs ts v
             -> b
foldlWithKey accum start (I.Store vs _ _) =
    Data.IntMap.foldl (\b (_, k, v) -> accum b k v) start vs
{-# INLINE foldlWithKey #-}

-- | The expression (@'Data.Store.foldlWithRawKey' f z s@) folds the store
-- using the given left-associative operator.
foldlWithRawKey :: (b -> I.RawKey krs ts -> v -> b)
                -> b
                -> I.Store krs irs ts v
                -> b
foldlWithRawKey accum start (I.Store vs _ _) =
    Data.IntMap.foldl (\b (ik, _, v) -> accum b (I.keyInternalToRaw ik) v) start vs
{-# INLINE foldlWithRawKey #-}

-- | The expression (@'Data.Store.foldl' f z s@) folds the store
-- using the given left-associative binary operator.
foldl :: (b -> v -> b)
      -> b
      -> I.Store krs irs ts v
      -> b
foldl accum start (I.Store vs _ _) =
    Data.IntMap.foldl (\b (_, _, v) -> accum b v) start vs
{-# INLINE foldl #-}

-- LISTS

-- | The expression (@'Data.Store.toList' store@) is a list of triples of
-- raw key, key and a value that are stored in @store@.
toList :: I.Store krs irs ts v -> [(I.RawKey krs ts, I.Key krs ts, v)]
toList (I.Store vs _ _) = Data.List.map (\(ik, k, v) -> (I.keyInternalToRaw ik, k, v)) $ Data.IntMap.elems vs
{-# INLINE toList #-}

-- | The expression (@'Data.Store.values' store@) is a list of values that
-- are stored in @store@.
values :: I.Store krs irs ts v -> [v]
values (I.Store vs _ _) = Data.List.map (\(_, _, v) -> v) $ Data.IntMap.elems vs
{-# INLINE values #-}

-- | The expression (@'Data.Store.values' store@) is a list of pairs of raw
-- key and key that are stored in @store@.
keys :: I.Store krs irs ts v -> [(I.RawKey krs ts, I.Key krs ts)]
keys (I.Store vs _ _) = Data.List.map (\(ik, k, _) -> (I.keyInternalToRaw ik, k)) $ Data.IntMap.elems vs
{-# INLINE keys #-}

-- | The expression (@'Data.Store.fromList' kvs@) is either a) (@Just
-- store@) where @store@ is a store containing exactly the given key-value
-- pairs or; b) @Nothing@ if inserting any of the key-value pairs would
-- cause a collision.
fromList :: I.Empty (I.Index irs ts) => [(I.Key krs ts, v)] -> Maybe (I.Store krs irs ts v)
fromList = Data.Foldable.foldrM (\(k, v) s -> snd <$> insert k v s) empty 
{-# INLINE fromList #-}

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

