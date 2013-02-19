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
, I.RawKey
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
, delete

  -- * Traversing
, map

  -- * Folding
, foldr
, foldrWithKey

, foldl
, foldlWithKey

  -- * List
, toList
, values
, keys
, fromList

  -- * Querying
, size
, lookup

  -- ** Selection
  --
  -- $selection
, Selection
, not'
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

-- $selection
-- Functions from this category are used to create selections.
--
-- Example:
--
-- > -- Select any content with rating between 3 and 4.
-- > let sel1 = sContentRating .> 3
-- >
-- > -- Select any content that is tagged with "haskell" or "category-theory"
-- > -- and is not tagged with "fluff".
-- > let sel2 = (sContentTag .== "haskell" .|| sContentTag .== "category-theory") .&& not' (sContentTag .== "fluff")
-- >
-- > -- Selection that is intersection of sel1 and sel2.
-- > let sel3 = sel1 .&& sel2
--
-- These selections can be then used in functions like lookup, update,
-- delete, etc.
--  
-- >>> lookup sel3 store
-- > -- key-value pairs that match the selection
--
-- >>> delete (not' sel3) store
-- > -- store with the key-value pairs that do not match the selection
--
-- >>> updateValues (\v -> Just v { contentRating = 5 }) sel3 store
-- > -- store with the selected key-value pairs updated 


-- $constructing-key
-- Functions from this category are used to create a key for your store. Function for
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
--
-- See also:
--
-- * 'Data.Store.Storable.Storable'

-- | Function for creating an auto-increment dimension. Can be used instead
-- of (@dimO x@) if the type is an instance of the
-- @'Data.Store.Internal.Auto'@ type-class.
dimA :: I.Auto t => I.KeyDimension I.O t
dimA = I.KeyDimensionA
{-# INLINE dimA #-}

-- | Function for creating dimensions with the relation
-- \"one-<anything>\".
dimO :: Ord t => t -> I.KeyDimension I.O t
dimO = I.KeyDimensionO
{-# INLINE dimO #-}

-- | Function for creating dimensions with the relation
-- \"many-<anything>\".
dimM :: Ord t => [t] -> I.KeyDimension I.M t
dimM = I.KeyDimensionM
{-# INLINE dimM #-}

-- | Function for connecting one dimension and rest of the key.
(.:) :: dim r t
     -> I.GenericKey dim rs1 ts1 
     -> I.GenericKey dim (r I.:. rs1) (t I.:. ts1)
(.:) = I.KN
{-# INLINE (.:) #-}
infixr 3 .:

-- | Function for connecting one dimensions with another (most often the
-- last dimension of the key).
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
-- >>> let content = Content "name" "body" ["t1", "t2"] 0.5
-- >>> insert (contentKey content) content store
-- > Just (1 :. "name" :. "body" :. ["t1", "t2"] :. 0.5, <updated_store>)
--
-- See also:
--
-- * 'Data.Store.Internal.Type.Key'
-- * 'Data.Store.Internal.Type.RawKey'
insert :: I.Key krs ts
       -> v
       -> I.Store krs irs ts v
       -> Maybe (I.RawKey krs ts, I.Store krs irs ts v)
insert k v (I.Store vals index nid) =
    (\res -> (I.keyInternalToRaw internal, mk res)) <$> I.indexInsertID internal nid index
    where
      mk ix = I.Store
        { I.storeV = Data.IntMap.insert nid (internal, v) vals
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
    { I.storeV = Data.IntMap.map (\(ik, v) -> (ik, tr v)) vs
    }
{-# INLINE map #-}

-- QUERYING

-- | The expression (@'Data.Store.size' store@) is the number of elements
-- in @store@. 
size :: I.Store krs irs ts v -> Int
size (I.Store vs _ _) = Data.IntMap.size vs
{-# INLINE size #-}

-- UPDATING

-- | The expression (@'Data.Store.update' tr sel s@) is equivalent
-- to (@'Data.Store.Selection.updateWithKey' tr' sel s@) where
-- (@tr' = (\_ v -> tr v) = const tr@).
update :: IsSelection sel
       => (v -> Maybe (v, Maybe (I.Key krs ts)))
       -> sel krs irs ts
       -> I.Store krs irs ts v
       -> Maybe (I.Store krs irs ts v)
update tr = updateWithKey (const tr)
{-# INLINE update #-}

-- | The expression (@'Data.Store.updateValues' tr sel s@) is equivalent
-- to (@'Data.Store.Selection.update' tr' sel s@) where
-- (@tr' = (maybe Nothing (\v -> Just (v, Nothing)) . tr)@).
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
foldrWithKey :: (I.RawKey krs ts -> v -> b -> b)
             -> b
             -> I.Store krs irs ts v
             -> b
foldrWithKey accum start (I.Store vs _ _) =
    Data.IntMap.foldr (\(ik, v) b -> accum (I.keyInternalToRaw ik) v b) start vs
{-# INLINE foldrWithKey #-}

-- | The expression (@'Data.Store.foldr' f z s@) folds the store
-- using the given right-associative binary operator.
foldr :: (v -> b -> b)
      -> b
      -> I.Store krs irs ts v
      -> b
foldr accum start (I.Store vs _ _) =
    Data.IntMap.foldr (\(_, v) b -> accum v b) start vs
{-# INLINE foldr #-}

-- | The expression (@'Data.Store.foldlWithKeys' f z s@) folds the store
-- using the given left-associative operator.
foldlWithKey :: (b -> I.RawKey krs ts -> v -> b)
              -> b
              -> I.Store krs irs ts v
              -> b
foldlWithKey accum start (I.Store vs _ _) =
    Data.IntMap.foldl (\b (ik, v) -> accum b (I.keyInternalToRaw ik) v) start vs
{-# INLINE foldlWithKey #-}

-- | The expression (@'Data.Store.foldl' f z s@) folds the store
-- using the given left-associative binary operator.
foldl :: (b -> v -> b)
      -> b
      -> I.Store krs irs ts v
      -> b
foldl accum start (I.Store vs _ _) =
    Data.IntMap.foldl (\b (_, v) -> accum b v) start vs
{-# INLINE foldl #-}

-- LISTS

-- | The expression (@'Data.Store.toList' store@) is a list of triples of
-- raw key, key and a value that are stored in @store@.
toList :: I.Store krs irs ts v -> [(I.RawKey krs ts, v)]
toList (I.Store vs _ _) = Data.List.map (\(ik, v) -> (I.keyInternalToRaw ik, v)) $ Data.IntMap.elems vs
{-# INLINE toList #-}

-- | The expression (@'Data.Store.values' store@) is a list of values that
-- are stored in @store@.
values :: I.Store krs irs ts v -> [v]
values (I.Store vs _ _) = Data.List.map snd $ Data.IntMap.elems vs
{-# INLINE values #-}

-- | The expression (@'Data.Store.values' store@) is a list of pairs of raw
-- key and key that are stored in @store@.
keys :: I.Store krs irs ts v -> [I.RawKey krs ts]
keys (I.Store vs _ _) = Data.List.map (I.keyInternalToRaw . fst) $ Data.IntMap.elems vs
{-# INLINE keys #-}

-- | The expression (@'Data.Store.fromList' kvs@) is either a) (@Just
-- store@) where @store@ is a store containing exactly the given key-value
-- pairs or; b) @Nothing@ if inserting any of the key-value pairs would
-- cause a collision.
fromList :: I.Empty (I.Index irs ts) => [(I.Key krs ts, v)] -> Maybe (I.Store krs irs ts v)
fromList = Data.Foldable.foldlM (\s (k, v) -> snd <$> insert k v s) empty 
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

