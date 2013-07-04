{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE CPP                 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} 

--------------------------------------------------------------------------------
-- |
--
-- Module : Data.Store
-- Copyright : (c) Petr Pilar 2012
-- License : BSD-style
--
-- Dictionary with multidimensional keys and type-safe interface.
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
-- > --------------------------------------------------------------------------------
-- > import           Control.Applicative
-- > import qualified Control.Monad.State as State
-- > --------------------------------------------------------------------------------
-- > import qualified Data.Store as S
-- > import           Data.Store (M, O, (.:), (.:.), (:.)(..), (.<), (.<=), (.>), (.>=), (./=), (.==), (.&&), (.||))
-- > --------------------------------------------------------------------------------
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
-- > data ContentStoreTag = ContentStoreTag
-- > 
-- > type ContentStoreTS  = ContentID :. String :. String :. String :. Double
-- > type ContentStoreKRS = O         :. O      :. O      :. M      :. O
-- > type ContentStoreIRS = O         :. O      :. M      :. M      :. M
-- > type ContentStore = S.Store ContentStoreTag ContentStoreKRS ContentStoreIRS ContentStoreTS Content
-- > type ContentStoreKey = S.Key ContentStoreKRS ContentStoreTS
-- > type ContentStoreSelection = S.Selection ContentStoreTag ContentStoreKRS ContentStoreIRS ContentStoreTS
-- > 
-- > sContentID :: (ContentStoreTag, S.N0)
-- > sContentID = (ContentStoreTag, S.n0)
-- > 
-- > sContentName :: (ContentStoreTag, S.N1)
-- > sContentName = (ContentStoreTag, S.n1)
-- > 
-- > sContentBody :: (ContentStoreTag, S.N2)
-- > sContentBody = (ContentStoreTag, S.n2)
-- > 
-- > sContentTag :: (ContentStoreTag, S.N3)
-- > sContentTag = (ContentStoreTag, S.n3)
-- > 
-- > sContentRating :: (ContentStoreTag, S.N4)
-- > sContentRating = (ContentStoreTag, S.n4)
--
-- Glossary
--
-- * Key (type/value) -- refers either to the type or value of a key of the
-- store.
--
-- * Key dimension -- refers to one dimension of a key (e.g.: article's
-- author, article's tag). Refers to the dimension as a whole, together
-- with its properties, etc.
--
-- * Key dimension value -- refers to some concrete value from the domain of the
-- dimension.
--
-- * Element (type/value) -- refers either to the type or value of the elements
-- (in literature, the term \"value\" is usually used, be here it would
-- clash far too often) of the store.
--
-- The implementation is based on "Data.Map", "Data.Set", "Data.IntMap" and
-- "Data.IntSet".
--
-- The following variables and constants are used in Big-O notation:
--
-- * /W/ -- the (constant) number of bits of "Int" (32 or 64).
-- 
-- * /d/ -- the (constant) number of dimensions of the store.
-- 
-- * /k/ -- the (variable) number of key dimensions values of a key (or
-- maximum of the number of key dimension values over all keys in case of for example
-- @'Data.Store.updateWithKey'@).
--
-- * /s/ -- the (variable) size of the output of the operation or the
-- (variable) number of elements affected by the operation. This is
-- often the number of key-element pairs that correspond to some selection.
-- 
-- * /s(sel)/ -- the (variable) number of key-element pairs that correspond
-- to a selection /sel/ if /s/ would otherwise be ambigious.
--
-- * /c/ -- the (variable) complexity of selection.
--
-- * /c(sel)/ -- the (variable) complexity of resolving the selection /sel/ if /c/
-- would otherwise be ambiguous.
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
, I.empty
, singleton

  -- * Inserting
, insert
, insert'
, unsafeInsert

  -- * Updating
, updateWithKey
, updateWithKey'
, update
, update'
, updateElements
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
, elements
, keys
, insertList
, insertList'
, unsafeInsertList
, fromList
, fromList'
, unsafeFromList

  -- * Querying
, size
, lookup
, lookupOrderByA
, lookupOrderByD

  -- ** Selection
  --
  -- $selection
, Selection
, not
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
, moduleName
) where

--------------------------------------------------------------------------------
import           Prelude hiding (lookup, map, foldr, foldl, not)
--------------------------------------------------------------------------------
import           Control.Applicative hiding (empty)
--------------------------------------------------------------------------------
import           Data.Monoid
import           Data.Maybe
import           Data.Functor.Identity
#if MIN_VERSION_containers(0,5,0)
import qualified Data.IntMap.Strict as Data.IntMap
#else
import qualified Data.IntMap
#endif
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
-- > -- key-element pairs that match the selection
--
-- >>> delete (not' sel3) store
-- > -- store with the key-element pairs that do not match the selection
--
-- >>> updateElements (\v -> Just v { contentRating = 5 }) sel3 store
-- > -- store with the selected key-element pairs updated 


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
-- This function creates a key for given element of type @Content@ (element), the ID
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


-- | The expression (@'Data.Store.singleton' k v@) is store that contains
-- only the @(k, v)@ as a key-element pair.
singleton :: I.Empty (I.Index irs ts)
          => I.Key krs ts -> v -> I.Store tag krs irs ts v
singleton k v = snd . fromJust $ insert k v I.empty
{-# INLINE singleton #-}

-- INSERTING

-- | The expression (@'Data.Store.insert' k e old@) is either
-- @Nothing@ if inserting the @(k, e)@ key-element pair would cause
-- a collision or (@Just (rk, new)@), where @rk@ is the raw key of
-- @k@ and @new@ is store containing the same key-element pairs as @old@ plus
-- @(k, e)@. 
--
-- Examples:
--
-- >>> let content = Content "name" "body" ["t1", "t2"] 0.5
-- >>> insert (contentKey content) content store
-- > Just (1 :. "name" :. "body" :. ["t1", "t2"] :. 0.5, <updated_store>)
--
-- Complexity: /O(min(n, W) + k * (log n + min(n, W)))/
--
-- See also:
--
-- * 'Data.Store.insert''
--
-- * 'Data.Store.Internal.Type.Key'
--
-- * 'Data.Store.Internal.Type.RawKey'
insert :: I.Key krs ts
       -> v
       -> I.Store tag krs irs ts v
       -> Maybe (I.RawKey krs ts, I.Store tag krs irs ts v)
insert k v old@(I.Store _ index _) =
    (\res -> (I.keyInternalToRaw internal, res)) <$> I.genericInsert I.indexInsertID internal v old
    where
      internal = I.keyToInternal index k

-- | The expression (@'Data.Store.insert'' k v old@) is @(rk, new)@,
-- where @rk@ is the raw key of @k@ and @new@ is a store that contains
-- the same key-element pairs as @old@ plus @(k, e)@.
-- Any key-value pairs from @old@ colliding with @(k, e)@ are not included in @new@.
--
-- Complexity: /O(min(n, W) + k * (log n + min(n, W)))/
--
-- See also:
--
-- * 'Data.Store.insert'
--
-- * 'Data.Store.Internal.Type.Key'
--
-- * 'Data.Store.Internal.Type.RawKey'
insert' :: I.Key krs ts
        -> e
        -> I.Store tag krs irs ts e
        -> (I.RawKey krs ts, I.Store tag krs irs ts e)
insert' k e old@(I.Store _ index _) =
    (I.keyInternalToRaw internal, runIdentity $! I.genericInsert I.indexInsertID' internal e old)
    where
      internal = I.keyToInternal index k
{-# INLINE insert' #-}

-- | UNSAFE! This function can corrupt the store.
--
-- The expression (@'Data.Store.unsafeInsert' k v old@) is @(rk, new)@,
-- where @rk@ is the raw key of @k@ and @new@ is a store that contains
-- the same key-element pairs as @old@ plus @(k, e)@.
-- Any key-value pairs from @old@ colliding with @(k, e)@ will cause UNDEFINED BEHAVIOUR.
--
-- See also:
--
-- * 'Data.Store.insert'
--
-- * 'Data.Store.insert''
--
-- * 'Data.Store.Internal.Type.Key'
--
-- * 'Data.Store.Internal.Type.RawKey'
unsafeInsert :: I.Key krs ts
             -> e
             -> I.Store tag krs irs ts e
             -> (I.RawKey krs ts, I.Store tag krs irs ts e)
unsafeInsert k e old@(I.Store _ index _) =
    (I.keyInternalToRaw internal, runIdentity $! I.genericInsert I.indexInsertID'' internal e old)
    where
      internal = I.keyToInternal index k
{-# INLINE unsafeInsert #-}

-- TRAVERSING

-- | The expression @('Data.Store.map' tr old@) is store where every element of
-- @old@ was transformed using the function @tr@.
map :: (v1 -> v2) -> I.Store tag krs irs ts v1 -> I.Store tag krs irs ts v2
map tr store@(I.Store vs _ _) = store
    { I.storeV = Data.IntMap.map (\(ik, v) -> (ik, tr v)) vs
    }
{-# INLINE map #-}

-- QUERYING

-- | The expression (@'Data.Store.Selection.lookup' sel store@) is
-- list of (raw key)-element pairs that match the selection.
-- 
-- Complexity: /O(c + s * min(n, W))/
--
-- See also:
--
--   * 'Data.Store.lookupOrderByA'
--
--   * 'Data.Store.lookupOrderByD'
lookup :: IsSelection sel => sel tag krs irs ts -> I.Store tag krs irs ts v -> [(I.RawKey krs ts, v)]
lookup sel s = I.genericLookup (resolve sel s) s
{-# INLINE lookup #-}

-- | The expression (@'Data.Store.Selection.lookup' sel store@) is
-- list of (raw key)-element pairs that match the selection.
-- The list is sorted in ascending order with respect to the specified dimension.
--
-- NOTE: The function only works (this is ensured on the type level) with when
-- the ordering is based on dimensions of type one-one and one-many.
--
-- Complexity: /O(c + (s * log(s)) + s * min(n, W))/
--
-- See also:
--
--   * 'Data.Store.lookupOrderByD'
--
--   * 'Data.Store.lookup'
lookupOrderByA :: (I.DimensionRelation n krs ts ~ I.O, I.GetDimension n (I.IKey krs ts), IsSelection sel)
               => sel tag krs irs ts
               -> (tag, n)
               -> I.Store tag krs irs ts v
               -> [(I.RawKey krs ts, v)]
lookupOrderByA sel (_, n) s = I.genericLookupAsc (resolve sel s) n s
{-# INLINE lookupOrderByA #-}

-- | The expression (@'Data.Store.Selection.lookup' sel store@) is
-- list of (raw key)-element pairs that match the selection.
-- The list is sorted in descending order with respect to the specified dimension.
--
-- NOTE: The function only works (this is ensured on the type level) with when
-- the ordering is based on dimensions of type one-one and one-many.
--
-- Complexity: /O(c + (s * log(s)) + s * min(n, W))/
--
-- See also:
--
--   * 'Data.Store.lookupOrderByA'
--
--   * 'Data.Store.lookup'
lookupOrderByD :: (I.DimensionRelation n krs ts ~ I.O, I.GetDimension n (I.IKey krs ts), IsSelection sel)
               => sel tag krs irs ts
               -> (tag, n)
               -> I.Store tag krs irs ts v
               -> [(I.RawKey krs ts, v)]
lookupOrderByD sel (_, n) s = I.genericLookupDesc (resolve sel s) n s
{-# INLINE lookupOrderByD #-}

-- | The expression (@'Data.Store.size' store@) is the number of elements
-- in @store@. 
size :: I.Store tag krs irs ts v -> Int
size (I.Store vs _ _) = Data.IntMap.size vs
{-# INLINE size #-}

-- UPDATING

-- | The expression (@'Data.Store.updateWithKey' tr sel old@)
-- is (@Just new@) where @new@ is a store containing the same key-element
-- pairs as @old@ except for any key-element pairs @(k, e)@ that match the
-- selection @sel@, those are updated as follows:
--
-- * If @(tr k e)@ is @Nothing@ the pair is not included in @new@.
--
-- * If @(tr k e)@ is (@Just (e', Nothing)@) the pair is replaced by pair @(k, e')@.
--
-- * If @(tr k e)@ is (@Just (e', Just k')@) the pair is replaced by pair @(k', e')@.
--
-- If any of the updated key-element pairs would cause a collision, the
-- result is @Nothing@.
--
-- Complexity: /O(c + s * (min(n, W) + k * (log n + min(n, W))))/
--
-- See also:
--
--   * 'Data.Store.updateWithKey''
updateWithKey :: IsSelection sel
              => (I.RawKey krs ts -> v -> Maybe (v, Maybe (I.Key krs ts)))
              -> sel tag krs irs ts
              -> I.Store tag krs irs ts v
              -> Maybe (I.Store tag krs irs ts v)
updateWithKey tr sel s = I.genericUpdateWithKey I.indexInsertID tr (resolve sel s) s
{-# INLINE updateWithKey #-}

-- | The expression (@'Data.Store.updateWithKey'' tr sel old@)
-- is @new@ where @new@ is a store containing the same key-element
-- pairs as @old@ except for any key-element pairs @(k, e)@ that match the
-- selection @sel@, those are updated as follows:
--
-- * If @(tr k e)@ is @Nothing@ the pair is not included in @new@.
--
-- * If @(tr k e)@ is (@Just (e', Nothing)@) the pair is replaced by pair @(k, e')@.
--
-- * If @(tr k e)@ is (@Just (e', Just k')@) the pair is replaced by pair @(k', e')@.
--
-- Any pairs of the original store @old@ that would, after the update, cause collisons
-- are not included in @new@. 
--
-- Complexity: /O(c + s * d * (min(n, W) + k * (log n + min(n, W))))/
--
-- See also:
--
--   * 'Data.Store.updateWithKey'
updateWithKey' :: IsSelection sel
               => (I.RawKey krs ts -> v -> Maybe (v, Maybe (I.Key krs ts)))
               -> sel tag krs irs ts
               -> I.Store tag krs irs ts v
               -> I.Store tag krs irs ts v
updateWithKey' tr sel s = runIdentity $! I.genericUpdateWithKey I.indexInsertID' tr (resolve sel s) s
{-# INLINE updateWithKey' #-}

-- | The expression (@'Data.Store.update' tr sel s@) is equivalent
-- to (@'Data.Store.Selection.updateWithKey' tr' sel s@) where
-- (@tr' = (\_ v -> tr v) = const tr@).
--
-- Complexity: /O(c + s * (min(n, W) + q * log n))/ 
update :: IsSelection sel
       => (v -> Maybe (v, Maybe (I.Key krs ts)))
       -> sel tag krs irs ts
       -> I.Store tag krs irs ts v
       -> Maybe (I.Store tag krs irs ts v)
update tr = updateWithKey (const tr)
{-# INLINE update #-}

-- | The expression (@'Data.Store.update'' tr sel s@) is equivalent
-- to (@'Data.Store.Selection.updateWithKey'' tr' sel s@) where
-- (@tr' = (\_ v -> tr v) = const tr@).
--
-- Complexity: /O(c + d * s * (min(n, W) + q * log n))/ 
update' :: IsSelection sel
        => (v -> Maybe (v, Maybe (I.Key krs ts)))
        -> sel tag krs irs ts
        -> I.Store tag krs irs ts v
        -> I.Store tag krs irs ts v
update' tr = updateWithKey' (const tr)
{-# INLINE update' #-}

-- | The expression (@'Data.Store.updateElements' tr sel s@) is equivalent
-- to (@'Data.Store.Selection.update' tr' sel s@) where
-- (@tr' = (maybe Nothing (\v -> Just (v, Nothing)) . tr)@).
--
-- Complexity: /O(c + s * min(n, W))/ 
updateElements :: IsSelection sel
               => (v -> Maybe v)
               -> sel tag krs irs ts
               -> I.Store tag krs irs ts v
               -> I.Store tag krs irs ts v
updateElements tr sel s =
  runIdentity $! I.genericUpdateWithKey I.indexInsertID'' tr' (resolve sel s) s
  where
    tr' _ = maybe Nothing (\v -> Just (v, Nothing)) . tr
    {-# INLINE tr' #-}
{-# INLINE updateElements #-}             

-- | The expression (@'Data.Store.Selection.delete' sel old@) is
-- equivalent to
-- (@'Data.Store.fromJust' $ 'Data.Store.Selection.update' (const Nothing) sel old@).
--
-- Complexity: /O(c + s * (min(n, W) + q * log n)/ 
delete :: IsSelection sel
       => sel tag krs irs ts
       -> I.Store tag krs irs ts v
       -> I.Store tag krs irs ts v
delete sel s =
  runIdentity $! I.genericUpdateWithKey I.indexInsertID'' (\_ _ -> Nothing) (resolve sel s) s
{-# INLINE delete #-}

-- FOLDING

-- | The expression (@'Data.Store.foldrWithKey' f z s@) folds the store
-- using the given right-associative operator.
foldrWithKey :: (I.RawKey krs ts -> v -> b -> b)
             -> b
             -> I.Store tag krs irs ts v
             -> b
foldrWithKey accum start (I.Store vs _ _) =
    Data.IntMap.foldr (\(ik, v) b -> accum (I.keyInternalToRaw ik) v b) start vs
{-# INLINE foldrWithKey #-}

-- | The expression (@'Data.Store.foldr' f z s@) folds the store
-- using the given right-associative binary operator.
foldr :: (v -> b -> b)
      -> b
      -> I.Store tag krs irs ts v
      -> b
foldr accum start (I.Store vs _ _) =
    Data.IntMap.foldr (\(_, v) b -> accum v b) start vs
{-# INLINE foldr #-}

-- | The expression (@'Data.Store.foldlWithKey' f z s@) folds the store
-- using the given left-associative operator.
foldlWithKey :: (b -> I.RawKey krs ts -> v -> b)
              -> b 
              -> I.Store tag krs irs ts v
              -> b
foldlWithKey accum start (I.Store vs _ _) =
    Data.IntMap.foldl (\b (ik, v) -> accum b (I.keyInternalToRaw ik) v) start vs
{-# INLINE foldlWithKey #-}

-- | The expression (@'Data.Store.foldl' f z s@) folds the store
-- using the given left-associative binary operator.
foldl :: (b -> v -> b)
      -> b
      -> I.Store tag krs irs ts v
      -> b
foldl accum start (I.Store vs _ _) =
    Data.IntMap.foldl (\b (_, v) -> accum b v) start vs
{-# INLINE foldl #-}

-- LISTS

-- | The expression (@'Data.Store.toList' store@) is a list of key-element pairs that are stored in @store@.
toList :: I.Store tag krs irs ts v -> [(I.RawKey krs ts, v)]
toList (I.Store vs _ _) = Data.List.map (\(ik, v) -> (I.keyInternalToRaw ik, v)) $ Data.IntMap.elems vs
{-# INLINE toList #-}

-- | The expression (@'Data.Store.elements' store@) is a list of elements that
-- are stored in @store@.
elements :: I.Store tag krs irs ts v -> [v]
elements (I.Store vs _ _) = Data.List.map snd $ Data.IntMap.elems vs
{-# INLINE elements #-}

-- | The expression (@'Data.Store.keys' store@) is a list of pairs raw
-- keys that are stored in @store@.
keys :: I.Store tag krs irs ts v -> [I.RawKey krs ts]
keys (I.Store vs _ _) = Data.List.map (I.keyInternalToRaw . fst) $ Data.IntMap.elems vs
{-# INLINE keys #-}

-- | The expression (@'Data.Store.fromList' kes@) is either
-- a) (@Just store@) where @store@ is a store containing exactly the given
-- key-element pairs or;
-- b) @Nothing@ if inserting any of the key-element pairs would
-- cause a collision.
--
-- See also:
--
-- * 'Data.Store.fromList''
--
-- * 'Data.Store.insertList'
--
-- * 'Data.Store.insertList''
fromList :: I.Empty (I.Index irs ts) => [(I.Key krs ts, v)] -> Maybe (I.Store tag krs irs ts v)
fromList = insertList I.empty
{-# INLINE fromList #-}

-- | The expression (@'Data.Store.fromList' old kes@) is either
-- a) (@Just store@) where @store@ is a store containing exactly the key element pairs of @old@ plus the given
-- key-element pairs @kes@ or;
-- b) @Nothing@ if inserting any of the key-element pairs would
-- cause a collision.
--
-- See also:
--
-- * 'Data.Store.insertList''
--
-- * 'Data.Store.fromList'
--
-- * 'Data.Store.fromList''
insertList :: I.Empty (I.Index irs ts) => I.Store tag krs irs ts v -> [(I.Key krs ts, v)] -> Maybe (I.Store tag krs irs ts v)
insertList = Data.Foldable.foldlM (\s (k, v) -> snd <$> insert k v s) 
{-# INLINE insertList #-}

-- | The expression (@'Data.Store.fromList'' kes@) is @store@
-- containing the given key-element pairs (colliding pairs are not included).
--
-- See also:
--
-- * 'Data.Store.fromList'
--
-- * 'Data.Store.insertList''
--
-- * 'Data.Store.insertList'
fromList' :: I.Empty (I.Index irs ts) => [(I.Key krs ts, v)] -> I.Store tag krs irs ts v
fromList' = insertList' I.empty
{-# INLINE fromList' #-}

-- | The expression (@'Data.Store.insertList'' old kes@) is @store@
-- containing the key-element pairs of @old@ plus the given key-element pairs @kvs@
-- (colliding pairs are not included and the pairs from @kes@ take precedence).
--
-- See also:
--
-- * 'Data.Store.insertList'
--
-- * 'Data.Store.fromList''
--
-- * 'Data.Store.fromList'
insertList' :: I.Empty (I.Index irs ts) => I.Store tag krs irs ts v -> [(I.Key krs ts, v)] -> I.Store tag krs irs ts v
insertList' = Data.List.foldl' (\s (k, v) -> snd $! insert' k v $! s)
{-# INLINE insertList' #-}

-- | UNSAFE! This function can corrupt the store.
-- 
-- The expression (@'Data.Store.unsafeFromList' kes@) is @store@
-- containing the given key-element pairs (colliding pairs cause UNDEFINED BEHAVIOUR).
--
-- See also:
--
-- * 'Data.Store.fromList'
--
-- * 'Data.Store.fromList'
unsafeFromList :: I.Empty (I.Index irs ts) => [(I.Key krs ts, v)] -> I.Store tag krs irs ts v
unsafeFromList = unsafeInsertList I.empty 
{-# INLINE unsafeFromList #-}

-- | UNSAFE! This function can corrupt the store.
-- 
-- The expression (@'Data.Store.unsafeInsertList' old kvs@) is @store@
-- containing the key-element pairs of @old@ plus the given key-element pairs @kvs@
-- (colliding pairs cause UNDEFINED BEHAVIOUR).
--
-- See also:
--
-- * 'Data.Store.insertList'
--
-- * 'Data.Store.insertList''
--
-- * 'Data.Store.fromList'
--
-- * 'Data.Store.fromList''
unsafeInsertList :: I.Empty (I.Index irs ts) => I.Store tag krs irs ts v -> [(I.Key krs ts, v)] -> I.Store tag krs irs ts v
unsafeInsertList = Data.Foldable.foldl (\s (k, v) -> snd $ unsafeInsert k v s)
{-# INLINE unsafeInsertList #-}

-- INSTANCES

instance Functor (I.Store tag krs irs ts) where
    fmap = map
    {-# INLINE fmap #-}

instance I.Empty (I.Index irs ts) => Monoid (I.Store tag krs irs ts v) where
    mempty = I.empty
    {-# INLINE mempty #-}

    mappend oldl (I.Store kes _ _) =
      Data.IntMap.foldl (\acc (ik, e) -> runIdentity $! I.genericInsert I.indexInsertID' ik e acc) oldl kes

-- UTILITY

showIndex :: Show (I.Index irs ts) => I.Store tag krs irs ts v -> String
showIndex (I.Store _ i _) = show i
{-# INLINE showIndex #-}

printIndex :: Show (I.Index irs ts) => I.Store tag krs irs ts v -> IO ()
printIndex = putStrLn . showIndex
{-# INLINE printIndex #-}

-- INTERNAL
