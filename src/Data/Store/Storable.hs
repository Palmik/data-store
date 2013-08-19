{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Store.Storable
( Storable(..)

, map
, insert
, insert'
, updateWithKey
, updateWithKey'
, update
, update'
, fromList
, fromList'
) where

--------------------------------------------------------------------------------
import           Data.Functor.Identity 
import qualified Data.List (map)
--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Type as I 
import qualified Data.Store.Internal.Function as I 
import qualified Data.Store as S 
import qualified Data.Store.Selection as S (IsSelection())
#if MIN_VERSION_containers(0,5,0)
import qualified Data.IntMap.Strict as Data.IntMap
#else
import qualified Data.IntMap
#endif
import Prelude hiding (map)
--------------------------------------------------------------------------------

-- | This type-class facilitates the common use case where the key under
-- which given values is to be indexed can be derived from the value.
--
-- Example:
--
-- The @Storable@ type-class instance for our @Content@ data type would look
-- like this:
--
-- > instance Storable Content where
-- >     type StoreKRS Content = O         :. O      :. O      :. M      :. O 
-- >     type StoreIRS Content = O         :. O      :. M      :. M      :. M
-- >     type StoreTS  Content = ContentID :. String :. String :. String :. Double
-- >
-- >     key (Content cn cb cts cr) = 
-- >         S.dimA .: S.dimO cn .: S.dimO cb .: S.dimM cts .:. S.dimO cr
-- 
-- NOTE: Using functions outside of this module to update the store may
-- cause inconsistencies with respect to the user defined
-- @'Data.Store.Storable.Storable'@ instance.
class Storable v where
    type StoreKRS t :: *
    type StoreIRS t :: *
    type StoreTS  t :: *
    
    key :: v -> S.Key (StoreKRS v) (StoreTS v)

-- | See @'Data.Store.insert'@ (the key of the inserted element is obtained using the
-- @'Data.Store.Storable.Storable.key'@ function).
insert :: Storable v
       => v
       -> S.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v
       -> Maybe (S.RawKey (StoreKRS v) (StoreTS v), S.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v)
insert v = S.insert (key v) v
{-# INLINE insert #-}

-- | See @'Data.Store.insert''@ (the key of the inserted element is obtained using the
-- @'Data.Store.Storable.Storable.key'@ function).
insert' :: Storable v
        => v
        -> S.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v
        -> (S.RawKey (StoreKRS v) (StoreTS v), S.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v)
insert' v = S.insert' (key v) v
{-# INLINE insert' #-}

-- | See @'Data.Store.Storable.update''@ (the keys of the updated elements are obtained using the
-- @'Data.Store.Storable.Storable.key'@ function).
map :: Storable v
    => (v -> v)
    -> S.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v
    -> S.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v
map tr s@(I.Store values _ _) = runIdentity $! I.genericUpdateWithKey
  I.indexInsertID'
  (\_ e -> let tre = tr e in Just (tre, Just $! key tre))
  (Data.IntMap.keysSet values)
  s
{-# INLINE map #-}

-- | See @'Data.Store.update'@ (the keys of the updated elements are obtained using the
-- @'Data.Store.Storable.Storable.key'@ function).
update :: (Storable v, S.IsSelection sel)
       => (v -> Maybe v)
       -> sel tag (StoreKRS v) (StoreIRS v) (StoreTS v)
       -> S.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v
       -> Maybe (S.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v)
update tr = S.update (maybe Nothing (\v -> Just (v, Just $! key v)) . tr)
{-# INLINE update #-}

-- | See @'Data.Store.update''@ (the keys of the updated elements are obtained using the
-- @'Data.Store.Storable.Storable.key'@ function).
update' :: (Storable v, S.IsSelection sel)
        => (v -> Maybe v)
        -> sel tag (StoreKRS v) (StoreIRS v) (StoreTS v)
        -> S.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v
        -> S.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v
update' tr = S.update' (maybe Nothing (\v -> Just (v, Just $! key v)) . tr)
{-# INLINE update' #-}

-- | See @'Data.Store.updateWithKey'@ (the keys of the updated elements are obtained using the
-- @'Data.Store.Storable.Storable.key'@ function).
updateWithKey :: (Storable v, S.IsSelection sel)
              => (S.RawKey (StoreKRS v) (StoreTS v) -> v -> Maybe v)
              -> sel tag (StoreKRS v) (StoreIRS v) (StoreTS v)
              -> S.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v
              -> Maybe (S.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v)
updateWithKey tr = S.updateWithKey (\rk vv -> maybe Nothing (\v -> Just (v, Just $! key v)) $ tr rk vv)
{-# INLINE updateWithKey #-}

-- | See @'Data.Store.updateWithKey''@ (the keys of the updated elements are obtained using the
-- @'Data.Store.Storable.Storable.key'@ function).
updateWithKey' :: (Storable v, S.IsSelection sel)
               => (S.RawKey (StoreKRS v) (StoreTS v) -> v -> Maybe v)
               -> sel tag (StoreKRS v) (StoreIRS v) (StoreTS v)
               -> S.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v
               -> S.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v
updateWithKey' tr = S.updateWithKey' (\rk vv -> maybe Nothing (\v -> Just (v, Just $! key v)) $ tr rk vv)
{-# INLINE updateWithKey' #-}

-- | See @'Data.Store.fromList'@ (the keys of the elements are obtained using the
-- @'Data.Store.Storable.Storable.key'@ function).
fromList :: (I.Empty (I.Index (StoreIRS v) (StoreTS v)), Storable v)
         => [v]
         -> Maybe (S.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v)
fromList = S.fromList . Data.List.map (\v -> (key v, v))
{-# INLINE fromList #-}

-- | See @'Data.Store.fromList''@ (the keys of the elements are obtained using the
-- @'Data.Store.Storable.Storable.key'@ function).
fromList' :: (I.Empty (I.Index (StoreIRS v) (StoreTS v)), Storable v)
          => [v]
          -> I.Store tag (StoreKRS v) (StoreIRS v) (StoreTS v) v
fromList' = S.fromList' . Data.List.map (\v -> (key v, v))
{-# INLINE fromList' #-}

