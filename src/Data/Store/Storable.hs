{-# LANGUAGE TypeFamilies #-}

module Data.Store.Storable
( Storable(..)

, insert
, update
, updateWithRawKey
) where

--------------------------------------------------------------------------------
import qualified Data.Store as S 
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
-- >     key (Content cn cb cts sr) = 
-- >         S.dimA .: S.dimO cn .: S.dimO cb .: S.dimM cts .:. S.dimO cr
class Storable v where
    type StoreKRS t :: *
    type StoreIRS t :: *
    type StoreTS  t :: *
    
    key :: v -> S.Key krs ts

-- | See @'Data.Store.insert'@.
insert :: Storable v
       => v
       -> S.Store (StoreKRS v) (StoreIRS v) (StoreTS v) v
       -> Maybe (S.RawKey (StoreKRS v) (StoreTS v), S.Store (StoreKRS v) (StoreIRS v) (StoreTS v) v)
insert v = S.insert (key v) v
{-# INLINE insert #-}

-- | See @'Data.Store.update'@.
update :: (Storable v, S.IsSelection sel)
       => (v -> Maybe v)
       -> sel (StoreKRS v) (StoreIRS v) (StoreTS v)
       -> S.Store (StoreKRS v) (StoreIRS v) (StoreTS v) v
       -> Maybe (S.Store (StoreKRS v) (StoreIRS v) (StoreTS v) v)
update tr = S.update (maybe Nothing (\v -> Just (v, Just $! key v)) . tr)
{-# INLINE update #-}

-- | See @'Data.Store.updateWithRawKey'@.
updateWithRawKey :: (Storable v, S.IsSelection sel)
                 => (S.RawKey (StoreKRS v) (StoreTS v) -> v -> Maybe v)
                 -> sel (StoreKRS v) (StoreIRS v) (StoreTS v)
                 -> S.Store (StoreKRS v) (StoreIRS v) (StoreTS v) v
                 -> Maybe (S.Store (StoreKRS v) (StoreIRS v) (StoreTS v) v)
updateWithRawKey tr = S.updateWithRawKey (\rk vv -> maybe Nothing (\v -> Just (v, Just $! key v)) $ tr rk vv)
{-# INLINE updateWithRawKey #-}
