{-# LANGUAGE CPP #-}

module Data.Map.Extra
( insertUnique
) where

--------------------------------------------------------------------------------
#ifdef MIN_VERSION_containers(0,5,0)
import qualified Data.Map.Strict as Data.Map
#else
import qualified Data.Map
#endif
--------------------------------------------------------------------------------

-- | The expression (@insertUnique k v old@)
-- returns (@Just (Data.Map.insert k v old)@) if the map @old@ did not contain
-- anything under the key @k@, otherwise returns @Nothing@.
insertUnique :: Ord k => k -> a -> Data.Map.Map k a -> Maybe (Data.Map.Map k a)
insertUnique k a m =
    case Data.Map.insertLookupWithKey (\_ _ o' -> o') k a m of
        (Nothing, res) -> Just res
        (Just _,  _) -> Nothing
{-# INLINE insertUnique #-}

