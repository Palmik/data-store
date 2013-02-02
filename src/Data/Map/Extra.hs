module Data.Map.Extra
( insertUnique
) where

import qualified Data.Map

-- | The expression (@insertUnique k v old@)
-- returns (@Just (Data.Map.insert k v old)@) if the map @old@ did not contain
-- anything under the key @k@, otherwise returns @Nothing@.
insertUnique :: Ord k => k -> a -> Data.Map.Map k a -> Maybe (Data.Map.Map k a)
insertUnique k a m =
    case Data.Map.insertLookupWithKey (\k' n' o' -> o') k a m of
        (Nothing, res) -> Just res
        (Just _,  res) -> Nothing
{-# INLINEABLE insertUnique #-}


