{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Store.Lens
( With(..)
) where

--------------------------------------------------------------------------------
import qualified Control.Lens
--------------------------------------------------------------------------------
import           Data.Monoid 
import           Data.Functor.Identity
--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Type     as I
import qualified Data.Store.Internal.Function as I
import qualified Data.Store.Selection         as I (IsSelection(resolve))
import qualified Data.Store                   as I ()
--------------------------------------------------------------------------------

class With sel where
    with :: I.Empty (I.Index irs ts) 
         => sel tag krs irs ts -> Control.Lens.Lens' (I.Store tag krs irs ts v)
                                                     (I.Store tag krs irs ts v)

instance I.IsSelection sel => With sel where
    with sel tr old =
      fmap (mappend (runIdentity $! I.genericUpdateWithKey I.indexInsertID'' (\_ _ -> Nothing) ids old))
           (tr (I.genericSubset ids old))
      where
        ids = I.resolve sel old

