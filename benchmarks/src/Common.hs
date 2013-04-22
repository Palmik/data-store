{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} 

module Common
( C01(..)
) where

--------------------------------------------------------------------------------
import           Control.DeepSeq (NFData(..))
import           Data.Data
--------------------------------------------------------------------------------

data C01 = C01 
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
                   ![Int]
    deriving (Eq, Ord, Show, Typeable)

instance NFData C01 where
    rnf (C01 x y z) = rnf x `seq` rnf y `seq` rnf z

