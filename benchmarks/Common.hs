module Common
( C01(..)
) where

--------------------------------------------------------------------------------
import           Control.DeepSeq (NFData(..))
--------------------------------------------------------------------------------

data C01 = C01 
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
                   ![Int]

instance NFData C01 where
    rnf (C01 x y z) = rnf x `seq` rnf y `seq` rnf z

