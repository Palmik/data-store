{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} 

module IS.B01
where

import Control.DeepSeq (NFData(rnf))
import Common
import Data.Data
import qualified Data.IxSet as I
import           Data.IxSet ((@=), (@>=))

type IS = I.IxSet C01

newtype D1 = D1 Int deriving (Eq, Ord, Typeable)
newtype D2 = D2 Int deriving (Eq, Ord , Typeable)
newtype D3 = D3 Int deriving (Eq, Ord, Typeable)

instance NFData D1 where
    rnf (D1 x) = rnf x

instance NFData D2 where
    rnf (D2 x) = rnf x

instance NFData D3 where
    rnf (D3 x) = rnf x

instance I.Indexable C01 where
    empty = I.ixSet
      [ I.ixFun $ \(C01 oo _ _) -> [D1 oo]
      , I.ixFun $ \(C01 _ om _) -> [D2 om]
      , I.ixFun $ \(C01 _ _ mm) -> map D3 mm
      ]

empty :: IS
empty = I.empty

size :: IS -> Int
size = I.size

insert :: C01 -> IS -> IS
insert = I.insert

lookupOOEQ :: Int -> IS -> IS
lookupOOEQ x s = s @= D1 x

lookupOOGE :: Int -> IS -> IS
lookupOOGE x s = s @>= D1 x

lookupOMEQ :: Int -> IS -> IS
lookupOMEQ x s = s @= D2 x

lookupOMGE :: Int -> IS -> IS
lookupOMGE x s = s @>= D2 x

lookupMMEQ :: Int -> IS -> IS
lookupMMEQ x s = s @= D3 x

