{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module HS.B01
where

--------------------------------------------------------------------------------
import           Control.DeepSeq (NFData(rnf))
--------------------------------------------------------------------------------
import           Common(C01(..))
--------------------------------------------------------------------------------
import qualified Data.HiggsSet               as H
import qualified Data.TrieMap.Representation as TM
--------------------------------------------------------------------------------

data C01Key = DimOO Int
            | DimOM Int
            | DimMM Int
  deriving (Eq, Ord)

instance NFData C01Key where
    rnf (DimOO x) = rnf x
    rnf (DimOM x) = rnf x
    rnf (DimMM x) = rnf x

instance Bounded C01Key where
    minBound = DimOO undefined
    maxBound = DimMM undefined

instance Enum C01Key where
    fromEnum (DimOO _) = 0
    fromEnum (DimOM _) = 1
    fromEnum (DimMM _) = 2

    toEnum 0 = DimOO undefined
    toEnum 1 = DimOM undefined
    toEnum 2 = DimMM undefined

instance TM.Repr C01Key where
    type Rep C01Key = Either (TM.Rep Int) (Either (TM.Rep Int) (Either (TM.Rep Int) ()))
    
    toRep (DimOO x) = Left  $! TM.toRep x
    toRep (DimOM x) = Right $! Left  $! TM.toRep x
    toRep (DimMM x) = Right $! Right $! Left $! TM.toRep x

instance H.Index C01Key where
    property (DimOO _) = H.Bijective
    property _         = H.NothingSpecial

instance H.Indexable C01 where
    type IndexOf C01 = C01Key

    project (DimOO _) (C01 oo _ _) = [DimOO oo]
    project (DimOM _) (C01 _ om _) = [DimOM om]
    project (DimMM _) (C01 _ _ mm) = map DimMM mm

type HS = H.HiggsSet C01 C01Key

empty :: HS
empty = H.empty

insert :: C01 -> HS -> HS
insert = H.insert 

lookupOOEQ :: Int -> HS -> [C01]
lookupOOEQ x = H.queryList (H.equals $! DimOO x)

lookupOOGE :: Int -> HS -> [C01]
lookupOOGE x = H.queryList (H.greaterEq $! DimOO x)

lookupOMEQ :: Int -> HS -> [C01]
lookupOMEQ x = H.queryList (H.equals $! DimOM x)

lookupOMGE :: Int -> HS -> [C01]
lookupOMGE x = H.queryList (H.greaterEq $! DimOM x)

lookupMMEQ :: Int -> HS -> [C01]
lookupMMEQ x = H.queryList (H.equals $! DimMM x)




