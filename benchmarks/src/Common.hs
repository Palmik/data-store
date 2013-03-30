{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} 

module Common
( C01(..)
) where

--------------------------------------------------------------------------------
import           Control.DeepSeq (NFData(..))
--------------------------------------------------------------------------------
import qualified Data.Table as TS
import qualified Data.IntSet
import qualified Data.Set
import qualified Data.HashSet

data C01 = C01 
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
                   ![Int]
    deriving Show

instance NFData C01 where
    rnf (C01 x y z) = rnf x `seq` rnf y `seq` rnf z

instance (TS.Tabular a, NFData a, NFData (TS.Tab a (TS.AnIndex a))) => NFData (TS.Table a) where
    rnf (TS.Table tab) = rnf tab

instance (NFData t, NFData a, NFData (TS.PKT t)) => NFData (TS.AnIndex t TS.Primary a) where
    rnf (TS.PrimaryMap m) = rnf m

instance (NFData t, NFData a, NFData (TS.PKT t)) => NFData (TS.AnIndex t TS.Supplemental a) where
    rnf (TS.SupplementalMap m) = rnf m

instance (NFData t, NFData (TS.PKT t)) => NFData (TS.AnIndex t TS.SupplementalInt Int) where
    rnf (TS.SupplementalIntMap m) = rnf m

instance (NFData t, NFData a, NFData (TS.PKT t)) => NFData (TS.AnIndex t TS.SupplementalHash a) where
    rnf (TS.SupplementalHashMap m) = rnf m

instance (NFData t, NFData a, NFData (TS.PKT t)) => NFData (TS.AnIndex t TS.Candidate a) where
    rnf (TS.CandidateMap m) = rnf m

instance (NFData t, NFData (TS.PKT t)) => NFData (TS.AnIndex t TS.CandidateInt Int) where
    rnf (TS.CandidateIntMap m) = rnf m

instance (NFData t, NFData a, NFData (TS.PKT t)) => NFData (TS.AnIndex t TS.CandidateHash a) where
    rnf (TS.CandidateHashMap m) = rnf m

instance (NFData t, NFData (TS.PKT t)) => NFData (TS.AnIndex t TS.InvertedInt Data.IntSet.IntSet) where
    rnf (TS.InvertedIntMap m) = rnf m

instance (NFData t, NFData a, NFData (TS.PKT t)) => NFData (TS.AnIndex t TS.Inverted (Data.Set.Set a)) where
    rnf (TS.InvertedMap m) = rnf m

instance (NFData t, NFData a, NFData (TS.PKT t)) => NFData (TS.AnIndex t TS.InvertedHash (Data.HashSet.HashSet a)) where
    rnf (TS.InvertedHashMap m) = rnf m

