{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} 

module Common
( C01(..)
) where

--------------------------------------------------------------------------------
import           Control.DeepSeq (NFData(..))
--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Type as DS
--------------------------------------------------------------------------------

data C01 = C01 
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
                   ![Int]
    deriving Show

instance NFData C01 where
    rnf (C01 x y z) = rnf x `seq` rnf y `seq` rnf z

-- DATA.STORE NFDATA

instance (NFData e, NFData (DS.IKey krs ts), NFData (DS.Index irs ts)) => NFData (DS.Store tag krs irs ts e) where
    rnf (DS.Store ke ix nid) = rnf ke `seq` rnf ix `seq` rnf nid

instance NFData t => NFData (DS.IndexDimension r t) where
    rnf (DS.IndexDimensionO m) = rnf m
    rnf (DS.IndexDimensionM m) = rnf m

instance NFData t => NFData (DS.Index DS.O t) where
    rnf (DS.I1 kd) = rnf kd
     
instance NFData t => NFData (DS.Index DS.M t) where
    rnf (DS.I1 kd) = rnf kd

instance (NFData t, NFData (DS.Index rt tt)) => NFData (DS.Index (r DS.:. rt) (t DS.:. tt)) where
    rnf (DS.IN kd kt) = rnf kd `seq` rnf kt
    rnf (DS.I1 _) = error "Impossible! (Index NFData)"

instance NFData t => NFData (DS.IKeyDimension r t) where
    rnf (DS.IKeyDimensionO x) = rnf x
    rnf (DS.IKeyDimensionM x) = rnf x

instance NFData t => NFData (DS.IKey DS.O t) where
    rnf (DS.K1 kd) = rnf kd
     
instance NFData t => NFData (DS.IKey DS.M t) where
    rnf (DS.K1 kd) = rnf kd

instance (NFData t, NFData (DS.IKey rt tt)) => NFData (DS.IKey (r DS.:. rt) (t DS.:. tt)) where
    rnf (DS.KN kd kt) = rnf kd `seq` rnf kt
    rnf (DS.K1 _) = error "Impossible! (IKey NFData)"

instance (NFData a, NFData b) => NFData (a DS.:. b) where
    rnf (a DS.:. b) = rnf a `seq` rnf b

