{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Store.Key
( Auto(..)
, Dimension(..)
, dimA
, dimN
, Key
, CEmptyKey(..)
, module Export
) where

--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Key as I
--------------------------------------------------------------------------------
import           Data.Store.Internal.Key as Export ( Z(..), S(..), K0, Dim, DimAuto
                                                   , N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, N10
                                                   , (:.)(..), (.:), KeyGeneric(..)
                                                   )
--------------------------------------------------------------------------------

dimN :: Ord a => [a] -> Dimension a I.Dim
dimN = Dimension
{-# INLINEABLE dimN #-}

dimA :: (Ord a, Auto a) => Dimension a I.DimAuto
dimA = DimensionAuto
{-# INLINEABLE dimA #-}

--------------------------------------------------------------------------------
-- KEY

type Key spec = I.KeyGeneric Dimension spec

--------------------------------------------------------------------------------
-- DIMENSION

class Auto a where
    initValue :: a
    nextValue :: a -> a

data Dimension a dt where
    Dimension     :: Ord a => [a] -> Dimension a I.Dim
    DimensionAuto :: (Ord a, Auto a) => Dimension a I.DimAuto

class CEmptyKey k where
    emptyKey :: k

instance Ord a => CEmptyKey (Key ((a, I.Dim) I.:. I.K0)) where
    emptyKey = I.K1 $ Dimension []

instance (Ord a, Auto a) => CEmptyKey (Key ((a, I.DimAuto) I.:. I.K0)) where
    emptyKey = I.K1 DimensionAuto

instance (Ord a1, Ord a2, CEmptyKey (Key ((a2, d2) I.:. t))) => CEmptyKey (Key ((a1, I.Dim) I.:. (a2, d2) I.:. t)) where
    emptyKey = Dimension [] I..: emptyKey

instance (Ord a1, Auto a1, CEmptyKey (Key ((a2, d2) I.:. t))) => CEmptyKey (Key ((a1, I.DimAuto) I.:. (a2, d2) I.:. t)) where
    emptyKey = DimensionAuto I..: emptyKey

