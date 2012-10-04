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
, DimensionInsertResult
, Key
, KeyInsertResult
, CEmptyKey(..)
, module Export
) where

--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Key as I
--------------------------------------------------------------------------------
import           Data.Store.Internal.Key as Export ( Z(..), S(..), K0, Dim, DimAuto
                                                   , N0, N1, N2, N3, N4, N5, N6, N7, N8, N9, N10
                                                   )
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- | KEY

type Key a dt t = I.KeyGeneric Dimension a dt t

--------------------------------------------------------------------------------
-- | DIMENSION

class Auto a where
    initValue :: a
    nextValue :: a -> a

data Dimension a dt where
    Dimension     :: Ord a => [a] -> Dimension a Dim
    DimensionAuto :: (Ord a, Auto a) => Dimension a DimAuto

type family   DimensionInsertResult a :: *
type instance DimensionInsertResult (Dimension a Dim)     = ()
type instance DimensionInsertResult (Dimension a DimAuto) = a

type family   KeyInsertResult a :: *
type instance KeyInsertResult K0 = ()
type instance KeyInsertResult (I.KeyGeneric dim a Dim     t) = KeyInsertResult t
type instance KeyInsertResult (I.KeyGeneric dim a DimAuto t) = (I.:.) (DimensionInsertResult (Dimension a DimAuto)) (KeyInsertResult t)

class CEmptyKey k where
    emptyKey :: k

instance Ord a => CEmptyKey (Key a Dim K0) where
    emptyKey = I.K1 $ Dimension []

instance (Ord a, Auto a) => CEmptyKey (Key a DimAuto K0) where
    emptyKey = I.K1 DimensionAuto

instance (Ord a1, Ord a2, CEmptyKey (Key a2 d2 t)) => CEmptyKey (Key a1 Dim (Key a2 d2 t)) where
    emptyKey = Dimension [] I..: emptyKey

instance (Ord a1, Auto a1, CEmptyKey (Key a2 d2 t)) => CEmptyKey (Key a1 DimAuto (Key a2 d2 t)) where
    emptyKey = DimensionAuto I..: emptyKey

