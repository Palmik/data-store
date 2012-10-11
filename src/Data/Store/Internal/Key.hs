{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Data.Store.Internal.Key
(
  -- * Type-level natural numbers
  Z(..)
, S(..)

  -- ** Type-level number synonyms
, N0
, N1
, N2
, N3
, N4
, N5
, N6
, N7
, N8
, N9
, N10
, ToInt(toInt)

  -- * Key & co.
, KeyGeneric(..)
, Dim
, DimAuto
, (:.)(..)

  -- ** Key type operations
, Dimensions
, DimensionType

, K0

  -- ** Key construction
, (.:)

  -- ** Key operations
, dimensions

, DimensionInternal(..)
, KeyInternal
) where

--------------------------------------------------------------------------------
import           Data.Proxy
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- | INTERNAL KEY TYPE

data DimensionInternal a dt where
    IDimension     :: [a] -> DimensionInternal a Dim
    IDimensionAuto :: a   -> DimensionInternal a DimAuto

type KeyInternal = KeyGeneric DimensionInternal


--------------------------------------------------------------------------------
-- | TYPE-LEVEL NATURAL NUMBERS

-- | Zero.
data Z   = Z
-- | Successor.
data S n = S n

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7
type N9 = S N8
type N10 = S N9

class ToInt n where
    toInt :: Proxy n -> Int

instance ToInt Z where
    toInt _ = 0

instance forall n . ToInt n => ToInt (S n) where
    toInt _ = 1 + toInt (Proxy :: Proxy n)

--------------------------------------------------------------------------------
-- | DIMENSION

data Dim
data DimAuto

--------------------------------------------------------------------------------
-- | KEY

-- | Key with zero dimensions.
data K0

-- | Generic key type.
data KeyGeneric dim spec  where
    -- | Creates key (n + 1)-dimensional key from n-dimensional one.
    KN :: Ord a1
       => dim a1 dt1
       -> KeyGeneric dim ((a2, dt2) :. s)
       -> KeyGeneric dim ((a1, dt1) :. (a2, dt2) :. s)
    
    -- | Creates 1-dimensional key.
    K1 :: Ord a1
       => dim a1 dt1
       -> KeyGeneric dim ((a1, dt1) :. K0) 

------
-- | KEY TYPE CONSTRUCTION

------
-- | KEY TYPE OPERATONS

-- | This type family lets us determine the number of dimensions of the given key.
type family   Dimensions a :: *
type instance Dimensions ((t, dt) :. K0) = S Z
type instance Dimensions ((t, dt) :. (t2, dt2) :. s) =
    S (Dimensions ((t2, dt2) :. s))

-- | This type family lets us determine the type of the given dimension.
type family   DimensionType a n :: *
type instance DimensionType ((t, dt) :. s) Z = t
type instance DimensionType ((t, dt) :. (t2, dt2) :. s) (S n) =
    DimensionType ((t2, dt2) :. s) n

data h :. t = h :. t

infixr 3 :.

------
-- | KEY CONSTRUCTION

-- | Creates (n + 1)-dimensional key from n-dimensional key.
(.:) :: Ord a1
     => dim a1 dt1
     -> KeyGeneric dim ((a2, dt2) :. s)
     -> KeyGeneric dim ((a1, dt1) :. (a2, dt2) :. s)
(.:) = KN
{-# INLINE (.:) #-}

infixr 3 .:

------
-- | KEY OPERATIONS

-- | Type-class for retrieving the number of dimension of the given key.
--
-- Examples:
--
-- > let p1 = key1 'a'
-- > let p2 = key2 'a' 'a'
-- > let p3 = key3 'a' 'a' 'a'
-- > dimensions p1
-- >>> 1
-- > dimensions p2
-- >>> 2
-- > dimensions p3
-- >>> 3
dimensions :: KeyGeneric dim ((a, d) :. t) -> Int
dimensions (K1   _) = 1
dimensions (KN _ l) = dimensions l + 1

