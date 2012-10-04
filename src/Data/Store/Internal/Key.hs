{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

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
, ToKeyInternal
) where


--------------------------------------------------------------------------------
-- | INTERNAL KEY TYPE

data DimensionInternal a dt where
    IDimension     :: [a] -> DimensionInternal a Dim
    IDimensionAuto :: a   -> DimensionInternal a DimAuto

type KeyInternal = KeyGeneric DimensionInternal

type family   ToKeyInternal a :: *
type instance ToKeyInternal K0 = K0
type instance ToKeyInternal (KeyGeneric dim a dt t) = KeyInternal a dt (ToKeyInternal t)


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


--------------------------------------------------------------------------------
-- | DIMENSION

data Dim
data DimAuto

--------------------------------------------------------------------------------
-- | KEY

-- | Key with zero dimensions.
data K0

-- | Generic key type.
data KeyGeneric dim a1 dt1 t where
    -- | Creates key (n + 1)-dimensional key from n-dimensional one.
    KN :: Ord a1
       => dim a1 dt1
       -> KeyGeneric dim a2 dt2 r
       -> KeyGeneric dim a1 dt1 (KeyGeneric dim a2 dt2 r)
    
    -- | Creates 1-dimensional key.
    K1 :: Ord a1
       => dim a1 dt1
       -> KeyGeneric dim a1 dt1 K0 

------
-- | KEY TYPE CONSTRUCTION

------
-- | KEY TYPE OPERATONS

-- | This type family lets us determine the number of dimensions of the given key.
type family   Dimensions a :: *
type instance Dimensions (KeyGeneric dim a d K0)                       = S Z
type instance Dimensions (KeyGeneric dim a d (KeyGeneric dim a2 d2 x)) =
    S (Dimensions (KeyGeneric dim a2 d2 x))

-- | This type family lets us determine the type of the given dimension.
type family   DimensionType a n :: *
type instance DimensionType (KeyGeneric dim a d x) (S Z)                 = a
type instance DimensionType (KeyGeneric dim a d (KeyGeneric dim a2 d2 x)) (S (S n)) =
    DimensionType (KeyGeneric dim a2 d2 x) (S n)

data h :. t = h :. t

------
-- | KEY CONSTRUCTION

-- | Creates (n + 1)-dimensional key from n-dimensional key.
(.:) :: Ord a1
     => dim a1 dt1
     -> KeyGeneric dim a2 dt2 r
     -> KeyGeneric dim a1 dt1 (KeyGeneric dim a2 dt2 r)
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
dimensions :: KeyGeneric dim a d t -> Int
dimensions (K1   _) = 1
dimensions (KN _ l) = dimensions l + 1

