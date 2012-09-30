{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Store.Key
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
, Key(..)
, KeyInsertResult
, Dimension(..)
, DimensionInsertResult
, Dim
, DimAuto
, Auto(..)
, (:.)(..)

  -- ** Key type operations
, Dimensions
, DimensionType

, K0

  -- ** Key construction
{-
, key1
, key2
, key3
, key4
, key5 h :. t = h :. t
, key6
, key7
, key8
, key9
, key10
-}
, (.:)

  -- ** Key operations
, dimensions
, CEmptyKey(..)
) where

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

class Auto a where
    initValue :: a
    nextValue :: a -> a

data Dim
data DimAuto

data Dimension a d where
    Dimension     :: Ord a => [a] -> Dimension a Dim
    DimensionAuto :: (Ord a, Auto a) => Dimension a DimAuto

type family   DimensionInsertResult a :: *
type instance DimensionInsertResult (Dimension a Dim)     = ()
type instance DimensionInsertResult (Dimension a DimAuto) = a

--------------------------------------------------------------------------------
-- | KEY

-- | Key with zero dimensions.
data K0

-- | Generic key type.
data Key a d t where
    KN :: Ord a => Dimension a d -> Key a2 d2 r -> Key a d (Key a2 d2 r) -- ^ Creates key (n + 1)-dimensional key from n-dimensional one.
    K1 :: Ord a => Dimension a d -> Key a d K0                           -- ^ Creates key 1-dimensional key.

------
-- | KEY TYPE CONSTRCTION

------
-- | KEY TYPE OPERATONS

-- | This type family lets us determine the number of dimensions of the given key.
type family   Dimensions a :: *
type instance Dimensions (Key a d K0)            = S Z
type instance Dimensions (Key a d (Key a2 d2 x)) = S (Dimensions (Key a2 d2 x))

-- | This type family lets us determine the type of the given dimension.
type family   DimensionType a n :: *
type instance DimensionType (Key a d x) (S Z)                 = a
type instance DimensionType (Key a d (Key a2 d2 x)) (S (S n)) = DimensionType (Key a2 d2 x) (S n)

type family   KeyInsertResult a :: *
type instance KeyInsertResult K0 = ()
type instance KeyInsertResult (Key a Dim     t) = KeyInsertResult t
type instance KeyInsertResult (Key a DimAuto t) = (:.) (DimensionInsertResult (Dimension a DimAuto)) (KeyInsertResult t)

data h :. t = h :. t

------
-- | KEY CONSTRUCTION

-- | Creates (n + 1)-dimensional key from n-dimensional key.
(.:) :: Ord a1 => Dimension a1 d1 -> Key a2 d2 r -> Key a1 d1 (Key a2 d2 r)
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
dimensions :: Key a d t -> Int
dimensions (K1   _) = 1
dimensions (KN _ l) = dimensions l + 1

class CEmptyKey k where
    emptyKey :: k

instance Ord a => CEmptyKey (Key a Dim K0) where
    emptyKey = K1 $ Dimension []

instance (Ord a, Auto a) => CEmptyKey (Key a DimAuto K0) where
    emptyKey = K1 DimensionAuto

instance (Ord a1, Ord a2, CEmptyKey (Key a2 d2 t)) => CEmptyKey (Key a1 Dim (Key a2 d2 t)) where
    emptyKey = Dimension [] .: emptyKey

instance (Ord a1, Auto a1, CEmptyKey (Key a2 d2 t)) => CEmptyKey (Key a1 DimAuto (Key a2 d2 t)) where
    emptyKey = DimensionAuto .: emptyKey
