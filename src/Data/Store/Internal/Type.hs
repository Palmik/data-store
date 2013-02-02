{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Store.Internal.Type
where

--------------------------------------------------------------------------------
import           Control.Applicative ((<$>), (<*>))
--------------------------------------------------------------------------------
import           Data.Monoid ((<>))
import qualified Data.Map    
import qualified Data.Set    
import qualified Data.IntMap
import qualified Data.IntSet
import qualified Data.List
import qualified Data.Foldable as F
--------------------------------------------------------------------------------

data DimensionOneOne
data DimensionOneMany
data DimensionManyOne
data DimensionManyMany

data IndexDimensionM
data IndexDimensionO

data KeyDimensionO
data KeyDimensionM

data Z = Z
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

type family   DimensionType t n :: *
type instance DimensionType (Key ((t, a) :. s)) (S n) = DimensionType (Key s) n
type instance DimensionType (Key (KeyDimensionO, a)) Z = a
type instance DimensionType (Key (KeyDimensionM, a)) Z = [a]
type instance DimensionType (Key ((KeyDimensionO, a) :. s)) Z = a
type instance DimensionType (Key ((KeyDimensionM, a) :. s)) Z = [a]

type instance DimensionType (Index ((t, a) :. s)) (S n) = DimensionType (Index s) n
type instance DimensionType (Index (IndexDimensionO, a)) Z = Data.Map.Map a Int
type instance DimensionType (Index (IndexDimensionM, a)) Z = Data.Map.Map a Data.IntSet.IntSet
type instance DimensionType (Index ((IndexDimensionO, a) :. s)) Z = Data.Map.Map a Int
type instance DimensionType (Index ((IndexDimensionM, a) :. s)) Z = Data.Map.Map a Data.IntSet.IntSet

type family   QueryDimensionType s n :: *
type instance QueryDimensionType ((t, a) :. s) (S n) = QueryDimensionType s n
type instance QueryDimensionType ((t, a) :. s)  Z    = a
type instance QueryDimensionType  (t, a)        Z    = a

type family   RawKeyType t :: *
type instance RawKeyType (Key ((KeyDimensionO, a) :. s)) =  a  :. RawKeyType (Key s)
type instance RawKeyType (Key ((KeyDimensionM, a) :. s)) = [a] :. RawKeyType (Key s)
type instance RawKeyType (Key (KeyDimensionO, a)) =  a
type instance RawKeyType (Key (KeyDimensionM, a)) = [a]

type family   KeyTag t :: *
type instance KeyTag DimensionOneOne   = KeyDimensionO
type instance KeyTag DimensionOneMany  = KeyDimensionO
type instance KeyTag DimensionManyOne  = KeyDimensionM
type instance KeyTag DimensionManyMany = KeyDimensionM

type family   IndexTag t :: *
type instance IndexTag DimensionOneOne   = IndexDimensionO
type instance IndexTag DimensionOneMany  = IndexDimensionM
type instance IndexTag DimensionManyOne  = IndexDimensionO
type instance IndexTag DimensionManyMany = IndexDimensionM

type family   KeySpec s :: *
type instance KeySpec ((t, a) :. s) = (KeyTag t, a) :. KeySpec s
type instance KeySpec  (t, a)       = (KeyTag t, a)

type family   IndexSpec s :: *
type instance IndexSpec ((t, a) :. s) = (IndexTag t, a) :. IndexSpec s
type instance IndexSpec  (t, a)       = (IndexTag t, a)

data Store s v = Store
    { storeV :: Data.IntMap.IntMap v
    , storeK :: Data.IntMap.IntMap (Key (KeySpec s))
    , storeI :: Index (IndexSpec s)
    , storeNID :: Int
    }

instance (Show (Key (KeySpec s)), Show v) => Show (Store s v) where
    show (Store vs ks _ _) = "[" <> go <> "]"
      where
        go = Data.List.intercalate "," $
               zipWith (\k v -> ("((" <> show k <> ") -> " <> show v <> ")"))
                       (F.toList ks)
                       (F.toList vs)

data Key spec where
    K1 :: KeyDimension t1 a1 -> Key (t1, a1)
    KN :: KeyDimension t1 a1 -> Key s -> Key ((t1, a1) :. s)

instance Show a1 => Show (Key (t1, a1)) where
    show (K1 d) = show d 

instance (Show a1, Show (Key s)) => Show (Key ((t1, a1) :. s)) where
    show (KN d k) = show d <> ", " <> show k

data Index spec where
    I1 :: IndexDimension t1 k1 -> Index (t1, k1)
    IN :: IndexDimension t1 k1 -> Index s -> Index ((t1, k1) :. s)

instance Show k1 => Show (Index (t1, k1)) where
    show (I1 d) = show d

instance (Show k1, Show (Index s)) => Show (Index ((t1, k1) :. s)) where
    show (IN d i) = show d <> "\n" <> show i

data KeyDimension t k where
    KeyDimensionM :: [k] -> KeyDimension KeyDimensionM k
    KeyDimensionO ::  k  -> KeyDimension KeyDimensionO k

instance Show k => Show (KeyDimension t k) where
    show (KeyDimensionM ks) = show ks
    show (KeyDimensionO k)  = show k

data IndexDimension t k where
    IndexDimensionM :: Data.Map.Map k Data.IntSet.IntSet
                    -> IndexDimension IndexDimensionM k
    
    IndexDimensionO :: Data.Map.Map k Int
                    -> IndexDimension IndexDimensionO k

instance Show k => Show (IndexDimension t k) where
    show (IndexDimensionM m) = show $ map (\(k, vs) -> (k, Data.IntSet.toList vs)) $ Data.Map.toList m
    show (IndexDimensionO m) = show $ Data.Map.toList m

newtype Condition = Condition (Bool, Bool, Bool)

class GetDimension x n where
    getDimension :: x -> n -> DimensionType x n

instance GetDimension (Key (KeyDimensionO, a)) Z where
    getDimension (K1 (KeyDimensionO x)) _ = x 

instance GetDimension (Key (KeyDimensionM, a)) Z where
    getDimension (K1 (KeyDimensionM x)) _ = x

instance GetDimension (Key ((KeyDimensionO, a) :. s)) Z where
    getDimension (KN (KeyDimensionO x) s) _ = x 

instance GetDimension (Key ((KeyDimensionM, a) :. s)) Z where
    getDimension (KN (KeyDimensionM x) s) _ = x

instance (GetDimension (Key s) n) => GetDimension (Key ((t, a) :. s)) (S n) where
    getDimension (KN _ s) (S n) = getDimension s n

instance GetDimension (Index (IndexDimensionO, a)) Z where
    getDimension (I1 (IndexDimensionO x)) _ = x 

instance GetDimension (Index (IndexDimensionM, a)) Z where
    getDimension (I1 (IndexDimensionM x)) _ = x

instance GetDimension (Index ((IndexDimensionO, a) :. s)) Z where
    getDimension (IN (IndexDimensionO x) s) _ = x 

instance GetDimension (Index ((IndexDimensionM, a) :. s)) Z where
    getDimension (IN (IndexDimensionM x) s) _ = x

instance (GetDimension (Index s) n) => GetDimension (Index ((t, a) :. s)) (S n) where
    getDimension (IN _ s) (S n) = getDimension s n

class ZipDimensions si sk where
    zipDimensions :: forall v . (forall a ti tk . Ord a
                                               => IndexDimension ti a
                                               -> KeyDimension tk a
                                               -> Maybe (IndexDimension ti a)
                                )
                  -> Index si
                  -> Key sk
                  -> Maybe (Index si)

instance Ord a => ZipDimensions (ti, a) (tk, a) where
    zipDimensions combine (I1 id) (K1 kd) = I1 <$> combine id kd

instance (Ord a, ZipDimensions si sk) => ZipDimensions ((ti, a) :. si) ((tk, a) :. sk) where
    zipDimensions combine (IN id is) (KN kd ks) = IN <$> combine id kd <*> zipDimensions combine is ks

class EmptyIndex si where
    emptyIndex :: Index si

instance EmptyIndex (IndexDimensionO, a) where
    emptyIndex = I1 (IndexDimensionO Data.Map.empty)

instance EmptyIndex (IndexDimensionM, a) where
    emptyIndex = I1 (IndexDimensionM Data.Map.empty)

instance EmptyIndex s => EmptyIndex ((IndexDimensionO, a) :. s) where
    emptyIndex = IN (IndexDimensionO Data.Map.empty) emptyIndex

instance EmptyIndex s => EmptyIndex ((IndexDimensionM, a) :. s) where
    emptyIndex = IN (IndexDimensionM Data.Map.empty) emptyIndex

data h :. t = h :. t
infixr 3 :.

-- | TEST

{-


type TestKeyType = Key ((KeyDimensionO, Int) :. (KeyDimensionM, String))
type TestIndexType = Index ((IndexDimensionM, Int) :. (IndexDimensionO, String))

testKey :: TestKeyType
testKey = KN (KeyDimensionO 1) $ K1 (KeyDimensionM ["Test1", "Test2"])

testRawKey :: RawKeyType TestKeyType
testRawKey = 5 :. ["aa", "bb"]

testIndexEmpty :: TestIndexType Double
testIndexEmpty = emptyIndex

-}


