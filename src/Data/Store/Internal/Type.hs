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
type instance DimensionType (Key (t, a)) Z = KeyDimension t a
type instance DimensionType (Key ((t, a) :. s)) Z = KeyDimension t a

type instance DimensionType (IKey ((t, a) :. s)) (S n) = DimensionType (IKey s) n
type instance DimensionType (IKey (t, a)) Z = IKeyDimension t a
type instance DimensionType (IKey ((t, a) :. s)) Z = IKeyDimension t a

type instance DimensionType (Index ((t, a) :. s)) (S n) = DimensionType (Index s) n
type instance DimensionType (Index (t, a)) Z = IndexDimension t a
type instance DimensionType (Index ((t, a) :. s)) Z = IndexDimension t a

type family   RawKeyType t :: *
type instance RawKeyType ((KeyDimensionO, a) :. s) =  a  :. RawKeyType s
type instance RawKeyType ((KeyDimensionM, a) :. s) = [a] :. RawKeyType s
type instance RawKeyType (KeyDimensionO, a) =  a
type instance RawKeyType (KeyDimensionM, a) = [a]

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

type family   SelectionDimensionType s n :: *
type instance SelectionDimensionType ((t, a) :. s) (S n) = SelectionDimensionType s n
type instance SelectionDimensionType ((t, a) :. s)  Z    = a
type instance SelectionDimensionType  (t, a)        Z    = a

data Store s v = Store
    { storeV :: Data.IntMap.IntMap (IKey (KeySpec s), Key (KeySpec s), v)
    , storeI :: Index (IndexSpec s)
    , storeNID :: Int
    }

instance (Show (Key (KeySpec s)), Show v) => Show (Store s v) where
    show (Store vs _ _) = "[" <> go <> "]"
      where
        go = Data.List.intercalate "," $ map (\(_, k, v) -> "((" <> show k <> "), " <> show v <> ")")
                                       $ F.toList vs

data GenericKey dim spec where
    K1 :: dim t1 a1 -> GenericKey dim (t1, a1)
    KN :: dim t1 a1 -> GenericKey dim s -> GenericKey dim ((t1, a1) :. s)

type  Key = GenericKey  KeyDimension
type IKey = GenericKey IKeyDimension

instance Show a1 => Show (Key (t1, a1)) where
    show (K1 d) = show d 

instance (Show a1, Show (Key s)) => Show (Key ((t1, a1) :. s)) where
    show (KN d k) = show d <> ", " <> show k

instance Show a1 => Show (IKey (t1, a1)) where
    show (K1 d) = show d 

instance (Show a1, Show (IKey s)) => Show (IKey ((t1, a1) :. s)) where
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

data IKeyDimension t k where
    IKeyDimensionM :: [k] -> IKeyDimension KeyDimensionM k
    IKeyDimensionO ::  k  -> IKeyDimension KeyDimensionO k

instance Show k => Show (IKeyDimension t k) where
    show (IKeyDimensionM ks) = show ks
    show (IKeyDimensionO k)  = show k

data IndexDimension t k where
    IndexDimensionM :: Data.Map.Map k Data.IntSet.IntSet
                    -> IndexDimension IndexDimensionM k
    
    IndexDimensionO :: Data.Map.Map k Int
                    -> Maybe k
                    -> IndexDimension IndexDimensionO k

instance Show k => Show (IndexDimension t k) where
    show (IndexDimensionM m) = show $ map (\(k, vs) -> (k, Data.IntSet.toList vs)) $ Data.Map.toList m
    show (IndexDimensionO m _) = show $ Data.Map.toList m

class ZipDimensions si sk where
    zipDimensions :: (forall a ti tk . Ord a
                                    => IndexDimension ti a
                                    -> IKeyDimension tk a
                                    -> Maybe (IndexDimension ti a)
                     )
                  -> Index si
                  -> IKey sk
                  -> Maybe (Index si)

instance Ord a => ZipDimensions (ti, a) (tk, a) where
    zipDimensions combine (I1 id) (K1 kd) = I1 <$> combine id kd

instance (Ord a, ZipDimensions si sk) => ZipDimensions ((ti, a) :. si) ((tk, a) :. sk) where
    zipDimensions combine (IN id is) (KN kd ks) = IN <$> combine id kd <*> zipDimensions combine is ks

class EmptyIndex si where
    emptyIndex :: Index si

instance EmptyIndex (IndexDimensionO, a) where
    emptyIndex = I1 (IndexDimensionO Data.Map.empty Nothing)

instance EmptyIndex (IndexDimensionM, a) where
    emptyIndex = I1 (IndexDimensionM Data.Map.empty)

instance EmptyIndex s => EmptyIndex ((IndexDimensionO, a) :. s) where
    emptyIndex = IN (IndexDimensionO Data.Map.empty Nothing) emptyIndex

instance EmptyIndex s => EmptyIndex ((IndexDimensionM, a) :. s) where
    emptyIndex = IN (IndexDimensionM Data.Map.empty) emptyIndex

-- | SELECTION

data Condition = Condition Bool Bool Bool

data SelectionDimension n spec where
    SelectionDimension :: (Ord (SelectionDimensionType (IndexSpec spec) n), MapIndexDimension (IndexSpec spec) n)
                       => n
                       -> Condition
                       -> SelectionDimensionType (IndexSpec spec) n
                       -> SelectionDimension n spec

class MapIndexDimension si n where
    mapIndexDimension :: forall b . 
                       ( forall ti a . Ord (SelectionDimensionType si n)
                                    => IndexDimension ti (SelectionDimensionType si n)
                                    -> b
                       )
                      -> n
                      -> Index si
                      -> b

instance Ord a => MapIndexDimension (t, a) Z where
    mapIndexDimension mapper _ (I1 d) = mapper d

instance Ord a => MapIndexDimension ((t, a) :. s) Z where
    mapIndexDimension mapper _ (IN d _) = mapper d

instance (Ord a, MapIndexDimension s n) => MapIndexDimension ((t, a) :. s) (S n) where
    mapIndexDimension mapper (S n) (IN _ s) = mapIndexDimension mapper n s


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


