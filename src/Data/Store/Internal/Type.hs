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
import           Data.Monoid ((<>))
import qualified Data.Map    
import qualified Data.IntMap
import qualified Data.IntSet
import qualified Data.List
import qualified Data.Foldable as F
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Store.Internal.Type"

data M 
data O

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

n0 :: N0
n0 = Z
n1 :: N1
n1 = S n0
n2 :: N2
n2 = S n1
n3 :: N3
n3 = S n2
n4 :: N4
n4 = S n3
n5 :: N5
n5 = S n4
n6 :: N6
n6 = S n5
n7 :: N7
n7 = S n6
n8 :: N8
n8 = S n7
n9 :: N9
n9 = S n8
n10 :: N10
n10 = S n9

type family   DimensionRelation n rs ts :: *
type instance DimensionRelation Z O ts = O
type instance DimensionRelation Z M ts = M
type instance DimensionRelation Z (r :. rt) (t :. tt) = r
type instance DimensionRelation (S n) (r :. rt) (t :. tt) = DimensionRelation n rt tt

type family   DimensionType n rs ts :: *
type instance DimensionType Z M t = t
type instance DimensionType Z O t = t
type instance DimensionType Z (r :. rt) (t :. tt) = t
type instance DimensionType (S n) (r :. rt) (t :. tt) = DimensionType n rt tt

type family   RawDimensionType n a :: *
type instance RawDimensionType n (Index irs ts) = IndexDimension (DimensionRelation n irs ts) (DimensionType n irs ts)

type family   RawKeyType kspec tspec :: *
type instance RawKeyType (O :. rt) (t :. tt) =  t  :. RawKeyType rt tt
type instance RawKeyType (M :. rt) (t :. tt) = [t] :. RawKeyType rt tt
type instance RawKeyType O t =  t
type instance RawKeyType M t = [t]

class (Ord k, Enum k, Bounded k) => Auto k where
instance (Ord k, Enum k, Bounded k) => Auto k where

data Store krs irs ts v = Store
    { storeV :: Data.IntMap.IntMap (IKey krs ts, Key krs ts, v)
    , storeI :: Index irs ts
    , storeNID :: Int
    }

instance (Show (Key krs ts), Show v) => Show (Store krs irs ts v) where
    show (Store vs _ _) = "[" <> go <> "]"
      where
        go = Data.List.intercalate "," $ map (\(_, k, v) -> "((" <> show k <> "), " <> show v <> ")")
                                       $ F.toList vs

data GenericKey dim rs ts where
    K1 :: dim r t -> GenericKey dim r t
    KN :: dim r t -> GenericKey dim rt tt -> GenericKey dim (r :. rt) (t :. tt)

type  Key = GenericKey  KeyDimension
type IKey = GenericKey IKeyDimension

instance Show t => Show (Key O t) where
    show (K1 d) = show d 

instance Show t => Show (Key M t) where
    show (K1 d) = show d 

instance (Show t, Show (Key rt tt)) => Show (Key (r :. rt) (t :. tt)) where
    show (KN d k) = show d <> ", " <> show k
    show (K1 _) = error $ moduleName <> ".Key.show: The impossible happened."

instance Show t => Show (IKey O t) where
    show (K1 d) = show d 

instance Show t => Show (IKey M t) where
    show (K1 d) = show d 

instance (Show t, Show (IKey rt tt)) => Show (IKey (r :. rt) (t :. tt)) where
    show (KN d k) = show d <> ", " <> show k
    show (K1 _) = error $ moduleName <> ".IKey.show: The impossible happened."

data Index rs ts where
    I1 :: Ord t => IndexDimension r t -> Index r t
    IN :: Ord t => IndexDimension r t -> Index rt tt -> Index (r :. rt) (t :. tt)

instance Show t => Show (Index O t) where
    show (I1 d) = show d

instance Show t => Show (Index M t) where
    show (I1 d) = show d

instance (Show t, Show (Index rt tt)) => Show (Index (r :. rt) (t :. tt)) where
    show (IN d i) = show d <> "\n" <> show i
    show (I1 _) = error $ moduleName <> ".Index.show: The impossible happened."

data KeyDimension r t where
    KeyDimensionM :: Ord t => [t] -> KeyDimension M t
    KeyDimensionO :: Ord t =>  t  -> KeyDimension O t
    KeyDimensionA :: Auto t => KeyDimension O t

instance Show t => Show (KeyDimension r t) where
    show (KeyDimensionM ts) = show ts
    show (KeyDimensionO t)  = show t
    show  KeyDimensionA     = show "Auto"

data IKeyDimension r t where
    IKeyDimensionM :: Ord t => [t] -> IKeyDimension M t
    IKeyDimensionO :: Ord t => t  -> IKeyDimension O t

instance Show t => Show (IKeyDimension r t) where
    show (IKeyDimensionM ts) = show ts
    show (IKeyDimensionO t)  = show t

data IndexDimension r t where
    IndexDimensionM :: Ord t
                    => Data.Map.Map t Data.IntSet.IntSet
                    -> IndexDimension M t
    
    IndexDimensionO :: Ord t
                    => Data.Map.Map t Int
                    -> IndexDimension O t

instance Show t => Show (IndexDimension r t) where
    show (IndexDimensionM m) = show $ map (\(k, vs) -> (k, Data.IntSet.toList vs)) $ Data.Map.toList m
    show (IndexDimensionO m) = show $ Data.Map.toList m

class GetDimension n a where
    getDimension :: n -> a -> RawDimensionType n a

instance GetDimension Z (Index O t) where
    getDimension _ (I1 ixd) = ixd

instance GetDimension Z (Index M t) where
    getDimension _ (I1 ixd) = ixd

instance GetDimension Z (Index (r :. rt) (t :. tt)) where
    getDimension _ (IN ixd _) = ixd
    getDimension _ (I1 _) = error $ moduleName <> ".Index.getDimension: The impossible happened."

instance GetDimension n (Index rt tt) => GetDimension (S n) (Index (r :. rt) (t :. tt)) where
    getDimension (S n) (IN _ ixt) = getDimension n ixt
    getDimension _ (I1 _) = error $ moduleName <> ".Index.getDimension: The impossible happened."

class Empty a where
    empty :: a

instance Ord t => Empty (Index O t) where
    empty = I1 (IndexDimensionO Data.Map.empty)

instance Ord t => Empty (Index M t) where
    empty = I1 (IndexDimensionM Data.Map.empty)

instance (Ord t, Empty (Index rt tt)) => Empty (Index (O :. rt) (t :. tt)) where
    empty = IN (IndexDimensionO Data.Map.empty) empty

instance (Ord t, Empty (Index rt tt)) => Empty (Index (M :. rt) (t :. tt)) where
    empty = IN (IndexDimensionM Data.Map.empty) empty

data h :. t = h :. t
infixr 3 :.

