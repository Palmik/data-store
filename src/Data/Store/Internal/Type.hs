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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Store.Internal.Type
where

--------------------------------------------------------------------------------
import           Data.Monoid ((<>))
import           Data.Data (Typeable, Typeable2)
import qualified Data.Data
import qualified Data.Map    
import qualified Data.IntMap
import qualified Data.IntSet
import qualified Data.List
import qualified Data.Foldable as F
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Store.Internal.Type"

-- | This is type-level tag for tagging dimensions of key and the index of a store.
-- You can think of @'Data.Store.Internal.Type.M'@ as an abbreviation for
-- "many".
--
-- * When @'Data.Store.Internal.Type.Key'@ dimension is tagged with
-- @'Data.Store.Internal.Type.M'@, it means that a single value can be
-- indexed under multiple \<something\>. Example: @Content@ has
-- many tags.
--
-- * When @'Data.Store.Internal.Type.Index'@ dimension is tagged with
-- @'Data.Store.Internal.Type.M'@, it means that a multiple values can be
-- indexed under a single \<something\>. Example: One rating value can be shared by
-- many @Content@s.
--
-- See also:
--
-- * 'Data.Store.Internal.Type.O'
--
-- * 'Data.Store.Internal.Type.Key'
--
-- * 'Data.Store.Internal.Type.Store'
data M 

-- | This is type-level tag for tagging dimensions of key and the index of a store.
-- You can think of @'Data.Store.Internal.Type.O'@ as an abbreviation for
-- "one".
--
-- * When @'Data.Store.Internal.Type.Key'@ dimension is tagged with
-- @'Data.Store.Internal.Type.O'@, it means that a single value is indexed
-- under exactly one \<something\>. Example: @Content@ has exactly one title.
--
-- * When @'Data.Store.Internal.Type.Index'@ dimension is tagged with
-- @'Data.Store.Internal.Type.O'@, it means that at most one value can be
-- indexed under one \<something\>. Example: One @ContentID@ value corresponds
-- to at most one @Content@.
--
-- See also:
--
-- * 'Data.Store.Internal.Type.M'
--
-- * 'Data.Store.Internal.Type.Key'
--
-- * 'Data.Store.Internal.Type.Store'
data O

-- | Type-level zero.
data Z = Z

-- | Type-level successor of a number.
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

-- | The pupose of the @'Data.Store.Internal.Type.RawKey'@ type family is
-- to derive a type of a \"raw key\" that is easier to pattern match against
-- than @'Data.Store.Internal.Key'@.
--
-- Example:
--
-- > RawKey (O :. O :. O :. M :. O) (ContentID :. String :. String :. String :. Double) ~ (ContentID :. String :. String :. [String] :. Double)
type family   RawKey kspec tspec :: *
type instance RawKey (O :. rt) (t :. tt) =  t  :. RawKey rt tt
type instance RawKey (M :. rt) (t :. tt) = [t] :. RawKey rt tt
type instance RawKey O t =  t
type instance RawKey M t = [t]

class (Ord k, Enum k, Bounded k) => Auto k where
instance (Ord k, Enum k, Bounded k) => Auto k where

-- | The store data type has four type arguments that define what and how
-- things are stored.
--
-- The @krs@ (key relation specification) and @irs@ (index relation
-- specification) define the relations between the dimensions of the key
-- and the values. To that end, we use @'Data.Store.Internal.Type.O'@ and
-- @'Data.Store.Internal.Type.M'@ type-level tags and
-- @'Data.Store.Type.Internal.(:.)'@ data type to create tuple of these
-- tags (to describe all the dimensions).
--
-- The possible relations are as follows:
--
-- * One-one: Every value has exactly one \<something\>. One \<something\>
-- corresponds to at most one value.
--
-- * One-many: Every value has exactly one \<something\>. One \<something\> can
-- correspond to many values.
--
-- * Many-one: Every value can have multiple \<something\>. One \<something\>
-- corresponds to at most one value.
--
-- * Many-many: Every value can have multiple \<something\>. One \<something\>
-- can correspond to many values.
--
-- The @ts@ (type specification) defines the type of the key's dimensions
-- and finally @v@ (value) is the type of the value.
--
-- In our example with @Content@, we have five dimensions: ID, name, body,
-- tags and rating. We would like our store to have these properties:
--
-- * @Content@ has one ID, only one content can have a given ID.
--
-- * @Content@ has one name, only one content can have a given name.
--
-- * @Content@ has one body, many contents can have the same content.
--
-- * @Content@ has many tags, many contents can have the same tag.
--
-- * @Content@ has one rating, many contents can have the same rating.
--
-- So in our case, we define:
--
-- > type ContentStoreKRS = O         :. O      :. O      :. M      :. O
-- > type ContentStoreIRS = O         :. O      :. M      :. M      :. M
-- > type ContentStoreTS  = ContentID :. String :. String :. String :. Double
-- > type ContentStore = Store ContentStoreKRS ContentStoreIRS ContentStoreTS Content
--
-- See also:
--
-- * 'Data.Store.Internal.Type.O'
--
-- * 'Data.Store.Internal.Type.M'
--
-- * 'Data.Store.Internal.Type.(:.)'
--
-- * 'Data.Store.Internal.Type.Key'
--
data Store krs irs ts v = Store
    { storeV :: Data.IntMap.IntMap (IKey krs ts, Key krs ts, v)
    , storeI :: Index irs ts
    , storeNID :: Int
    } deriving (Typeable)

instance (Show (Key krs ts), Show v) => Show (Store krs irs ts v) where
    show (Store vs _ _) = "[" <> go <> "]"
      where
        go = Data.List.intercalate "," $ map (\(_, k, v) -> "((" <> show k <> "), " <> show v <> ")")
                                       $ F.toList vs

data GenericKey dim rs ts where
    K1 :: dim r t -> GenericKey dim r t
    KN :: dim r t -> GenericKey dim rt tt -> GenericKey dim (r :. rt) (t :. tt)

instance Typeable2 (GenericKey dim) where
    typeOf2 (K1 _) = Data.Data.mkTyConApp (Data.Data.mkTyCon3 "data-store" moduleName "K1") []
    typeOf2 (KN _ _) = Data.Data.mkTyConApp (Data.Data.mkTyCon3 "data-store" moduleName "KN") []

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

instance Typeable2 Index where
    typeOf2 (I1 _) = Data.Data.mkTyConApp (Data.Data.mkTyCon3 "data-store" moduleName "I1") []
    typeOf2 (IN _ _) = Data.Data.mkTyConApp (Data.Data.mkTyCon3 "data-store" moduleName "IN") []

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

deriving instance Typeable2 KeyDimension

instance Show t => Show (KeyDimension r t) where
    show (KeyDimensionM ts) = show ts
    show (KeyDimensionO t)  = show t
    show  KeyDimensionA     = show "Auto"

data IKeyDimension r t where
    IKeyDimensionM :: Ord t => [t] -> IKeyDimension M t
    IKeyDimensionO :: Ord t => t  -> IKeyDimension O t

deriving instance Typeable2 IKeyDimension

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

-- | Data type for creating tuples, it is used to:
--
-- * Create type-level tuples of relation tags for relation specification of
-- the key and the index of the store.
--
-- > M :. O :. O :. M
--
-- * Create type-level tuples of types for type specification of the key
-- and index of the store.
--
-- > Int :. Double :. String :. String
--
-- * Create value-level tuples to return raw key (with resolved
-- auto-increment dimensions).
--
-- > [1, 2, 3] :. 3.5 :. "Foo" :. ["Bar1", "Bar2"]
data h :. t = h :. t
infixr 3 :.

