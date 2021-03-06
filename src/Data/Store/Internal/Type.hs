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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Data.Store.Internal.Type
where

--------------------------------------------------------------------------------
import           Control.Applicative ((<$>), (<*>))
import           Control.DeepSeq (NFData(rnf))
--------------------------------------------------------------------------------
import           Data.Data (Typeable, Typeable2)
import qualified Data.Data
import qualified Data.List
import           Data.Monoid ((<>))
#if MIN_VERSION_containers(0,5,0)
import qualified Data.IntMap.Strict as Data.IntMap
import qualified Data.Map.Strict    as Data.Map
#else
import qualified Data.IntMap
import qualified Data.Map
#endif
import qualified Data.IntSet
import qualified Data.Foldable as F

import qualified Data.SafeCopy  as Ser
import qualified Data.Serialize as Ser (Serialize, get, put)
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Store.Internal.Type"

-- | This is type-level tag for tagging dimensions of key and the index of a store.
-- You can think of @'Data.Store.Internal.Type.M'@ as an abbreviation for
-- "many".
--
-- * When @'Data.Store.Internal.Type.Key'@ dimension is tagged with
-- @'Data.Store.Internal.Type.M'@, it means that a single element can be
-- indexed under multiple key dimension values. Example: @Content@ (element) has
-- many tags.
--
-- * When @'Data.Store.Internal.Type.Index'@ dimension is tagged with
-- @'Data.Store.Internal.Type.M'@, it means that a multiple elements can be
-- indexed under a single key dimension values. Example: One rating can be shared by
-- many @Content@s (elements).
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
-- @'Data.Store.Internal.Type.O'@, it means that a single element is indexed
-- under exactly one key dimension value. Example: @Content@ (element) has exactly one title.
--
-- * When @'Data.Store.Internal.Type.Index'@ dimension is tagged with
-- @'Data.Store.Internal.Type.O'@, it means that at most one element can be
-- indexed under one key dimension value. Example: One @ContentID@ corresponds
-- to at most one @Content@ (element).
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
type instance RawDimensionType n (IKey krs ts) = IKeyDimension (DimensionRelation n krs ts) (DimensionType n krs ts)

type family   LookupMap n krs irs ts v :: *
type instance LookupMap n krs irs ts v =
  Data.Map.Map (DimensionType n krs ts) (LookupMapElement (DimensionRelation n krs ts) (RawKey krs ts, v))

type family   LookupMapElement r e :: *
type instance LookupMapElement O e = e
type instance LookupMapElement M e = [e]

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

type family   RawKeyDimension n kspec tspec :: *
type instance RawKeyDimension Z (O :. rt) (t :. tt) = t
type instance RawKeyDimension Z (M :. rt) (t :. tt) = [t]
type instance RawKeyDimension Z O t = t
type instance RawKeyDimension Z M t = [t]
type instance RawKeyDimension (S n) (r :. rt) (t :. tt) = RawKeyDimension n rt tt

class (Ord k, Enum k, Bounded k) => Auto k where
instance (Ord k, Enum k, Bounded k) => Auto k where

-- | The store data type has five type arguments that define what and how
-- things are stored.
--
-- The @tag@ is arbitrary type (of kind @*@) that is intented to be unique
-- anong all types of stores. The intended purpose is to prevent accidental
-- mixup when creating a selection (since the selection tag of any
-- dimension is a pair consisting of the @tag@ and type-level natural
-- number that corresponds to the dimension).
-- 
-- The @ts@ describes the types of the dimensions. In (the common) case
-- when our store has multiple dimensions, @'Data.Store.Internal.Type(:.)'@
-- is used to create a type-list out of the types of the dimensions.
--
-- The @v@ is the type of the element stored in the store.
--
-- The @krs@ (key relation specification) and @irs@ (index relation
-- specification) define the relations between the dimensions of the key
-- and the elements. To that end, we use @'Data.Store.Internal.Type.O'@ and
-- @'Data.Store.Internal.Type.M'@ type-level tags and
-- @'Data.Store.Internal.Type.(:.)'@ data type to create type-list of these
-- tags (to describe all the dimensions).
--
-- The possible relations are:
--
-- * One-one: Every intem is indexed under exactly one key dimension value. One key dimension value
-- corresponds to at most one element.
--
-- * One-many: Every element is indexed under exactly one key dimension value. One key dimension value can
-- correspond to many elements.
--
-- * Many-one: Every element can be indexed under multiple (zero or more) key dimension values. One key dimension value
-- corresponds to at most one elements.
--
-- * Many-many: Every element cab be indexed under multiple (zero or more) key dimension value. One key dimension value
-- can correspond to many elements.
--
-- The @ts@ (type specification) defines the type of the key's dimensions
-- and finally @v@ is the type of the elements stored.
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
-- * @Content@ has many tags, many contents can have tte same tag.
--
-- * @Content@ has one rating, many contents can have the same rating.
--
-- So in our case, we define:
--
-- > type ContentStoreKRS = O         :. O      :. O      :. M      :. O
-- > type ContentStoreIRS = O         :. O      :. M      :. M      :. M
-- > type ContentStoreTS  = ContentID :. String :. String :. String :. Double
-- > type ContentStore = 'Data.Store.Store' ContentStoreTag ContentStoreKRS ContentStoreIRS ContentStoreTS Content
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
data Store tag krs irs ts v = Store
    { storeV :: !(Data.IntMap.IntMap (IKey krs ts, v))
    , storeI :: !(Index irs ts)
    , storeNID :: {-# UNPACK #-} !Int
    } deriving (Typeable)

instance (Show h, Show t) => Show (h :. t) where
    show (h :. t) = show h <> " :. " <> show t

instance (Ser.Serialize (IKey krs ts), Ser.Serialize (Index irs ts), Ser.Serialize v) => Ser.Serialize (Store tag krs irs ts v) where
    get = Store <$> Ser.get <*> Ser.get <*> Ser.get
    put (Store vs ix nid) = Ser.put vs >> Ser.put ix >> Ser.put nid

instance (Ser.SafeCopy (IKey krs ts), Ser.SafeCopy (Index irs ts), Ser.SafeCopy v) => Ser.SafeCopy (Store tag krs irs ts v) where
    getCopy = Ser.contain $ Store <$> Ser.safeGet <*> Ser.safeGet <*> Ser.safeGet
    putCopy (Store vs ix nid) = Ser.contain $ Ser.safePut vs >> Ser.safePut ix >> Ser.safePut nid

instance (Show (IKey krs ts), Show v) => Show (Store tag krs irs ts v) where
    show (Store vs _ _) = "[" <> go <> "]"
      where
        go = Data.List.intercalate "," $ map (\(ik, v) -> "((" <> show ik <> "), " <> show v <> ")")
                                       $ F.toList vs

data GenericKey dim rs ts where
    KN :: !(dim r t) -> !(GenericKey dim rt tt) -> GenericKey dim (r :. rt) (t :. tt)
    K1 :: !(dim r t) -> GenericKey dim r t

instance Eq (GenericKey IKeyDimension rs ts) where
    (K1 x) == (K1 y) = x == y
    (KN x xt) == (KN y yt) = x == y && xt == yt
    _ == _ = False    

    (K1 x) /= (K1 y) = x /= y
    (KN x xt) /= (KN y yt) = x /= y || xt /= yt
    _ /= _ = True

{-
instance Eq (dim O t) => Eq (GenericKey dim O t) where
    (K1 x) == (K1 y) = x == y
    (K1 x) /= (K1 y) = x /= y

instance Eq (dim M t) => Eq (GenericKey dim M t) where
    (K1 x) == (K1 y) = x == y
    (K1 x) /= (K1 y) = x /= y

instance (Eq (dim r t), Eq (GenericKey dim rt tt)) => Eq (GenericKey dim (r :. rt) (t :. tt)) where
    (KN x xt) == (KN y yt) = x == y && xt == yt
    _ == _ = False

    (KN x xt) /= (KN y yt) = x /= y || xt /= yt
    _ /= _ = True
-}

instance Ser.Serialize (dim O t) => Ser.Serialize (GenericKey dim O t) where
    get = K1 <$> Ser.get
    put (K1 d) = Ser.put d

instance Ser.Serialize (dim M t) => Ser.Serialize (GenericKey dim M t) where
    get = K1 <$> Ser.get
    put (K1 d) = Ser.put d

instance (Ser.Serialize (GenericKey dim rt tt), Ser.Serialize (dim r t)) => Ser.Serialize (GenericKey dim (r :. rt) (t :. tt)) where
    get = KN <$> Ser.get <*> Ser.get
    put (KN d dt) = Ser.put d >> Ser.put dt
    put (K1 _) = error $ moduleName <> ".GenricKey.put: The impossible happened."

instance Ser.SafeCopy (dim O t) => Ser.SafeCopy (GenericKey dim O t) where
    getCopy = Ser.contain $ K1 <$> Ser.safeGet
    putCopy (K1 d) = Ser.contain $ Ser.safePut d

instance Ser.SafeCopy (dim M t) => Ser.SafeCopy (GenericKey dim M t) where
    getCopy = Ser.contain $ K1 <$> Ser.safeGet
    putCopy (K1 d) = Ser.contain $ Ser.safePut d

instance (Ser.SafeCopy (GenericKey dim rt tt), Ser.SafeCopy (dim r t)) => Ser.SafeCopy (GenericKey dim (r :. rt) (t :. tt)) where
    getCopy = Ser.contain $ KN <$> Ser.safeGet <*> Ser.safeGet
    putCopy (KN d dt) = Ser.contain $ Ser.safePut d >> Ser.safePut dt
    putCopy (K1 _) = error $ moduleName <> ".GenricKey.putCopy: The impossible happened."

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
    IN :: Ord t => !(IndexDimension r t) -> !(Index rt tt) -> Index (r :. rt) (t :. tt)
    I1 :: Ord t => !(IndexDimension r t) -> Index r t

instance (Ord t, Ser.Serialize t) => Ser.Serialize (Index O t) where
    get = I1 <$> Ser.get
    put (I1 ixd) = Ser.put ixd

instance (Ord t, Ser.Serialize t) => Ser.Serialize (Index M t) where
    get = I1 <$> Ser.get
    put (I1 ixd) = Ser.put ixd

instance (Ord t, Ser.Serialize t, Ser.Serialize (Index rt tt)) => Ser.Serialize (Index (O :. rt) (t :. tt)) where
    get = IN <$> Ser.get <*> Ser.get
    put (IN ixd ixt) = Ser.put ixd >> Ser.put ixt
    put (I1 _) = error $ moduleName <> ".Index.put: The impossible happened (#1)."

instance (Ord t, Ser.Serialize t, Ser.Serialize (Index rt tt)) => Ser.Serialize (Index (M :. rt) (t :. tt)) where
    get = IN <$> Ser.get <*> Ser.get
    put (IN ixd ixt) = Ser.put ixd >> Ser.put ixt
    put (I1 _) = error $ moduleName <> ".Index.put: The impossible happened (#2)."

instance (Ord t, Ser.SafeCopy t) => Ser.SafeCopy (Index O t) where
    getCopy = Ser.contain $ I1 <$> Ser.safeGet
    putCopy (I1 ixd) = Ser.contain $ Ser.safePut ixd

instance (Ord t, Ser.SafeCopy t) => Ser.SafeCopy (Index M t) where
    getCopy = Ser.contain $ I1 <$> Ser.safeGet
    putCopy (I1 ixd) = Ser.contain $ Ser.safePut ixd

instance (Ord t, Ser.SafeCopy t, Ser.SafeCopy (Index rt tt)) => Ser.SafeCopy (Index (O :. rt) (t :. tt)) where
    getCopy = Ser.contain $ IN <$> Ser.safeGet <*> Ser.safeGet
    putCopy (IN ixd ixt) = Ser.contain $ Ser.safePut ixd >> Ser.safePut ixt
    putCopy (I1 _) = error $ moduleName <> ".Index.putCopy: The impossible happened (#1)."

instance (Ord t, Ser.SafeCopy t, Ser.SafeCopy (Index rt tt)) => Ser.SafeCopy (Index (M :. rt) (t :. tt)) where
    getCopy = Ser.contain $ IN <$> Ser.safeGet <*> Ser.safeGet
    putCopy (IN ixd ixt) = Ser.contain $ Ser.safePut ixd >> Ser.safePut ixt
    putCopy (I1 _) = error $ moduleName <> ".Index.putCopy: The impossible happened (#2)."

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
    KeyDimensionO :: Ord t =>  t  -> KeyDimension O t
    KeyDimensionM :: Ord t => [t] -> KeyDimension M t
    KeyDimensionA :: Auto t => KeyDimension O t

deriving instance Typeable2 KeyDimension

instance Show t => Show (KeyDimension r t) where
    show (KeyDimensionM ts) = show ts
    show (KeyDimensionO t)  = show t
    show  KeyDimensionA     = show "Auto"

data IKeyDimension r t where
    IKeyDimensionO :: Ord t => t  -> IKeyDimension O t
    IKeyDimensionM :: Ord t => [t] -> IKeyDimension M t

instance Eq (IKeyDimension r t) where
    (IKeyDimensionM x) == (IKeyDimensionM y) = x == y
    (IKeyDimensionO x) == (IKeyDimensionO y) = x == y
    _ == _ = False

    (IKeyDimensionM x) /= (IKeyDimensionM y) = x /= y
    (IKeyDimensionO x) /= (IKeyDimensionO y) = x /= y
    _ /= _ = True    

deriving instance Typeable2 IKeyDimension

instance (Ord t, Ser.Serialize t) => Ser.Serialize (IKeyDimension O t) where
    get = IKeyDimensionO <$> Ser.get
    put (IKeyDimensionO x) = Ser.put x

instance (Ord t, Ser.Serialize t) => Ser.Serialize (IKeyDimension M t) where
    get = IKeyDimensionM <$> Ser.get
    put (IKeyDimensionM x) = Ser.put x

instance (Ord t, Ser.SafeCopy t) => Ser.SafeCopy (IKeyDimension O t) where
    getCopy = Ser.contain $ IKeyDimensionO <$> Ser.safeGet
    putCopy (IKeyDimensionO x)  = Ser.contain $ Ser.safePut x

instance (Ord t, Ser.SafeCopy t) => Ser.SafeCopy (IKeyDimension M t) where
    getCopy = Ser.contain $ IKeyDimensionM <$> Ser.safeGet
    putCopy (IKeyDimensionM x)  = Ser.contain $ Ser.safePut x

instance Show t => Show (IKeyDimension r t) where
    show (IKeyDimensionM ts) = show ts
    show (IKeyDimensionO t)  = show t

data IndexDimension r t where
    IndexDimensionO :: Ord t
                    => !(Data.Map.Map t Int)
                    -> IndexDimension O t
    
    IndexDimensionM :: Ord t
                    => !(Data.Map.Map t Data.IntSet.IntSet)
                    -> IndexDimension M t

instance (Ord t, Ser.Serialize t) => Ser.Serialize (IndexDimension O t) where
    get = IndexDimensionO <$> Ser.get
    put (IndexDimensionO x) = Ser.put x

instance (Ord t, Ser.Serialize t) => Ser.Serialize (IndexDimension M t) where
    get = IndexDimensionM <$> Ser.get
    put (IndexDimensionM x) = Ser.put x

instance (Ord t, Ser.SafeCopy t) => Ser.SafeCopy (IndexDimension O t) where
    getCopy = Ser.contain $ IndexDimensionO <$> Ser.safeGet
    putCopy (IndexDimensionO x)  = Ser.contain $ Ser.safePut x

instance (Ord t, Ser.SafeCopy t) => Ser.SafeCopy (IndexDimension M t) where
    getCopy = Ser.contain $ IndexDimensionM <$> Ser.safeGet
    putCopy (IndexDimensionM x)  = Ser.contain $ Ser.safePut x

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

instance GetDimension Z (IKey O t) where
    getDimension _ (K1 kd) = kd

instance GetDimension Z (IKey M t) where
    getDimension _ (K1 kd) = kd

instance GetDimension Z (IKey (r :. rt) (t :. tt)) where
    getDimension _ (KN kd _) = kd
    getDimension _ (K1 _) = error $ moduleName <> ".IKey.getDimension: The impossible happened."

instance GetDimension n (IKey rt tt) => GetDimension (S n) (IKey (r :. rt) (t :. tt)) where
    getDimension (S n) (KN _ kt) = getDimension n kt
    getDimension _ (K1 _) = error $ moduleName <> ".IKey.getDimension: The impossible happened."

data TT
data FF

type family   EmptyProxyIsSpecial t :: *
type instance EmptyProxyIsSpecial Int = TT

class Empty a where
    empty :: a

class EmptyProxy flag a where
    emptyProxy :: flag -> a

instance Ord t => Empty (Index O t) where
    empty = I1 (IndexDimensionO Data.Map.empty)

instance Ord t => Empty (Index M t) where
    empty = I1 (IndexDimensionM Data.Map.empty)

instance (Ord t, Empty (Index rt tt)) => Empty (Index (O :. rt) (t :. tt)) where
    empty = IN (IndexDimensionO Data.Map.empty) empty

instance (Ord t, Empty (Index rt tt)) => Empty (Index (M :. rt) (t :. tt)) where
    empty = IN (IndexDimensionM Data.Map.empty) empty

instance Empty (Index irs ts) => Empty (Store tag krs irs ts e) where
    empty = Store
        { storeV = Data.IntMap.empty
        , storeI = empty
        , storeNID = 0
        }
    {-# INLINE empty #-}

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


-- NFDATA INSTANCES

instance (NFData e, NFData (IKey krs ts), NFData (Index irs ts)) => NFData (Store tag krs irs ts e) where
    rnf (Store ke ix nid) = rnf ke `seq` rnf ix `seq` rnf nid

instance NFData t => NFData (IndexDimension r t) where
    rnf (IndexDimensionO m) = rnf m
    rnf (IndexDimensionM m) = rnf m

instance NFData t => NFData (Index O t) where
    rnf (I1 kd) = rnf kd
     
instance NFData t => NFData (Index M t) where
    rnf (I1 kd) = rnf kd

instance (NFData t, NFData (Index rt tt)) => NFData (Index (r :. rt) (t :. tt)) where
    rnf (IN kd kt) = rnf kd `seq` rnf kt
    rnf (I1 _) = error "Impossible! (Index NFData)"

instance NFData t => NFData (IKeyDimension r t) where
    rnf (IKeyDimensionO x) = rnf x
    rnf (IKeyDimensionM x) = rnf x

instance NFData t => NFData (IKey O t) where
    rnf (K1 kd) = rnf kd
     
instance NFData t => NFData (IKey M t) where
    rnf (K1 kd) = rnf kd

instance (NFData t, NFData (IKey rt tt)) => NFData (IKey (r :. rt) (t :. tt)) where
    rnf (KN kd kt) = rnf kd `seq` rnf kt
    rnf (K1 _) = error "Impossible! (IKey NFData)"

instance (NFData a, NFData b) => NFData (a :. b) where
    rnf (a :. b) = rnf a `seq` rnf b


