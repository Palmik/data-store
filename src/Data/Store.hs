{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Store
( I.Store(..)

  -- * Store Operations
  -- ** Creation
, empty

  -- ** Insertion
, insert

  -- ** Updates
, update
) where

--------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Applicative hiding (empty)
--------------------------------------------------------------------------------
import qualified Data.IntMap        as IM
import qualified Data.IntSet        as IS
import qualified Data.Map           as M
import qualified Data.Vector        as V
import qualified Data.Vector.Extra  as V
--------------------------------------------------------------------------------
import qualified Data.Store.Key                     as I
import qualified Data.Store.Query                   as I
import qualified Data.Store.Query.Selection         as I
import qualified Data.Store.Internal                as I
import qualified Data.Store.Internal.Key            as I
import qualified Data.Store.Internal.Index          as I
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Store"

type family   InsertResult a :: *
type instance InsertResult ((a, I.Dim)     I.:. I.K0) = ()
type instance InsertResult ((a, I.DimAuto) I.:. I.K0) = a I.:. ()
type instance InsertResult ((a, I.Dim)     I.:. (b, dt) I.:. r) = InsertResult ((I.:.) (b, dt) r)
type instance InsertResult ((a, I.DimAuto) I.:. (b, dt) I.:. r) = (I.:.) a (InsertResult ((I.:.) (b, dt) r))

-- | Creates an empty 'Store'.
--
-- TODO: Find a way to remove the 'CEmptyKey' context.
empty :: forall tag spec v . (I.CEmptyKey (I.Key spec))
      => I.Store tag spec v
empty = I.Store
    { I.storeValues = IM.empty
    , I.storeIndex  = emptyStoreIndex ekey
    , I.storeNextID = 0
    }
    where
      ekey :: I.Key spec 
      ekey = I.emptyKey

      emptyStoreIndex :: forall spec1 . I.Key spec1 -> I.StoreIndex
      emptyStoreIndex (I.K1 d)   = V.singleton $ emptyIndex d
      emptyStoreIndex (I.KN d r) = emptyIndex d `V.cons` emptyStoreIndex r 

      emptyIndex :: forall a d . I.Dimension a d -> I.Index
      emptyIndex (I.Dimension _) = I.Index     (M.empty :: M.Map a IS.IntSet)
      emptyIndex I.DimensionAuto = I.IndexAuto (M.empty :: M.Map a IS.IntSet) I.initValue


-- | Inserts the given element into existing 'Store'.
insert :: I.Key spec                  
       -> v                           
       -> I.Store tag spec v
       -> (I.Store tag spec v, InsertResult spec)
insert key value I.Store{..} = (I.Store
    { I.storeValues = IM.insert storeNextID (value, keyInternal) storeValues
    , I.storeIndex  = newStoreIndex 
    , I.storeNextID = succ storeNextID 
    }, toInsertResult keyInternal)
    where
      (newStoreIndex, keyInternal) = insertToIndex 0 key storeIndex

      -- | Recursively inserts the new ID under indices of every dimension
      -- of the key.
      insertToIndex :: Int            -- ^ The position of the dimension of the head of the key in the store index vector.
                    -> I.Key spec     -- ^ The key.
                    -> I.StoreIndex   -- ^ The store index.
                    -> (I.StoreIndex, I.KeyInternal spec)
      -- Standard dimension, 1-dimensional key.
      insertToIndex d (I.K1 kh@(I.Dimension _)) index =
          second I.K1 $ indexUpdate kh d index

      -- Auto-increment dimension, 1-dimensional key.
      insertToIndex d (I.K1 kh@I.DimensionAuto) index =
          second I.K1 $ indexUpdate kh d index

      -- Standard dimension, (n + 1)-dimensional key.
      insertToIndex d (I.KN kh@(I.Dimension _) kt) index =
          let (nindex, res) = indexUpdate kh d index
          in  second (I.KN res) $ insertToIndex (d + 1) kt nindex    

      -- Auto-increment dimension (n + 1)-dimensional key.
      insertToIndex d (I.KN kh@I.DimensionAuto kt) index =
          let (nindex, res) = indexUpdate kh d index
          in  second (I.KN res) $ insertToIndex (d + 1) kt nindex    

      -- | Inserts the new ID under indices of the given dimension.
      indexUpdate :: I.Dimension a d -- ^ The dimension to be inserted.
                  -> Int             -- ^ The position of the dimension in the store index vector.
                  -> I.StoreIndex    -- ^ The store index.
                  -> (I.StoreIndex, I.DimensionInternal a d)
      indexUpdate d = V.updateAt' (I.insertDimension d storeNextID)

      toInsertResult :: I.KeyInternal spec
                     -> InsertResult spec
      toInsertResult (I.K1 (I.IDimensionAuto v))    = v I.:. ()
      toInsertResult (I.K1 (I.IDimension _))        = ()
      toInsertResult (I.KN (I.IDimensionAuto v) kt) = v I.:. toInsertResult kt
      toInsertResult (I.KN (I.IDimension _) kt)     = toInsertResult kt


update :: forall tag k v . I.QuerySelection tag k v
       -> (v -> Maybe (v, Maybe (I.Key k)))
       -> I.Query tag k v (I.Store tag k v)
update querySelection fun = go <$> resolve querySelection <*> I.queryStore
    where
      go :: IS.IntSet -> I.Store tag k v -> I.Store tag k v
      go selection store = IS.foldl' step store selection

      step :: I.Store tag k v -> Int -> I.Store tag k v
      step acc@(I.Store values index _) oid =
          case IM.lookup oid values of
              -- | Object with the given ID does not exist.
              Nothing -> acc
              Just (v, k) ->
                case fun v of
                    -- | We are changing the value of the object.
                    Just (nv, Nothing) -> acc
                      { I.storeValues = IM.insert oid (nv, k) values
                      }
                    -- | We are changing the value and key of the object.
                    Just (nv, Just nk) -> acc
                      { I.storeValues = IM.insert oid (nv, newKey) values
                      , I.storeIndex  = insertByKey newKey oid $ deleteByKey k index
                      }
                      where
                        newKey = makeKey k nk
                    -- | We are deleting the object. 
                    Nothing -> acc
                      { I.storeValues = IM.delete oid values
                      , I.storeIndex  = deleteByKey k index
                      }

      
      makeKey :: I.KeyInternal kx -> I.Key kx -> I.KeyInternal kx
      makeKey (I.K1 (I.IDimension _)) (I.K1 (I.Dimension xs)) = I.K1 (I.IDimension xs)
      makeKey (I.K1 (I.IDimensionAuto k)) (I.K1 I.DimensionAuto) = I.K1 (I.IDimensionAuto k)
      makeKey (I.KN (I.IDimension _) r) (I.KN (I.Dimension xs) nr) = I.KN (I.IDimension xs) (makeKey r nr)
      makeKey (I.KN (I.IDimensionAuto k) r) (I.KN I.DimensionAuto nr) = I.KN (I.IDimensionAuto k) (makeKey r nr)
      makeKey _ _ = error $ moduleName ++ ".update: impossible happened." -- This can not happen.

      deleteByKey :: I.KeyInternal spec -> I.StoreIndex -> I.StoreIndex 
      deleteByKey ikey sindex = go' ikey sindex 0
        where
          go' :: I.KeyInternal spec -> I.StoreIndex -> Int -> I.StoreIndex
          go' (I.K1 (I.IDimension ks))    acc n = V.updateAt (I.delete ks)  n acc
          go' (I.K1 (I.IDimensionAuto k)) acc n = V.updateAt (I.delete [k]) n acc
          go' (I.KN (I.IDimension ks) r)    acc n = V.updateAt (I.delete ks)  n $ go' r acc (n + 1) 
          go' (I.KN (I.IDimensionAuto k) r) acc n = V.updateAt (I.delete [k]) n $ go' r acc (n + 1) 
      
      insertByKey :: I.KeyInternal spec -> I.ObjectID -> I.StoreIndex -> I.StoreIndex 
      insertByKey ikey oid sindex = go' ikey sindex 0
        where
          go' :: I.KeyInternal spec -> I.StoreIndex -> Int -> I.StoreIndex
          go' (I.K1 d)   acc n = V.updateAt (I.insertDimensionInternal d oid) n acc
          go' (I.KN d r) acc n = V.updateAt (I.insertDimensionInternal d oid) n $ go' r acc (n + 1) 

resolve :: I.QuerySelection tag k v -> I.Query tag k v IS.IntSet
resolve querySelection = go <$> querySelection <*> I.queryStore
    where
      go :: I.Selection tag k -> I.Store tag k v -> IS.IntSet
      go (I.SelectGT p x) (I.Store _ index _) = snd $ I.split x (index V.! I.toInt p)
      go (I.SelectLT p x) (I.Store _ index _) = fst $ I.split x (index V.! I.toInt p)
      go (I.SelectUnion s1 s2) st = go s1 st `IS.union` go s2 st
      go (I.SelectIntersection s1 s2) st = go s1 st `IS.intersection` go s2 st

