{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Store.Query.Selection
(
) where

--------------------------------------------------------------------------------
import           Control.Applicative
--------------------------------------------------------------------------------
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Vector as V
--------------------------------------------------------------------------------
import qualified Data.Store.Internal                as I
import qualified Data.Store.Internal.Key            as I
import qualified Data.Store.Internal.DimensionIndex as I
import qualified Data.Store.Key                     as I
import qualified Data.Store.Query                   as I
--------------------------------------------------------------------------------

type QuerySelection tag k v = I.Query tag k v (I.Selection tag k) 

update :: forall tag k v . QuerySelection tag k v
       -> (v -> Maybe (v, Maybe k))
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
                      , I.storeIndex  = insertID oid newKey $ removeID oid k index
                      }
                      where
                        newKey = makeKey k nk
                    -- | We are deleting the object. 
                    Nothing -> undefined

      
      makeKey :: I.ToKeyInternal (I.Key a dt r) -> I.Key a dt r -> I.ToKeyInternal (I.Key a dt r)
      -- makeKey (I.K1 (I.IDimension _)) (I.K1 (I.Dimension xs)) = I.K1 (I.IDimension xs)
      makeKey (I.K1 (I.IDimensionAuto k)) (I.K1 I.DimensionAuto) = I.K1 (I.IDimensionAuto k)
      -- makeKey (I.KN (I.IDimension _) r) (I.KN (I.Dimension xs) nr) = I.KN (I.IDimension xs) (makeKey r nr)
      makeKey (I.KN (I.IDimensionAuto k) r) (I.KN I.DimensionAuto nr) = I.KN (I.IDimensionAuto k) (makeKey r nr)

      removeID = undefined
      insertID = undefined

resolve :: QuerySelection tag k v -> I.Query tag k v IS.IntSet
resolve querySelection = go <$> querySelection <*> I.queryStore
    where
      go :: I.Selection tag k -> I.Store tag k v -> IS.IntSet
      go (I.SelectGT p x) (I.Store _ index _) = snd $ I.split x (index V.! I.toInt p)
      go (I.SelectLT p x) (I.Store _ index _) = fst $ I.split x (index V.! I.toInt p)
      go (I.SelectUnion s1 s2) st = go s1 st `IS.union` go s2 st
      go (I.SelectIntersection s1 s2) st = go s1 st `IS.intersection` go s2 st
