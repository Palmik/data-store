{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Data.Store.Internal.Function
where

--------------------------------------------------------------------------------
import           Control.Applicative hiding (empty)
--------------------------------------------------------------------------------
import           Data.Monoid ((<>))
import           Data.Maybe
import qualified Data.List
import qualified Data.Foldable as F 
#if MIN_VERSION_containers(0,5,0)
import qualified Data.IntMap.Strict as Data.IntMap
import qualified Data.Map           as Data.Map
#else
import qualified Data.IntMap
import qualified Data.Map
#endif
import qualified Data.Map.Extra
import qualified Data.IntSet
import qualified Data.IntSet.Extra
--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Type as I
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Store.Internal.Function"

genericSubset :: I.Empty (I.Index irs ts) => Data.IntSet.IntSet -> I.Store tag krs irs ts v -> I.Store tag krs irs ts v
genericSubset ids old@(I.Store vs _ _) =
    Data.IntSet.foldl' (\acc i -> case Data.IntMap.lookup i vs of
                                   Just (ik, e) -> fromJust $! insertInternal ik e acc
                                   _ -> acc
                       ) I.empty ids
{-# INLINE genericSubset #-}

genericLookup :: Data.IntSet.IntSet -> I.Store tag krs irs ts v -> [(I.RawKey krs ts, v)]
genericLookup ids (I.Store vs _ _) =
    Data.IntSet.foldl' (\acc i -> case Data.IntMap.lookup i vs of
                                   Just (ik, v) -> (keyInternalToRaw ik, v) : acc
                                   _ -> acc
                       ) [] ids
{-# INLINE genericLookup #-}

genericUpdateWithKey :: (I.RawKey krs ts -> v -> Maybe (v, Maybe (I.Key krs ts)))
                     -> Data.IntSet.IntSet
                     -> I.Store tag krs irs ts v
                     -> Maybe (I.Store tag krs irs ts v)
genericUpdateWithKey tr ids old = Data.IntSet.Extra.foldlM' accum old ids
    where
      accum store@(I.Store vs ix nid) i =
          case Data.IntMap.lookup i vs of
            Just (ik, v) ->
                case tr (keyInternalToRaw ik) v of
                  -- User wants to update the element & key.
                  Just (nv, Just nk) -> let nik = mkik nk ik in 
                    if nik /= ik
                       -- The keys are different: update the element & key.
                       then (\nix -> I.Store
                         { I.storeV = Data.IntMap.insert i (ik, nv) vs
                         , I.storeI = nix
                         , I.storeNID = nid
                         }) <$> indexInsertID nik i (indexDeleteID ik i ix)
                       -- The keys are identical: update the element.
                       else Just I.Store
                         { I.storeV = Data.IntMap.insert i (ik, nv) vs
                         , I.storeI = ix
                         , I.storeNID = nid
                         }

                  -- Update the element.
                  Just (nv, Nothing) -> Just I.Store
                    { I.storeV = Data.IntMap.insert i (ik, nv) vs
                    , I.storeI = ix
                    , I.storeNID = nid
                    }

                  -- Delete.
                  Nothing -> Just I.Store
                    { I.storeV = Data.IntMap.delete i vs
                    , I.storeI = indexDeleteID ik i ix
                    , I.storeNID = nid
                    }
            _ -> Just store
      {-# INLINEABLE accum #-}
{-# INLINE genericUpdateWithKey #-}

genericUpdateWithKey' :: (I.RawKey krs ts -> v -> Maybe (v, Maybe (I.Key krs ts)))
                      -> Data.IntSet.IntSet
                      -> I.Store tag krs irs ts v
                      -> I.Store tag krs irs ts v
genericUpdateWithKey' tr ids old = Data.IntSet.foldl' accum old ids
    where
      accum store@(I.Store vs ix nid) i =
          case Data.IntMap.lookup i vs of
            Just (ik, v) ->
                case tr (keyInternalToRaw ik) v of
                  -- User wants to update the element & key.
                  Just (nv, Just nk) -> let nik = mkik nk ik in 
                    if nik /= ik
                       -- The keys are different: update the element & key.
                       then insertWithEID' i nik nv (store { I.storeI = indexDeleteID ik i ix })
                       -- The keys are identical: update the element.
                       else I.Store
                         { I.storeV = Data.IntMap.insert i (ik, nv) vs
                         , I.storeI = ix
                         , I.storeNID = nid
                         }

                  -- Update the element.
                  Just (nv, Nothing) -> I.Store
                    { I.storeV = Data.IntMap.insert i (ik, nv) vs
                    , I.storeI = ix
                    , I.storeNID = nid
                    }

                  -- Delete.
                  Nothing -> I.Store
                    { I.storeV = Data.IntMap.delete i vs
                    , I.storeI = indexDeleteID ik i ix
                    , I.storeNID = nid
                    }
            _ -> store
      {-# INLINEABLE accum #-}
{-# INLINE genericUpdateWithKey' #-}

mkik :: I.Key krs ts -> I.IKey krs ts -> I.IKey krs ts
mkik (I.K1 I.KeyDimensionA) ik@(I.K1 _) = ik
mkik (I.K1 (I.KeyDimensionO d))   (I.K1 _)   = I.K1 (I.IKeyDimensionO d)
mkik (I.K1 (I.KeyDimensionM d))   (I.K1 _)   = I.K1 (I.IKeyDimensionM d)
mkik (I.KN I.KeyDimensionA s) (I.KN ik is) = I.KN ik $ mkik s is
mkik (I.KN (I.KeyDimensionO d) s) (I.KN _ is) = I.KN (I.IKeyDimensionO d) $ mkik s is
mkik (I.KN (I.KeyDimensionM d) s) (I.KN _ is) = I.KN (I.IKeyDimensionM d) $ mkik s is
mkik _ _ = error $ moduleName <> ".genericUpdate.mkik: The impossible happened."
{-# INLINEABLE mkik #-}



keyInternalToRaw :: I.IKey krs ts -> I.RawKey krs ts
keyInternalToRaw (I.K1 (I.IKeyDimensionO x)) = x
keyInternalToRaw (I.K1 (I.IKeyDimensionM x)) = x
keyInternalToRaw (I.KN (I.IKeyDimensionO x) s) = x I.:. keyInternalToRaw s
keyInternalToRaw (I.KN (I.IKeyDimensionM x) s) = x I.:. keyInternalToRaw s
{-# INLINE keyInternalToRaw #-}

keyFromInternal :: I.IKey krs ts -> I.Key krs ts
keyFromInternal (I.K1 (I.IKeyDimensionO x)) = I.K1 (I.KeyDimensionO x)
keyFromInternal (I.K1 (I.IKeyDimensionM x)) = I.K1 (I.KeyDimensionM x)
keyFromInternal (I.KN (I.IKeyDimensionO x) s) = I.KN (I.KeyDimensionO x) (keyFromInternal s)
keyFromInternal (I.KN (I.IKeyDimensionM x) s) = I.KN (I.KeyDimensionM x) (keyFromInternal s)

insertInternal :: I.IKey krs ts
               -> v
               -> I.Store tag krs irs ts v
               -> Maybe (I.Store tag krs irs ts v)
insertInternal internal v (I.Store vals index nid) = mk <$> indexInsertID internal nid index
    where
      mk ix = I.Store
        { I.storeV = Data.IntMap.insert nid (internal, v) vals
        , I.storeI = ix
        , I.storeNID = nid + 1
        }
      {-# INLINE mk #-}

insertInternal' :: I.IKey krs ts
                -> e
                -> I.Store tag krs irs ts e
                -> I.Store tag krs irs ts e
insertInternal' internal e old@(I.Store _ _ nid) = 
    insertWithEID' nid internal e (old
        { I.storeNID = nid + 1 
        })
{-# INLINE insertInternal' #-}

-- Inserts the given key-element pair under the given element identifier.
-- Does not increase @storeNID@.
insertWithEID' :: Int
               -> I.IKey krs ts
               -> e
               -> I.Store tag krs irs ts e
               -> I.Store tag krs irs ts e
insertWithEID' eid internal e old@(I.Store vals index _) = old
    { I.storeV = Data.IntMap.insert eid (internal, e) nvals
    , I.storeI = nindex
    }
    where
      (nvals, nindex) = Data.IntSet.foldl' go (vals, ix) collisions
      -- {-# INLINEABLE (nindex, nvals) #-}
      
      (collisions, ix) = indexInsertID' internal eid index
      -- {-# INLINEABLE (collisions, ix) #-}
          
      go (v', i') c =
        case Data.IntMap.updateLookupWithKey (\_ _ -> Nothing) c v' of
          (Just (ik, _), v'') -> (v'', indexDeleteID ik c i')
          _ -> error $ moduleName <> ".insert.go: The impossible happened."
      {-# INLINEABLE go #-}
{-# INLINE insertWithEID' #-}

keyToInternal :: I.Index irs ts -> I.Key krs ts -> I.IKey krs ts
keyToInternal (I.I1 ix) (I.K1 I.KeyDimensionA) = I.K1 (I.IKeyDimensionO $! nextKey ix) 
keyToInternal (I.I1 _) (I.K1 (I.KeyDimensionO x)) = I.K1 (I.IKeyDimensionO x) 
keyToInternal (I.I1 _) (I.K1 (I.KeyDimensionM x)) = I.K1 (I.IKeyDimensionM x) 
keyToInternal (I.IN ix is) (I.KN I.KeyDimensionA s) = I.KN (I.IKeyDimensionO $! nextKey ix) $ keyToInternal is s
keyToInternal (I.IN _ is) (I.KN (I.KeyDimensionO x) s) = I.KN (I.IKeyDimensionO x) $ keyToInternal is s
keyToInternal (I.IN _ is) (I.KN (I.KeyDimensionM x) s) = I.KN (I.IKeyDimensionM x) $ keyToInternal is s 
keyToInternal _ _ = error $ moduleName <> ".insert.keyToInternal: Impossible happened."
{-# INLINE keyToInternal #-}
      
nextKey :: I.Auto t => I.IndexDimension r t -> t
nextKey i =
  case i of
    (I.IndexDimensionM m) -> nextKey' m
    (I.IndexDimensionO m) -> nextKey' m
  where
    nextKey' m = if Data.Map.null m
                   then minBound
                   else succ . fst $! Data.Map.findMax m
    {-# INLINE nextKey' #-}
{-# INLINE nextKey #-}

indexInsertID' :: I.IKey krs ts
               -> Int
               -> I.Index irs ts
               -> (Data.IntSet.IntSet, I.Index irs ts)
indexInsertID' ik i = zipDimensions (zipInsert' i) ik 
{-# INLINE indexInsertID' #-}

indexInsertID :: I.IKey krs ts
              -> Int
              -> I.Index irs ts
              -> Maybe (I.Index irs ts)
indexInsertID ik i = zipDimensions (zipInsert i) ik 
{-# INLINE indexInsertID #-}

zipInsert' :: Ord t => Int -> I.IKeyDimension kr t -> I.IndexDimension ir t -> (Data.IntSet.IntSet, I.IndexDimension ir t)
zipInsert' val key index =
    case (index, key) of
      (I.IndexDimensionO m, I.IKeyDimensionO k)  -> I.IndexDimensionO <$> goO k val m
      (I.IndexDimensionO m, I.IKeyDimensionM ks) -> I.IndexDimensionO <$> Data.List.foldl'
          (\(c, am) k -> let (c', m') = goO k val am in (Data.IntSet.union c c', m')
          ) (Data.IntSet.empty, m) ks
      (I.IndexDimensionM m, I.IKeyDimensionO k)  -> (Data.IntSet.empty, I.IndexDimensionM $ goM k val m)
      (I.IndexDimensionM m, I.IKeyDimensionM ks) -> (Data.IntSet.empty, I.IndexDimensionM $ Data.List.foldl' (\acc k -> goM k val acc) m ks)
    where
      goO :: Ord k => k -> Int -> Data.Map.Map k Int -> (Data.IntSet.IntSet, Data.Map.Map k Int)
      goO k a m =
          case Data.Map.insertLookupWithKey (\_ _ _ -> a) k a m of
              (Nothing, res) -> (Data.IntSet.empty, res) 
              (Just  i, res) -> (Data.IntSet.singleton i, res)
      {-# INLINE goO #-}

      goM :: Ord k => k -> Int -> Data.Map.Map k Data.IntSet.IntSet -> Data.Map.Map k Data.IntSet.IntSet
      goM k v = Data.Map.insertWith (\_ s -> Data.IntSet.insert v s) k (Data.IntSet.singleton v)
      {-# INLINE goM #-}
{-# INLINEABLE zipInsert' #-}

zipInsert :: Ord t => Int -> I.IKeyDimension kr t -> I.IndexDimension ir t -> Maybe (I.IndexDimension ir t)
zipInsert val key index =
    case (index, key) of
      (I.IndexDimensionO m, I.IKeyDimensionO k)  -> I.IndexDimensionO <$> goO k val m
      (I.IndexDimensionO m, I.IKeyDimensionM ks) -> I.IndexDimensionO <$> F.foldlM (\acc k -> goO k val acc) m ks
      (I.IndexDimensionM m, I.IKeyDimensionO k)  -> Just . I.IndexDimensionM $ goM k val m
      (I.IndexDimensionM m, I.IKeyDimensionM ks) -> Just . I.IndexDimensionM $ Data.List.foldl' (\acc k -> goM k val acc) m ks
    where
      goO :: Ord k => k -> Int -> Data.Map.Map k Int -> Maybe (Data.Map.Map k Int)
      goO = Data.Map.Extra.insertUnique
      {-# INLINE goO #-}

      goM :: Ord k => k -> Int -> Data.Map.Map k Data.IntSet.IntSet -> Data.Map.Map k Data.IntSet.IntSet
      goM k v = Data.Map.insertWith (\_ s -> Data.IntSet.insert v s) k (Data.IntSet.singleton v)
      {-# INLINE goM #-}
{-# INLINEABLE zipInsert #-}

indexDeleteID :: I.IKey krs ts
              -> Int
              -> I.Index irs ts
              -> I.Index irs ts
indexDeleteID ik i ix = fromJust $ zipDimensions (zipDelete i) ik ix
{-# INLINE indexDeleteID #-}

zipDelete :: Ord t => Int -> I.IKeyDimension kr t -> I.IndexDimension ir t -> Maybe (I.IndexDimension ir t)
zipDelete val key index = Just $
    case (index, key) of
      (I.IndexDimensionO m, I.IKeyDimensionO k)  -> I.IndexDimensionO $ goO k val m
      (I.IndexDimensionO m, I.IKeyDimensionM ks) -> I.IndexDimensionO $ F.foldl' (\acc k -> goO k val acc) m ks
      (I.IndexDimensionM m, I.IKeyDimensionO k)  -> I.IndexDimensionM $ goM k val m
      (I.IndexDimensionM m, I.IKeyDimensionM ks) -> I.IndexDimensionM $ F.foldl' (\acc k -> goM k val acc) m ks
    where
      goO :: Ord k => k -> Int -> Data.Map.Map k Int -> Data.Map.Map k Int
      goO k v = Data.Map.update (\i -> if i == v then Nothing else Just i) k
      {-# INLINE goO #-}

      goM :: Ord k => k -> Int -> Data.Map.Map k Data.IntSet.IntSet -> Data.Map.Map k Data.IntSet.IntSet
      goM k v = Data.Map.update
          (\ids -> let nids = Data.IntSet.delete v ids in
                    if Data.IntSet.null nids
                       then Nothing
                       else Just nids
          ) k
      {-# INLINE goM #-}
{-# INLINEABLE zipDelete #-}

zipDimensions :: Applicative f => (forall ir kr t . Ord t => I.IKeyDimension kr t -> I.IndexDimension ir t -> f (I.IndexDimension ir t))
              -> I.IKey  krs ts
              -> I.Index irs ts
              -> f (I.Index irs ts)
zipDimensions combine (I.K1 kd) (I.I1 ixd) = I.I1 <$> combine kd ixd
zipDimensions combine (I.KN kd kt) (I.IN ixd it) = I.IN <$> combine kd ixd <*> zipDimensions combine kt it
zipDimensions _ _ _ = error $ moduleName <> ".zipDimensions: The impossible happened."
{-# INLINEABLE zipDimensions #-}

