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
import           Data.Functor.Identity 
import qualified Data.List
#if MIN_VERSION_containers(0,5,0)
import qualified Data.IntMap.Strict as Data.IntMap
import qualified Data.Map.Strict as Data.Map
#else
import qualified Data.IntMap
import qualified Data.Map
#endif
import qualified Data.IntSet
import qualified Data.IntSet.Extra
--------------------------------------------------------------------------------
import qualified Data.Store.Internal.Type as I
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Store.Internal.Function"

genericSubset :: I.Empty (I.Index irs ts)
              => Data.IntSet.IntSet
              -> I.Store tag krs irs ts v
              -> I.Store tag krs irs ts v
genericSubset ids (I.Store vs _ _) =
  Data.IntSet.foldr (\i acc ->
    case Data.IntMap.lookup i vs of
      Just (ik, e) -> runIdentity $ genericInsert indexInsertID'' ik e acc
      _ -> acc
    ) I.empty ids
{-# INLINE genericSubset #-}

genericLookup :: Data.IntSet.IntSet
              -> I.Store tag krs irs ts v
              -> [(I.RawKey krs ts, v)]
genericLookup ids (I.Store vs _ _) = {-# SCC "genericLookup" #-} 
  Data.IntSet.foldr (\i acc ->
    case Data.IntMap.lookup i vs of
      Just (ik, v) -> (keyInternalToRaw ik, v) : acc
      _ -> acc
    ) [] ids
{-# INLINE genericLookup #-}

genericUpdateWithKey :: (Applicative f, Monad f)
                     => (I.IKey krs ts -> Int -> I.Store tag krs irs ts e -> f (I.Store tag krs irs ts e))
                     -> (I.RawKey krs ts -> e -> Maybe (e, Maybe (I.Key krs ts)))
                     -> Data.IntSet.IntSet
                     -> I.Store tag krs irs ts e
                     -> f (I.Store tag krs irs ts e)
genericUpdateWithKey ins tr ids old = Data.IntSet.Extra.foldlM' accum old ids
    where
      accum store@(I.Store vs ix nid) i =
          case Data.IntMap.lookup i vs of
            Just (ik, v) ->
                case tr (keyInternalToRaw ik) v of
                  -- User wants to update the element & key.
                  Just (nv, Just nk) -> let nik = mergeKeys nk ik in 
                    if nik /= ik
                       -- The keys are different: update the element & key.
                       then insertPair i nik nv <$> ins nik i (store { I.storeI = indexDeleteID ik i ix })
                       -- The keys are identical: update the element.
                       else pure $! insertPair i nik nv store

                  -- Update the element.
                  Just (nv, Nothing) -> pure $! insertPair i ik nv store

                  -- Delete.
                  Nothing -> pure I.Store
                    { I.storeV = Data.IntMap.delete i vs
                    , I.storeI = indexDeleteID ik i ix
                    , I.storeNID = nid
                    }
            _ -> pure store
      {-# INLINEABLE accum #-}
      
      insertPair i' ik' e' s'@(I.Store es' _ _) = s' 
        { I.storeV = Data.IntMap.insert i' (ik', e') es'
        }
      {-# INLINE insertPair #-}
{-# INLINE genericUpdateWithKey #-}


mergeKeys :: I.Key krs ts -> I.IKey krs ts -> I.IKey krs ts
mergeKeys (I.K1 I.KeyDimensionA) ik@(I.K1 _) = ik
mergeKeys (I.K1 (I.KeyDimensionO d))   (I.K1 _)   = I.K1 (I.IKeyDimensionO d)
mergeKeys (I.K1 (I.KeyDimensionM d))   (I.K1 _)   = I.K1 (I.IKeyDimensionM d)
mergeKeys (I.KN I.KeyDimensionA s) (I.KN ik is) = I.KN ik $ mergeKeys s is
mergeKeys (I.KN (I.KeyDimensionO d) s) (I.KN _ is) = I.KN (I.IKeyDimensionO d) $ mergeKeys s is
mergeKeys (I.KN (I.KeyDimensionM d) s) (I.KN _ is) = I.KN (I.IKeyDimensionM d) $ mergeKeys s is
mergeKeys _ _ = error $ moduleName <> ".genericUpdate.mergeKeys: The impossible happened."
{-# INLINEABLE mergeKeys #-}

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

genericInsert :: Applicative f 
              => (I.IKey krs ts -> Int -> I.Store tag krs irs ts e -> f (I.Store tag krs irs ts e))
              -> I.IKey krs ts
              -> e
              -> I.Store tag krs irs ts e
              -> f (I.Store tag krs irs ts e)
genericInsert ins ik e old@(I.Store _ _ nid) = go <$> ins ik nid old
  where
    go s'@(I.Store es' _ _) = s'
      { I.storeV = Data.IntMap.insert nid (ik, e) es'
      , I.storeNID = nid + 1
      }
    {-# INLINE go #-}
{-# INLINE genericInsert #-}

-- | Inserts the given element identifier into the store's index under the given
-- internal key.
--
-- In case of collisions: returns 'Data.Maybe.Nothing'.
indexInsertID :: I.IKey krs ts
              -> Int
              -> I.Store tag krs irs ts e
              -> Maybe (I.Store tag krs irs ts e)
indexInsertID ik eid old@(I.Store _ index _) =
  if Data.List.null $ findCollisions ik index
    then Just $! runIdentity $! indexInsertID'' ik eid old
    else Nothing
{-# INLINE indexInsertID #-}

-- | Inserts the given element identifier into the store's index under the given
-- internal key.
--
-- In case of collisions: deletes them.
indexInsertID' :: I.IKey krs ts
               -> Int
               -> I.Store tag krs irs ts e
               -> Identity (I.Store tag krs irs ts e)
indexInsertID' ik eid old@(I.Store _ index _) = --{-# SCC "indexInsertID'" #-} 
  indexInsertID'' ik eid $! Data.IntSet.foldl' go old collisions
  where
    go s'@(I.Store es' ix' _) i =
      case Data.IntMap.updateLookupWithKey (\_ _ -> Nothing) i es' of
        (Just (ik', _), v'') -> s'
          { I.storeV = v''
          , I.storeI = indexDeleteID ik' i ix'
          } 
        _ -> error $ moduleName <> ".insertInternal'.go: The impossible happened."
    {-# INLINEABLE go #-}

    collisions = Data.IntSet.delete eid $! Data.IntSet.fromList $! findCollisions ik index
    {-# INLINE collisions #-}
{-# INLINE indexInsertID' #-}

-- | UNSAFE. Inserts the given element identifier into the store's index under the given
-- internal key.
-- 
-- In case of collisions: ignores them.
indexInsertID'' :: I.IKey krs ts
                -> Int
                -> I.Store tag krs irs ts e
                -> Identity (I.Store tag krs irs ts e)
indexInsertID'' ik eid old@(I.Store _ index _) = --{-# SCC "indexInsertID''" #-}
  zipped `seq` Identity $! old { I.storeI = zipped }
  where
    zipped = zipD ik index

    zipD :: I.IKey krs ts -> I.Index irs ts -> I.Index irs ts
    zipD (I.KN kd kt) (I.IN ixd it) = I.IN (combine kd ixd) $! zipD kt it
    zipD (I.K1 kd) (I.I1 ixd) = I.I1 $! combine kd ixd
    zipD _ _ = error $ moduleName <> ".indexInsertID''.zipD: The impossible happened."
    {-# INLINE zipD #-}

    combine :: I.IKeyDimension krs ts -> I.IndexDimension irs ts -> I.IndexDimension irs ts
    combine kd ixd =
      case (ixd, kd) of
        (I.IndexDimensionO m, I.IKeyDimensionO k)  ->
          I.IndexDimensionO $! goO k eid m
        
        (I.IndexDimensionO m, I.IKeyDimensionM ks) ->
          I.IndexDimensionO $! Data.List.foldl' (\acc k -> goO k eid acc) m ks 

        (I.IndexDimensionM m, I.IKeyDimensionO k)  ->
          I.IndexDimensionM $! goM k eid $! m

        (I.IndexDimensionM m, I.IKeyDimensionM ks) ->
          I.IndexDimensionM $! Data.List.foldl' (\acc k -> goM k eid acc) m ks
    {-# INLINEABLE combine #-}

    goO :: Ord k => k -> Int -> Data.Map.Map k Int -> Data.Map.Map k Int
    goO = Data.Map.insert 
    {-# INLINE goO #-}

    goM :: Ord k => k -> Int -> Data.Map.Map k Data.IntSet.IntSet -> Data.Map.Map k Data.IntSet.IntSet
    goM k e = Data.Map.insertWith (\_ s -> Data.IntSet.insert e s) k (Data.IntSet.singleton e)
    {-# INLINE goM #-}
{-# INLINE indexInsertID'' #-}

findCollisions :: I.IKey krs ts -> I.Index irs ts -> [Int]
findCollisions ik ix = {-# SCC "findCollisions" #-} zipD ik ix [] 
  where
    zipD :: I.IKey krs ts -> I.Index irs ts -> [Int] -> [Int]
    zipD (I.KN kd kt) (I.IN ixd it) = combine kd ixd . zipD kt it
    zipD (I.K1 kd) (I.I1 ixd) = combine kd ixd
    zipD _ _ = error $ moduleName <> ".findCollisions.zipD: The impossible happened."

    combine :: I.IKeyDimension krs ts -> I.IndexDimension irs ts -> [Int] -> [Int]
    combine kd ixd = 
      case (ixd, kd) of
        (I.IndexDimensionO m, I.IKeyDimensionO k)  -> goO k m
        (I.IndexDimensionO m, I.IKeyDimensionM ks) -> foldr (\k acc -> goO k m . acc) id ks
        _ -> id
    {-# INLINE combine #-}

    goO :: Ord k => k -> Data.Map.Map k Int -> [Int] -> [Int]
    goO k m =
      case Data.Map.lookup k $! m of
        Nothing -> id
        Just  i -> (i:)
    {-# INLINE goO #-}

-- | Deletes EID fron an index.
indexDeleteID :: I.IKey krs ts
              -> Int
              -> I.Index irs ts
              -> I.Index irs ts
indexDeleteID ik eid = zipD ik
  where
    zipD :: I.IKey krs ts -> I.Index irs ts -> I.Index irs ts
    zipD (I.KN kd kt) (I.IN ixd it) = I.IN (combine kd ixd) $! zipD kt it
    zipD (I.K1 kd) (I.I1 ixd) = I.I1 $! combine kd ixd
    zipD _ _ = error $ moduleName <> ".indexDeleteID.zipD: The impossible happened."
    {-# INLINEABLE zipD #-}

    combine :: Ord t => I.IKeyDimension kr t -> I.IndexDimension ir t -> I.IndexDimension ir t
    combine key index =
      case (index, key) of
        (I.IndexDimensionO m, I.IKeyDimensionO k)  -> I.IndexDimensionO $! goO m k
        (I.IndexDimensionO m, I.IKeyDimensionM ks) -> I.IndexDimensionO $! Data.List.foldl' goO m ks
        (I.IndexDimensionM m, I.IKeyDimensionO k)  -> I.IndexDimensionM $! goM m k
        (I.IndexDimensionM m, I.IKeyDimensionM ks) -> I.IndexDimensionM $! Data.List.foldl' goM m ks
    {-# INLINEABLE combine #-}

    goO :: Ord k => Data.Map.Map k Int -> k -> Data.Map.Map k Int
    goO m k = Data.Map.update (\i' -> if i' == eid then Nothing else Just i') k m
    {-# INLINE goO #-}

    goM :: Ord k => Data.Map.Map k Data.IntSet.IntSet -> k -> Data.Map.Map k Data.IntSet.IntSet
    goM m k = Data.Map.update
      (\ids -> let nids = Data.IntSet.delete eid ids in
        if Data.IntSet.null nids
          then Nothing
          else Just nids
      ) k m
    {-# INLINE goM #-}
{-# INLINE indexDeleteID #-}

