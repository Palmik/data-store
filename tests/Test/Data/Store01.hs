{-# LANGUAGE TypeOperators #-}

module Test.Data.Store01
( tests
) where

--------------------------------------------------------------------------------
import Control.Arrow
import Control.Applicative
--------------------------------------------------------------------------------
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
--------------------------------------------------------------------------------
import qualified Data.List as L
import           Data.Maybe
import qualified Data.Foldable
import qualified Data.Set
import qualified Data.IntSet
--------------------------------------------------------------------------------
import qualified Data.Store as S
import           Data.Store (M, O, (.:), (.:.), (:.)(..), (.<), (.<=), (.>), (.>=), (./=), (.==), (.&&), (.||))
--------------------------------------------------------------------------------

data D = D
    { dOM :: Int
    , dMO :: [Int]
    , dMM :: [Int]
    } deriving (Eq, Ord, Show)

type DID = Int

data DStoreTag = DStoreTag

type DSTS  = DID :. Int :. Int :. Int
type DSKRS = O       :. O   :. M   :. M
type DSIRS = O       :. M   :. O   :. M
type DS = S.Store DStoreTag DSKRS DSIRS DSTS D
type DSKey = S.Key DSKRS DSTS
type DSSelection = S.Selection DStoreTag DSKRS DSIRS DSTS

sOO :: (DStoreTag, S.N0)
sOO = (DStoreTag, S.n0)

sOM :: (DStoreTag, S.N1)
sOM = (DStoreTag, S.n1)

sMO :: (DStoreTag, S.N2)
sMO = (DStoreTag, S.n2)

sMM :: (DStoreTag, S.N3)
sMM = (DStoreTag, S.n3)

makeKey :: Int -> Int -> [Int] -> [Int] -> DSKey
makeKey oo om mo mm =
    S.dimO oo .: S.dimO om .: S.dimM mo .:. S.dimM mm

vkey :: D -> DSKey
vkey (D om mo mm) =
    S.dimA .: S.dimO om .: S.dimM mo .:. S.dimM mm


vkey' :: Int -> D -> DSKey
vkey' i (D om mo mm) =
    S.dimO i .: S.dimO om .: S.dimM mo .:. S.dimM mm

tests :: [Test]
tests =
    [ testProperty "insert1" prop_insert1
    , testProperty "insert2" prop_insert2

    , testProperty "insert'1" prop_insert'1

    , testProperty "lookup1" prop_lookup1
    , testProperty "lookupOrderBy1" prop_lookupOrderBy1

    , testProperty "update1" prop_update1
    , testProperty "update2" prop_update2
    , testProperty "update3" prop_update3
    ]

-- | Tests insert (auto-incrementation) #1.
prop_insert1 (D om mo mm) =
    case S.insert (vkey v) v emptyS of
      Just (i :. _, store) -> (i == minBound) && (S.size store == 1)
      _ -> False

    where
      emptyS :: DS
      emptyS = S.empty

      v = D om (L.nub mo) (L.nub mm)

-- | Tests insert (auto-incrementation) #2.
prop_insert2 = ids == map fst inserts
    where
      (i0 :. _, s0) = fromJust $ S.insert (vkey $ mval 0) (mval 0) S.empty

      inserts :: [(Int, DS)]
      inserts =
        foldl (\acc@((_, s') : _) v -> let (i :. _, s) = fromJust $ S.insert (vkey v) v s'
                                       in  (i, s) : acc
              ) [(i0, s0)] ds

      ids :: [Int]
      ids = reverse . take 100 $ iterate succ minBound

      ds :: [D]
      ds = map mval [1..99]

      mval :: Int -> D
      mval i = D i [i] [i]

-- | Tests insert' (deleting collisions)
prop_insert'1 xs =
  (Data.Set.fromList (S.elements store1) == Data.Set.fromList (S.elements store2)) &&
  checkLookup sOO oos &&
  checkLookup sOM oms &&
  checkLookup sMO mos &&
  checkLookup sMM mms
  where
    lookupSet :: DSSelection -> DS -> Data.Set.Set D
    lookupSet sel s = Data.Set.fromList $ map snd $ S.lookup sel s

    checkLookup dim xs =
      all (\x -> lookupSet (dim .== x) store1 == lookupSet (dim .== x) store2) xs

    kes :: [(DSKey, D)]
    kes = zipWith (\i x -> (vkey' i x, x)) [0 ..] xs

    oos :: [Int]
    oos = [ 0 .. 5000 ]

    oms :: [Int]
    oms = Data.IntSet.toList $
      foldr (\(D om _ _) acc -> Data.IntSet.insert om acc) Data.IntSet.empty xs

    mos :: [Int]
    mos = Data.IntSet.toList $
      foldr (\(D _ mo _) acc -> Data.IntSet.union (Data.IntSet.fromList mo) acc) Data.IntSet.empty xs

    mms :: [Int]
    mms = Data.IntSet.toList $
      foldr (\(D _ _ mm) acc -> Data.IntSet.union (Data.IntSet.fromList mm) acc) Data.IntSet.empty xs

    store1 :: DS
    store1 = S.fromList' kes

    store2 :: DS
    store2 = foldl (\acc (k, x) -> snd $ S.insert' k x acc) store1 kes

-- | Tests insert, lookup (EQ, LT, GT, NEQ) #1.
prop_lookup1 = byOO_EQ  && byOM_EQ  && byMO_EQ  && byMM_EQ  &&
               byOO_LT  && byOM_LT  && byMO_LT  && byMM_LT  &&
               byOO_GT  && byOM_GT  && byMO_GT  && byMM_GT  &&
               byOO_NEQ && byOM_NEQ && byMO_NEQ && byMM_NEQ
    where
      byOO_EQ :: Bool
      byOO_EQ = all (\r -> length r == 1) $
        map (\k -> S.lookup (sOO .== k) store) oos

      byOM_EQ :: Bool
      byOM_EQ = all (\r -> length r == 50) $
        map (\k -> S.lookup (sOM .== k) store) oms

      byMO_EQ :: Bool
      byMO_EQ = all (\r -> length r == 1) $
        map (\k -> S.lookup (sMO .== k) store) mos

      byMM_EQ :: Bool
      byMM_EQ = all (\(k, r) -> length r == (100 - k)) $
        map (\k -> (k, S.lookup (sMM .== k) store)) mms

      byOO_LT :: Bool
      byOO_LT = all (\(k, r) -> length r == (k + minBound)) $
        map (\k -> (k, S.lookup (sOO .< k) store)) oos

      byOM_LT :: Bool
      byOM_LT = all (\(k, r) -> length r == (k * 50)) $
        map (\k -> (k, S.lookup (sOM .< k) store)) oms

      byMO_LT :: Bool
      byMO_LT = all (\(k, r) -> length r == k) $
        map (\k -> (k, S.lookup (sMO .< k) store)) mos

      byMM_LT :: Bool
      byMM_LT = all (\(k, r) -> length r == (if k == 0 then 0 else 100)) $
        map (\k -> (k, S.lookup (sMM .< k) store)) mms

      byOO_GT :: Bool
      byOO_GT = all (\(k, r) -> length r == (99 - k + minBound)) $
        map (\k -> (k, S.lookup (sOO .> k) store)) oos

      byOM_GT :: Bool
      byOM_GT = all (\(k, r) -> length r == ((1 - k) * 50)) $
        map (\k -> (k, S.lookup (sOM .> k) store)) oms

      byMO_GT :: Bool
      byMO_GT = all (\(k, r) -> length r == (99 - k)) $
        map (\k -> (k, S.lookup (sMO .> k) store)) mos

      byMM_GT :: Bool
      byMM_GT = all (\(k, r) -> length r == (99 - k)) $
        map (\k -> (k, S.lookup (sMM .> k) store)) mms

      byOO_NEQ :: Bool
      byOO_NEQ = all (\(k, r) -> length r == 99) $
        map (\k -> (k, S.lookup (sOO ./= k) store)) oos

      byOM_NEQ :: Bool
      byOM_NEQ = all (\(k, r) -> length r == 50) $
        map (\k -> (k, S.lookup (sOM ./= k) store)) oms

      byMO_NEQ :: Bool
      byMO_NEQ = all (\(k, r) -> length r == 99) $
        map (\k -> (k, S.lookup (sMO ./= k) store)) mos

      byMM_NEQ :: Bool
      byMM_NEQ = all (\(k, r) -> length r == (if k == 0 then 99 else 100)) $
        map (\k -> (k, S.lookup (sMM ./= k) store)) mms

      store :: DS
      store = foldl (\s v -> snd . fromJust $ S.insert (vkey v) v s) S.empty ds

      ds :: [D]
      ds = map mval [0..99]

      mval :: Int -> D
      mval i = D (i `mod` 2) [i] [0..i]

      -- Every key in this list corresponds to exactly 1 d.
      oos :: [Int]
      oos = take 100 $ iterate succ minBound

      -- Every key in this list corresponds to exactly 50 ds.
      oms :: [Int]
      oms = [0, 1]

      -- Every key in this list corresponds to exactly 1 d.
      mos :: [Int]
      mos = [0..99]

      -- Every key 'k' in this list corresponds to exactly '100 - k'
      -- ds.
      mms :: [Int]
      mms = [0..99]

-- | Tests insert, lookupOrderByA #1.
prop_lookupOrderBy1 = l1 && l2
    where
      l1 :: Bool
      l1 = isSortedOM (S.lookupOrderByA (sMM .>= 20) sOM store) &&
           isSortedOM (S.lookupOrderByA (sMM .<= 50 .&& sOM .== 5) sOM store) &&
           isSortedOM (S.lookupOrderByA (sOO .>= 20) sOM store) &&
           isSortedOM (S.lookupOrderByA (sOM .>= 3) sOM store)

      isSortedOM :: [(S.RawKey DSKRS DSTS, D)] -> Bool
      isSortedOM [] = True
      isSortedOM [_] = True
      isSortedOM ((_ :. x1 :. _, _) : (_ :. x2 :. _, _) : t) = x1 <= x2 && isSortedOM t

      l2 :: Bool
      l2 = isSortedOO (S.lookupOrderByA (sMM .>= 20) sOO store) &&
           isSortedOO (S.lookupOrderByA (sMM .<= 50 .&& sOM .== 1) sOO store) &&
           isSortedOO (S.lookupOrderByA (sOO .>= 20) sOO store) &&
           isSortedOO (S.lookupOrderByA (sOM .>= 3) sOO store)

      isSortedOO :: [(S.RawKey DSKRS DSTS, D)] -> Bool
      isSortedOO [] = True
      isSortedOO [_] = True
      isSortedOO ((x1 :. _, _) : (x2 :. _, _) : t) = x1 <= x2 && isSortedOO t

      store :: DS
      store = foldl (\s v -> snd . fromJust $ S.insert (vkey v) v s) S.empty ds

      ds :: [D]
      ds = map mval [0..99]

      mval :: Int -> D
      mval i = D (i `mod` 20) [i] [0..i]

      -- Every key in this list corresponds to exactly 1 d.
      oos :: [Int]
      oos = take 100 $ iterate succ minBound

      -- Every key in this list corresponds to exactly 50 ds.
      oms :: [Int]
      oms = [0..19]

      -- Every key in this list corresponds to exactly 1 d.
      mos :: [Int]
      mos = [0..99]

      -- Every key 'k' in this list corresponds to exactly '100 - k'
      -- ds.
      mms :: [Int]
      mms = [0..99]

-- | Tests insert, delete #1
prop_update1 = deleteMM
    where
      deleteMM :: Bool
      deleteMM = all (\(k, s, l) -> S.size s == k && length l == 0) $
        map (\k -> let res = S.delete (sMM .== k) store
                   in  (k, res, S.lookup (sMM .== k) res)
            ) mms

      store :: DS
      store = foldl (\s v -> snd . fromJust $ S.insert (vkey v) v s) S.empty ds

      ds :: [D]
      ds = map mval [0..99]

      mval :: Int -> D
      mval i = D (i `mod` 2) [i] [0..i]

      mms :: [Int]
      mms = [0..99]

-- | Tests insert, delete #2
prop_update2 = test1
    where
      test1 :: Bool
      test1 = lookupRes1 == [v2] &&
              lookupRes2 == [v3] &&
              S.size res == 2
        where
          res = S.delete ((sOM .== 1) .&& (sMM .== 1)) store
          lookupRes1 = map snd $ S.lookup (sOM .== 1) res
          lookupRes2 = map snd $ S.lookup (sMM .== 1) res

      store :: DS
      store = foldl (\s v -> snd . fromJust $ S.insert (vkey v) v s) S.empty ds

      ds :: [D]
      ds = [v1, v2, v3]

      v1 = D 1 [1] [1]
      v2 = D 1 [2] [2, 3]
      v3 = D 2 [3] [1, 2]

-- | Tests insert, update (changing key)
prop_update3 = test1
    where
      test1 :: Bool
      test1 = lookupRes1 == [v2] &&
              lookupRes2 == [v3] &&
              lookupRes3 == [v1] &&
              lookupRes4 == [v1] &&
              lookupRes5 == [v1] &&
              S.size res == 3
        where
          res = fromJust $ S.update (\v -> Just (v, Just $ makeKey 0 0 [0] [0])) (sOO .== minBound) store
          lookupRes1 = map snd $ S.lookup (sOM .== 1) res
          lookupRes2 = map snd $ S.lookup (sMM .== 1) res
          lookupRes3 = map snd $ S.lookup (sOM .== 0) res
          lookupRes4 = map snd $ S.lookup (sMO .== 0) res
          lookupRes5 = map snd $ S.lookup (sMM .== 0) res

      store :: DS
      store = foldl (\s v -> snd . fromJust $ S.insert (vkey v) v s) S.empty ds

      ds :: [D]
      ds = [v1, v2, v3]

      v1 = D 1 [1] [1]
      v2 = D 1 [2] [2, 3]
      v3 = D 2 [3] [1, 2]

--------------------------------------------------------------------------------
-- | QuickCheck machinery.

instance Arbitrary D where
    arbitrary = (\(om, mo, mm) -> D om (L.nub mo) (L.nub mm)) <$> arbitrary

