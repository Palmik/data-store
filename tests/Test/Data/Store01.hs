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
import           Data.Proxy
--------------------------------------------------------------------------------
import qualified Data.Store as S
import           Data.Store (M, O, (.:), (.:.), (:.)(..), (.<), (.<=), (.>), (.>=), (./=), (.==), (.&&), (.||))
--------------------------------------------------------------------------------

data Value = Value
    { valueOM :: Int
    , valueMO :: [Int]
    , valueMM :: [Int]
    } deriving (Eq, Show)

type ValueID = Int

type VStoreTS  = ValueID :. Int :. Int :. Int
type VStoreKRS = O       :. O   :. M   :. M 
type VStoreIRS = O       :. M   :. O   :. M 
type VStore = S.Store VStoreKRS VStoreIRS VStoreTS Value
type VStoreKey = S.Key VStoreKRS VStoreTS
type VStoreSelection = S.Selection VStoreKRS VStoreIRS VStoreTS

sOO :: S.N0
sOO = S.n0

sOM :: S.N1
sOM = S.n1

sMO :: S.N2
sMO = S.n2

sMM :: S.N3
sMM = S.n3

makeKey :: Int -> Int -> [Int] -> [Int] -> VStoreKey
makeKey oo om mo mm = 
    S.dimO oo .: S.dimO om .: S.dimM mo .:. S.dimM mm

vkey :: Value -> VStoreKey
vkey (Value om mo mm) =  
    S.dimA .: S.dimO om .: S.dimM mo .:. S.dimM mm

vkey' :: Int -> Value -> VStoreKey
vkey' i (Value om mo mm) = 
    S.dimO i .: S.dimO om .: S.dimM mo .:. S.dimM mm

tests :: [Test]
tests =
    [ testProperty "insert1" prop_insert1
    , testProperty "insert2" prop_insert2
    
    , testProperty "lookup1" prop_lookup1
    
    , testProperty "update1" prop_update1
    , testProperty "update2" prop_update2
    , testProperty "update3" prop_update3
    ]

-- | Tests insert (auto-incrementation) #1.
prop_insert1 (Value om mo mm) =
    case S.insert (vkey v) v emptyS of
      Just (i :. _, store) -> (i == minBound) && (S.size store == 1)
      _ -> False

    where
      emptyS :: VStore
      emptyS = S.empty

      v = Value om (L.nub mo) (L.nub mm)

-- | Tests insert (auto-incrementation) #2.
prop_insert2 = ids == map fst inserts
    where
      (i0 :. _, s0) = fromJust $ S.insert (vkey $ mval 0) (mval 0) S.empty

      inserts :: [(Int, VStore)]
      inserts =
        foldl (\acc@((_, s') : _) v -> let (i :. _, s) = fromJust $ S.insert (vkey v) v s'
                                       in  (i, s) : acc
              ) [(i0, s0)] values

      ids :: [Int]
      ids = reverse . take 100 $ iterate succ minBound

      values :: [Value]
      values = map mval [1..99]

      mval :: Int -> Value
      mval i = Value i [i] [i]

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
      
      store :: VStore
      store = foldl (\s v -> snd . fromJust $ S.insert (vkey v) v s) S.empty values

      values :: [Value]
      values = map mval [0..99]

      mval :: Int -> Value
      mval i = Value (i `mod` 2) [i] [0..i]

      -- Every key in this list corresponds to exactly 1 value.
      oos :: [Int]
      oos = take 100 $ iterate succ minBound

      -- Every key in this list corresponds to exactly 50 values.
      oms :: [Int]
      oms = [0, 1]

      -- Every key in this list corresponds to exactly 1 value.
      mos :: [Int]
      mos = [0..99]

      -- Every key 'k' in this list corresponds to exactly '100 - k'
      -- values.
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

      store :: VStore
      store = foldl (\s v -> snd . fromJust $ S.insert (vkey v) v s) S.empty values

      values :: [Value]
      values = map mval [0..99]

      mval :: Int -> Value
      mval i = Value (i `mod` 2) [i] [0..i]
      
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

      store :: VStore
      store = foldl (\s v -> snd . fromJust $ S.insert (vkey v) v s) S.empty values

      values :: [Value]
      values = [v1, v2, v3]

      v1 = Value 1 [1] [1]
      v2 = Value 1 [2] [2, 3]
      v3 = Value 2 [3] [1, 2]
  
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

      store :: VStore
      store = foldl (\s v -> snd . fromJust $ S.insert (vkey v) v s) S.empty values

      values :: [Value]
      values = [v1, v2, v3]

      v1 = Value 1 [1] [1]
      v2 = Value 1 [2] [2, 3]
      v3 = Value 2 [3] [1, 2]

--------------------------------------------------------------------------------
-- | QuickCheck machinery.

instance Arbitrary Value where
    arbitrary = (\(om, mo, mm) -> Value om mo mm) <$> arbitrary

