{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators  #-}

module Test.Data.Store01
( tests
) where

--------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Applicative
--------------------------------------------------------------------------------
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
--------------------------------------------------------------------------------
import qualified Data.List as L
import           Data.Proxy
--------------------------------------------------------------------------------
import qualified Data.Store       as I
import qualified Data.Store.Key   as I
import           Data.Store.Key             ((:.)(..), (.:))
import           Data.Store.Query.Selection 
--------------------------------------------------------------------------------

newtype PersonID = PersonID Int deriving (Eq, Ord, Show)

instance I.Auto PersonID where
    initValue = PersonID 1
    nextValue (PersonID n) = PersonID $ n + 1

data Person = Person
    { personName :: String
    , personAge :: Int
    , personCredits :: Int
    } deriving (Eq, Show)

data PersonStoreTag 

type PersonKeySpec = ((PersonID, I.DimAuto) :. (String, I.Dim) :. (Int, I.Dim) :. (Int, I.Dim) :. I.K0) 
type PersonKey     = I.Key PersonKeySpec

type PersonStore = I.Store PersonStoreTag PersonKeySpec Person

personKey :: Person -> PersonKey
personKey (Person pn pa pc) =
    I.DimensionAuto .: I.Dimension [pn] .: I.Dimension [pa] .: I.K1 (I.Dimension [pc])

sPersonID :: Proxy (PersonStoreTag, I.Z)
sPersonID = Proxy

sPersonName :: Proxy (PersonStoreTag, I.S I.Z)
sPersonName = Proxy

sPersonAge :: Proxy (PersonStoreTag, I.S (I.S I.Z))
sPersonAge = Proxy

sPersonCredits :: Proxy (PersonStoreTag, I.S (I.S (I.S I.Z)))
sPersonCredits = Proxy

tests :: [Test]
tests =
    [ testProperty "insert1" prop_insert1
    , testProperty "insert2" prop_insert2

    , testProperty "lookup1" prop_lookup1
    , testProperty "lookup2" prop_lookup2
    , testProperty "lookup3" prop_lookup3
    , testProperty "lookup4" prop_lookup4
    
    , testProperty "update1" prop_update1
    , testProperty "update2" prop_update2
    , testProperty "update3" prop_update3
    , testProperty "update4" prop_update4
    ]

-- | Tests insert (auto-incrementation) #1.
prop_insert1 person = id1 == I.initValue && id2 == I.nextValue I.initValue
    where
      s0 = I.empty :: PersonStore 
      (s1, id1 :. ()) = I.insert (personKey person) person s0
      (_,  id2 :. ()) = I.insert (personKey person) person s1

-- | Tests insert (auto-incrementation) #2.
prop_insert2 person = take 100 (map snd inserts) == take 100 values
    where
      s0 = I.empty :: PersonStore 
      (s1, id1 :. ())  = I.insert (personKey person) person s0 
      inserts = iterate (\(s, _) -> let (sn, idn :. ()) = I.insert (personKey person) person s
                                    in  (sn, idn)
                        ) (s1, id1)
      values = iterate I.nextValue I.initValue 

-- | Tests insert, lookup (EQ) #1.
prop_lookup1 persons = lookupByName && lookupByAge && lookupByID 
    where
      lookupByName :: Bool
      lookupByName = all (not . L.null) $
          map (\k -> I.lookup (sPersonName .== k) store) names

      lookupByAge :: Bool
      lookupByAge = all (not . L.null) $
          map (\k -> I.lookup (sPersonAge .== k) store) ages

      lookupByID :: Bool
      lookupByID = all (\xs -> length xs == 1) $
          map (\k -> I.lookup (sPersonID .== k) store) pids

      store :: PersonStore
      store = insertList (map (id &&& personKey) persons) I.empty

      names = map personName persons
      ages  = map personAge  persons
      pids  = take number $ iterate I.nextValue I.initValue
      number = length persons

-- | Tests insert, lookup (EQ) #2.
prop_lookup2 = lookupByName && lookupByAge 
    where
      lookupByName :: Bool
      lookupByName = all (\(res, n) -> length res == n) $
          zip (map (\k -> I.lookup (sPersonName .== k) store) names) [1..100] 

      lookupByAge :: Bool
      lookupByAge = all (\(res, n) -> length res == n) $
          zip (map (\k -> I.lookup (sPersonAge .== k) store) ages) [1..100] 

      store :: PersonStore
      store = L.foldl' (\acc (p, n) -> insertList (replicate n (p, personKey p)) acc
                       ) I.empty persons
                    
      names = map (personName . fst) persons
      ages  = map (personAge  . fst) persons

      -- | We will have 100 distinct persons in both key dimensions.
      persons = zip (map (\x -> Person (show x) x x) [1..100]) [1..100]

-- | Tests insert, lookup (LT, LTE, GT, GTE) #3.
prop_lookup3 = lookupByAgeLT && lookupByAgeLTE && lookupByAgeGT && lookupByAgeGTE
    where
      lookupByAgeLTE :: Bool
      lookupByAgeLTE = all (\(res, n) -> length res == n) $ 
          zip (map (\k -> I.lookup (sPersonAge .<= k) store) ages) [1..100]

      lookupByAgeLT :: Bool
      lookupByAgeLT = all (\(res, n) -> length res == n) $
          zip (map (\k -> I.lookup (sPersonAge .< k) store) ages) [0..99]

      lookupByAgeGTE :: Bool
      lookupByAgeGTE = all (\(res, n) -> length res == n) $ 
          zip (map (\k -> I.lookup (sPersonAge .>= k) store) ages) [100, 99..1]

      lookupByAgeGT :: Bool
      lookupByAgeGT = all (\(res, n) -> length res == n) $
          zip (map (\k -> I.lookup (sPersonAge .> k) store) ages) [99, 98..0]

      store :: PersonStore
      store = insertList (map (\p -> (p, personKey p)) persons) I.empty

      ages  = map personAge  persons

      persons = map (\x -> Person (show x) x x) [1..100] 

-- | Test insert, lookup (LT, AND, OR) #4.
prop_lookup4 = lookupBy_LT_OR_LT_1 && lookupBy_LT_AND_LT_1 &&
               lookupBy_LT_OR_LT_2 && lookupBy_LT_AND_LT_2
    where
      -- age < n || credits < (101 - n)
      -- size should be always 99
      lookupBy_LT_OR_LT_1 :: Bool
      lookupBy_LT_OR_LT_1 = all ((== 99) . length) $
          map (\(a, c) -> I.lookup (sPersonAge .< a .|| sPersonCredits .< c) store) lookup1

      -- age < n && credits < (101 - n)
      -- size should be always 0
      lookupBy_LT_AND_LT_1 :: Bool
      lookupBy_LT_AND_LT_1 = all L.null $ 
          map (\(a, c) -> I.lookup (sPersonAge .< a .&& sPersonCredits .< c) store) lookup1

      -- age < n || credits < n
      -- size should be always min(2n - 2, 100)
      lookupBy_LT_OR_LT_2 :: Bool
      lookupBy_LT_OR_LT_2 = all (\(res, n) -> length res == n) $ flip zip (map (min 100) [0,2..198])$
          map (\(a, c) -> I.lookup (sPersonAge .< a .|| sPersonCredits .< c) store) lookup2

      -- age < n && credits < n
      -- size should be always max(2n - 102, 0)
      lookupBy_LT_AND_LT_2 :: Bool
      lookupBy_LT_AND_LT_2 = all (\(res, n) -> length res == n) $ flip zip (map (\n -> max 0 (2*n - 102)) [1..100]) $ 
          map (\(a, c) -> I.lookup (sPersonAge .< a .&& sPersonCredits .< c) store) lookup2

      store :: PersonStore
      store = insertList (map (id &&& personKey) persons) I.empty

      lookup1 :: [(Int, Int)]
      lookup1 = zip [1..100] [100,99..1]
      lookup2 :: [(Int, Int)]
      lookup2 = zip [1..100] [1..100]

      persons = map (\x -> Person (show x) x (101 - x)) [1..100] 

-- | Tests insert, update (deleting) #1.
prop_update1 p = deleteEverything && deleteNothing &&
                 deleteLT && deleteGT
    where
      deleteEverything :: Bool
      deleteEverything = I.size (I.update (const Nothing) everything store) == 0

      deleteNothing :: Bool
      deleteNothing = I.size (I.update (const Nothing) nothing store) == 100

      deleteLT :: Bool
      deleteLT =
          I.size (I.update (const Nothing) (sPersonAge .< param) store) ==
                 (max 0 $ min 100 $ 100 - param + 1)

      deleteGT :: Bool
      deleteGT =
          I.size (I.update (const Nothing) (sPersonAge .> param) store) ==
                 (max 0 $ min 100 param)

      store :: PersonStore
      store = insertList (map (id &&& personKey) persons) I.empty

      persons = map (\x -> Person (show x) x x) [1..100] 

      param :: Int
      param = 1 + p `mod` 200

-- | Tests insert, update (changing element value) #2.
prop_update2 = all (\(p, _) -> personAge p == 50) $
    I.lookup (sPersonID .<= treshold) updatedStore
    where
      updatedStore :: PersonStore
      updatedStore =
          I.update (\p -> Just (p { personAge = 50 }, Nothing))
                   (sPersonID .<= treshold) store

      treshold :: PersonID
      treshold = PersonID 50

      store :: PersonStore
      store = insertList (map (id &&& personKey) persons) I.empty
      
      persons = map (\x -> Person (show x) x x) [1..100] 

-- | Tests insert, update (changing element key) #3.
prop_update3 = lookupOld && lookupNew && lookupByID
    where
      lookupOld :: Bool
      lookupOld = L.null $ I.lookup (sPersonAge .<= 100) updatedStore

      lookupNew :: Bool
      lookupNew = length (I.lookup (sPersonAge .== 500) updatedStore) == 100

      lookupByID :: Bool
      lookupByID = all ((== 1) . length) $ 
          map (\k -> I.lookup (sPersonID .== k) updatedStore) pids

      pids :: [PersonID]
      pids = take 100 $ iterate I.nextValue I.initValue

      updatedStore :: PersonStore
      updatedStore =
          I.update (\p -> Just (p, Just $ personKey $ Person "a" 500 500))
                   everything store

      store :: PersonStore
      store = insertList (map (id &&& personKey) persons) I.empty
      
      persons = map (\x -> Person (show x) x x) [1..100] 

-- | Tests insert, update (deleting) #4
prop_update4 = delete1XX && delete11X && deleteX12 
  where
    delete1XX :: Bool
    delete1XX = I.size res == 2                
      where
        res = I.update (const Nothing) (sPersonName .== "1") store

    delete11X :: Bool
    delete11X = (I.size res == 2) && (resLookup == [ Person "2" 1 2 ])
      where
        res = I.update (const Nothing) (sPersonName .== "1" .&& sPersonAge .== 1) store
        resLookup = map fst $ I.lookup (sPersonAge .== 1) res    

    deleteX12 :: Bool
    deleteX12 = (I.size res == 2) && (resLookup1 == [ Person "1" 1 1 ])
                                  && (resLookup2 == [ Person "3" 2 2 ])
      where
        res = I.update (const Nothing) (sPersonAge .== 1 .&& sPersonCredits .== 2) store
        resLookup1 = map fst $ I.lookup (sPersonAge .== 1) res
        resLookup2 = map fst $ I.lookup (sPersonCredits .== 2) res

    store :: PersonStore
    store = insertList (map (id &&& personKey) persons) I.empty
    persons = [ Person "1" 1 1, Person "2" 1 2, Person "3" 2 2 ]

insertList :: [(v, I.Key k)] -> I.Store tag k v -> I.Store tag k v
insertList es st = L.foldl' (\acc (e, k) -> I.insert' k e acc) st es

--------------------------------------------------------------------------------
-- | QuickCheck machinery.

instance Arbitrary Person where
    arbitrary = (\(x, y, z) -> Person x y z) <$> arbitrary

