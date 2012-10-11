{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators  #-}

module Test.Data.Store01
( tests
) where

--------------------------------------------------------------------------------
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

newtype PersonID = PersonID Int deriving (Eq, Ord)

instance I.Auto PersonID where
    initValue = PersonID 1
    nextValue (PersonID n) = PersonID $ n + 1

data Person = Person
    { personName :: String
    , personAge  :: Int
    } deriving Show

data PersonStoreTag 

type PersonKeySpec = ((PersonID, I.DimAuto) :. (String, I.Dim) :. (Int, I.Dim) :. I.K0) 
type PersonKey     = I.Key PersonKeySpec

type PersonStore = I.Store PersonStoreTag PersonKeySpec Person

personKey :: Person -> PersonKey
personKey (Person pn pa) = I.DimensionAuto .: I.Dimension [pn] .: I.K1 (I.Dimension [pa])

sPersonID :: Proxy (PersonStoreTag, I.Z)
sPersonID = Proxy

sPersonName :: Proxy (PersonStoreTag, I.S I.Z)
sPersonName = Proxy

sPersonAge :: Proxy (PersonStoreTag, I.S (I.S I.Z))
sPersonAge = Proxy

tests :: [Test]
tests =
    [ testProperty "insert1" prop_insert1
    , testProperty "insert2" prop_insert2
    , testProperty "lookup1" prop_lookup1
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
prop_lookup1 persons = lookupByNameOK && lookupByAgeOK && lookupByIDOK 
    where
      lookupByNameOK :: Bool
      lookupByNameOK = all (not . L.null) $
          map (\k -> I.lookup (sPersonName .== k) store) names

      lookupByAgeOK :: Bool
      lookupByAgeOK = all (not . L.null) $
          map (\k -> I.lookup (sPersonAge .== k) store) ages

      lookupByIDOK :: Bool
      lookupByIDOK = all (\xs -> length xs == 1) $
          map (\k -> I.lookup (sPersonID .== k) store) pids

      store :: PersonStore
      store = foldr (\p acc -> fst $ I.insert (personKey p) p acc) I.empty persons

      names :: [String]
      names = map personName persons

      ages :: [Int]
      ages  = map personAge  persons

      pids :: [PersonID]
      pids  = take number $ iterate I.nextValue I.initValue
      
      number = length persons

--------------------------------------------------------------------------------
-- | QuickCheck machinery.

instance Arbitrary Person where
    arbitrary = uncurry Person <$> arbitrary

