{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators  #-}

module Test.Data.Store
( tests
) where

--------------------------------------------------------------------------------
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
--------------------------------------------------------------------------------
import qualified Data.Store     as I
import qualified Data.Store.Key as I
import           Data.Store.Key      ((:.)(..), (.:))
--------------------------------------------------------------------------------

newtype PersonID = PersonID Int deriving (Eq, Ord)

instance I.Auto PersonID where
    initValue = PersonID 1
    nextValue (PersonID n) = PersonID $ n + 1

data Person = Person
    { personName :: String
    , personAge  :: Int
    }

data PersonStoreTag 

type PersonKeySpec = ((PersonID, I.DimAuto) :. (String, I.Dim) :. (Int, I.Dim) :. I.K0) 
type PersonKey     = I.Key PersonKeySpec

type PersonStore = I.Store PersonStoreTag PersonKeySpec Person

personKey :: Person -> PersonKey
personKey (Person pn pa) = I.DimensionAuto .: I.Dimension [pn] .: I.K1 (I.Dimension [pa])

tests :: [Test]
tests =
    [ testProperty "insert1" prop_insert1
    , testProperty "insert2" prop_insert2
    ]

-- | Tests whether auto-incrementation seems to work #2.
prop_insert1 pn pa = id1 == I.initValue && id2 == I.nextValue I.initValue
    where
      s0 = I.empty :: PersonStore 
      (s1, id1 :. ()) = I.insert (personKey person) person s0
      (_,  id2 :. ()) = I.insert (personKey person) person s1
      person = Person pn pa

-- | Tests whether auto-incrementation seems to work #2.
prop_insert2 pn pa = take 100 (map snd inserts) == take 100 values
    where
      s0 = I.empty :: PersonStore 
      (s1, id1 :. ())  = I.insert (personKey person) person s0 
      inserts = iterate (\(s, _) -> let (sn, idn :. ()) = I.insert (personKey person) person s
                                    in  (sn, idn)
                        ) (s1, id1)
      values = iterate I.nextValue I.initValue 
      person = Person pn pa

