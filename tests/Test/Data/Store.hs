{-# LANGUAGE EmptyDataDecls #-}

module Test.Data.Store
( tests
) where

--------------------------------------------------------------------------------
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
--------------------------------------------------------------------------------
import qualified Data.Store     as I
import qualified Data.Store.Key as I
--------------------------------------------------------------------------------

instance I.Auto Int where
    initValue = 1
    nextValue = succ 

data PersonStore 

data Person = Person
    { personName :: String
    , personAge  :: Int
    }

type PersonKey = I.Key Int I.DimAuto (I.Key String I.Dim (I.Key Int I.Dim I.K0)) 

personKey :: Person -> PersonKey
personKey (Person pn pa) = I.DimensionAuto I..: I.Dimension [pn] I..: I.K1 (I.Dimension [pa])

tests :: [Test]
tests =
    [ testProperty "insert1" prop_insert1
    , testProperty "insert2" prop_insert2
    ]

-- | Tests whether auto-incrementation seems to work #2.
prop_insert1 pn pa = id1 == I.initValue && id2 == I.nextValue I.initValue
    where
      s0 = I.empty :: I.Store PersonStore PersonKey Person 
      (s1, id1 I.:. ()) = I.insert (personKey person) person s0
      (_,  id2 I.:. ()) = I.insert (personKey person) person s1
      person = Person pn pa

-- | Tests whether auto-incrementation seems to work #2.
prop_insert2 pn pa = take 100 (map snd inserts) == take 100 values
    where
      (s1, id1 I.:. ())  = I.insert (personKey person) person (I.empty :: I.Store PersonStore PersonKey Person)
      inserts = iterate (\(s, _) -> let (sn, idn I.:. ()) = I.insert (personKey person) person s
                                    in  (sn, idn)
                        ) (s1, id1)
      values  = iterate I.nextValue I.initValue 
      person = Person pn pa

