{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP   #-}

module Main
( main
) where

#define BENCH_SMALL
#define BENCH_ESSENTIALS

--------------------------------------------------------------------------------
import           Control.DeepSeq (NFData(..))
import           Control.Exception.Base (evaluate)
import           Control.Monad.Trans (liftIO)
--------------------------------------------------------------------------------
import           Data.List
import qualified Data.Map.Strict as Data.Map
--------------------------------------------------------------------------------
import qualified Criterion.Config as C
import qualified Criterion.Main   as C
--------------------------------------------------------------------------------
import qualified DS.B01
import qualified TS.B01
import qualified IS.B01
--------------------------------------------------------------------------------
import Common
--------------------------------------------------------------------------------

data RNF where
    RNF :: NFData a => a -> RNF

instance NFData RNF where
    rnf (RNF x) = rnf x

main :: IO ()
main = C.defaultMainWith C.defaultConfig (liftIO . evaluate $ rnf
  [
    RNF elems100000
  , RNF elems200000
  , RNF elems400000
  , RNF elems800000

  , RNF elems5000x5000
  , RNF elems10000x5000

  , RNF ds100000
  , RNF ds200000

  , RNF map100000
  , RNF map200000
  
  , RNF is100000
  , RNF is200000
 
  , RNF ts100000
  , RNF ts200000

  , RNF elem9999999
  , RNF elem2500
  ])
  -- Insert 1 element into a store of size N. No collisions.
  [
    {-
    C.bgroup "insert (Int) 01 100000"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (DS.B01.insert elem9999999) ds100000
      , C.bench "DS (Unsafe)" $ C.whnf (DS.B01.insertUnsafe elem9999999) ds100000
#ifndef BENCH_DS
      , C.bench "Map" $ C.whnf (insertMap elem9999999) map10000
      , C.bench "IS" $ C.whnf (IS.B01.force . IS.B01.insert elem9999999) is100000
      , C.bench "TS" $ C.whnf (TS.B01.force . TS.B01.insert elem9999999) ts100000
#endif
      ]
    ]
  , C.bgroup "insert (Int) 01 200000"
    [ C.bcompare
      [ C.bench "DS" $ C.nf (DS.B01.insert elem9999999) ds200000
      , C.bench "DS (Unsafe)" $ C.nf (DS.B01.insertUnsafe elem9999999) ds200000
#ifndef BENCH_DS
      , C.bench "Map" $ C.whnf (insertMap elem9999999) map20000
      , C.bench "IS" $ C.whnf (IS.B01.force . IS.B01.insert elem9999999) is200000
      , C.bench "TS" $ C.whnf (TS.B01.force . TS.B01.insert elem9999999) ts200000
#endif
      ]
    ]
  , C.bgroup "insert-collision (Int) 01 100000"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (DS.B01.insert elem2500) ds100000
#ifndef BENCH_DS
      , C.bench "Map" $ C.whnf (insertMap elem2500) map100000
      , C.bench "IS" $ C.whnf (IS.B01.force . IS.B01.insert elem2500) is100000
      , C.bench "TS" $ C.whnf (TS.B01.force . TS.B01.insert elem2500) ts100000
#endif
      ]
    ]
  , C.bgroup "insert-collision (Int) 01 200000"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (DS.B01.insert elem2500) ds200000
#ifndef BENCH_DS
      , C.bench "Map" $ C.whnf (insertMap elem2500) map200000
      , C.bench "IS" $ C.whnf (IS.B01.force . IS.B01.insert elem2500) is200000
      , C.bench "TS" $ C.whnf (TS.B01.force . TS.B01.insert elem2500) ts200000
#endif
      ]
    ]
    -}
    C.bgroup "lookup OO EQ (Int) 01 200000"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (forceList . DS.B01.lookupOOEQ 10000) ds200000
      , C.bench "DS (Lens)" $ C.whnf (forceList . DS.B01.lookupOOEQLens 10000) ds200000
#ifndef BENCH_DS
      , C.bench "Map" $ C.nf (Data.Map.lookup 10000) map200000
      , C.bench "IS" $ C.whnf (forceList . IS.B01.lookupOOEQ 10000) is200000
      , C.bench "TS" $ C.whnf (forceList . TS.B01.lookupOOEQ 10000) ts200000
#endif
      ]
    ]
  , C.bgroup "lookup OO GE (Int) 01 200000 (500)"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (forceList . DS.B01.lookupOOGE 199500) ds200000
      , C.bench "DS (Lens)" $ C.whnf (forceList . DS.B01.lookupOOGELens 199500) ds200000
#ifndef BENCH_DS
      , C.bench "IS" $ C.whnf (forceList . IS.B01.lookupOOGE 199500) is200000
      , C.bench "TS" $ C.whnf (forceList . TS.B01.lookupOOGE 199500) ts200000
#endif
      ]
    ]
  , C.bgroup "lookup OM EQ (Int) 01 200000"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (forceList . DS.B01.lookupOMEQ 200) ds200000
      , C.bench "DS (Lens)" $ C.whnf (forceList . DS.B01.lookupOMEQLens 200) ds200000
#ifndef BENCH_DS
      , C.bench "IS" $ C.whnf (forceList . IS.B01.lookupOMEQ 200) is200000
      , C.bench "TS" $ C.whnf (forceList . TS.B01.lookupOMEQ 200) ts200000
#endif
      ]
    ]
  , C.bgroup "lookup OM GE (Int) 01 200000 (500)"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (forceList . DS.B01.lookupOMGE 39900) ds200000
      , C.bench "DS (Lens)" $ C.whnf (forceList . DS.B01.lookupOMGELens 39900) ds200000
#ifndef BENCH_DS
      , C.bench "IS" $ C.whnf (forceList . IS.B01.lookupOMGE 39900) is200000
      , C.bench "TS" $ C.whnf (forceList . TS.B01.lookupOMGE 39900) ts200000
#endif
      ]
    ]
  , C.bgroup "lookup MM EQ (Int) 01 200000"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (forceList . DS.B01.lookupMMEQ 200) ds200000
      , C.bench "DS (Lens)" $ C.whnf (forceList . DS.B01.lookupMMEQLens 200) ds200000
#ifndef BENCH_DS
      , C.bench "IS" $ C.whnf (forceList . IS.B01.lookupMMEQ 200) is200000
      , C.bench "TS" $ C.whnf (forceList . TS.B01.lookupMMEQ 200) ts200000
#endif
      ]
    ]
  ]


---

insertListDS :: [C01] -> DS.B01.DS -> DS.B01.DS
insertListDS xs s0 = foldl' (flip DS.B01.insert) s0 xs

insertListTS :: [C01] -> TS.B01.TS -> TS.B01.TS
insertListTS xs s0 = snd $!
  foldl' (\(n, acc) x -> if n == t
                           then rnf acc `seq` (0, TS.B01.insert x acc)
                           else (n + 1, TS.B01.insert x acc)
         ) (0 :: Int, s0) xs
  where t = 10000

insertListIS :: [C01] -> IS.B01.IS -> IS.B01.IS
insertListIS xs s0 = snd $! 
  foldl' (\(n, acc) x -> if n == t
                           then rnf acc `seq` (0, IS.B01.insert x acc)
                           else (n + 1, IS.B01.insert x acc)
         ) (0 :: Int, s0) xs
  where t = 10000

insertListMap :: [C01] -> Data.Map.Map Int C01 -> Data.Map.Map Int C01
insertListMap xs s0 = foldl' (flip insertMap) s0 xs

insertMap :: C01 -> Data.Map.Map Int C01 -> Data.Map.Map Int C01
insertMap x@(C01 oo _ _) = Data.Map.insert oo x

-- MAP

map100000 :: Data.Map.Map Int C01
map100000 = insertListMap elems100000 Data.Map.empty

map200000 :: Data.Map.Map Int C01
map200000 = insertListMap elems200000 Data.Map.empty

-- IS

is100000 :: IS.B01.IS
is100000 = insertListIS elems100000 IS.B01.empty

is200000 :: IS.B01.IS
is200000 = insertListIS elems200000 IS.B01.empty

-- DS

ds100000 :: DS.B01.DS
ds100000 = insertListDS elems100000 DS.B01.empty

ds200000 :: DS.B01.DS
ds200000 = insertListDS elems200000 DS.B01.empty

-- TS

ts100000 :: TS.B01.TS
ts100000 = insertListTS elems100000 TS.B01.empty

ts200000 :: TS.B01.TS
ts200000 = insertListTS elems200000 TS.B01.empty

