{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP   #-}

module Main
( main
) where

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

#define BENCH_SMALL
#define BENCH_ESSENTIALS

data RNF where
    RNF :: NFData a => a -> RNF

instance NFData RNF where
    rnf (RNF x) = rnf x

main :: IO ()
main = C.defaultMainWith C.defaultConfig (liftIO . evaluate $ rnf
  [ RNF elems500
  , RNF elems10000
  , RNF elems20000
#ifndef BENCH_SMALL
  , RNF elems100000
  , RNF elems200000
  , RNF elems400000
  , RNF elems800000
#endif

  , RNF elems5000x5000
  , RNF elems10000x5000

  , RNF ds10000
  , RNF ds20000
#ifndef BENCH_SMALL
  , RNF ds100000
  , RNF ds200000
  --, RNF ds400000
  --, RNF ds800000
#endif

  , RNF map10000
  , RNF map20000
#ifndef BENCH_SMALL
  , RNF map100000
  , RNF map200000
  --, RNF map400000
  --, RNF map800000
#endif
  
  , RNF is10000
  , RNF is20000
#ifndef BENCH_SMALL
  , RNF is100000
  , RNF is200000
#endif
  
  , RNF ts10000
  , RNF ts20000
#ifndef BENCH_SMALL
  , RNF ts100000
  , RNF ts200000
#endif

  , RNF elem9999999
  , RNF elem2500
  ])
  -- Insert 1 element into a store of size N. No collisions.
  [ {-
    C.bgroup "insert (Int) 01 10000"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (DS.B01.insert elem9999999) ds10000
      , C.bench "DS (Unsafe)" $ C.whnf (DS.B01.insertUnsafe elem9999999) ds10000
#ifndef BENCH_DS
      , C.bench "Map" $ C.whnf (insertMap elem9999999) map10000
      , C.bench "IS" $ C.whnf (IS.B01.force . IS.B01.insert elem9999999) is10000
      , C.bench "TS" $ C.whnf (TS.B01.force . TS.B01.insert elem9999999) ts10000
#endif
      ]
    ]
  , C.bgroup "insert (Int) 01 20000"
    [ C.bcompare
      [ C.bench "DS" $ C.nf (DS.B01.insert elem9999999) ds20000
      , C.bench "DS (Unsafe)" $ C.nf (DS.B01.insertUnsafe elem9999999) ds20000
#ifndef BENCH_DS
      , C.bench "Map" $ C.whnf (insertMap elem9999999) map20000
      , C.bench "IS" $ C.whnf (IS.B01.force . IS.B01.insert elem9999999) is20000
      , C.bench "TS" $ C.whnf (TS.B01.force . TS.B01.insert elem9999999) ts20000
#endif
      ]
    ]
  , C.bgroup "insert-collision (Int) 01 10000"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (DS.B01.insert elem2500) ds10000
#ifndef BENCH_DS
      , C.bench "Map" $ C.whnf (insertMap elem2500) map10000
      , C.bench "IS" $ C.whnf (IS.B01.force . IS.B01.insert elem2500) is10000
      , C.bench "TS" $ C.whnf (TS.B01.force . TS.B01.insert elem2500) ts10000
#endif
      ]
    ]
  , C.bgroup "insert-collision (Int) 01 20000"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (DS.B01.insert elem2500) ds20000
#ifndef BENCH_DS
      , C.bench "Map" $ C.whnf (insertMap elem2500) map20000
      , C.bench "IS" $ C.whnf (IS.B01.force . IS.B01.insert elem2500) is20000
      , C.bench "TS" $ C.whnf (TS.B01.force . TS.B01.insert elem2500) ts20000
#endif
      ]
    ]
  -}
    C.bgroup "lookup OO EQ (Int) 01 20000"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (forceList . DS.B01.lookupOOEQ 10000) ds20000
      , C.bench "DS (Lens)" $ C.whnf (forceList . DS.B01.lookupOOEQLens 10000) ds20000
#ifndef BENCH_DS
      , C.bench "Map" $ C.nf (Data.Map.lookup 10000) map20000
      , C.bench "IS" $ C.whnf (forceList . IS.B01.lookupOOEQ 10000) is20000
      , C.bench "TS" $ C.whnf (forceList . TS.B01.lookupOOEQ 10000) ts20000
#endif
      ]
    ]
  , C.bgroup "lookup OO GE (Int) 01 20000 (500)"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (forceList . DS.B01.lookupOOGE 19500) ds20000
      , C.bench "DS (Lens)" $ C.whnf (forceList . DS.B01.lookupOOGELens 19500) ds20000
#ifndef BENCH_DS
      , C.bench "IS" $ C.whnf (forceList . IS.B01.lookupOOGE 19500) is20000
      , C.bench "TS" $ C.whnf (forceList . TS.B01.lookupOOGE 19500) ts20000
#endif
      ]
    ]
  , C.bgroup "lookup OM EQ (Int) 01 20000"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (forceList . DS.B01.lookupOMEQ 200) ds20000
      , C.bench "DS (Lens)" $ C.whnf (forceList . DS.B01.lookupOMEQLens 200) ds20000
#ifndef BENCH_DS
      , C.bench "IS" $ C.whnf (forceList . IS.B01.lookupOMEQ 200) is20000
      , C.bench "TS" $ C.whnf (forceList . TS.B01.lookupOMEQ 200) ts20000
#endif
      ]
    ]
  , C.bgroup "lookup OM GE (Int) 01 20000 (500)"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (forceList . DS.B01.lookupOMGE 3900) ds20000
      , C.bench "DS (Lens)" $ C.whnf (forceList . DS.B01.lookupOMGELens 3900) ds20000
#ifndef BENCH_DS
      , C.bench "IS" $ C.whnf (forceList . IS.B01.lookupOMGE 3900) is20000
      , C.bench "TS" $ C.whnf (forceList . TS.B01.lookupOMGE 3900) ts20000
#endif
      ]
    ]
  , C.bgroup "lookup MM EQ (Int) 01 20000"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (forceList . DS.B01.lookupMMEQ 200) ds20000
      , C.bench "DS (Lens)" $ C.whnf (forceList . DS.B01.lookupMMEQLens 200) ds20000
#ifndef BENCH_DS
      , C.bench "IS" $ C.whnf (forceList . IS.B01.lookupMMEQ 200) is20000
      , C.bench "TS" $ C.whnf (forceList . TS.B01.lookupMMEQ 200) ts20000
#endif
      ]
    ]

  -- BIG INPUTS
  -- Insert 1 element into a store of size N. No collisions.
#ifndef BENCH_SMALL 
  , C.bgroup "insert (Int) 01 100000"
    [ C.bcompare
      [ C.bench "DS" $ C.nf (DS.B01.insert elem9999999) ds100000
      , C.bench "DS (Unsafe)" $ C.nf (DS.B01.insertUnsafe elem9999999) ds100000
#ifndef BENCH_DS
      , C.bench "IS" $ C.nf (IS.B01.insert elem9999999) is100000
      , C.bench "Map" $ C.whnf (insertMap elem9999999) map100000
      , C.bench "TS" $ C.nf (TS.B01.insert elem9999999) ts100000
#endif
      ]
    ]
  , C.bgroup "insert (Int) 01 200000"
    [ C.bcompare
      [ C.bench "DS" $ C.nf (DS.B01.insert elem9999999) ds200000
      , C.bench "DS (Unsafe)" $ C.whnf (DS.B01.insertUnsafe elem9999999) ds200000
#ifndef BENCH_DS
      , C.bench "IS" $ C.nf (IS.B01.insert elem9999999) is200000
      , C.bench "Map" $ C.whnf (insertMap elem9999999) map200000
      , C.bench "TS" $ C.nf (TS.B01.insert elem9999999) ts200000
#endif
      ]
    ]
  , C.bgroup "insert-collision (Int) 01 100000"
    [ C.bcompare
      [ C.bench "DS" $ C.nf (DS.B01.insert elem2500) ds100000
#ifndef BENCH_DS
      , C.bench "IS" $ C.whnf (IS.B01.insert elem2500) is100000
      , C.bench "Map" $ C.nf (insertMap elem2500) map100000
      , C.bench "TS" $ C.nf (TS.B01.insert elem2500) ts100000
#endif
      ]
    ]
  , C.bgroup "insert-collision (Int) 01 200000"
    [ C.bcompare
      [ C.bench "DS" $ C.nf (DS.B01.insert elem2500) ds200000
#ifndef BENCH_DS
      , C.bench "IS" $ C.nf (IS.B01.insert elem2500) is200000
      , C.bench "Map" $ C.nf (insertMap elem2500) map200000
      , C.bench "TS" $ C.nf (TS.B01.insert elem2500) ts200000
#endif
      ]
    ]
  , C.bgroup "lookup OO EQ (Int) 01 200000"
    [ C.bcompare
      [ C.bench "DS" $ C.nf (DS.B01.lookupOOEQ 2500) ds200000
      , C.bench "DS (Lens)" $ C.nf (DS.B01.lookupOOEQLens 2500) ds200000
#ifndef BENCH_DS
      , C.bench "IS" $ C.nf (IS.B01.lookupOOEQ 2500) is200000
      , C.bench "Map" $ C.whnf (Data.Map.lookup 2500) map200000
      , C.bench "TS" $ C.nf (TS.B01.lookupOOEQ 2500) ts200000
#endif
      ]
    ]
  , C.bgroup "lookup OO GE (Int) 01 200000 (500)"
    [ C.bcompare
      [ C.bench "DS" $ C.nf (DS.B01.lookupOOGE 199000) ds200000
      , C.bench "DS (Lens)" $ C.nf (DS.B01.lookupOOGELens 199000) ds200000
#ifndef BENCH_DS
      , C.bench "IS" $ C.nf (IS.B01.lookupOOGE 199000) is200000
      , C.bench "TS" $ C.nf (TS.B01.lookupOOGE 199500) ts200000
#endif
      ]
    ]
  , C.bgroup "lookup OM EQ (Int) 01 200000"
    [ C.bcompare
      [ C.bench "DS" $ C.nf (DS.B01.lookupOMEQ 200) ds200000
      , C.bench "DS (Lens)" $ C.nf (DS.B01.lookupOMEQLens 200) ds200000
#ifndef BENCH_DS
      , C.bench "IS" $ C.nf (IS.B01.lookupOMEQ 200) is200000
      , C.bench "TS" $ C.nf (TS.B01.lookupOMEQ 200) ts200000
#endif
      ]
    ]
  , C.bgroup "lookup OM GE (Int) 01 200000 (500)"
    [ C.bcompare
      [ C.bench "DS" $ C.nf (DS.B01.lookupOMGE 39900) ds200000
      , C.bench "DS (Lens)" $ C.nf (DS.B01.lookupOMGELens 39900) ds200000
#ifndef BENCH_DS
      , C.bench "IS" $ C.nf (IS.B01.lookupOMGE 39900) is200000
      , C.bench "TS" $ C.nf (TS.B01.lookupOMGE 39900) ts200000
#endif
      ]
    ]
  , C.bgroup "lookup MM EQ (Int) 01 200000"
    [ C.bcompare
      [ C.bench "DS" $ C.nf (DS.B01.lookupMMEQ 200) ds200000
      , C.bench "DS (Lens)" $ C.nf (DS.B01.lookupMMEQLens 200) ds200000
#ifndef BENCH_DS
      , C.bench "IS" $ C.nf (IS.B01.lookupMMEQ 200) is200000
      , C.bench "TS" $ C.nf (TS.B01.lookupMMEQ 200) ts200000
#endif
      ]
    ]
-- BENCH_SMALL
#endif

{-
#ifndef BENCH_ESSENTIALS
  -- Insert N elements into an empty store (the inserts are accumulative). No collisions.
  , C.bgroup "insert-accum (Int) 01 10000"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (insertListDS elems10000) DS.B01.empty
      , C.bench "DS (Unsafe)" $ C.whnf (insertListDSUnsafe elems10000) DS.B01.empty
#ifndef BENCH_DS
      , C.bench "IS" $ C.whnf (IS.B01.force . insertListIS elems10000) IS.B01.empty
      , C.bench "TS" $ C.whnf (TS.B01.force . insertListTS elems10000) TS.B01.empty
#endif
      ]
    ]
  
  -- Insert N elements into store of the same N elements (the inserts are
  -- accumulative, thus we basically "overwrite" the shole store).
  -- Collisions (obviously).
    , C.bgroup "insert-accum-collisions (Int) 01 10000"
    [ C.bcompare
      [ C.bench "DS" $ C.whnf (insertListDS elems10000) ds10000
#ifndef BENCH_DS      
      , C.bench "IS" $ C.whnf (IS.B01.force . insertListTS elems10000) is10000
      , C.bench "TS" $ C.whnf (TS.B01.force . insertListTS elems10000) ts10000
#endif
      ]
    ]
-- ESSENTIALS
#endif
-}
  ]


---

insertListDS :: [C01] -> DS.B01.DS -> DS.B01.DS
insertListDS xs s0 = foldl' (flip DS.B01.insert) s0 xs

#ifndef BENCH_ESSENTIALS
insertListDSUnsafe :: [C01] -> DS.B01.DS -> DS.B01.DS
insertListDSUnsafe xs s0 = foldl' (flip DS.B01.insertUnsafe) s0 xs
#endif

insertListTS :: [C01] -> TS.B01.TS -> TS.B01.TS
insertListTS xs s0 = foldl' (flip TS.B01.insert) s0 xs

insertListIS :: [C01] -> IS.B01.IS -> IS.B01.IS
insertListIS xs s0 = foldl' (\s x -> IS.B01.insert x $! s) s0 xs

insertListMap :: [C01] -> Data.Map.Map Int C01 -> Data.Map.Map Int C01
insertListMap xs s0 = foldl' (flip insertMap) s0 xs

insertMap :: C01 -> Data.Map.Map Int C01 -> Data.Map.Map Int C01
insertMap x@(C01 oo _ _) = Data.Map.insert oo x

-- MAP

map10000 :: Data.Map.Map Int C01
map10000 = insertListMap elems10000 Data.Map.empty

map20000 :: Data.Map.Map Int C01
map20000 = insertListMap elems20000 Data.Map.empty

#ifndef BENCH_SMALL
map100000 :: Data.Map.Map Int C01
map100000 = insertListMap elems100000 Data.Map.empty

map200000 :: Data.Map.Map Int C01
map200000 = insertListMap elems200000 Data.Map.empty

map800000 :: Data.Map.Map Int C01
map800000 = insertListMap elems800000 Data.Map.empty
#endif

-- IS

is10000 :: IS.B01.IS
is10000 = insertListIS elems10000 IS.B01.empty

is20000 :: IS.B01.IS
is20000 = insertListIS elems20000 IS.B01.empty

#ifndef BENCH_SMALL
is100000 :: IS.B01.IS
is100000 = insertListIS elems100000 IS.B01.empty

is200000 :: IS.B01.IS
is200000 = insertListIS elems200000 IS.B01.empty
#endif

-- DS

ds10000 :: DS.B01.DS
ds10000 = insertListDS elems10000 DS.B01.empty

ds20000 :: DS.B01.DS
ds20000 = insertListDS elems20000 DS.B01.empty

#ifndef BENCH_SMALL
ds100000 :: DS.B01.DS
ds100000 = insertListDS elems100000 DS.B01.empty

ds200000 :: DS.B01.DS
ds200000 = insertListDS elems200000 DS.B01.empty

ds400000 :: DS.B01.DS
ds400000 = insertListDS elems400000 DS.B01.empty

ds800000 :: DS.B01.DS
ds800000 = insertListDS elems800000 DS.B01.empty
#endif

-- TS

ts10000 :: TS.B01.TS
ts10000 = insertListTS elems10000 TS.B01.empty

ts20000 :: TS.B01.TS
ts20000 = insertListTS elems20000 TS.B01.empty

#ifndef BENCH_SMALL
ts100000 :: TS.B01.TS
ts100000 = insertListTS elems100000 TS.B01.empty

ts200000 :: TS.B01.TS
ts200000 = insertListTS elems200000 TS.B01.empty
#endif

-- ELEM

elem9999999 :: C01
elem9999999 = head $! generate 9999999 1 

elem2500 :: C01
elem2500 = head $! generate 2500 1 

elems5000x5000 :: [C01]
elems5000x5000 = generate 5000 5000

elems10000x5000 :: [C01]
elems10000x5000 = generate 10000 5000

elems500 :: [C01]
elems500 = generate 19500 500

elems10000 :: [C01]
elems10000 = generate 0 10000

elems20000 :: [C01]
elems20000 = generate 0 20000

#ifndef BENCH_SMALL
elems100000 :: [C01]
elems100000 = generate 0 100000

elems200000 :: [C01]
elems200000 = generate 0 200000

elems400000 :: [C01]
elems400000 = generate 0 400000

elems800000 :: [C01]
elems800000 = generate 0 800000
#endif

generate :: Int -> Int -> [C01]
generate o n = map (\x -> C01 x (x `div` s) [x .. x + s]) [o .. (n + o) - 1]
  where
    s = 5

forceList :: [a] -> ()
forceList ll = seq (go ll) ()
  where
    go [] = ()
    go (_:xs) = go xs

