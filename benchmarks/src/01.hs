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
--------------------------------------------------------------------------------
import qualified Criterion.Config as C
import qualified Criterion.Main   as C
import qualified Criterion.Monad  as C
--------------------------------------------------------------------------------
import qualified DS.B01
import qualified TS.B01
--------------------------------------------------------------------------------
import Common
--------------------------------------------------------------------------------

data RNF where
    RNF :: NFData a => a -> RNF

instance NFData RNF where
    rnf (RNF x) = rnf x

main :: IO ()
main = C.defaultMainWith C.defaultConfig (liftIO . evaluate $ rnf
  [ RNF elems01x5000
  , RNF elems01x10000
  , RNF elems01x15000
  , RNF elems01x50000
  , RNF elems01x100000
  , RNF elems01x150000

  , RNF elems01x5000x5000
  , RNF elems01x10000x5000

  , RNF ds01x5000 
  , RNF ds01x10000

  , RNF ts01x5000
  , RNF ts01x10000
  ])
  -- Insert N elements into an empty store (the inserts are accumulative). No collisions.
  [ C.bgroup "insert (Int) 01 5000"
    [ C.bcompare
      [ C.bench "DS" $ C.nf (insertDS01 elems01x5000) DS.B01.empty
      , C.bench "DS (Unsafe)" $ C.nf (insertDS01Unsafe elems01x5000) DS.B01.empty
#ifndef BENCH_DS
      , C.bench "TS" $ C.nf (insertTS01 elems01x5000) TS.B01.empty
#endif
      ]
    ]
  , C.bgroup "insert (Int) 01 10000"
    [ C.bcompare
      [ C.bench "DS" $ C.nf (insertDS01 elems01x10000) DS.B01.empty
      , C.bench "DS (Unsafe)" $ C.nf (insertDS01Unsafe elems01x10000) DS.B01.empty
#ifndef BENCH_DS
      , C.bench "TS" $ C.nf (insertTS01 elems01x10000) TS.B01.empty
#endif
      ]
    ]
  
  -- Insert N elements into store of the same N elements (the inserts are
  -- accumulative, thus we basically "overwrite" the shole store).
  -- Collisions (obviously).
  , C.bgroup "insert-collisions (Int) 01 5000"
    [ C.bcompare
      [ C.bench "DS" $ C.nf (insertDS01 elems01x5000) ds01x5000
#ifndef BENCH_DS
      , C.bench "TS" $ C.nf (insertTS01 elems01x5000) ts01x5000
#endif
      ]
    ]
    , C.bgroup "insert-collisions (Int) 01 10000"
    [ C.bcompare
      [ C.bench "DS" $ C.nf (insertDS01 elems01x10000) ds01x10000
#ifndef BENCH_DS      
      , C.bench "TS" $ C.nf (insertTS01 elems01x10000) ts01x10000
#endif
      ]
    ]

  -- Lookup in store of N elements.
  , C.bgroup "lookup (Int) 01"
    [ C.bgroup "5000"
      [ C.bcompare
        [ C.bench "DS OO EQ" $ C.nf (lookupDS01xOOEQ 5000) ds01x5000
        , C.bench "DS OO EQ (Lens)" $ C.nf (lookupDS01xOOEQLens 5000) ds01x5000
#ifndef BENCH_DS
        , C.bench "TS OO EQ" $ C.nf (lookupTS01xOOEQ 5000) ts01x5000
#endif
        ]
      , C.bcompare
        [ C.bench "DS OO GE" $ C.nf (lookupDS01xOOGE 5000) ds01x5000
        , C.bench "DS OO GE (Lens)" $ C.nf (lookupDS01xOOGELens 5000) ds01x5000
#ifndef BENCH_DS
        , C.bench "TS OO GE" $ C.nf (lookupTS01xOOGE 5000) ts01x5000
#endif
        ]
      , C.bcompare
        [ C.bench "DS OM EQ" $ C.nf (lookupDS01xOMEQ 5000) ds01x5000
        , C.bench "DS OM EQ (Lens)" $ C.nf (lookupDS01xOMEQLens 5000) ds01x5000
#ifndef BENCH_DS
        , C.bench "TS OM EQ" $ C.nf (lookupTS01xOMEQ 5000) ts01x5000
#endif
        ]
      , C.bcompare
        [ C.bench "DS OM GE" $ C.nf (lookupDS01xOMGE 5000) ds01x5000
        , C.bench "DS OM GE (Lens)" $ C.nf (lookupDS01xOMGELens 5000) ds01x5000
#ifndef BENCH_DS
        , C.bench "TS OM GE" $ C.nf (lookupTS01xOMGE 5000) ts01x5000
#endif
        ]
      , C.bcompare
        [ C.bench "DS MM EQ" $ C.nf (lookupDS01xMMEQ 5000) ds01x5000
        , C.bench "DS MM EQ (Lens)" $ C.nf (lookupDS01xMMEQLens 5000) ds01x5000
#ifndef BENCH_DS
        , C.bench "TS MM EQ" $ C.nf (lookupTS01xMMEQ 5000) ts01x5000
#endif
        ]
      ]
#ifndef BENCH_SHALLOW
    , C.bgroup "10000"
      [ C.bcompare
        [ C.bench "DS OO EQ" $ C.nf (lookupDS01xOOEQ 10000) ds01x10000
        , C.bench "DS OO EQ (Lens)" $ C.nf (lookupDS01xOOEQLens 10000) ds01x10000
#ifndef BENCH_DS
        , C.bench "TS OO EQ" $ C.nf (lookupTS01xOOEQ 10000) ts01x10000
#endif
        ]
      , C.bcompare
        [ C.bench "DS OO GE" $ C.nf (lookupDS01xOOGE 10000) ds01x10000
        , C.bench "DS OO GE (Lens)" $ C.nf (lookupDS01xOOGELens 10000) ds01x10000
#ifndef BENCH_DS
        , C.bench "TS OO GE" $ C.nf (lookupTS01xOOGE 10000) ts01x10000
#endif
        ]
      , C.bcompare
        [ C.bench "DS OM EQ" $ C.nf (lookupDS01xOMEQ 10000) ds01x10000
        , C.bench "DS OM EQ (Lens)" $ C.nf (lookupDS01xOMEQLens 10000) ds01x10000
#ifndef BENCH_DS
        , C.bench "TS OM EQ" $ C.nf (lookupTS01xOMEQ 10000) ts01x10000
#endif
        ]
      , C.bcompare
        [ C.bench "DS OM GE" $ C.nf (lookupDS01xOMGE 10000) ds01x10000
        , C.bench "DS OM GE (Lens)" $ C.nf (lookupDS01xOMGELens 10000) ds01x10000
#ifndef BENCH_DS
        , C.bench "TS OM GE" $ C.nf (lookupTS01xOMGE 10000) ts01x10000
#endif
        ]
      , C.bcompare
        [ C.bench "DS MM EQ" $ C.nf (lookupDS01xMMEQ 10000) ds01x10000
        , C.bench "DS MM EQ (Lens)" $ C.nf (lookupDS01xMMEQLens 10000) ds01x10000
#ifndef BENCH_DS
        , C.bench "TS MM EQ" $ C.nf (lookupTS01xMMEQ 10000) ts01x10000
#endif
        ]
      ]
#endif
    ]
  ]

---

lookupDS01xOOEQ :: Int -> DS.B01.DS -> [[(DS.B01.DSRawKey, C01)]] 
lookupDS01xOOEQ size o = map (`DS.B01.lookupOOEQ` o) [ 0, (size `div` 1000) .. size ]

lookupDS01xOOGE :: Int -> DS.B01.DS -> [[(DS.B01.DSRawKey, C01)]] 
lookupDS01xOOGE size o = map (`DS.B01.lookupOOGE` o) [ 0, (size `div` 10) .. size ]

lookupDS01xOMEQ :: Int -> DS.B01.DS -> [[(DS.B01.DSRawKey, C01)]] 
lookupDS01xOMEQ size o = map (`DS.B01.lookupOMEQ` o) [ 0, (s `div` 10) .. s ]
  where s = size `div` 5

lookupDS01xOMGE :: Int -> DS.B01.DS -> [[(DS.B01.DSRawKey, C01)]] 
lookupDS01xOMGE size o = map (`DS.B01.lookupOMGE` o) [ 0, (s `div` 10) .. s ]
  where s = size `div` 5

lookupDS01xMMEQ :: Int -> DS.B01.DS -> [[(DS.B01.DSRawKey, C01)]] 
lookupDS01xMMEQ size o = map (`DS.B01.lookupMMEQ` o) [ 0, (size `div` 10) .. size ]

-- lookup DS (Lens)

lookupDS01xOOEQLens :: Int -> DS.B01.DS -> [DS.B01.DS] 
lookupDS01xOOEQLens size o = map (`DS.B01.lookupOOEQLens` o) [ 0, (size `div` 1000) .. size ]

lookupDS01xOOGELens :: Int -> DS.B01.DS -> [DS.B01.DS]
lookupDS01xOOGELens size o = map (`DS.B01.lookupOOGELens` o) [ 0, (size `div` 10) .. size ]

lookupDS01xOMEQLens :: Int -> DS.B01.DS -> [DS.B01.DS]
lookupDS01xOMEQLens size o = map (`DS.B01.lookupOMEQLens` o) [ 0, (s `div` 10) .. s ]
  where s = size `div` 5

lookupDS01xOMGELens :: Int -> DS.B01.DS -> [DS.B01.DS]
lookupDS01xOMGELens size o = map (`DS.B01.lookupOMGELens` o) [ 0, (s `div` 10) .. s ]
  where s = size `div` 5

lookupDS01xMMEQLens :: Int -> DS.B01.DS -> [DS.B01.DS]
lookupDS01xMMEQLens size o = map (`DS.B01.lookupMMEQLens` o) [ 0, (size `div` 10) .. size ]

-- lookup TS

lookupTS01xOOEQ :: Int -> TS.B01.TS -> [TS.B01.TS] 
lookupTS01xOOEQ size o = map (`TS.B01.lookupOOEQ` o) [ 0, (size `div` 1000) .. size ]

lookupTS01xOOGE :: Int -> TS.B01.TS -> [TS.B01.TS]
lookupTS01xOOGE size o = map (`TS.B01.lookupOOGE` o) [ 0, (size `div` 10) .. size ]

lookupTS01xOMEQ :: Int -> TS.B01.TS -> [TS.B01.TS]
lookupTS01xOMEQ size o = map (`TS.B01.lookupOMEQ` o) [ 0, (s `div` 10) .. s ]
  where s = size `div` 5

lookupTS01xOMGE :: Int -> TS.B01.TS -> [TS.B01.TS]
lookupTS01xOMGE size o = map (`TS.B01.lookupOMGE` o) [ 0, (s `div` 10) .. s ]
  where s = size `div` 5

lookupTS01xMMEQ :: Int -> TS.B01.TS -> [TS.B01.TS]
lookupTS01xMMEQ size o = map (`TS.B01.lookupMMEQ` o) [ 0, (size `div` 10) .. size ]

---

insertDS01 :: [C01] -> DS.B01.DS -> DS.B01.DS
insertDS01 xs s0 = foldl' (flip DS.B01.insert) s0 xs

insertDS01Unsafe :: [C01] -> DS.B01.DS -> DS.B01.DS
insertDS01Unsafe xs s0 = foldl' (flip DS.B01.insertUnsafe) s0 xs

insertTS01 :: [C01] -> TS.B01.TS -> TS.B01.TS
insertTS01 xs s0 = foldl' (flip TS.B01.insert) s0 xs

---

ds01x5000 :: DS.B01.DS
ds01x5000 = insertDS01 elems01x5000 DS.B01.empty

ds01x10000 :: DS.B01.DS
ds01x10000 = insertDS01 elems01x10000 DS.B01.empty

ts01x5000 :: TS.B01.TS
ts01x5000 = insertTS01 elems01x5000 TS.B01.empty

ts01x10000 :: TS.B01.TS
ts01x10000 = insertTS01 elems01x10000 TS.B01.empty

elems01x5000 :: [C01]
elems01x5000 = generate01 0 5000

elems01x5000x5000 :: [C01]
elems01x5000x5000 = generate01 5000 5000

elems01x10000x5000 :: [C01]
elems01x10000x5000 = generate01 10000 5000

elems01x10000 :: [C01]
elems01x10000 = generate01 0 10000

elems01x15000 :: [C01]
elems01x15000 = generate01 0 15000

elems01x50000 :: [C01]
elems01x50000 = generate01 0 50000

elems01x100000 :: [C01]
elems01x100000 = generate01 0 100000

elems01x150000 :: [C01]
elems01x150000 = generate01 0 150000

generate01 :: Int -> Int -> [C01]
generate01 o n = map (\x -> C01 x (x `div` s) [x .. x + s]) [o .. (n + o) - 1]
  where
    s = 5


