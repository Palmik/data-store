{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

module Main
( main
) where

--------------------------------------------------------------------------------
import           Control.DeepSeq (NFData(..))
import           Control.Exception.Base (evaluate)
import           Control.Monad.Trans (liftIO)
--------------------------------------------------------------------------------
import qualified Criterion.Config as C
import qualified Criterion.Main   as C

import qualified Data.HiggsSet as H
import           Data.List
--------------------------------------------------------------------------------
import Common
import HS.B01
--------------------------------------------------------------------------------

data RNF where
    RNF :: NFData a => a -> RNF

instance NFData RNF where
    rnf (RNF x) = rnf x

main :: IO ()
main = C.defaultMainWith C.defaultConfig (liftIO . evaluate $ rnf
  [
    RNF elems50000
  , RNF elems100000
  , RNF elems200000

  , RNF hs50000
  , RNF hs100000
  , RNF hs200000
  
  , RNF elem9999999
  , RNF elem2500
  ])
  -- Insert 1 element into a store of size N. No collisions.
  [
#ifdef BENCH_SMALL
    C.bgroup "insertLookup (Int) 01 100000"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.insertLookup 9999999 9999999 9999999) hs100000
      ]
    ] ,
#endif
    C.bgroup "insertLookup (Int) 01 200000"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.insertLookup 9999999 9999999 9999999) hs200000
      ]
    ]
#ifdef BENCH_SMALL
  , C.bgroup "lookup OO EQ (Int) 01 50000"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.lookupOOEQ 10000) hs50000
      ]
    ]
  , C.bgroup "lookup OO GE (Int) 01 50000 (500)"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.lookupOOGE 49500) hs50000
      ]
    ]
  , C.bgroup "lookup OM EQ (Int) 01 50000"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.lookupOMEQ 200) hs50000
      ]
    ]
  , C.bgroup "lookup OM GE (Int) 01 50000 (500)"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.lookupOMGE 9900) hs50000
      ]
    ]
  , C.bgroup "lookup MM EQ (Int) 01 50000"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.lookupMMEQ 200) hs50000
      ]
    ]
  , C.bgroup "lookup OO EQ (Int) 01 100000"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.lookupOOEQ 10000) hs100000
      ]
    ]
  , C.bgroup "lookup OO GE (Int) 01 100000 (500)"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.lookupOOGE 99500) hs100000
      ]
    ]
  , C.bgroup "lookup OM EQ (Int) 01 100000"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.lookupOMEQ 200) hs100000
      ]
    ]
  , C.bgroup "lookup OM GE (Int) 01 100000 (500)"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.lookupOMGE 19900) hs100000
      ]
    ]
  , C.bgroup "lookup MM EQ (Int) 01 100000"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.lookupMMEQ 200) hs100000
      ]
    ]
#endif
  , C.bgroup "lookup OO EQ (Int) 01 200000"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.lookupOOEQ 10000) hs200000
      ]
    ]
  , C.bgroup "lookup OO GE (Int) 01 200000 (500)"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.lookupOOGE 199500) hs200000
      ]
    ]
  , C.bgroup "lookup OM EQ (Int) 01 200000"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.lookupOMEQ 200) hs200000
      ]
    ]
  , C.bgroup "lookup OM GE (Int) 01 200000 (500)"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.lookupOMGE 39900) hs200000
      ]
    ]
  , C.bgroup "lookup MM EQ (Int) 01 200000"
    [ C.bcompare
      [ C.bench "HS" $ C.whnf (forceList . HS.B01.lookupMMEQ 200) hs200000
      ]
    ]
  ]

hs50000 :: HS.B01.HS
hs50000 = insertListHS elems50000 HS.B01.empty

hs100000 :: HS.B01.HS
hs100000 = insertListHS elems100000 HS.B01.empty

hs200000 :: HS.B01.HS
hs200000 = insertListHS elems200000 HS.B01.empty

insertListHS :: [C01] -> HS.B01.HS -> HS.B01.HS
insertListHS xs s0 = snd $!
  foldl' (\(n, acc) x -> if n == t
                           then rnf acc `seq` (0, HS.B01.insert x acc)
                           else (n + 1, HS.B01.insert x acc)
         ) (0 :: Int, s0) xs
  where t = 10000

