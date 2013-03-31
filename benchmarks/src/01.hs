{-# LANGUAGE GADTs #-}

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

main :: IO ()
main = C.defaultMainWith C.defaultConfig force benchmarks
    where
      force :: C.Criterion ()
      force = liftIO . evaluate $ rnf
        [ rnf elems01x5000
        , rnf elems01x10000
        , rnf elems01x15000
        , rnf elems01x50000
        , rnf elems01x100000
        , rnf elems01x150000
        
        , rnf ds01x5000 
        , rnf ds01x10000

        , rnf ts01x5000
        , rnf ts01x10000
        ]


benchmarks :: [C.Benchmark]
benchmarks =
  [ C.bgroup "insert (Int) 01"
    [ C.bgroup "5000"
      [ C.bench "DS" $ C.nf (insertDS01 elems01x5000) DS.B01.empty
      , C.bench "DS (NC)" $ C.nf (insertDS01NC elems01x5000) DS.B01.empty
      , C.bench "TS" $ C.nf (insertTS01 elems01x5000) TS.B01.empty
      ]
    , C.bgroup "10000"
      [ C.bench "DS" $ C.nf (insertDS01 elems01x10000) DS.B01.empty
      , C.bench "DS (NC)" $ C.nf (insertDS01NC elems01x10000) DS.B01.empty
      , C.bench "TS" $ C.nf (insertTS01 elems01x10000) TS.B01.empty
      ]
    {-
    , C.bgroup "15000"
      [ C.bench "DS" $ C.nf (insertDS01 elems01x15000) DS.B01.empty
      , C.bench "DS (NC)" $ C.nf (insertDS01NC elems01x15000) DS.B01.empty
      , C.bench "TS" $ C.nf (insertTS01 elems01x15000) TS.B01.empty
      ]
    , C.bgroup "50000"
      [ C.bench "DS" $ C.nf (insertDS01 elems01x50000) DS.B01.empty
      , C.bench "DS (NC)" $ C.nf (insertDS01NC elems01x50000) DS.B01.empty
      , C.bench "TS" $ C.nf (insertTS01 elems01x50000) TS.B01.empty
      ]
    , C.bgroup "100000"
      [ C.bench "DS" $ C.nf (insertDS01 elems01x100000) DS.B01.empty
      , C.bench "DS (NC)" $ C.nf (insertDS01NC elems01x100000) DS.B01.empty
      , C.bench "TS" $ C.nf (insertTS01 elems01x100000) TS.B01.empty
      ]
    , C.bgroup "150000"
      [ C.bench "DS" $ C.nf (insertDS01 elems01x150000) DS.B01.empty
      , C.bench "DS (NC)" $ C.nf (insertDS01NC elems01x150000) DS.B01.empty
      , C.bench "TS" $ C.nf (insertTS01 elems01x150000) TS.B01.empty
      ]
    -}
    ]
  , C.bgroup "lookup (Int) 01"
    [ C.bgroup "5000"
      [ C.bench "DS" $ C.nf (lookupDS01 5000) ds01x5000
      , C.bench "TS" $ C.nf (lookupTS01 5000) ts01x5000
      ]
    , C.bgroup "10000"
      [ C.bench "DS" $ C.nf (lookupDS01 10000) ds01x10000
      , C.bench "TS" $ C.nf (lookupTS01 10000) ts01x10000
      ]
    ]
  ]

ds01x5000 :: DS.B01.DS
ds01x5000 = insertDS01 elems01x5000 DS.B01.empty

ds01x10000 :: DS.B01.DS
ds01x10000 = insertDS01 elems01x10000 DS.B01.empty

ts01x5000 :: TS.B01.TS
ts01x5000 = insertTS01 elems01x5000 TS.B01.empty

ts01x10000 :: TS.B01.TS
ts01x10000 = insertTS01 elems01x10000 TS.B01.empty

elems01x5000 :: [C01]
elems01x5000 = generate01 5000

elems01x10000 :: [C01]
elems01x10000 = generate01 10000

elems01x15000 :: [C01]
elems01x15000 = generate01 15000

elems01x50000 :: [C01]
elems01x50000 = generate01 50000

elems01x100000 :: [C01]
elems01x100000 = generate01 100000

elems01x150000 :: [C01]
elems01x150000 = generate01 150000

---

lookupDS01 :: Int -> DS.B01.DS -> [[(DS.B01.DSRawKey, C01)]] 
lookupDS01 size o =
  [ DS.B01.lookupOOEQ 0 o
  , DS.B01.lookupOOGE 0 o
  , DS.B01.lookupOOGE (size `div` 2) o
  , DS.B01.lookupOOGE (size `div` 3) o

  , DS.B01.lookupOMEQ 10 o
  , DS.B01.lookupOMGE ((size `div` 5) `div` 2) o
  , DS.B01.lookupOMGE ((size `div` 5) `div` 3) o
  
  , DS.B01.lookupMMEQ 10 o
  , DS.B01.lookupMMEQ 100 o
  ]

lookupTS01 :: Int -> TS.B01.TS -> [TS.B01.TS] 
lookupTS01 size o =
  [ TS.B01.lookupOOEQ 0 o
  , TS.B01.lookupOOGE 0 o
  , TS.B01.lookupOOGE (size `div` 2) o
  , TS.B01.lookupOOGE (size `div` 3) o

  , TS.B01.lookupOMEQ 10 o
  , TS.B01.lookupOMGE ((size `div` 5) `div` 2) o
  , TS.B01.lookupOMGE ((size `div` 5) `div` 3) o
  
  , TS.B01.lookupMMEQ 10 o
  , TS.B01.lookupMMEQ 100 o
  ]

---

insertDS01 :: [C01] -> DS.B01.DS -> DS.B01.DS
insertDS01 xs s0 = foldl' (flip DS.B01.insert) s0 xs

insertDS01NC :: [C01] -> DS.B01.DS -> DS.B01.DS
insertDS01NC xs s0 = foldl' (flip DS.B01.insertNC) s0 xs

insertTS01 :: [C01] -> TS.B01.TS -> TS.B01.TS
insertTS01 xs s0 = foldl' (flip TS.B01.insert) s0 xs

---

generate01 :: Int -> [C01]
generate01 n = map (\x -> C01 x (x `div` s) [x .. x + s]) [0 .. n - 1]
  where
    s = 5


