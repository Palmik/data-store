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
        , rnf elems01x50000
        ]


benchmarks :: [C.Benchmark]
benchmarks =
  [ C.bgroup "insert 5000"
    [ C.bgroup "01 (Int)"
      [ C.bench "DS" $ C.whnf (insertDS01 elems01x5000) DS.B01.empty
      , C.bench "TS" $ C.whnf (insertTS01 elems01x5000) TS.B01.empty
      ]
    ]
  , C.bgroup "insert 50000"
    [ C.bgroup "01 (Int)"
      [ C.bench "DS" $ C.whnf (insertDS01 elems01x50000) DS.B01.empty
      , C.bench "TS" $ C.whnf (insertTS01 elems01x50000) TS.B01.empty
      ]
    ]
  ]

elems01x5000 :: [C01]
elems01x5000 = generate01 5000

elems01x50000 :: [C01]
elems01x50000 = generate01 50000

insertDS01 :: [C01] -> DS.B01.DS -> DS.B01.DS
insertDS01 xs s0 = foldl' (flip DS.B01.insert) s0 xs

insertTS01 :: [C01] -> TS.B01.TS -> TS.B01.TS
insertTS01 xs s0 = foldl' (flip TS.B01.insert) s0 xs

generate01 :: Int -> [C01]
generate01 n = map (\x -> C01 x (x `div` s) [x .. x + s]) [0 .. n]
  where
    s = 5

