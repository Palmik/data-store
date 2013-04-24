{-# LANGUAGE GADTs #-}

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
    RNF elems100000
  , RNF elems200000

  , RNF hs100000
  , RNF hs200000
  ])
  -- Insert 1 element into a store of size N. No collisions.
  [
    C.bgroup "lookup OO EQ (Int) 01 200000"
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

