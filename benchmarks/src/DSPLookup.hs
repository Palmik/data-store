{-# LANGUAGE GADTs #-}

-- Profiling inserts.

module Main
where


import Control.DeepSeq (NFData(rnf))
import           Control.Exception.Base (evaluate)
import           Control.Monad.Trans (liftIO)

import Common
import qualified Data.List
import qualified Data.Map.Strict as Data.Map
import qualified DS.B01
import qualified Data.Store as S
import qualified Criterion.Config as C
import qualified Criterion.Main   as C

data RNF where
    RNF :: NFData a => a -> RNF

instance NFData RNF where
    rnf (RNF x) = rnf x

{-
main :: IO ()
main = C.defaultMainWith C.defaultConfig (liftIO . evaluate $ rnf
  [ RNF elems01x200000
  , RNF ds01x200000
  ])
  [ C.bench "lookup OO GE" $! C.nf (DS.B01.lookupOOGE 199500) ds01x200000
  , C.bench "lookup OM GE" $! C.nf (DS.B01.lookupOMGE 39900) ds01x200000
  ]

ds01x200000 :: DS.B01.DS
ds01x200000 = S.fromList' $! map (\x -> (DS.B01.key x, x)) elems01x200000

elems01x200000 :: [C01]
elems01x200000 = generate01 0 200000

-}

main :: IO ()
main = C.defaultMainWith C.defaultConfig (liftIO . evaluate $ rnf
  [ RNF elems
  , RNF elemsDS
  , RNF elemsMap
  , RNF elem9999999
  ])
  [ C.bench "insert DS" $! C.nf (DS.B01.insert elem9999999) elemsDS
  , C.bench "insert Map" $! C.nf (insertMap elem9999999) elemsMap
  ]

elem9999999 :: C01
elem9999999 = head $! generate01 9999999 1 

elemsDS :: DS.B01.DS
elemsDS = insertListDS elems DS.B01.empty

elemsMap :: Data.Map.Map Int C01
elemsMap = insertListMap elems Data.Map.empty

elems :: [C01]
elems = generate01 0 1600000

insertListDS :: [C01] -> DS.B01.DS -> DS.B01.DS
insertListDS xs s0 = Data.List.foldl' (flip DS.B01.insert) s0 xs

insertListMap :: [C01] -> Data.Map.Map Int C01 -> Data.Map.Map Int C01
insertListMap xs s0 = Data.List.foldl' (flip insertMap) s0 xs

insertMap :: C01 -> Data.Map.Map Int C01 -> Data.Map.Map Int C01
insertMap x@(C01 oo _ _) m = Data.Map.insert oo x m

generate01 :: Int -> Int -> [C01]
generate01 o n = map (\x -> C01 x (x `div` s) [x .. x + s]) [o .. (n + o) - 1]
  where
    s = 5

