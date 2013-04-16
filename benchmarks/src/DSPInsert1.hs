-- Profiling inserts.

module Main
where


import Control.DeepSeq (NFData(rnf))
import Data.Functor.Identity

import Common
import qualified Data.List
import qualified DS.B01
import qualified Data.Store as S
import qualified Data.Store.Internal.Type     as S
import qualified Data.Store.Internal.Function as S

type Store = DS.B01.DS
type Key   = S.IKey DS.B01.DSKRS DS.B01.DSTS

mkkey :: Int -> Int -> [Int] -> Key
mkkey oo om mm = S.KN (S.IKeyDimensionO oo)
                (S.KN (S.IKeyDimensionO om)
                (S.K1 (S.IKeyDimensionM mm)))

main :: IO ()
main = do
  return $! rnf ke5000
  return $! rnf $! go5000' 
  --return $! rnf $! go5000''

go5000'  = Data.List.foldl' (\acc (k, e) -> insert' k e acc) S.empty ke5000
go5000'' = Data.List.foldl' (\acc (k, e) -> insert'' k e acc) S.empty ke5000

insert' :: Key -> Int -> Store -> Store
insert' k e s = runIdentity $! S.indexInsertID' k e s

insert'' :: Key -> Int -> Store -> Store
insert'' k e s = runIdentity $! S.indexInsertID'' k e s

ke5000 :: [(Key, Int)]
ke5000 = gen01 0 5000

gen01 :: Int -> Int -> [(Key, Int)]
gen01 o n = map (\x -> (mkkey x (x `div` s) [x .. x + s], x)) [o .. (n + o) - 1]
  where
    s = 5

