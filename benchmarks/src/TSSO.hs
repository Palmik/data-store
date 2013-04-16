module Main
where

import Control.DeepSeq (NFData(rnf))
import Common
import qualified TS.B01
import qualified Data.Table as T

main :: IO ()
main = do
  return $! rnf elements
  return $! rnf test

test :: TS.B01.TS
test = T.fromList elements

elements :: [C01]
elements = map (\x -> C01 x (x `div` s) [x .. x + s]) [0 .. 40000]
  where
    s = 5

