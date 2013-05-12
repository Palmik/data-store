{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} 

-- #define BENCH_ESSENTIALS

module Common
where

--------------------------------------------------------------------------------
import           Control.DeepSeq (NFData(..))
import           Data.Data
--------------------------------------------------------------------------------

data C01 = C01 
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
                   ![Int]
    deriving (Eq, Ord, Show, Typeable)

instance NFData C01 where
    rnf (C01 x y z) = rnf x `seq` rnf y `seq` rnf z

elem9999999 :: C01
elem9999999 = head $! generate 9999999 1 

elem2500 :: C01
elem2500 = head $! generate 2500 1 

elems5000x5000 :: [C01]
elems5000x5000 = generate 5000 5000

elems10000x5000 :: [C01]
elems10000x5000 = generate 10000 5000

-- #ifdef BENCH_SMALL
elems10000 :: [C01]
elems10000 = generate 0 10000

elems20000 :: [C01]
elems20000 = generate 0 20000

-- #else
elems50000 :: [C01]
elems50000 = generate 0 50000

elems100000 :: [C01]
elems100000 = generate 0 100000

elems200000 :: [C01]
elems200000 = generate 0 200000

elems400000 :: [C01]
elems400000 = generate 0 400000

elems800000 :: [C01]
elems800000 = generate 0 800000
-- #endif

generate :: Int -> Int -> [C01]
generate o n = map (\x -> C01 x (x `div` s) [x .. x + s]) [o .. (n + o) - 1]
  where
    s = 5

forceList :: [a] -> ()
forceList ll = seq (go ll) ()
  where
    go [] = ()
    go (_:xs) = go xs


