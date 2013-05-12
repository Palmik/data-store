{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} 

module TS.B01
where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Lens
--------------------------------------------------------------------------------
import qualified Data.IntSet
import           Data.Foldable (toList)
import           Data.Functor.Identity
--------------------------------------------------------------------------------
import Common (C01(..))
--------------------------------------------------------------------------------
import qualified Data.Table as T
--------------------------------------------------------------------------------

size :: T.Table t -> Int
size = T.count

instance T.Tabular C01 where
  type PKT C01 = Int
  data Key k C01 b where
    D1 :: T.Key T.Primary         C01 Int
    D2 :: T.Key T.SupplementalInt C01 Int
    D3 :: T.Key T.InvertedInt     C01 Data.IntSet.IntSet
  data Tab C01 i = C01IX
    { ixd1 :: i T.Primary         Int
    , ixd2 :: i T.SupplementalInt Int
    , ixd3 :: i T.InvertedInt     Data.IntSet.IntSet
    }

  fetch D1 (C01 x _ _) = x
  fetch D2 (C01 _ x _) = x
  fetch D3 (C01 _ _ x) = Data.IntSet.fromList x

  primary = D1
  primarily D1 r = r

  mkTab f =
    C01IX <$> f D1
          <*> f D2
          <*> f D3
  
  forTab (C01IX d1 d2 d3) f =
    let x1 = f D1 d1
        x2 = f D2 d2
        x3 = f D3 d3
    in
    C01IX <$> seq x1 x1 
          <*> seq x2 x2
          <*> seq x3 x3

  ixTab (C01IX x _ _) D1 = x
  ixTab (C01IX _ x _) D2 = x
  ixTab (C01IX _ _ x) D3 = x

instance NFData (T.Tab C01 (T.AnIndex C01)) where
    rnf (C01IX x1 x2 x3) = rnf x1 `seq` rnf x2 `seq` rnf x3

type TS = T.Table C01

insert :: C01 -> TS -> TS
insert = T.insert

insertLookup :: Int -> Int -> Int -> TS -> [C01]
insertLookup d1 d2 d3 s = 
  toList (new ^. T.with D1 (==) d1) ++
  toList (new ^. T.with D2 (==) d2) ++
  toList (new ^. T.withAny D3 [d3])
  where new = T.insert (C01 d1 d2 [d3]) s

lookupOOEQ :: Int -> TS -> [C01]
lookupOOEQ x o = toList (o ^. T.with D1 (==) x)

lookupOOGE :: Int -> TS -> [C01]
lookupOOGE x o = toList (o ^. T.with D1 (>=) x)

lookupOMEQ :: Int -> TS -> [C01]
lookupOMEQ x o = toList (o ^. T.with D2 (==) x)

lookupOMGE :: Int -> TS -> [C01]
lookupOMGE x o = toList (o ^. T.with D2 (>=) x)

lookupMMEQ :: Int -> TS -> [C01]
lookupMMEQ x o = toList (o ^. T.withAny D3 [x])

empty :: TS
empty = T.empty

force :: TS -> ()
force T.EmptyTable = ()
force (T.Table tab) = seq go ()
  where
    go :: T.Tab C01 (T.AnIndex C01)
    go = runIdentity $! T.forTab tab (\_ i -> Identity $! case i of
      T.PrimaryMap m -> m `seq` T.PrimaryMap m
      T.CandidateMap m -> m `seq` T.CandidateMap m
      T.CandidateIntMap m -> m `seq` T.CandidateIntMap m
      T.CandidateHashMap m -> m `seq` T.CandidateHashMap m
      T.SupplementalMap m -> m `seq` T.SupplementalMap m
      T.SupplementalIntMap m -> m `seq` T.SupplementalIntMap m
      T.SupplementalHashMap m -> m `seq` T.SupplementalHashMap m
      T.InvertedMap m -> m `seq` T.InvertedMap m
      T.InvertedIntMap m -> m `seq` T.InvertedIntMap m
      T.InvertedHashMap m -> m `seq` T.InvertedHashMap m)

