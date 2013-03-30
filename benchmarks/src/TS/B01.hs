{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module TS.B01
where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.DeepSeq (NFData(..))
--------------------------------------------------------------------------------
import Data.Maybe
import qualified Data.IntSet
--------------------------------------------------------------------------------
import Common (C01(..))
--------------------------------------------------------------------------------
import qualified Data.Table as T
--------------------------------------------------------------------------------

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
    C01IX <$> f D1 d1
          <*> f D2 d2
          <*> f D3 d3

  ixTab (C01IX x _ _) D1 = x
  ixTab (C01IX _ x _) D2 = x
  ixTab (C01IX _ _ x) D3 = x

instance NFData (T.Tab C01 (T.AnIndex C01)) where
    rnf (C01IX x1 x2 x3) = rnf x1 `seq` rnf x2 `seq` rnf x3

type TS = T.Table C01

insert :: C01 -> TS -> TS
insert = T.insert

empty :: TS
empty = T.empty

