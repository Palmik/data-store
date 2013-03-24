{-# LANGUAGE TypeOperators #-}

module DS.B01
where

--------------------------------------------------------------------------------
import Data.Maybe
--------------------------------------------------------------------------------
import Common (C01(..))
--------------------------------------------------------------------------------
import qualified Data.Store as S
import           Data.Store (M, O, (.:), (.:.), (:.)(..), (.<), (.<=), (.>), (.>=), (./=), (.==), (.&&), (.||))
--------------------------------------------------------------------------------

type DSTS  = Int :. Int :. Int
type DSKRS = O   :. O   :. M
type DSIRS = O   :. M   :. M

type DS    = S.Store DSKRS DSIRS DSTS C01
type DSKey = S.Key DSKRS DSTS
type DSSel = S.Selection DSKRS DSIRS DSTS

key :: C01 -> DSKey
key (C01 d1 d2 d3) = S.dimO d1 .: S.dimO d2 .:. S.dimM d3

sD1 :: S.N0
sD1 = S.n0

sD2 :: S.N1
sD2 = S.n1

sD3 :: S.N2
sD3 = S.n2

insert :: C01 -> DS -> DS
insert e s = snd $! S.insert' (key e) e s

insertNC :: C01 -> DS -> DS
insertNC e s = snd . fromJust $! S.insert (key e) e s

empty :: DS
empty = S.empty

