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

data DSTag = DSTag

type DSTS  = Int :. Int :. Int
type DSKRS = O   :. O   :. M
type DSIRS = O   :. M   :. M

type DS    = S.Store DSTag DSKRS DSIRS DSTS C01
type DSKey = S.Key DSKRS DSTS
type DSSel = S.Selection DSTag DSKRS DSIRS DSTS

key :: C01 -> DSKey
key (C01 d1 d2 d3) = S.dimO d1 .: S.dimO d2 .:. S.dimM d3

sD1 :: (DSTag, S.N0)
sD1 = (DSTag, S.n0)

sD2 :: (DSTag, S.N1)
sD2 = (DSTag, S.n1)

sD3 :: (DSTag, S.N2)
sD3 = (DSTag, S.n2)

insert :: C01 -> DS -> DS
insert e s = snd $! S.insert' (key e) e s

insertNC :: C01 -> DS -> DS
insertNC e s = snd $! fromJust $! S.insert (key e) e s

empty :: DS
empty = S.empty

{-
gen  n = map (\x -> C01 x (x `div` 5) [x .. x + 5]) [0 .. (n - 1)]
gen' n = map (\x -> C01 (x `div` 2) (x `div` 5) [x .. x + 5]) [0 .. (n - 1)]
store = foldl' (flip DS.B01.insert) empty $! gen 8
newKey (C01 _ d2 d3) = key $ C01 0 d2 d3
storeU = S.updateWithKey' (\_ e -> Just (e, Just $ newKey e)) (sD1 .>= 0) store
-}
