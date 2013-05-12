{-# LANGUAGE TypeOperators #-}

module DS.B01
where

--------------------------------------------------------------------------------
import Control.Lens
--------------------------------------------------------------------------------
import Common (C01(..))
--------------------------------------------------------------------------------
import qualified Data.Store as S
import           Data.Store (M, O, (.:), (.:.), (:.)(..), (.>=), (.==))
import           Data.Store.Lens
--------------------------------------------------------------------------------

size :: DS -> Int
size = S.size

data DSTag = DSTag

type DSTS  = Int :. Int :. Int
type DSKRS = O   :. O   :. M
type DSIRS = O   :. M   :. M

type DS    = S.Store DSTag DSKRS DSIRS DSTS C01
type DSKey = S.Key DSKRS DSTS
type DSRawKey = S.RawKey DSKRS DSTS
type DSSel = S.Selection DSTag DSKRS DSIRS DSTS

key :: C01 -> DSKey
key (C01 d1 d2 d3) = S.dimO d1 .: S.dimO d2 .:. S.dimM d3
{-# INLINE key #-}

sD1 :: (DSTag, S.N0)
sD1 = (DSTag, S.n0)
{-# INLINE sD1 #-}

sD2 :: (DSTag, S.N1)
sD2 = (DSTag, S.n1)
{-# INLINE sD2 #-}

sD3 :: (DSTag, S.N2)
sD3 = (DSTag, S.n2)
{-# INLINE sD3 #-}

insert :: C01 -> DS -> DS
insert e s = snd $! S.insert' (key e) e s

insertLookup :: Int -> Int -> Int -> DS -> [(S.RawKey DSKRS DSTS, C01)]
insertLookup d1 d2 d3 s =
  S.lookup (sD1 .== d1) new ++
  S.lookup (sD2 .== d2) new ++
  S.lookup (sD3 .== d3) new
  where new = snd $! S.insert' (key e) e s
        e   = C01 d1 d2 [d3]

insertUnsafe :: C01 -> DS -> DS
insertUnsafe e s = snd $! S.unsafeInsert (key e) e s

lookupOOEQ :: Int -> DS -> [(S.RawKey DSKRS DSTS, C01)]
lookupOOEQ x = S.lookup (sD1 .== x)

lookupOOEQLens :: Int -> DS -> [C01]
lookupOOEQLens x o = S.elements (o ^. with (sD1 .== x))

lookupOOGE :: Int -> DS -> [(S.RawKey DSKRS DSTS, C01)]
lookupOOGE x = S.lookup (sD1 .>= x)

lookupOOGELens :: Int -> DS -> [C01]
lookupOOGELens x o = S.elements (o ^. with (sD1 .>= x))

lookupOMEQ :: Int -> DS -> [(S.RawKey DSKRS DSTS, C01)]
lookupOMEQ x = S.lookup (sD2 .== x)

lookupOMEQLens :: Int -> DS -> [C01]
lookupOMEQLens x o = S.elements (o ^. with (sD2 .== x))

lookupOMGE :: Int -> DS -> [(S.RawKey DSKRS DSTS, C01)]
lookupOMGE x = S.lookup (sD2 .>= x)

lookupOMGELens :: Int -> DS -> [C01]
lookupOMGELens x o = S.elements (o ^. with (sD2 .>= x))

lookupMMEQ :: Int -> DS -> [(S.RawKey DSKRS DSTS, C01)]
lookupMMEQ x = S.lookup (sD3 .== x)

lookupMMEQLens :: Int -> DS -> [C01] 
lookupMMEQLens x o = S.elements (o ^. with (sD3 .== x))

empty :: DS
empty = S.empty

{-
gen  n = map (\x -> C01 x (x `div` 5) [x .. x + 5]) [0 .. (n - 1)]
gen' n = map (\x -> C01 (x `div` 2) (x `div` 5) [x .. x + 5]) [0 .. (n - 1)]
store = foldl' (flip DS.B01.insert) empty $! gen 8
newKey (C01 _ d2 d3) = key $ C01 0 d2 d3
storeU = S.updateWithKey' (\_ e -> Just (e, Just $ newKey e)) (sD1 .>= 0) store
-}
