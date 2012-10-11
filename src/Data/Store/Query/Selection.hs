{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}

module Data.Store.Query.Selection
( -- * Selection
  -- ** Selection Operators
  (.||)
, (.&&)
, (.<)
, (.>)
, (.<=)
, (.>=)
, (.==)
) where

--------------------------------------------------------------------------------
import           Data.Proxy
--------------------------------------------------------------------------------
import qualified Data.Store.Internal                as I
import qualified Data.Store.Internal.Key            as I
--------------------------------------------------------------------------------

(.||) :: I.Selection tag k -> I.Selection tag k -> I.Selection tag k
(.||) = I.SelectUnion
{-# INLINEABLE (.||) #-}
infixr 3 .||

(.&&) :: I.Selection tag k -> I.Selection tag k -> I.Selection tag k
(.&&) = I.SelectIntersection
{-# INLINEABLE (.&&) #-}
infixr 3 .&&

(.>) :: (I.ToInt n, Ord (I.DimensionType k n))
     => Proxy (tag, n)
     -> I.DimensionType k n
     -> I.Selection tag k
(.>) = I.SelectGT
{-# INLINEABLE (.>) #-}
infixr 3 .>

(.<) :: (I.ToInt n, Ord (I.DimensionType k n))
     => Proxy (tag, n)
     -> I.DimensionType k n
     -> I.Selection tag k
(.<) = I.SelectLT
{-# INLINEABLE (.<) #-}
infixr 3 .<

(.<=) :: (I.ToInt n, Ord (I.DimensionType k n))
     => Proxy (tag, n)
     -> I.DimensionType k n
     -> I.Selection tag k
(.<=) = I.SelectLTE
{-# INLINEABLE (.<=) #-}
infixr 3 .<=

(.>=) :: (I.ToInt n, Ord (I.DimensionType k n))
     => Proxy (tag, n)
     -> I.DimensionType k n
     -> I.Selection tag k
(.>=) = I.SelectGTE
{-# INLINEABLE (.>=) #-}
infixr 3 .>=

(.==) :: (I.ToInt n, Ord (I.DimensionType k n), (I.DimensionType k n) ~ d)
     => Proxy (tag, n)
     -> d
     -> I.Selection tag k
(.==) = I.SelectEQ
{-# INLINEABLE (.==) #-}
infixr 3 .==

