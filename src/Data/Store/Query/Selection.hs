{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}

module Data.Store.Query.Selection
( -- * Selection
  -- ** Selection Data Type
  I.Selection

  -- ** Selection Operators
, (.||)
, (.&&)
, (.<)
, (.>)
, (.<=)
, (.>=)
, (.==)
, everything
, nothing
) where

--------------------------------------------------------------------------------
import           Data.Proxy
--------------------------------------------------------------------------------
import qualified Data.Store.Internal                as I
import qualified Data.Store.Internal.Key            as I
--------------------------------------------------------------------------------

(.||) :: I.Selection tag k -> I.Selection tag k -> I.Selection tag k
(.||) = I.SelectOR
{-# INLINEABLE (.||) #-}
infixr 2 .||

(.&&) :: I.Selection tag k -> I.Selection tag k -> I.Selection tag k
(.&&) = I.SelectAND
{-# INLINEABLE (.&&) #-}
infixr 3 .&&

(.>) :: (I.ToInt n, Ord (I.DimensionType k n))
     => Proxy (tag, n)
     -> I.DimensionType k n
     -> I.Selection tag k
(.>) = I.SelectGT
{-# INLINEABLE (.>) #-}
infix 4 .>

(.<) :: (I.ToInt n, Ord (I.DimensionType k n))
     => Proxy (tag, n)
     -> I.DimensionType k n
     -> I.Selection tag k
(.<) = I.SelectLT
{-# INLINEABLE (.<) #-}
infix 4 .<

(.<=) :: (I.ToInt n, Ord (I.DimensionType k n))
     => Proxy (tag, n)
     -> I.DimensionType k n
     -> I.Selection tag k
(.<=) = I.SelectLTE
{-# INLINEABLE (.<=) #-}
infix 4 .<=

(.>=) :: (I.ToInt n, Ord (I.DimensionType k n))
     => Proxy (tag, n)
     -> I.DimensionType k n
     -> I.Selection tag k
(.>=) = I.SelectGTE
{-# INLINEABLE (.>=) #-}
infix 4 .>=

(.==) :: (I.ToInt n, Ord (I.DimensionType k n))
     => Proxy (tag, n)
     -> I.DimensionType k n
     -> I.Selection tag k
(.==) = I.SelectEQ
{-# INLINEABLE (.==) #-}
infix 4 .==

everything :: I.Selection tag k
everything = I.SelectALL
{-# INLINEABLE everything #-}

nothing :: I.Selection tag k
nothing = I.SelectNONE
{-# INLINEABLE nothing #-}


