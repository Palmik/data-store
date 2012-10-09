{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Store.Query.Selection
( QuerySelection
) where

--------------------------------------------------------------------------------
import qualified Data.Store.Internal                as I
--------------------------------------------------------------------------------

type QuerySelection tag k v = I.Query tag k v (I.Selection tag k) 

