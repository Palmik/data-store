module Data.Store.Query
( runQuery
, I.Query
) where

--------------------------------------------------------------------------------
import           Control.Monad.Reader
--------------------------------------------------------------------------------
import qualified Data.Store.Internal as I
--------------------------------------------------------------------------------

runQuery :: I.Query tag k v a -> I.Store tag k v -> a
runQuery = runReader . I.unQuery


