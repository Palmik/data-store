module Data.Store.Query
( runQuery
, queryStore
, I.Query
) where

--------------------------------------------------------------------------------
import           Control.Monad.Reader
--------------------------------------------------------------------------------
import qualified Data.Store.Internal as I
--------------------------------------------------------------------------------

runQuery :: I.Query tag k v a -> I.Store tag k v -> a
runQuery = runReader . I.unQuery

queryStore :: I.Query tag k v (I.Store tag k v)
queryStore = I.Query ask

