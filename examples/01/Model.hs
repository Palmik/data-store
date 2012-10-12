{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE EmptyDataDecls #-}

module Model
( Article(..)
, ArticleID

, ArticleKey
, ArticleKeySpec
, ArticleStore
, ArticleStoreTag
, ArticleSelection

, sArticleID
, sArticleName
, sArticleBody
, sArticleTag

, insert
, lookup
, lookupByID
) where

--------------------------------------------------------------------------------
import           Prelude hiding (lookup)
--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.State
--------------------------------------------------------------------------------
import qualified Data.Text as TS

import qualified Data.Store                 as DS
import qualified Data.Store.Key             as DS
import           Data.Store.Key                   ((.:), (:.)(..))
import qualified Data.Store.Query.Selection as DS
import           Data.Store.Query.Selection       ((.==))

import           Data.Proxy
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- | FUNCTIONS

insert :: Article -> Model ArticleID
insert article = do
    (new, aid :. ()) <- DS.insert (articleKey article) article <$> get
    put new >> return aid

lookup :: ArticleSelection -> Model [(Article, ArticleID)]
lookup selection = tr . DS.lookup selection <$> get
    where
      tr = map (\(v, aid :. ()) -> (v, aid))

lookupByID :: ArticleID -> Model (Maybe Article)
lookupByID aid = tr . DS.lookup (sArticleID .== aid) <$> get
    where
      tr [(v, _)] = Just v
      tr _        = Nothing

--------------------------------------------------------------------------------
-- | TYPES

-- | Simple ADT representing an article.
data Article = Article
    { articleName :: TS.Text
    , articleBody :: TS.Text
    , articleTags :: [TS.Text]
    } deriving (Eq, Ord, Show)

newtype ArticleID = ArticleID Int deriving (Eq, Ord, Show)

instance DS.Auto ArticleID where
    initValue = ArticleID 1
    nextValue (ArticleID n) = ArticleID $ n + 1

-- | In your application you will most likely want to wrap this sort of
-- thing in newtype and provide your own restricted interface to the
-- monad.
type Model = State ArticleStore

--------------------------------------------------------------------------------
-- | BOILERPLATE 

-- | Type synonym for key key specification.
type ArticleKeySpec =
    (  (ArticleID, DS.DimAuto)
    :. (TS.Text, DS.Dim)
    :. (TS.Text, DS.Dim)
    :. (TS.Text, DS.Dim) :. DS.K0)

data ArticleStoreTag 
type ArticleStore    = DS.Store ArticleStoreTag ArticleKeySpec Article

type ArticleSelection = DS.Selection ArticleStoreTag ArticleKeySpec
type ArticleKey = DS.Key ArticleKeySpec

articleKey :: Article -> ArticleKey
articleKey (Article n b ts) = DS.dimA
                           .: DS.dimN [n]
                           .: DS.dimN [b]
                           .: DS.K1 (DS.dimN ts)

-- | Shortcut for selecting on article ID.
sArticleID :: Proxy (ArticleStoreTag, DS.N0)
sArticleID = Proxy

-- | Shortcut for selecting on article name.
sArticleName :: Proxy (ArticleStoreTag, DS.N1)
sArticleName = Proxy

-- | Shortcut for selecting on article body.
sArticleBody :: Proxy (ArticleStoreTag, DS.N2)
sArticleBody = Proxy

-- | Shortcut for selecting on article tags.
sArticleTag :: Proxy (ArticleStoreTag, DS.N3)
sArticleTag = Proxy

-- | BOILERPLATE 
--------------------------------------------------------------------------------


