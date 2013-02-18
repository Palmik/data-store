{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Example03
where

--------------------------------------------------------------------------------
import           Control.Applicative
import qualified Control.Monad.State  as State
import qualified Control.Monad.Reader as Reader
--------------------------------------------------------------------------------
import           Data.SafeCopy (SafeCopy, base, deriveSafeCopy) 
import qualified Data.Acid
--------------------------------------------------------------------------------
import qualified Data.Store as S hiding (insert, update, updateWithRawKey)
import           Data.Store (M, O, (.:), (.:.), (:.)(..), (.<), (.<=), (.>), (.>=), (./=), (.==), (.&&), (.||))
import           Data.Store.Storable as S
--------------------------------------------------------------------------------

-- | DATA.STORE & DATA.ACID STUFF

data ContentStatus = ContentDraft | ContentPublished
    deriving (Eq, Ord, Show)
$(deriveSafeCopy 0 'base ''ContentStatus)

type ContentID = Int

data Content = Content
    { contentName :: String  
    , contentBody :: String  
    , contentTags :: [String]
    , contentRating :: Double
    , contentStatus :: ContentStatus
    }
$(deriveSafeCopy 0 'base ''Content)

instance Storable Content where
     type StoreKRS Content = O         :. O      :. O   :. M      :. O      :. O
     type StoreIRS Content = O         :. O      :. M   :. M      :. M      :. M
     type StoreTS  Content = ContentID :. String :. Int :. String :. Double :. ContentStatus

     key (Content cn cb cts cr cs) = 
         S.dimA .:
         S.dimO cn .:
         S.dimO (length $ words cb) .:
         S.dimM cts .:
         S.dimO cr .:.
         S.dimO cs

type ContentStore = S.Store (StoreKRS Content) (StoreIRS Content) (StoreTS Content) Content
type ContentStoreSelection = S.Selection (StoreKRS Content) (StoreIRS Content) (StoreTS Content)

sContentID :: S.N0
sContentID = S.n0

sContentName :: S.N1
sContentName = S.n1

sContentWordCount :: S.N2
sContentWordCount = S.n2

sContentTag :: S.N3
sContentTag = S.n3

sContentRating :: S.N4
sContentRating = S.n4

sContentStatus :: S.N5
sContentStatus = S.n5

data SiteData = SiteData
    { siteContents :: ContentStore
    }

emptySiteData :: SiteData
emptySiteData = SiteData
    { siteContents = S.empty
    }

createContent :: Content -> Data.Acid.Update SiteData (Maybe ContentID)
createContent c = do
    old@SiteData{..} <- State.get
    case S.insert c siteContents of
        Just (cid :. _, new) -> State.put old { siteContents = new } >> return (Just cid)
        _ -> return Nothing

updateContent :: ContentID -> Content -> Data.Acid.Update SiteData Bool
updateContent cid c = do
    old@SiteData{..} <- State.get
    case S.update (const $ Just c) (sContentID .== cid) siteContents of
        Just new -> State.put old { siteContents = new } >> return True
        _ -> return False

deleteContentByID :: ContentID -> Data.Acid.Update SiteData ()
deleteContentByID cid = do
    old@SiteData{..} <- State.get
    State.put old
        { siteContents = S.delete (sContentID .== cid) siteContents
        }

lookupContentByID :: ContentID -> Data.Acid.Query SiteData (Maybe Content)
lookupContentByID cid = 
    go . S.lookup (sContentID .== cid) <$> Reader.asks siteContents
    where
      go [(_, c)] = Just c
      go _ = Nothing

lookupContentByStatus :: ContentStatus -> Data.Acid.Query SiteData [(ContentID, Content)]
lookupContentByStatus cs = 
    go . S.lookup (sContentStatus .== cs) <$> Reader.asks siteContents
    where
      go = map (\(cid :. _, c) -> (cid, c))

-- | HAPPSTACK STUFF


