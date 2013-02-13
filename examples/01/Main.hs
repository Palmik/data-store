{-# LANGUAGE TypeOperators #-}

module Example01
where

--------------------------------------------------------------------------------
import           Control.Applicative
import qualified Control.Monad.State as State
--------------------------------------------------------------------------------
import qualified Data.Store as S
import           Data.Store (M, O, (.:), (.:.), (:.)(..), (.<), (.<=), (.>), (.>=), (./=), (.==), (.&&), (.||))
--------------------------------------------------------------------------------

data Content = Content
    { contentName :: String  
    , contentBody :: String  
    , contentTags :: [String]
    , contentRating :: Double
    }

type ContentID = Int

-- Content has one ID, only one content can have a given ID.
-- Content has one name, only one content can have a given name.
-- Content has one body, many contents can have the same content.
-- Content has many tags, many contents can have the same tag.
-- Content has one rating, many contents can have the same rating.

type ContentStoreTS  = ContentID :. String :. String :. String :. Double
type ContentStoreKRS = O         :. O      :. O      :. M      :. O
type ContentStoreIRS = O         :. O      :. M      :. M      :. M
type ContentStore = S.Store ContentStoreKRS ContentStoreIRS ContentStoreTS Content
type ContentStoreKey = S.Key ContentStoreKRS ContentStoreTS
type ContentStoreSelection = S.Selection ContentStoreKRS ContentStoreIRS ContentStoreTS

sContentID :: S.N0
sContentID = S.n0

sContentName :: S.N1
sContentName = S.n1

sContentBody :: S.N2
sContentBody = S.n2

sContentTag :: S.N3
sContentTag = S.n3

sContentRating :: S.N4
sContentRating = S.n4

contentKey :: Content -> ContentStoreKey
contentKey (Content cn cb ct cr) =
   S.dimA .: S.dimO cn .: S.dimO cb .: S.dimM ct .:. S.dimO cr

type Model = State.State ContentStore

insertContent :: Content -> Model (Maybe ContentID)
insertContent c = do
    res <- S.insert (contentKey c) c <$> State.get
    case res of
        Just (cid :. _, new) -> State.put new >> return (Just cid)
        _ -> return Nothing

lookupBySelection :: ContentStoreSelection -> Model [Content]
lookupBySelection sel = go . S.lookup sel <$> State.get
    where
      go = map snd 

lookupByID :: ContentID -> Model (Maybe Content)
lookupByID cid = go . S.lookup (sContentID .== cid) <$> State.get
    where
      go [(_, c)] = Just c
      go _        = Nothing

deleteByID :: ContentID -> Model ()
deleteByID cid = deleteBySelection (sContentID .== cid) 

deleteBySelection :: ContentStoreSelection -> Model ()
deleteBySelection sel = State.modify $ S.delete sel

