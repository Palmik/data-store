{-# LANGUAGE TypeOperators #-}

module Example01
where

import qualified Data.Store as S
import           Data.Store (M, O, (.:), (.:.), (:.), (.<), (.<=), (.>), (.>=), (./=), (.==), (.&&), (.||))

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
type ContentStore = S.Store ContentStoreKRS ContentStoreIRS ContentStoreTS
type ContentStoreKey = S.Key ContentStoreKRS ContentStoreTS

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

makeKey :: Content -> ContentStoreKey
makeKey (Content cn cb ct cr) =
   S.dimA .: S.dimO cn .: S.dimO cb .: S.dimM ct .:. S.dimO cr

