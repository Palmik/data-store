{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Example03
where

--------------------------------------------------------------------------------
import           Control.Applicative
import qualified Control.Monad.State  as State
import qualified Control.Monad.Reader as Reader
import           Control.Exception (bracket)
--------------------------------------------------------------------------------
import           Data.List (intercalate)
import           Data.Char (toLower)
import           Data.Data (Data, Typeable)
import           Data.SafeCopy (SafeCopy, base, deriveSafeCopy) 
import qualified Data.Acid
--------------------------------------------------------------------------------
import qualified Data.Store as S hiding (insert, update, updateWithRawKey)
import           Data.Store (M, O, (.:), (.:.), (:.)(..), (.<), (.<=), (.>), (.>=), (./=), (.==), (.&&), (.||))
import           Data.Store.Storable as S
--------------------------------------------------------------------------------

-- | DATA.STORE & DATA.ACID STUFF

data ContentStatus = ContentDraft | ContentPublished
    deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ContentStatus)

type ContentID = Int

data Content = Content
    { contentName :: String  
    , contentBody :: String  
    , contentTags :: [String]
    , contentRating :: Double
    , contentStatus :: ContentStatus
    } deriving (Data, Typeable)
$(deriveSafeCopy 0 'base ''Content)

instance Show Content where
    show (Content cn cb cts cr cs) = concat
      [ "\n[NAME]:\n", cn, " ", show cs
      , "\n[BODY]:\n", cb
      , "\n[TAGS]: ", intercalate ", " cts
      ]

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
    } deriving (Typeable)
$(deriveSafeCopy 0 'base ''SiteData)

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

$(Data.Acid.makeAcidic ''SiteData
    [ 'createContent
    , 'updateContent
    , 'deleteContentByID
    , 'lookupContentByID
    , 'lookupContentByStatus
    ])

-- | INTERACTIVE MAIN STUFF

data Command = CCreate Content
             | CUpdate ContentID Content
             | CLookupByID ContentID
             | CQuit

main :: IO ()
main =
   bracket (Data.Acid.openLocalState emptySiteData) Data.Acid.closeAcidState run

run :: Data.Acid.AcidState SiteData -> IO ()
run db = do
    putStr "λ> "
    mcommand <- getCommand
    case mcommand of
        Just command -> handleCommand command
        Nothing -> putStrLn "Such command does not exist." >> run db

    where
      handleCommand (CQuit) = putStrLn "Good bye... cruel world." 
      handleCommand (CCreate c) = do
        mcid <- Data.Acid.update db (CreateContent c)
        case mcid of
          Just cid -> putStrLn $ "Content created; ID " ++ show cid
          Nothing  -> putStrLn $ "ERROR! Content not created (likely because of collision)."
        run db

      handleCommand (CUpdate cid c) = do
        succ <- Data.Acid.update db (UpdateContent cid c)
        if succ
           then putStrLn $ "Content updated; ID " ++ show cid
           else putStrLn $ "ERROR! Content not updated (likely because of collision)."
        run db      

      handleCommand (CLookupByID cid) = do
        mc <- Data.Acid.query db (LookupContentByID cid)
        case mc of
          Just c  -> print c
          Nothing -> putStrLn "ERROR! No content matches the given criteria."
        run db

getCommand :: IO (Maybe Command)
getCommand = do
    rc <- parseRawCommand . map toLower <$> getLine 
    case rc of
      Just ("quit", _, _) -> return $ Just CQuit
      Just ("create", r, rr) -> handleCreate r rr
      Just ("update", r, rr) -> handleUpdate r rr
      Just ("lookupbyid", r, rr) -> handleLookupByID r rr
      _ -> return Nothing
    where
      parseRawCommand str =
        case words str of
          [] -> Nothing
          (c:rest) -> Just (c, rest, unwords rest)

      handleCreate :: [String] -> String -> IO (Maybe Command)
      handleCreate _ _ = Just . CCreate <$> getContent

      handleUpdate :: [String] -> String -> IO (Maybe Command)
      handleUpdate [scid] _ =
        case safeRead scid of
          Just cid -> Just . CUpdate cid <$> getContent
          Nothing  -> return Nothing
      
      handleLookupByID :: [String] -> String -> IO (Maybe Command)
      handleLookupByID [scid] _ =
        case safeRead scid of
          Just cid -> return . Just $ CLookupByID cid
          Nothing  -> return Nothing

      getContent :: IO Content
      getContent = do
          putStrLn "[NAME]: "
          cname <- getLine
          putStrLn "\n[BODY]: "
          cbody <- getLine
          putStrLn "\n[TAGS]: "
          ctags <- words . map toLower <$> getLine
          return Content
            { contentName = cname
            , contentBody = cbody
            , contentTags = ctags
            , contentRating = 0
            , contentStatus = ContentDraft
            } 

safeRead :: (Read a) => String -> Maybe a
safeRead s =
    case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

