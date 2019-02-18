{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Web.Spock
import           Web.Spock.Config

import           Control.Monad
import           Control.Monad.Trans
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           Data.Pool
import qualified Data.Text                      as T
import           Data.Time.Clock

import qualified Accounts.DB                    as Accounts
import           Accounts.Model

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import qualified Entries.DB                     as Entries
import           Entries.Model

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char                      (toLower)
import           GHC.Generics

import           Network.HTTP.Types.Status
import           Text.Blaze.Html                (Html, toHtml)
import           Text.Blaze.Html.Renderer.Utf8  (renderHtml)
import qualified Text.Blaze.Html5               as H

data CreateAccountDTO = CreateAccountDTO
    { caName        :: String
    , caDescription :: Maybe String
    } deriving (Generic)

data AccountDTO = AccountDTO
    { aId          :: Int
    , aName        :: String
    , aDescription :: Maybe String
    } deriving (Generic)

data CreateEntryDTO = CreateEntryDTO
    { ceDescription :: Maybe String
    , ceAmount      :: Double
    , ceAttachments :: [String]
    } deriving (Generic)

data EntryDTO = EntryDTO
    { eEntryId     :: Int
    , eAccountId   :: Int
    , eDescription :: Maybe String
    , eAmount      :: Double
    , eAttachments :: [String]
    } deriving (Generic)

instance FromJSON CreateAccountDTO where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 2 }
instance ToJSON AccountDTO where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = map toLower . drop 1 }
instance FromJSON CreateEntryDTO where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 2 }
instance ToJSON EntryDTO where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = map toLower . drop 1 }


data MySession = EmptySession

main :: IO ()
main = do
    let conn = PCConn $ ConnBuilder (open "sqlite.db") close (PoolCfg 1 1 nominalDay)
    spockCfg <- defaultSpockCfg EmptySession conn ()
    runSpock 8080 (spock spockCfg app)


blaze :: MonadIO m => Html -> ActionCtxT ctx m a
blaze = lazyBytes . renderHtml

viewEntries :: [Entry] -> H.Html
viewEntries entries = do
    H.table $ do
        H.thead $ do
            H.tr $ do
                H.th "ID"
                H.th "Description"
                H.th "Amount"
                H.th "Attachments"
        H.tbody $ do
            forM_ entries $ \entry -> do
                H.tr $ do
                    H.td . toHtml . unEntryId . entryEntryId $ entry
                    H.td . toHtml . (fromMaybe "-") . entryDescription $ entry
                    H.td . toHtml . entryAmount $ entry
                    H.td "-"

app :: SpockM Connection MySession () ()
app = do
    runQuery Accounts.migration
    runQuery Entries.migration

    get root $
        text "Hello World!"
    post "accounts" $ do
        (CreateAccountDTO name desc) <- jsonBody'
        account <- runQuery $ Accounts.create (CreateAccount name desc)
        Web.Spock.json $ mapAccount account
    get "accounts" $ do
        accounts <- runQuery Accounts.list
        Web.Spock.json $ mapAccount <$> accounts
    get ("accounts" <//> var <//> "entries.html") $ \accountId -> do
        maybeAccount <- runQuery $ Accounts.get accountId
        case maybeAccount of
            Just (Account actualAccountId _ _) -> do
                entries <- runQuery $ Entries.list actualAccountId
                blaze $ viewEntries entries
            Nothing -> do
                setStatus status404
                text "Account not found"
    get ("accounts" <//> var <//> "entries") $ \accountId -> do
        maybeAccount <- runQuery $ Accounts.get accountId
        case maybeAccount of
            Just (Account actualAccountId _ _) -> do
                entries <- runQuery $ Entries.list actualAccountId
                Web.Spock.json $ mapEntry <$> entries
            Nothing -> do
                setStatus status404
                text "Account not found"
    post ("accounts" <//> var <//> "entries") $ \accountId -> do
        (CreateEntryDTO desc amount attachments) <- jsonBody'
        maybeAccount <- runQuery $ Accounts.get accountId
        case maybeAccount of
            Just (Account actualAccountId _ _) -> do
                entry <- runQuery $ Entries.create $ CreateEntry actualAccountId desc amount attachments
                Web.Spock.json $ mapEntry entry
            Nothing -> do
                setStatus status404
                text "Account not found"
    where
        mapEntry (Entry (EntryId id) (AccountId accountId) desc amount attachments) = EntryDTO id accountId desc amount attachments
        mapAccount (Account (AccountId id) name desc) = AccountDTO id name desc







