{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.Trans
import           Data.IORef
import           Data.Monoid
import           Data.Pool
import qualified Data.Text                      as T
import           Data.Time.Clock

import qualified Accounts.DB                    as Accounts
import           Accounts.Model
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

import Data.Char (toLower)
import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics

data CreateAccountDTO = CreateAccountDTO
    { caName        :: String
    , caDescription :: Maybe String
    } deriving (Generic)

data AccountDTO = AccountDTO
    { aId          :: Int
    , aName        :: String
    , aDescription :: Maybe String
    } deriving (Generic)

instance FromJSON CreateAccountDTO where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 2 }
instance ToJSON AccountDTO where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = map toLower . drop 1 }

data MySession = EmptySession

main :: IO ()
main = do
    let conn = PCConn $ ConnBuilder (open "sqlite.db") close (PoolCfg 1 1 nominalDay)
    spockCfg <- defaultSpockCfg EmptySession conn ()
    runSpock 8080 (spock spockCfg app)

app :: SpockM Connection MySession () ()
app = do
    runQuery Accounts.migration

    get root $
        text "Hello World!"
    post "accounts" $ do
        (CreateAccountDTO name desc) <- jsonBody'
        (Account (AccountId id) _ _) <- runQuery $ Accounts.create (CreateAccount name desc)
        Web.Spock.json $ AccountDTO id name desc
    get "accounts" $ do
        accounts <- runQuery Accounts.list
        Web.Spock.json $ f <$> accounts
        where
            f (Account (AccountId id) name desc) = AccountDTO id name desc

