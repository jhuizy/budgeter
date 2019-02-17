{-# LANGUAGE OverloadedStrings #-}

module Accounts.DB where

import           Accounts.Model
import           Data.Maybe
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data AccountEntity = AccountEntity
  { accountEntityId          :: Int
  , accountEntityName        :: String
  , accountEntityDescription :: Maybe String
  }

instance FromRow AccountEntity where
  fromRow = AccountEntity <$> field <*> field <*> field

instance ToRow AccountEntity where
  toRow (AccountEntity id_ name desc) = toRow (id_, name, desc)

migration :: Connection -> IO ()
migration conn = execute_ conn "CREATE TABLE IF NOT EXISTS accounts (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, description TEXT)"

entityToAccount :: AccountEntity -> Account
entityToAccount (AccountEntity id name desc) = Account (AccountId id) name desc

list :: Connection -> IO [Account]
list conn = do
  entities <- query_ conn "SELECT id, name, description FROM accounts" :: IO [AccountEntity]
  return $ fmap entityToAccount entities

get :: Int -> Connection -> IO (Maybe Account)
get id conn = do
  entity <- query conn "SELECT id, name, description FROM accounts WHERE id = ?" (Only id)
  return $ entityToAccount <$> listToMaybe entity

create :: CreateAccount -> Connection -> IO Account
create (CreateAccount name desc) conn = do
  execute conn "INSERT INTO accounts (name, description) VALUES (?, ?)" (name, desc)
  id <- lastInsertRowId conn
  return $ Account (AccountId $ fromEnum id) name desc

