{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Common.DB where

import           Accounts.Model
import           Data.Maybe
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import  Data.Text

-- TODO break up into smaller parts
-- class DbCrud id c r where
--   migration :: Connection -> IO ()
--   create :: c -> Connection -> IO r
--   list :: id -> Connection -> IO [r]
--   get :: id -> Connection -> IO (Maybe r)
--   update :: id -> c -> Connection -> IO r
--   delete :: id -> Connection -> IO ()

data SqlField = SqlField
  { sqlFieldName :: Text
  , sqlFieldType :: Text
  , sqlFieldNullable :: Bool
  } deriving (Show, Eq)

data SqlPkField = SqlPkField
  { sqlPkFieldSqlField :: SqlField
  , sqlPkFieldOptions :: Text
  } deriving (Show, Eq)

data SqlTable = SqlTable
  { sqlTableName :: Text
  , sqlTablePk :: SqlPkField
  , sqlTableFields :: [SqlField]
  } deriving (Show, Eq)

data Account = Account
  { accountName :: String
  , accountDesc :: Maybe String 
  } deriving (Show, Eq)

accountSqlTable :: SqlTable
accountSqlTable = SqlTable name pk fields 
  where
    name = "accounts"
    pk = SqlPkField (SqlField "id" "INTEGER" False) "AUTOINCREMENT"
    fields = 
      [ SqlField "name" "TEXT" False
      , SqlField "desc" "TEXT" True
      ]

getDDL :: SqlTable -> Text -> Text
getDDL t = Data.Text.unwords
  [ "SELECT" 
  , fields
  , "FROM"
  , table
  , "WHERE"
  , idPredicate
  , ";"
  ]
  where
    fields = undefined
    table = sqlTableName t
    idPredicate = 

migrationDDL :: SqlTable -> Text
migrationDDL t = Data.Text.unwords
  [ "CREATE TALBE IF NOT EXISTS"
  , sqlTableName t
  , "("
  , sqlFieldName . sqlPkFieldSqlField . sqlTablePk $ t
  , sqlFieldType . sqlPkFieldSqlField . sqlTablePk $ t
  , "PRIMARY KEY"
  , sqlPkFieldOptions . sqlTablePk $ t
  , ","
  , fieldsDDL
  , ");"
  ]
 where
  toFieldDDL f = Data.Text.unwords
    [ sqlFieldName f
    , sqlFieldType f
    , if sqlFieldNullable f then "" else "NOT NULL"
    ]
  fieldsDDL = intercalate "," (toFieldDDL <$> sqlTableFields t)

migration :: SqlTable -> Connection -> IO ()
migration t conn = execute_ conn $ Query $ migrationDDL t
-- migration :: Connection -> IO ()
-- migration conn = execute_ conn "CREATE TABLE IF NOT EXISTS accounts (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, description TEXT)"

-- entityToAccount :: AccountEntity -> Account
-- entityToAccount (AccountEntity id name desc) = Account (AccountId id) name desc

-- list :: Connection -> IO [Account]
-- list conn = do
--   entities <- query_ conn "SELECT id, name, description FROM accounts" :: IO [AccountEntity]
--   return $ fmap entityToAccount entities

-- get :: Int -> Connection -> IO (Maybe Account)
-- get id conn = do
--   entity <- query conn "SELECT id, name, description FROM accounts WHERE id = ?" (Only id)
--   return $ entityToAccount <$> listToMaybe entity

-- create :: CreateAccount -> Connection -> IO Account
-- create (CreateAccount name desc) conn = do
--   execute conn "INSERT INTO accounts (name, description) VALUES (?, ?)" (name, desc)
--   id <- lastInsertRowId conn
--   return $ Account (AccountId $ fromEnum id) name desc

