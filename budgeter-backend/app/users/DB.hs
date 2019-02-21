{-# LANGUAGE OverloadedStrings #-}

module Users.DB where

import           Data.Maybe
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToField
import           Users.Model

data UserEntity = UserEntity
  { userEntityId       :: Int
  , userEntityEmail    :: String
  , userEntityPassword :: String
  } deriving (Show)

instance FromRow UserEntity where
  fromRow = UserEntity <$> field <*> field <*> field

instance ToRow UserEntity where
  toRow (UserEntity id_ email password) = toRow (id_, email, password)

migration :: Connection -> IO ()
migration conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, email TEXT NOT NULL, password TEXT NOT NULL)"

create :: CreateUser -> Connection -> IO User
create (CreateUser (UserEmail email) (UserHashedPassword password)) conn = do
  execute conn "INSERT INTO users (email, password) VALUES (?, ?)" (email, password)
  id <- lastInsertRowId conn
  return $ User (UserId $ fromEnum id) (UserEmail email) (UserHashedPassword password)

find :: UserEmail -> Connection -> IO (Maybe User)
find (UserEmail email) conn = do
  users <- query conn "SELECT id, email, password FROM users WHERE email = ?" (Only email) :: IO [UserEntity]
  return $ userEntityToUser <$> listToMaybe users
  where
    userEntityToUser (UserEntity id email password) = User (UserId id) (UserEmail email) (UserHashedPassword password)

getUserPassword :: UserEmail -> Connection -> IO (Maybe UserHashedPassword)
getUserPassword (UserEmail email) conn = do
  emails <- fmap fromOnly <$> (query conn "SELECT password FROM users WHERE email = ?" (Only email) :: IO [Only String])
  return $ UserHashedPassword <$> listToMaybe emails


