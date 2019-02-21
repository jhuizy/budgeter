{-# LANGUAGE OverloadedStrings #-}

module Entries.DB
  ( migration
  , create
  , list
  , listByCategory
  ) where

import           Accounts.Model
import           Control.Monad
import           Data.Maybe
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToField
import           Entries.Model

data EntryEntity = EntryEntity
  { entryEntityId          :: Int
  , entryEntityAccountId   :: Int
  , entryEntityDescription :: Maybe String
  , entryEntityAmount      :: Double
  }

data AttachmentEntity = AttachmentEntity
  { attachmentEntityId      :: Int
  , attachmentEntityEntryId :: Int
  , attachmentEntityUrl     :: String
  }

data CategoryEntity = CategoryEntity
  { categoryEntityId    :: Int
  , categoryEntityLabel :: String
  }

data CategoryEntryEntity = CategoryEntryEntity
  { categoryEntryEntityCategoryId :: Int
  , categoryEntryEntityEntryId    :: Int
  }

instance FromRow EntryEntity where
  fromRow = EntryEntity <$> field <*> field <*> field <*> field

instance FromRow AttachmentEntity where
  fromRow = AttachmentEntity <$> field <*> field <*> field

instance FromRow CategoryEntity where
  fromRow = CategoryEntity <$> field <*> field

instance FromRow CategoryEntryEntity where
  fromRow = CategoryEntryEntity <$> field <*> field

instance ToRow EntryEntity where
  toRow (EntryEntity id_ accountId desc amount) = toRow (id_, accountId, desc, amount)

instance ToRow AttachmentEntity where
  toRow (AttachmentEntity id_ entryId url) = toRow( id_, entryId, url)

instance ToRow CategoryEntity where
  toRow (CategoryEntity id_ label) = toRow (id_, label)

instance ToRow CategoryEntryEntity where
  toRow (CategoryEntryEntity categoryId entryId) = toRow (categoryId, entryId)

migration :: Connection -> IO ()
migration conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS entries (id INTEGER PRIMARY KEY AUTOINCREMENT, account_id INTEGER REFERENCES accounts(id) ON UPDATE CASCADE, description TEXT, amount REAL NOT NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS attachments (id INTEGER PRIMARY KEY AUTOINCREMENT, entry_id INTEGER REFERENCES entries(id) ON UPDATE CASCADE, url TEXT NOT NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS categories (id INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT NOT NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS category_entries (category_id INTEGER REFERENCES categories(id) ON UPDATE CASCADE, entry_id INTEGER REFERENCES entries(id) ON UPDATE CASCADE)"

create :: CreateEntry -> Connection -> IO Entry
create (CreateEntry (AccountId accountId) desc amount categories attachments) conn = do
  execute conn "INSERT INTO entries (account_id, description, amount) VALUES (?, ?, ?)" (accountId, desc, amount)
  id <- lastInsertRowId conn
  categoryIds <- (fmap . fmap) categoryEntityId $ traverse (flip getOrCreateLabel conn) categories
  traverse (insertCategoryEntryJoin id) categoryIds
  traverse (insertAttachment id) attachments
  return $ Entry (EntryId $ fromEnum id) (AccountId accountId) desc amount categories attachments
  where
    insertAttachment entryId attachment = execute conn "INSERT INTO attachments (entry_id, url) VALUES (?, ?)" (entryId, attachment)
    insertCategoryEntryJoin entryId categoryId = execute conn "INSERT INTO category_entries (category_id, entry_id) VALUES (?, ?)" (categoryId, entryId)

list :: AccountId -> Connection -> IO [Entry]
list (AccountId accountId) conn = do
  entries <- query conn "SELECT id, account_id, description, amount FROM entries WHERE account_id = ?" (Only accountId) :: IO [EntryEntity]
  forM entries $ fetchEntry conn

fetchAttachments :: Connection -> Int -> IO [String]
fetchAttachments conn entryId = fmap fromOnly <$> (query conn "SELECT url FROM attachments WHERE entry_id = ?" (Only entryId) :: IO [Only String])

fetchCategories :: Connection -> Int -> IO [String]
fetchCategories conn entryId = fmap fromOnly <$> (query conn "SELECT label FROM categories INNER JOIN category_entries ON category_id = id WHERE entry_id = ?" (Only entryId) :: IO [Only String])

fetchEntry :: Connection -> EntryEntity -> IO Entry
fetchEntry conn (EntryEntity entryId accountId description amount) = do
  attachments <- fetchAttachments conn entryId
  categories <- fetchCategories conn entryId
  return $ Entry (EntryId entryId) (AccountId accountId) description amount categories attachments

listByCategory :: String -> Connection -> IO [Entry]
listByCategory label conn = do
  categories <- query conn "SELECT id FROM categories WHERE label = ?" (Only label) :: IO [Only Int]
  case listToMaybe categories of
    Nothing -> return []
    Just categoryId -> do
      entryIds <- query conn "SELECT entry_id FROM category_entries WHERE category_id = ?" categoryId :: IO [Only Int]
      entryEntities <- forM entryIds $ \entryId -> query conn "SELECT id, account_id, description, amount FROM entries WHERE id = ?" entryId :: IO [EntryEntity]
      forM (concat entryEntities) $ fetchEntry conn

getOrCreateLabel :: String -> Connection -> IO CategoryEntity
getOrCreateLabel label conn = do
  ids <- query conn "SELECT id FROM categories WHERE label = ?" (Only label) :: IO [Only Int]
  id <- case listToMaybe ids of
    Just (Only id) -> return id
    Nothing -> do
      execute conn "INSERT INTO categories (label) VALUES (?)" (Only label)
      fromEnum <$> lastInsertRowId conn
  return $ CategoryEntity id label

