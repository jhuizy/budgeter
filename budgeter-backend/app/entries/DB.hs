{-# LANGUAGE OverloadedStrings #-}

module Entries.DB where

import           Accounts.Model
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
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

instance FromRow EntryEntity where
  fromRow = EntryEntity <$> field <*> field <*> field <*> field

instance FromRow AttachmentEntity where
  fromRow = AttachmentEntity <$> field <*> field <*> field

instance ToRow EntryEntity where
  toRow (EntryEntity id_ accountId desc amount) = toRow (id_, accountId, desc, amount)

instance ToRow AttachmentEntity where
  toRow (AttachmentEntity id_ entryId url) = toRow(id_, entryId, url)

migration :: Connection -> IO ()
migration conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS entries (id INTEGER PRIMARY KEY AUTOINCREMENT, account_id INTEGER REFERENCES accounts(id) ON UPDATE CASCADE, description TEXT, amount REAL NOT NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS attachments (id INTEGER PRIMARY KEY AUTOINCREMENT, entry_id INTEGER REFERENCES entries(id) ON UPDATE CASCADE, url TEXT NOT NULL)"

create :: CreateEntry -> Connection -> IO Entry
create (CreateEntry (AccountId accountId) desc amount attachments) conn = do
  execute conn "INSERT INTO entries (account_id, description, amount) VALUES (?, ?, ?)" (accountId, desc, amount)
  id <- lastInsertRowId conn
  traverse (insertAttachment id) attachments
  return $ Entry (EntryId $ fromEnum id) (AccountId accountId) desc amount attachments
  where
    insertAttachment entryId attachment = execute conn "INSERT INTO attachments (entry_id, url) VALUES (?, ?)" (entryId, attachment)


list :: AccountId -> Connection -> IO [Entry]
list (AccountId accountId) conn = do
  entries <- query conn "SELECT id, account_id, description, amount FROM entries WHERE account_id = ?" (Only accountId) :: IO [EntryEntity]
  traverse fetchAttachments entries
  where
    fetchAttachments (EntryEntity entryId accountId description amount) = do
      attachments <- query conn "SELECT id, entry_id, url FROM attachments WHERE entry_id = ?" (Only entryId)
      let urls = attachmentEntityUrl <$> attachments
      return $ Entry (EntryId $ fromEnum entryId) (AccountId accountId) description amount urls

