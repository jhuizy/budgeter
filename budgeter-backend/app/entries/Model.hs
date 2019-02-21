module Entries.Model where

import           Accounts.Model

newtype EntryId = EntryId { unEntryId :: Int } deriving (Show)

data CreateEntry = CreateEntry
  { createEntryAccountId   :: AccountId
  , createEntryDescription :: Maybe String
  , createEntryAmount      :: Double
  , createEntryCategories  :: [String]
  , createEntryAttachments :: [String]
  } deriving (Show)

data Entry = Entry
  { entryEntryId     :: EntryId
  , entryAccountId   :: AccountId
  , entryDescription :: Maybe String
  , entryAmount      :: Double
  , entryCategories  :: [String]
  , entryAttachments :: [String]
  } deriving (Show)

