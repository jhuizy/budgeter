module Accounts.Model where

newtype AccountId = AccountId { unAccountId :: Int } deriving (Show)

data CreateAccount = CreateAccount
  { createAccountName :: String
  , createAccountDescription :: Maybe String
  } deriving (Show)

data Account = Account
  { accountId :: AccountId
  , accountName :: String
  , accountDescription :: Maybe String
  } deriving (Show)
