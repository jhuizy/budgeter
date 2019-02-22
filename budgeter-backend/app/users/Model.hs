{-# LANGUAGE OverloadedStrings #-}

module Users.Model where

import           Web.JWT

import qualified Data.Text             as T

newtype UserId = UserId { unUserId :: Int } deriving (Show)
newtype UserEmail = UserEmail { unUserEmail :: String } deriving (Show)
newtype UserHashedPassword = UserHashedPassword { unUserHashedPassword :: String } deriving (Show, Eq)
newtype UserToken = UserToken { unUserToken :: String } deriving (Show)

data CreateUser = CreateUser
  { createUserEmail    :: UserEmail
  , createUserPassword :: UserHashedPassword
  } deriving (Show)

data LoginUser = LoginUser
  { loginUserEmail    :: UserEmail
  , loginUserPassword :: UserHashedPassword
  } deriving (Show)

data User = User
  { userId       :: UserId
  , userEmail    :: UserEmail
  , userPassword :: UserHashedPassword
  } deriving (Show)

mkUserEmail :: String -> Maybe UserEmail
mkUserEmail = Just . UserEmail

mkUserHashedPassword :: String -> IO UserHashedPassword
mkUserHashedPassword = return . UserHashedPassword

mkUserToken :: User -> IO UserToken
mkUserToken user = return . UserToken . T.unpack $ encodeSigned HS256 s claims
    where
      s = secret . T.pack $ "secret"
      subject = stringOrURI . T.pack . show . userEmail $ user
      expirationDate = 234234042323
      claims = def { Web.JWT.exp = numericDate expirationDate, Web.JWT.sub = subject }
