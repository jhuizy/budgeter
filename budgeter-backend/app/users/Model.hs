module Users.Model where

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
mkUserEmail s = Just . UserEmail

mkUserHashedPassword :: String -> IO UserHashedPassword
mkUserHashedPassword = return . UserHashedPassword 

mkUserToken :: User -> IO UserToken
mkUserToken = return . UserToken . show