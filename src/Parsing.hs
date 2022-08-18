{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Data.Aeson 
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Clock (UTCTime)

data UserList = UserList { users' :: [User] }

instance ToJSON UserList where
  toJSON (UserList users) = object ["user_list" .= users]

data User = User {id :: Int
                 ,name :: String
                 ,login :: String
                 ,password :: String
                 ,date :: UTCTime
                 ,admin :: Bool
                 }

instance ToJSON User where
  toJSON (User id' name' login' password' date' admin') = object ["id" .= id'
                                                    ,"name" .= name'
                                                    ,"login" .= login']
     
data Body = Body {name1 :: String
                 ,login1 :: String
                 ,password1 :: String}

instance FromJSON Body where
  parseJSON (Object body) = Body <$> body .: "name"
                                 <*> body .: "login"
                                 <*> body .: "password"

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field
