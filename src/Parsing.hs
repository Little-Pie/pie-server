{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Data.Aeson 
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Clock (UTCTime)

data UserList = UserList { users' :: [User] }

instance ToJSON UserList where
  toJSON (UserList users) = object ["user_list" .= users]

data User = User {user_id :: Int
                 ,name :: String
                 ,login :: String
                 ,password :: String
                 ,date :: UTCTime
                 ,admin :: Bool
                 ,posting_news :: Bool
                 }

instance ToJSON User where
  toJSON (User user_id' name' login' password' date' admin' posting_news') = object ["id" .= user_id'
                                                                               ,"name" .= name'
                                                                               ,"login" .= login'
                                                                               ,"created_at" .= date'
                                                                               ,"is_admin" .= admin'
                                                                               ,"posting_news" .= posting_news'
                                                                               ]

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field <*> field
