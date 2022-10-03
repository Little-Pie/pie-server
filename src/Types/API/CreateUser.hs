{-# LANGUAGE OverloadedStrings #-}

module Types.API.CreateUser where

import Data.Aeson 

data CreateUserRequest = CreateUserRequest {name :: String
                                           ,login :: String
                                           ,password :: String
                                           ,isAdmin :: Bool
                                           ,isAuthor :: Bool
                                           }

instance FromJSON CreateUserRequest where
  parseJSON (Object createUserRequest) = CreateUserRequest <$> createUserRequest .: "name"
                                                           <*> createUserRequest .: "login"
                                                           <*> createUserRequest .: "password"
                                                           <*> createUserRequest .: "isAdmin"
                                                           <*> createUserRequest .: "isAuthor"