{-# LANGUAGE OverloadedStrings #-}

module Types.API.CreateUser where

import Data.Aeson 

data CreateUserRequest = CreateUserRequest {name :: String
                                           ,login :: String
                                           ,password :: String
                                           }

instance FromJSON CreateUserRequest where
  parseJSON (Object createUserRequest) = CreateUserRequest <$> createUserRequest .: "name"
                                                           <*> createUserRequest .: "login"
                                                           <*> createUserRequest .: "password"