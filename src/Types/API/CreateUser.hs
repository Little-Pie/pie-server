{-# LANGUAGE OverloadedStrings #-}

module Types.API.CreateUser where

import Data.Aeson 

data CreateUserRequest = CreateUserRequest {createName :: String
                                           ,createLogin :: String
                                           ,createPassword :: String
                                           }

instance FromJSON CreateUserRequest where
  parseJSON (Object createUserRequest) = CreateUserRequest <$> createUserRequest .: "name"
                                                           <*> createUserRequest .: "login"
                                                           <*> createUserRequest .: "password"