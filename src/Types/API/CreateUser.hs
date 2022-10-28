{-# LANGUAGE OverloadedStrings #-}

module Types.API.CreateUser where

import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value (..), parseJSON, (.:))

data CreateUserRequest = CreateUserRequest
  { name :: String,
    login :: String,
    password :: String,
    isAdmin :: Bool,
    isAuthor :: Bool
  }

instance FromJSON CreateUserRequest where
  parseJSON (Object createUserRequest) =
    CreateUserRequest <$> createUserRequest .: "name"
      <*> createUserRequest .: "login"
      <*> createUserRequest .: "password"
      <*> createUserRequest .: "isAdmin"
      <*> createUserRequest .: "isAuthor"
  parseJSON _ = mzero
