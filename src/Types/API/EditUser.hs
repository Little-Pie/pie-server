{-# LANGUAGE OverloadedStrings #-}

module Types.API.EditUser where

import Data.Aeson 

data EditUserRequest = EditUserRequest {userId :: Maybe Int
                                       ,name :: Maybe String
                                       ,login :: Maybe String
                                       ,password :: Maybe String
                                       }

instance FromJSON EditUserRequest where
  parseJSON (Object editUserRequest) = EditUserRequest <$> editUserRequest .:? "userId"
                                                       <*> editUserRequest .:? "name"
                                                       <*> editUserRequest .:? "login"
                                                       <*> editUserRequest .:? "password"