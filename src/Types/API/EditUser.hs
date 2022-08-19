{-# LANGUAGE OverloadedStrings #-}

module Types.API.EditUser where

import Data.Aeson 

data EditUserRequest = EditUserRequest {editName :: Maybe String
                                       ,editLogin :: Maybe String
                                       ,editPassword :: Maybe String
                                       ,editId :: Maybe Int
                                       }

instance FromJSON EditUserRequest where
  parseJSON (Object editUserRequest) = EditUserRequest <$> editUserRequest .:? "name"
                                                       <*> editUserRequest .:? "login"
                                                       <*> editUserRequest .:? "password"
                                                       <*> editUserRequest .:? "int"