{-# LANGUAGE OverloadedStrings #-}

module Endpoints.DeleteUser where

import Types.Entities.User
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

deleteUser :: Connection -> Int -> IO (LBS.ByteString)
deleteUser conn userId' = do
    users <- query_ conn "select * from users" :: IO [User]
    case userId' `elem` (map userId users) of
        False -> pure "There are no users with such id"
        True -> do
          execute conn "DELETE FROM users WHERE id=(?)" $ (Only userId')
          pure "User is deleted"