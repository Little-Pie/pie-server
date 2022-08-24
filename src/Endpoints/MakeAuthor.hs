{-# LANGUAGE OverloadedStrings #-}

module Endpoints.MakeAuthor where

import Types.Entities.User
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

makeAuthor :: Connection -> Int -> IO (LBS.ByteString)
makeAuthor conn userId' = do
    users <- query conn "select * from users where id=(?)" (Only userId') :: IO [User]
    case users of
      [] -> pure "There are no users with such id"
      _ -> do
        execute conn "UPDATE users SET is_author = (?) WHERE id = (?)" $ (True,userId')
        pure "Now this user is an author"