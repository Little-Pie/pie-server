{-# LANGUAGE OverloadedStrings #-}

module Endpoints.MakeAdmin where

import Types.Entities.User
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

makeAdmin :: Connection -> Int -> IO (LBS.ByteString)
makeAdmin conn userId' = do
    users <- query conn "select * from users where id=(?)" (Only userId') :: IO [User]
    case users of
      [] -> pure "There are no users with such id"
      [x] -> if isAdmin x
      then pure "This user is already admin"
      else do
        execute conn "UPDATE users SET is_admin = (?) WHERE id = (?)" $ (True,userId')
        pure "Now this user is an admin"