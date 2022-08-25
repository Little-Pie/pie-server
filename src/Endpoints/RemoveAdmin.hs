{-# LANGUAGE OverloadedStrings #-}

module Endpoints.RemoveAdmin where

import Types.Entities.User
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

removeAdmin :: Connection -> Int -> IO (LBS.ByteString)
removeAdmin conn userId' = do
    users <- query conn "select * from users where id=(?)" (Only userId') :: IO [User]
    case users of
      [] -> pure "There are no users with such id"
      [x] -> if not $ isAdmin x
      then pure "This user is already not admin"
      else do
        execute conn "UPDATE users SET is_admin = (?) WHERE id = (?)" $ (False,userId')
        pure "Now this user is not an admin"