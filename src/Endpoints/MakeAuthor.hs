{-# LANGUAGE OverloadedStrings #-}

module Endpoints.MakeAuthor where

import qualified DbQuery.User as DB
import qualified Types.API.Id as API
import Types.Entities.User
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

makeAuthor :: Connection -> LBS.ByteString -> IO (LBS.ByteString)
makeAuthor conn body = case decode body :: Maybe API.IdRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    let editUserId = API.id bodyParsed
    users <- DB.getUserById conn editUserId
    case users of
      [] -> pure "There are no users with such id"
      [x] -> if isAuthor x
      then pure "This user is already author"
      else DB.makeAuthor conn editUserId