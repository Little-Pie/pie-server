{-# LANGUAGE OverloadedStrings #-}

module Endpoints.MakeAuthor where

import qualified DbQuery.User as DB
import qualified Types.API.Id as API
import Types.Entities.User
import Data.Aeson
import Database.PostgreSQL.Simple
import Network.Wai (Response)
import qualified Data.ByteString.Lazy as LBS
import Helpers

makeAuthor :: Connection -> LBS.ByteString -> IO Response
makeAuthor conn body = case decode body :: Maybe API.IdRequest of
  Nothing -> pure $ responseBadRequest "Couldn't parse body"
  Just bodyParsed -> do
    let editUserId = API.id bodyParsed
    users <- DB.getUserById conn editUserId
    case users of
      [] -> pure $ responseBadRequest "There are no users with such id"
      [x] -> if isAuthor x
      then pure $ responseBadRequest "This user is already author"
      else do
        DB.makeAuthor conn editUserId
        pure $ responseOk "Now this user is an author"