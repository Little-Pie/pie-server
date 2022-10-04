{-# LANGUAGE OverloadedStrings #-}

module DbQuery.Image where

import Types.Entities.Image
import Database.PostgreSQL.Simple

getImageById :: Connection -> Int -> IO [Image]
getImageById conn imageId =
  query conn "SELECT * FROM images WHERE id=(?)" (Only imageId)