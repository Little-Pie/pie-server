{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config (getConfig)
import Routing (application)
import Helpers (localPG, withLogging, dropTables, printLogDebug, printLogError)
import Network.Wai.Handler.Warp (run)
import Database.PostgreSQL.Simple (connect, close)

main :: IO ()
main = do
  mbConfig <- getConfig
  case mbConfig of
    Nothing -> putStrLn "Couldn't parse config"
    Just config -> do
      conn <- connect $ localPG config
      --dropTables conn
      printLogDebug config "Server port is 4000"
      printLogError config "Serving..."
      run 4000 $ withLogging $ application conn config
      close conn
