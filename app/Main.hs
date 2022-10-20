{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config (getConfig)
import Routing (application)
import Helpers (localPG, withLogging, dropTables, printDebug, printRelease, printWarning, printError)
import Network.Wai.Handler.Warp (run)
import Database.PostgreSQL.Simple (connect, close)
import System.IO (IOMode(..), hClose, openFile)

main :: IO ()
main = do
  mbConfig <- getConfig
  case mbConfig of
    Nothing -> putStrLn "Couldn't parse config"
    Just config -> do
      conn <- connect $ localPG config
      --dropTables conn
      logFile <- openFile "logFile.txt" AppendMode
      printDebug config logFile "Server port is 4000"
      printRelease config logFile "Hello"
      printWarning config logFile "Bye"
      printError config logFile "Serving..."
      run 4000 $ withLogging $ application conn config
      hClose logFile
      close conn
