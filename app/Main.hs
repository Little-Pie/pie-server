{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (Config (..), Environment (..), getConfig)
import Database.PostgreSQL.Simple (close, connect)
import Helpers (dropTables, localPG, printDebug, printError, printRelease, printWarning, withLogging)
import Network.Wai.Handler.Warp (run)
import Routing (application)
import System.IO (IOMode (..), hClose, openFile)

main :: IO ()
main = do
  mbConfig <- getConfig
  case mbConfig of
    Nothing -> putStrLn "Couldn't parse config"
    Just config@Config {..} -> do
      conn <- connect $ localPG config
      --dropTables conn
      logHandle <- openFile "logFile.txt" AppendMode
      let env = Environment limit offset conn logHandle loggingLevel
      printError env "Serving..."
      run 4000 $ withLogging $ application env
      hClose logHandle
      close conn
