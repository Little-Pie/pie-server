{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (Config (..), Environment (..), getConfig)
import Database.PostgreSQL.Simple (close, connect)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (..), MigrationContext (..), runMigration)
import DbQuery.Test (deleteFromTables, dropTables, fillTables)
import Hash (makeStringHash)
import Helpers (localPG, printDebug, printError, printRelease, printWarning, withLogging)
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
      logHandle <- openFile "logFile.txt" AppendMode
      let env = Environment limit offset conn logHandle loggingLevel
      --dropTables conn
      --deleteFromTables conn
      execMigrations env
      --fillTables conn
      printError env "Serving..."
      run 4000 $ withLogging $ application env
      hClose logHandle
      close conn

execMigrations :: Environment -> IO ()
execMigrations Environment {..} = do
  schemaRes <-
    runMigration $
      MigrationContext MigrationInitialization True conn
  res <-
    runMigration $
      MigrationContext (MigrationDirectory "migrations") True conn
  pure ()
