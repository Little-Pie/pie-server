{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (Config (..), Environment (..), getConfig)
import Database.PostgreSQL.Simple (Connection, close, connect)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (..), MigrationContext (..), runMigration)
import DbQuery.Test (deleteFromTables, dropTables, fillTables)
import Hash (makeStringHash)
import Helpers (localPG, printDebug, printError, printRelease, printWarning, withLogging)
import Network.Wai.Handler.Warp (run)
import Routing (application)
import System.Environment (getArgs)
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
      args <- getArgs
      runMigrations args conn
      printError env "Serving..."
      putStrLn "Serving..."
      run 4000 $ withLogging env $ application env
      hClose logHandle
      close conn

execMigrations :: Connection -> IO ()
execMigrations conn = do
  schemaRes <-
    runMigration $
      MigrationContext MigrationInitialization True conn
  res <-
    runMigration $
      MigrationContext (MigrationDirectory "migrations") True conn
  pure ()

runMigrations :: [String] -> Connection -> IO ()
runMigrations ["f"] conn = do
  dropTables conn
  execMigrations conn
  fillTables conn
runMigrations _ conn = execMigrations conn
