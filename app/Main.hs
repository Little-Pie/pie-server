{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (Config (..), Environment (..), getConfig)
import Control.Monad.Reader (liftIO, runReaderT)
--import Control.Exception (SomeException, catch)
import Database.PostgreSQL.Simple (Connection, close, connect)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (..), MigrationContext (..), runMigration)
import DbQuery.Test (deleteFromTables, dropTables, fillTables)
import Hash (makeStringHash)
import Helpers (localPG, printDebug, printError, printRelease, printWarning, withLogging)
import Network.Wai (Application)
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
      putStrLn "Serving..."
      run 4000 $ appWithEnv env
      hClose logHandle
      close conn
  where
    appWithEnv :: Environment -> Application
    appWithEnv env request respond = runReaderT (withLogging application request respond) env

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
