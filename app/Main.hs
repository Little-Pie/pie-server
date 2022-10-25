{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (Config (..), Environment (..), getConfig)
import Control.Exception (SomeException)
import Control.Monad.Reader (liftIO, runReaderT)
import Database.PostgreSQL.Simple (Connection, close, connect)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (..), MigrationContext (..), runMigration)
import DbQuery.Test (deleteFromTables, dropTables, fillTables)
import Hash (makeStringHash)
import Helpers (localPG, printError, responsePlainText, withLogging)
import Network.HTTP.Types (status500)
import Network.Wai (Application, Request, Response)
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, setOnException, setOnExceptionResponse, setPort)
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
      let settings = setOnExceptionResponse exceptionResponseSettings $ setOnException (exceptionSettings env) $ setPort 4000 defaultSettings
      runSettings settings $ appWithEnv env
      hClose logHandle
      close conn
  where
    appWithEnv :: Environment -> Application
    appWithEnv env request respond = runReaderT (withLogging application request respond) env

exceptionSettings :: Environment -> Maybe Request -> SomeException -> IO ()
exceptionSettings env mbReq exception = runReaderT (printError $ show exception) env

exceptionResponseSettings :: SomeException -> Response
exceptionResponseSettings exception = responsePlainText status500 "Something went wrong"

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
