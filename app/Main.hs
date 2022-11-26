{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (Config (..), Environment (..), getConfig)
import Control.Exception (SomeException)
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Data.List (isInfixOf)
import Database.PostgreSQL.Simple (Connection, close, connect)
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand (..),
    MigrationContext (..),
    runMigration,
  )
import DbQuery.Test (dropTables, fillTables)
import Helpers (localPG, printLog, responsePlainText, withLogging)
import Logging (LoggingLevel (..))
import Network.HTTP.Types (status500)
import Network.Wai (Application, Request, Response)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setOnException,
    setOnExceptionResponse,
    setPort,
  )
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
      runReaderT (printLog Debug "Serving...") env
      let settings =
            setOnExceptionResponse exceptionResponseSettings $
              setOnException (exceptionSettings env) $ setPort 4000 defaultSettings
      runSettings settings $ appWithEnv env
      hClose logHandle
      close conn
  where
    appWithEnv :: Environment -> Application
    appWithEnv env request respond = runReaderT (withLogging application request respond) env

exceptionSettings :: Environment -> Maybe Request -> SomeException -> IO ()
exceptionSettings env _ exception = runReaderT (printLog Error $ show exception) env

exceptionResponseSettings :: SomeException -> Response
exceptionResponseSettings exception
  | "libpq" `isInfixOf` show exception = responsePlainText status500 "No connection to database"
  | otherwise = responsePlainText status500 "Something went wrong"

execMigrations :: Connection -> IO ()
execMigrations conn = do
  void $
    runMigration $
      MigrationContext MigrationInitialization True conn
  void $
    runMigration $
      MigrationContext (MigrationDirectory "migrations") True conn

runMigrations :: [String] -> Connection -> IO ()
runMigrations ["f"] conn = do
  dropTables conn
  execMigrations conn
  fillTables conn
runMigrations _ conn = execMigrations conn
