module Main where

import Config (App, Config (..), Environment (..), getConfig)
import Control.Exception (SomeException)
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand (..),
    MigrationContext (..),
    runMigration,
  )
import DbQuery.Test (dropTables, fillTables)
import Helpers (localPG, printLog, responsePlainText, withDbConnection, withLogging)
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
      logHandle <- openFile "logFile.txt" AppendMode
      let env = Environment limit offset logHandle loggingLevel (localPG config)
      args <- getArgs
      runReaderT (runMigrations args) env
      putStrLn "Serving..."
      runReaderT (printLog Debug "Serving...") env
      let settings =
            setOnExceptionResponse exceptionResponseSettings $
              setOnException (exceptionSettings env) $ setPort 4000 defaultSettings
      runSettings settings $ appWithEnv env
      hClose logHandle
  where
    appWithEnv :: Environment -> Application
    appWithEnv env request respond = runReaderT (withLogging application request respond) env

exceptionSettings :: Environment -> Maybe Request -> SomeException -> IO ()
exceptionSettings env _ exception = runReaderT (printLog Error $ show exception) env

exceptionResponseSettings :: SomeException -> Response
exceptionResponseSettings _ = responsePlainText status500 "Something went wrong"

execMigrations :: App ()
execMigrations = do
  void $ withDbConnection $ runMigration . MigrationContext MigrationInitialization True
  void $ withDbConnection $ runMigration . MigrationContext (MigrationDirectory "migrations") True

runMigrations :: [String] -> App ()
runMigrations ["f"] = do
  dropTables
  execMigrations
  fillTables
runMigrations _ = execMigrations
