{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Monad (mzero)
import Control.Monad.Reader (ReaderT)
import Data.Aeson (FromJSON, Value (..), decodeStrict, parseJSON, (.:))
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple (Connection)
import Logging (LoggingLevel)
import System.IO (Handle)

data Config = Config
  { limit :: Int,
    offset :: Int,
    connectHost :: String,
    connectDatabase :: String,
    connectUser :: String,
    connectPassword :: String,
    loggingLevel :: LoggingLevel
  }

instance FromJSON Config where
  parseJSON (Object config) =
    Config
      <$> config .: "limit"
      <*> config .: "offset"
      <*> config .: "connectHost"
      <*> config .: "connectDatabase"
      <*> config .: "connectUser"
      <*> config .: "connectPassword"
      <*> config .: "loggingLevel"
  parseJSON _ = mzero

getConfig :: IO (Maybe Config)
getConfig = do
  rawJSON <- BS.readFile "config.json"
  let result = decodeStrict rawJSON :: Maybe Config
  case result of
    Nothing -> return Nothing
    Just conf -> return $ Just conf

data Environment = Environment
  { limit :: Int,
    offset :: Int,
    conn :: Connection,
    logHandle :: Handle,
    loggingLevel :: LoggingLevel
  }

type App = ReaderT Environment IO
