{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Monad.Reader (ReaderT)
import Data.Aeson (FromJSON, decodeStrict)
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
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
  deriving (Generic, FromJSON)

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
