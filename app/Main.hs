{-# LANGUAGE OverloadedStrings #-}

module Main where

import Routing
import Helpers
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application)
import Database.PostgreSQL.Simple
import Data.Time.Clock

main :: IO ()
main = do
  mbConfig <- getConfig
  case mbConfig of
    Nothing -> putStrLn "Couldn't parse config"
    Just config -> do
      conn <- connect $ localPG config
      putStrLn "Serving..."
      run 4000 $ withLogging $ application conn config
      close conn