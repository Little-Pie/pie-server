{-# LANGUAGE OverloadedStrings #-}

module Main where

import Routing (application)
import Helpers (getConfig, localPG, withLogging)
import Network.Wai.Handler.Warp (run)
import Database.PostgreSQL.Simple (connect, close)

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
