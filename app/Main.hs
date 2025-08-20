{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite (createSqlitePool, runMigration, runSqlPool)
import Model (migrateAll)
import Monad (AppConfig (..))
import Server (runApp)

main :: IO ()
main = do
  -- Create a connection pool
  pool <- runStderrLoggingT $ createSqlitePool "/app/data/todos.db" 5

  -- Run migrations using the pool
  runSqlPool (runMigration migrateAll) pool

  -- Create the AppConfig
  let config = AppConfig {appPool = pool}

  -- Run the app
  runApp config
