{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

--main :: IO ()
-- main = putStrLn "Hello, Haskell!"

import GHC.Generics (Generic)
import Data.Text (Text)
import Servant
import Network.Wai.Handler.Warp (run)

-- Corrected and Finalized Imports:
import Data.Aeson (FromJSON, ToJSON(..), object, (.=)) -- ADDED toJSON method, object, and (.=)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Persist (Entity(..), Key, selectList, insertEntity, getEntity, replace, delete, SelectOpt(Asc)) -- CHANGED Entity to Entity(..)
import Database.Persist.Sqlite (fromSqlKey, runSqlite, runMigration, SqlPersistT) -- ADDED fromSqlKey
import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)



-- This block uses Template Haskell to generate all the necessary
-- data types and database functions from a simple definition.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo
    title Text
    completed Bool
    deriving Show Eq Generic
|]

-- This instance teaches Aeson how to convert our database entity into JSON.
instance ToJSON (Entity Todo) where
  toJSON (Entity todoId todo) =
    object
      [ "id" .= fromSqlKey todoId
      , "title" .= todoTitle todo
      , "completed" .= todoCompleted todo
      ]

-- Add this new data type for Create/Update payloads
data TodoPayload = TodoPayload
  { payloadTitle :: Text
  , payloadCompleted :: Bool
  } deriving (Show, Generic)

-- Also add FromJSON instances so can parse these from request bodies
instance FromJSON TodoPayload
instance ToJSON TodoPayload -- For consistency

-- Update the TodoAPI type to be a full CRUD API
type TodoAPI =
       -- GET /todos
       "todos" :> Get '[JSON] [Entity Todo]

       -- POST /todos
  :<|> "todos" :> ReqBody '[JSON] TodoPayload :> Post '[JSON] (Entity Todo)

       -- GET /todos/{id}
  :<|> "todos" :> Capture "id" (Key Todo) :> Get '[JSON] (Entity Todo)

       -- PUT /todos/{id}
  :<|> "todos" :> Capture "id" (Key Todo) :> ReqBody '[JSON] TodoPayload :> Put '[JSON] (Entity Todo)

       -- DELETE /todos/{id}
  :<|> "todos" :> Capture "id" (Key Todo) :> Delete '[JSON] ()

-- Create
postTodo :: TodoPayload -> Handler (Entity Todo)
postTodo payload = runDb $ do
  let newTodo = Todo (payloadTitle payload) (payloadCompleted payload)
  insertEntity newTodo

-- Read (List)
getTodos :: Handler [Entity Todo]
getTodos = runDb $ selectList [] [Asc TodoId]

-- Read (Single)
getTodoById :: Key Todo -> Handler (Entity Todo)
getTodoById todoId = do
  maybeTodo <- runDb $ getEntity todoId
  case maybeTodo of
    Just todo -> return todo
    Nothing   -> throwError err404

-- Update
putTodo :: Key Todo -> TodoPayload -> Handler (Entity Todo)
putTodo todoId payload = runDb $ do
  let updatedTodo = Todo (payloadTitle payload) (payloadCompleted payload)
  replace todoId updatedTodo
  getEntity todoId >>= maybe (liftIO $ fail "Could not find updated todo") return

-- Delete
deleteTodo :: Key Todo -> Handler ()
deleteTodo todoId = runDb $ delete todoId

-- Combine all handlers into the server
server :: Server TodoAPI
server = getTodos
    :<|> postTodo
    :<|> getTodoById
    :<|> putTodo
    :<|> deleteTodo

-- Create the WAI Application from  API type and server logic
app :: Application
app = serve (Proxy :: Proxy TodoAPI) server

-- The main entry point of the application
main :: IO ()
main = do
  -- Run migrations directly in IO
  runSqlite "todos.db" (runMigration migrateAll)

  putStrLn "Starting server on http://localhost:8080"
  run 8080 app

-- Helper to run a database query within a Servant Handler
runDb query = liftIO $ runSqlite "todos.db" query