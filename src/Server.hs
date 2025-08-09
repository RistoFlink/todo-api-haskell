{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Api (TodoAPI)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Database.Persist (Entity, Key, delete, getEntity, insertEntity, replace, selectList, SelectOpt(Asc))
import Model
import Monad (AppM(..), AppConfig, runDb)
import Network.Wai.Handler.Warp (run)
import Servant

-- This function converts AppM actions into the Handlers that Servant expects.
appMToHandler :: AppConfig -> AppM a -> Handler a
appMToHandler config (AppM action) = runReaderT action config

-- The server logic implementation
serverLogic :: ServerT TodoAPI AppM
serverLogic =
       healthCheck
  :<|> getTodos
  :<|> postTodo
  :<|> getTodoById
  :<|> putTodo
  :<|> deleteTodo

-- Combine all handlers into the server
server :: AppConfig -> Server TodoAPI
server config = hoistServer (Proxy :: Proxy TodoAPI) (appMToHandler config) serverLogic

-- Run the application
runApp :: AppConfig -> IO ()
runApp config = do
    putStrLn "Starting server on http://localhost:8080"
    run 8080 (serve (Proxy :: Proxy TodoAPI) (server config))

-- Handlers
healthCheck :: AppM String
healthCheck = return "OK"

getTodos :: AppM [Entity Todo]
getTodos = runDb $ selectList [] [Asc TodoTitle]

postTodo :: TodoPayload -> AppM (Entity Todo)
postTodo payload = runDb $ do
  let newTodo = Todo (payloadTitle payload) (payloadCompleted payload)
  insertEntity newTodo

getTodoById :: Key Todo -> AppM (Entity Todo)
getTodoById todoId = do
  maybeTodo <- runDb $ getEntity todoId
  case maybeTodo of
    Just todo -> return todo
    Nothing   -> throwError err404

putTodo :: Key Todo -> TodoPayload -> AppM (Entity Todo)
putTodo todoId payload = runDb $ do
  let updatedTodo = Todo (payloadTitle payload) (payloadCompleted payload)
  replace todoId updatedTodo
  getEntity todoId >>= maybe (liftIO $ fail "Could not find updated todo") return

deleteTodo :: Key Todo -> AppM ()
deleteTodo todoId = runDb $ delete todoId