{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Api (TodoAPI)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Time (getCurrentTime)
import Database.Persist (Entity (..), SelectOpt (Desc, LimitTo, OffsetBy), count, delete, getEntity, insertEntity, replace, selectList, (==.))
import Model
import Monad (AppConfig, AppM (..), runDb)
import Network.Wai.Handler.Warp (run)
import Servant
import Validation (validateTodoPayload)

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

getTodos :: Maybe Bool -> Maybe Int -> Maybe Int -> AppM TodoResponse
getTodos maybeCompleted maybeLimit maybeOffset = do
  let filters = case maybeCompleted of
        Nothing -> []
        Just isCompleted -> [TodoCompleted ==. isCompleted]

  -- Set defaults: limit 10, offset 0
  let limit' = maybe 10 (max 1 . min 100) maybeLimit -- Between 1-100, default 10
  let offset' = maybe 0 (max 0) maybeOffset -- At least 0, default 0

  -- Get total count for pagination metadata
  totalCount' <- runDb $ count filters

  -- Get paginated results
  todos' <- runDb $ selectList filters [Desc TodoCreatedAt, LimitTo limit', OffsetBy offset']

  return $ TodoResponse todos' totalCount' limit' offset'

postTodo :: TodoPayload -> AppM (Entity Todo)
postTodo payload = do
  -- Validate the payload first
  (validTitle, validCompleted) <- case validateTodoPayload payload of
    Left err -> throwError err
    Right result -> return result

  now <- liftIO getCurrentTime
  let newTodo = Todo validTitle validCompleted now now
  runDb $ insertEntity newTodo

getTodoById :: Key Todo -> AppM (Entity Todo)
getTodoById todoId = do
  maybeTodo <- runDb $ getEntity todoId
  case maybeTodo of
    Just todo -> return todo
    Nothing -> throwError err404

putTodo :: Key Todo -> TodoPayload -> AppM (Entity Todo)
putTodo todoId payload = do
  -- Validate the payload first
  (validTitle, validCompleted) <- case validateTodoPayload payload of
    Left err -> throwError err
    Right result -> return result

  now <- liftIO getCurrentTime
  -- First check if the todo exists
  maybeTodo <- runDb $ getEntity todoId
  case maybeTodo of
    Nothing -> throwError err404
    Just (Entity _ originalTodo) -> do
      let updatedTodo = Todo validTitle validCompleted (todoCreatedAt originalTodo) now
      runDb $ replace todoId updatedTodo
      return $ Entity todoId updatedTodo

deleteTodo :: Key Todo -> AppM ()
deleteTodo todoId = do
  -- Check if todo exists before deleting
  maybeTodo <- runDb $ getEntity todoId
  case maybeTodo of
    Nothing -> throwError err404
    Just _ -> runDb $ delete todoId
