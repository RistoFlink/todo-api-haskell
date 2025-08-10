{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Api (TodoAPI)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Time (getCurrentTime)
import Database.Persist (Entity (..), SelectOpt (Desc, LimitTo, OffsetBy), count, delete, getEntity, insertEntity, replace, selectList, (==.))
import qualified Model as M
import Monad (AppConfig, AppM (..), runDb)
import Network.Wai.Handler.Warp (run)
import Servant
import Validation (validateCreateTodoPayload, validateUpdateTodoPayload)

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

getTodos :: Maybe Bool -> Maybe Int -> Maybe Int -> AppM M.TodoResponse
getTodos maybeCompleted maybeLimit maybeOffset = do
  let filters = case maybeCompleted of
        Nothing -> []
        Just isCompleted -> [M.TodoCompleted ==. isCompleted]

  -- Set defaults: limit 10, offset 0
  let limit' = maybe 10 (max 1 . min 100) maybeLimit -- Between 1-100, default 10
  let offset' = maybe 0 (max 0) maybeOffset -- At least 0, default 0

  -- Get total count for pagination metadata
  totalCount' <- runDb $ count filters

  -- Get paginated results
  todos' <- runDb $ selectList filters [Desc M.TodoCreatedAt, LimitTo limit', OffsetBy offset']

  return $ M.TodoResponse todos' totalCount' limit' offset'

postTodo :: M.CreateTodoPayload -> AppM (Entity M.Todo)
postTodo payload = do
  -- Validate the payload first
  (validTitle, validCompleted) <- case validateCreateTodoPayload payload of
    Left err -> throwError err
    Right result -> return result

  now <- liftIO getCurrentTime
  let newTodo = M.Todo validTitle validCompleted now now
  runDb $ insertEntity newTodo

getTodoById :: M.Key M.Todo -> AppM (Entity M.Todo)
getTodoById todoId = do
  maybeTodo <- runDb $ getEntity todoId
  case maybeTodo of
    Just todo -> return todo
    Nothing -> throwError err404

putTodo :: M.Key M.Todo -> M.UpdateTodoPayload -> AppM (Entity M.Todo)
putTodo todoId payload = do
  -- Validate the payload first
  (maybeTitle, maybeCompleted) <- case validateUpdateTodoPayload payload of
    Left err -> throwError err
    Right result -> return result
  now <- liftIO getCurrentTime
  -- First check if the todo exists
  maybeTodo <- runDb $ getEntity todoId
  case maybeTodo of
    Nothing -> throwError err404
    Just (Entity _ originalTodo) -> do
      -- Use original values as defaults if not provided
      let updatedTitle = case maybeTitle of
            Nothing -> M.todoTitle originalTodo
            Just t -> t
      let updatedCompleted = case maybeCompleted of
            Nothing -> M.todoCompleted originalTodo
            Just c -> c
      let updatedTodo = M.Todo updatedTitle updatedCompleted (M.todoCreatedAt originalTodo) now
      runDb $ replace todoId updatedTodo
      return $ Entity todoId updatedTodo

deleteTodo :: M.Key M.Todo -> AppM ()
deleteTodo todoId = do
  -- Check if todo exists before deleting
  maybeTodo <- runDb $ getEntity todoId
  case maybeTodo of
    Nothing -> throwError err404
    Just _ -> runDb $ delete todoId
