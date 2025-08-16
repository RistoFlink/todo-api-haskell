{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Api (TodoAPI)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist (Entity (..), SelectOpt (LimitTo, OffsetBy), count, delete, getEntity, insertEntity, replace, selectList, (==.))
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
    :<|> getOverdueTodos
    :<|> getTodosDueSoon

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

getTodos :: Maybe Bool -> Maybe Int -> Maybe Int -> Maybe String -> Maybe String -> AppM M.TodoResponse
getTodos maybeCompleted maybeLimit maybeOffset maybeSortParam maybeSearchParam = do
  let sortBy = M.parseSortParam maybeSortParam
  let filters = case maybeCompleted of
        Nothing -> []
        Just isCompleted -> [M.TodoCompleted ==. isCompleted]

  -- Set defaults: limit 10, offset 0
  let limit' = maybe 10 (max 1 . min 100) maybeLimit
  let offset' = maybe 0 (max 0) maybeOffset

  -- Get total count for pagination metadata
  totalCount' <- runDb $ count filters

  -- Get paginated results with sorting
  let sortOps = M.toSelectOpt sortBy
  todos' <- runDb $ selectList filters (sortOps ++ [LimitTo limit', OffsetBy offset'])
  let searchFilteredTodos = case maybeSearchParam of
        Nothing -> todos'
        Just searchTerm ->
          filter
            ( \(Entity _ todo) ->
                T.isInfixOf (T.toLower $ T.pack searchTerm) (T.toLower $ M.todoTitle todo)
            )
            todos'

  return $ M.TodoResponse searchFilteredTodos totalCount' limit' offset'

postTodo :: M.CreateTodoPayload -> AppM (Entity M.Todo)
postTodo payload = do
  -- Validate the payload first
  (validTitle, validCompleted) <- case validateCreateTodoPayload payload of
    Left err -> throwError err
    Right result -> return result
  -- Validate due date
  validDueDate <- case M.createDueDate payload of
    Nothing -> return Nothing
    Just _ -> do
      validation <- liftIO $ M.validateDueDate (M.createDueDate payload)
      case validation of
        Left err -> throwError err400 {errBody = "Invalid due date: " <> fromString err}
        Right validDate -> return validDate

  now <- liftIO getCurrentTime
  let priority = fromMaybe M.Medium (M.createPriority payload)
  let newTodo = M.Todo validTitle validCompleted now now validDueDate priority
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
      let updatedPriority = case M.updatePriority payload of
            Nothing -> M.todoPriority originalTodo
            Just p -> p

      -- Validate and handle due date update
      validateDueDate <- case M.updateDueDate payload of
        Nothing -> return (M.todoDueDate originalTodo)
        Just _ -> do
          validation <- liftIO $ M.validateDueDate (M.updateDueDate payload)
          case validation of
            Left err -> throwError err400 {errBody = "Invalid due date: " <> fromString err}
            Right validDate -> return validDate

      let updatedTodo = M.Todo updatedTitle updatedCompleted (M.todoCreatedAt originalTodo) now validateDueDate updatedPriority
      runDb $ replace todoId updatedTodo
      return $ Entity todoId updatedTodo

deleteTodo :: M.Key M.Todo -> AppM ()
deleteTodo todoId = do
  -- Check if todo exists before deleting
  maybeTodo <- runDb $ getEntity todoId
  case maybeTodo of
    Nothing -> throwError err404
    Just _ -> runDb $ delete todoId

getOverdueTodos :: AppM [Entity M.Todo]
getOverdueTodos = runDb M.getOverdueTodos

getTodosDueSoon :: Maybe Int -> AppM [Entity M.Todo]
getTodosDueSoon maybeHours = do
  let hours = fromMaybe 24 maybeHours
  let threshold = fromIntegral hours * 60 * 60
  runDb $ M.getTodosDueSoon threshold