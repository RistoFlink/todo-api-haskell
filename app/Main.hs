{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

--main :: IO ()
-- main = putStrLn "Hello, Haskell!"

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Servant
import Network.Wai.Handler.Warp (run)

-- Todo data type
data Todo = Todo
  { todoId :: Int
  , todoTitle :: Text
  , todoCompleted :: Bool
  } deriving (Show, Generic)

-- A boilerplate instance to allow the Todo type to be converted to JSON.
-- Aeson uses the 'Generic' derivation to figure out how to do this automatically.
instance ToJSON Todo

-- define the type for the API
type TodoAPI = "todos" :> Get '[JSON] [Todo]
          :<|> "todos" :> Capture "id" Int :> Get '[JSON] Todo

-- A mock database as an example
mockTodos :: [Todo]
mockTodos =
  [ Todo 1 "Buy milk" False
  , Todo 2 "Write Haskell backend example" True
  , Todo 3 "Profit!" False
  ]

-- The handler for the GET /todos endpoint
getTodos :: Handler [Todo]
getTodos = return mockTodos

-- The handler for the GET /todos/{id} endpoint
getTodoById :: Int -> Handler Todo
getTodoById todoIdParam =
  -- In a real app, you would query a database. Here just find it in the list.
  case find (\todo -> todoIdParam == todoId todo) mockTodos of
    Just todo -> return todo
    Nothing   -> throwError err404 { errBody = "Todo not found." }
  where
    -- A helper from the standard Data.List library
    find :: (a -> Bool) -> [a] -> Maybe a
    find _ [] = Nothing
    find p (x:xs) | p x       = Just x
                  | otherwise = find p xs

-- We combine the handlers into a single server value.
-- The order must match the API type definition
server :: Server TodoAPI
server = getTodos :<|> getTodoById

-- Create the WAI Application from  API type and server logic
app :: Application
app = serve (Proxy :: Proxy TodoAPI) server

-- The main entry point of the application
main :: IO ()
main = do
  putStrLn "Starting server on http://localhost:8080"
  run 8080 app