{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Database.Persist (Entity)
import Model
import Servant

type TodoAPI =
  "health" :> Get '[JSON] String
    :<|> "todos"
      :> QueryParam "completed" Bool
      :> QueryParam "limit" Int
      :> QueryParam "offset" Int
      :> QueryParam "sort" String
      :> QueryParam "search" String
      :> QueryParam "priority" String
      :> Get '[JSON] TodoResponse
    :<|> "todos" :> ReqBody '[JSON] CreateTodoPayload :> Post '[JSON] (Entity Todo)
    :<|> "todos" :> Capture "id" (Key Todo) :> Get '[JSON] (Entity Todo)
    :<|> "todos" :> Capture "id" (Key Todo) :> ReqBody '[JSON] UpdateTodoPayload :> Put '[JSON] (Entity Todo)
    :<|> "todos" :> Capture "id" (Key Todo) :> Delete '[JSON] ()
    :<|> "todos" :> "overdue" :> Get '[JSON] [Entity Todo]
    :<|> "todos" :> "due-soon" :> QueryParam "hours" Int :> Get '[JSON] [Entity Todo]
