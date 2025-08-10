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
      :> Get '[JSON] TodoResponse
    :<|> "todos" :> ReqBody '[JSON] CreateTodoPayload :> Post '[JSON] (Entity Todo)
    :<|> "todos" :> Capture "id" (Key Todo) :> Get '[JSON] (Entity Todo)
    :<|> "todos" :> Capture "id" (Key Todo) :> ReqBody '[JSON] UpdateTodoPayload :> Put '[JSON] (Entity Todo)
    :<|> "todos" :> Capture "id" (Key Todo) :> Delete '[JSON] ()
