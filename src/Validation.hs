{-# LANGUAGE OverloadedStrings #-}

module Validation where

import Data.Text (Text)
import qualified Data.Text as T
import Model (TodoPayload (..))
import Servant (ServerError, err400, errBody)

-- Simple validation functions that throw Servant errors

-- Validate todo title
validateTitle :: Text -> Maybe Text
validateTitle titleText
  | T.null (T.strip titleText) = Nothing
  | T.length titleText > 200 = Nothing
  | otherwise = Just (T.strip titleText)

-- Create validation error
titleValidationError :: ServerError
titleValidationError = err400 {errBody = "Title must be non-empty and less than 200 characters"}

-- Validate and extract fields from payload
validateTodoPayload :: TodoPayload -> Either ServerError (Text, Bool)
validateTodoPayload payload =
  case validateTitle (title payload) of
    Nothing -> Left titleValidationError
    Just validTitle -> Right (validTitle, completed payload)