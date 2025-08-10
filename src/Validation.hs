{-# LANGUAGE OverloadedStrings #-}

module Validation where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Model (CreateTodoPayload (..), UpdateTodoPayload (..))
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

-- Validate CREATE payload (title required)
validateCreateTodoPayload :: CreateTodoPayload -> Either ServerError (Text, Bool)
validateCreateTodoPayload payload =
  case validateTitle (createTitle payload) of
    Nothing -> Left titleValidationError
    Just validTitle -> Right (validTitle, fromMaybe False (createCompleted payload))

-- Validate UPDATE payload (both fields optional)
validateUpdateTodoPayload :: UpdateTodoPayload -> Either ServerError (Maybe Text, Maybe Bool)
validateUpdateTodoPayload payload =
  case updateTitle payload of
    Nothing -> Right (Nothing, updateCompleted payload) -- No title to validate
    Just titleText -> case validateTitle titleText of
      Nothing -> Left titleValidationError
      Just validTitle -> Right (Just validTitle, updateCompleted payload)
