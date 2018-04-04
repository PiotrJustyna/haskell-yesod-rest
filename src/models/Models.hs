{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Models
  ( Feedback(..)
  ) where

import qualified Data.Text as T
import Data.Aeson.Types
import Yesod

data Feedback = Feedback
  { id :: T.Text
  , experience :: T.Text
  , comment :: T.Text
  }

instance ToJSON Feedback where
  toJSON Feedback {..} =
    object ["id" .= id, "experience" .= experience, "comment" .= comment]

instance FromJSON Feedback where
  parseJSON = withObject "Feedback" $ \v -> Feedback
    <$> v .: "id"
    <*> v .: "experience"
    <*> v .: "comment"