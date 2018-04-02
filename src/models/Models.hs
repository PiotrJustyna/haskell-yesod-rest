{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Models
  ( Feedback(..)
  ) where

import qualified Data.Text as T
import Yesod

data Feedback = Feedback
  { id :: T.Text
  , experience :: T.Text
  , comment :: T.Text
  }

instance ToJSON Feedback where
  toJSON Feedback {..} =
    object ["id" .= id, "experience" .= experience, "comment" .= comment]
