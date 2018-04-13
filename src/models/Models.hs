{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Models
  ( Feedback(..)
  , newFeedback
  , NewFeedbackRequest(..)
  ) where

import Data.Aeson.Types
import qualified Data.Text as T
import Data.UUID
import Data.UUID.V4
import Yesod

data Feedback = Feedback
  { id :: UUID
  , experience :: T.Text
  , comment :: T.Text
  }

instance ToJSON Feedback where
  toJSON Feedback {..} =
    object ["id" .= id, "experience" .= experience, "comment" .= comment]

instance FromJSON Feedback where
  parseJSON =
    withObject "Feedback" $ \v ->
      Feedback <$> v .: "id" <*> v .: "experience" <*> v .: "comment"

newFeedback :: NewFeedbackRequest -> IO Feedback
newFeedback (NewFeedbackRequest newExperience newComment) = do
  newFeedbackID <- nextRandom
  return $ Feedback newFeedbackID newExperience newComment

data NewFeedbackRequest = NewFeedbackRequest
  { newExperience :: T.Text
  , newComment :: T.Text
  }

instance ToJSON NewFeedbackRequest where
  toJSON NewFeedbackRequest {..} =
    object ["newExperience" .= newExperience, "newComment" .= newComment]

instance FromJSON NewFeedbackRequest where
  parseJSON =
    withObject "NewFeedbackRequest" $ \v ->
      NewFeedbackRequest <$> v .: "newExperience" <*> v .: "newComment"
