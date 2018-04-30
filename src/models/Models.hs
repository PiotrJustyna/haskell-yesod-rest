{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Models
  ( Feedback(..)
  , DirtyFeedback(..)
  , newFeedback
  , updatedFeedback
  ) where

import Data.Aeson.Types
import qualified Data.Text as T
import Data.UUID
import Data.UUID.V4
import Yesod

--------------------------------------------------
data Feedback = Feedback
  { id :: UUID
  , dirtyFeedback :: DirtyFeedback
  }

instance ToJSON Feedback where
  toJSON Feedback {..} = object ["id" .= id, "dirtyFeedback" .= dirtyFeedback]

instance FromJSON Feedback where
  parseJSON =
    withObject "Feedback" $ \v ->
      Feedback <$> v .: "id" <*> v .: "dirtyFeedback"

--------------------------------------------------
data DirtyFeedback = DirtyFeedback
  { experience :: T.Text
  , comment :: T.Text
  }

instance ToJSON DirtyFeedback where
  toJSON DirtyFeedback {..} =
    object ["experience" .= experience, "comment" .= comment]

instance FromJSON DirtyFeedback where
  parseJSON =
    withObject "DirtyFeedback" $ \v ->
      DirtyFeedback <$> v .: "experience" <*> v .: "comment"

--------------------------------------------------
newFeedback :: DirtyFeedback -> IO Feedback
newFeedback (DirtyFeedback experience comment) = do
  newFeedbackID <- nextRandom
  return $ Feedback newFeedbackID (DirtyFeedback experience comment)

updatedFeedback :: T.Text -> DirtyFeedback -> Maybe Feedback
updatedFeedback updatedFeedbackId (DirtyFeedback experience comment) =
  case (fromString $ T.unpack updatedFeedbackId) of
    Just updatedFeedbackId ->
      Just $ Feedback updatedFeedbackId (DirtyFeedback experience comment)
    Nothing -> Nothing
