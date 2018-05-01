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
  { target :: UUID
  , targetOwner :: UUID
  , feedbackGiver :: UUID
  , experience :: T.Text
  , comment :: T.Text
  }

instance ToJSON DirtyFeedback where
  toJSON DirtyFeedback {..} =
    object
      [ "target" .= target
      , "targetOwner" .= targetOwner
      , "feedbackGiver" .= feedbackGiver
      , "experience" .= experience
      , "comment" .= comment
      ]

instance FromJSON DirtyFeedback where
  parseJSON =
    withObject "DirtyFeedback" $ \v ->
      DirtyFeedback <$> v .: "target" <*> v .: "targetOwner" <*>
      v .: "feedbackGiver" <*>
      v .: "experience" <*>
      v .: "comment"

--------------------------------------------------
newFeedback :: DirtyFeedback -> IO Feedback
newFeedback (DirtyFeedback target targetOwner feedbackGiver experience comment) = do
  newFeedbackID <- nextRandom
  return $
    Feedback
      newFeedbackID
      (DirtyFeedback target targetOwner feedbackGiver experience comment)

updatedFeedback :: T.Text -> DirtyFeedback -> Maybe Feedback
updatedFeedback updatedFeedbackId (DirtyFeedback target targetOwner feedbackGiver experience comment) =
  case (fromString $ T.unpack updatedFeedbackId) of
    Just updatedFeedbackId ->
      Just $
      Feedback
        updatedFeedbackId
        (DirtyFeedback target targetOwner feedbackGiver experience comment)
    Nothing -> Nothing
