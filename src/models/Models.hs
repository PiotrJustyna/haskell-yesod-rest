module Models
  ( Feedback(..)
  ) where

import Data.Text (Text)

data Feedback = Feedback
  { experience :: Text
  , comment :: Text
  }
