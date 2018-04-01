{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Lib

import Data.Text (Text)
import Yesod

data Feedback = Feedback
  { experience :: Text
  , comment :: Text
  }

instance ToJSON Feedback where
  toJSON Feedback {..} =
    object ["experience" .= experience, "comment" .= comment]

data App =
  App

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Value
getHomeR = returnJson $ Feedback "poor" "it wasn't great"

main :: IO ()
main = warp 3000 App
