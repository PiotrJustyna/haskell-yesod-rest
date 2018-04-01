{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Lib
import Models

import Yesod

instance ToJSON Feedback where
  toJSON Feedback {..} =
    object ["experience" .= experience, "comment" .= comment]

data App =
  App

mkYesod
  "App"
  [parseRoutes|
/           HomeR       GET
/feedback   FeedbackR   GET
|]

instance Yesod App

getFeedbackR :: Handler Value
getFeedbackR = returnJson $ Feedback "poor" "it wasn't great"

getHomeR :: Handler String
getHomeR = return "Welcome to Feedback on Anything."

main :: IO ()
main = warp 3000 App
