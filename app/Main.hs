{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Lib
import Models

import qualified  Data.ByteString.Lazy as BS
import qualified  Data.Text as T
import Data.Aeson (encode)
import Yesod

data App =
  App

mkYesod
  "App"
  [parseRoutes|
/                       HomeR           GET
/feedback               FeedbackR       GET
/feedback/#T.Text       FeedbackByIdR   GET PUT
|]

instance Yesod App

getFeedbackR :: Handler Value
getFeedbackR = returnJson [Feedback "1" "poor" "it wasn't great", Feedback "2" "pretty bad" "nope."]

getFeedbackByIdR :: T.Text -> Handler Value
getFeedbackByIdR x = returnJson $ Feedback x "ok" "passable"

putFeedbackByIdR :: T.Text -> Handler Value
putFeedbackByIdR x = do
  liftIO $ BS.writeFile path (encode newFeedback)
  returnJson newFeedback
  where
    path = "feedback/" ++ (T.unpack x) ++ ".json"
    newFeedback = Feedback x "excellent" "result of HTTP PUT"

getHomeR :: Handler String
getHomeR = return "Welcome to Feedback on Anything."

main :: IO ()
main = warp 3000 App
