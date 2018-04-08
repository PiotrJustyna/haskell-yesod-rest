{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Lib
import Models

import Control.Monad
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import System.Directory
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
getFeedbackR = do
  paths <- liftIO $ getDirectoryContents "feedback"
  feedback <-
    liftIO $
    foldM
      (\files path ->
         fmap
           (\file -> (decodedFile file) : files)
           (BS.readFile $ "feedback/" ++ path))
      []
      (filter (\x -> length x > 2) paths)
  returnJson feedback
  where
    decodedFile file = (decode file) :: Maybe Feedback

getFeedbackByIdR :: T.Text -> Handler Value
getFeedbackByIdR x = do
  content <- liftIO $ BS.readFile path
  returnJson $ ((decode content) :: Maybe Feedback)
  where
    path = "feedback/" ++ (T.unpack x) ++ ".json"

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
