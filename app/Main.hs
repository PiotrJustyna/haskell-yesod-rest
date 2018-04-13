{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Data.UUID
import Models
import System.Directory
import Yesod

data App =
  App

mkYesod
  "App"
  [parseRoutes|
/                       HomeR           GET
/feedback               FeedbackR       GET POST
/feedback/#T.Text       FeedbackByIdR   GET DELETE
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

postFeedbackR :: Handler Value
postFeedbackR = do
  newFeedbackRequest <- requireJsonBody :: Handler NewFeedbackRequest
  feedback <- liftIO $ newFeedback newFeedbackRequest
  liftIO $ BS.writeFile (path feedback) (encode feedback)
  returnJson feedback
  where
    path (Feedback id experience comment) =
      "feedback/" ++ (toString id) ++ ".json"

getFeedbackByIdR :: T.Text -> Handler Value
getFeedbackByIdR x = do
  content <- liftIO $ BS.readFile path
  returnJson $ ((decode content) :: Maybe Feedback)
  where
    path = "feedback/" ++ (T.unpack x) ++ ".json"

deleteFeedbackByIdR :: T.Text -> Handler String
deleteFeedbackByIdR x = do
  liftIO $ removeFile path
  return "Feedback deleted."
  where
    path = "feedback/" ++ (T.unpack x) ++ ".json"

getHomeR :: Handler String
getHomeR = return "Welcome to Feedback on Anything."

main :: IO ()
main = warp 3000 App
