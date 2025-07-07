--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Site.Talks 
  ( Talk(..)
  , loadTalks
  , talksContext
  ) where

import Hakyll
import GHC.Generics
import Data.Aeson
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as L
import Text.Pandoc (runPure, readMarkdown, writeHtml5String, def)
import Text.Pandoc.Error (PandocError)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Talk data structure
data Talk = Talk
  { talkTitle :: String
  , talkDescription :: String  -- Markdown content
  , talkOrganisation :: String
  , talkYear :: Int
  , talkMonth :: Int
  , talkVideoUrl :: Maybe String
  , talkSlidesUrl :: Maybe String
  } deriving (Generic, Show)

instance FromJSON Talk where
  parseJSON = withObject "Talk" $ \o -> Talk
    <$> o .: "title"
    <*> o .: "description"
    <*> o .: "organisation"
    <*> o .: "year"
    <*> o .: "month"
    <*> o .:? "videoUrl"
    <*> o .:? "slidesUrl"

instance ToJSON Talk

--------------------------------------------------------------------------------
-- Load talks from JSON file
loadTalks :: FilePath -> IO [Talk]
loadTalks filePath = do
  jsonData <- L.readFile filePath
  case eitherDecode jsonData of
    Left err -> do
      putStrLn $ "Error parsing talks JSON: " ++ err
      return []
    Right talks -> return talks

-- Convert markdown to HTML
markdownToHtml :: String -> String
markdownToHtml markdown = 
  case runPure (readMarkdown def (T.pack markdown) >>= writeHtml5String def) of
    Left _ -> markdown  -- Fallback to original text if parsing fails
    Right html -> T.unpack html

-- Create a context for a single talk
talkContext :: Talk -> Context String
talkContext talk = mconcat
  [ constField "title" (talkTitle talk)
  , constField "description" (markdownToHtml $ talkDescription talk)
  , constField "organisation" (talkOrganisation talk)
  , constField "year" (show $ talkYear talk)
  , constField "month" (show $ talkMonth talk)
  , constField "date" (formatDate (talkYear talk) (talkMonth talk))
  , constField "videoUrl" (fromMaybe "" $ talkVideoUrl talk)
  , constField "slidesUrl" (fromMaybe "" $ talkSlidesUrl talk)
  , constField "hasVideo" (if null (fromMaybe "" $ talkVideoUrl talk) then "false" else "true")
  , constField "hasSlides" (if null (fromMaybe "" $ talkSlidesUrl talk) then "false" else "true")
  ]

-- Format date for display
formatDate :: Int -> Int -> String
formatDate year month = monthName month ++ " " ++ show year
  where
    monthName 1 = "January"
    monthName 2 = "February"
    monthName 3 = "March"
    monthName 4 = "April"
    monthName 5 = "May"
    monthName 6 = "June"
    monthName 7 = "July"
    monthName 8 = "August"
    monthName 9 = "September"
    monthName 10 = "October"
    monthName 11 = "November"
    monthName 12 = "December"
    monthName _ = "Unknown"

-- Create context for list of talks
talksContext :: [Talk] -> Context String
talksContext talks = listField "talks" talkItemContext (mapM makeItem talks)
  where
    talkItemContext = mconcat
      [ field "title" (return . talkTitle . itemBody)
      , field "description" (return . markdownToHtml . talkDescription . itemBody)
      , field "organisation" (return . talkOrganisation . itemBody)
      , field "year" (return . show . talkYear . itemBody)
      , field "month" (return . show . talkMonth . itemBody)
      , field "date" (\item -> return $ formatDate (talkYear $ itemBody item) (talkMonth $ itemBody item))
      , field "videoUrl" (return . fromMaybe "" . talkVideoUrl . itemBody)
      , field "slidesUrl" (return . fromMaybe "" . talkSlidesUrl . itemBody)
      , field "hasVideo" (\item -> return $ if null (fromMaybe "" $ talkVideoUrl $ itemBody item) then "false" else "true")
      , field "hasSlides" (\item -> return $ if null (fromMaybe "" $ talkSlidesUrl $ itemBody item) then "false" else "true")
      ]