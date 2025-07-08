{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Site.Talks
  ( Talk (..),
    loadTalks,
    talksContext,
    talkToItem,
    talkRssContext,
  )
where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Dhall
import GHC.Generics
import Hakyll
import Numeric.Natural (Natural)
import System.FilePath (takeBaseName)
import Text.Pandoc (def, readMarkdown, runPure, writeHtml5String)
import Text.Pandoc.Error (PandocError)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Talk data structure
data Talk = Talk
  { title :: T.Text,
    description :: T.Text, -- Markdown content
    organisation :: T.Text,
    year :: Natural,
    month :: Natural,
    video :: Maybe T.Text,
    slides :: Maybe T.Text
  }
  deriving (Generic, Show)

instance Dhall.FromDhall Talk

--------------------------------------------------------------------------------
-- Load talks from Dhall file
loadTalks :: FilePath -> IO [Talk]
loadTalks filePath = do
  Dhall.inputFile (Dhall.list Dhall.auto) filePath

-- Convert markdown to HTML
markdownToHtml :: T.Text -> String
markdownToHtml markdown =
  case runPure (readMarkdown def markdown >>= writeHtml5String def) of
    Left _ -> T.unpack markdown -- Fallback to original text if parsing fails
    Right html -> T.unpack html

-- Create a context for a single talk
talkContext :: Talk -> Context String
talkContext talk =
  mconcat
    [ constField "title" (T.unpack $ title talk),
      constField "description" (markdownToHtml $ description talk),
      constField "organisation" (T.unpack $ organisation talk),
      constField "year" (show $ year talk),
      constField "month" (show $ month talk),
      constField "date" (formatDate (year talk) (month talk)),
      constField "videoUrl" (T.unpack $ fromMaybe T.empty $ video talk),
      constField "slidesUrl" (T.unpack $ fromMaybe T.empty $ slides talk),
      constField "hasVideo" (if T.null (fromMaybe T.empty $ video talk) then "false" else "true"),
      constField "hasSlides" (if T.null (fromMaybe T.empty $ slides talk) then "false" else "true")
    ]

-- Format date for display
formatDate :: Natural -> Natural -> String
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
    talkItemContext =
      mconcat
        [ field "title" (return . T.unpack . title . itemBody),
          field "description" (return . markdownToHtml . description . itemBody),
          field "organisation" (return . T.unpack . organisation . itemBody),
          field "year" (return . show . year . itemBody),
          field "month" (return . show . month . itemBody),
          field "date" (\item -> return $ formatDate (year $ itemBody item) (month $ itemBody item)),
          field "videoUrl" (return . T.unpack . fromMaybe T.empty . video . itemBody),
          field "slidesUrl" (return . T.unpack . fromMaybe T.empty . slides . itemBody),
          field "hasVideo" (\item -> return $ if T.null (fromMaybe T.empty $ video $ itemBody item) then "false" else "true"),
          field "hasSlides" (\item -> return $ if T.null (fromMaybe T.empty $ slides $ itemBody item) then "false" else "true")
        ]

--------------------------------------------------------------------------------
-- Convert Talk to Item String for RSS feeds
talkToItem :: Talk -> Compiler (Item String)
talkToItem talk = do
  let slug = talkSlug talk
      identifier = fromFilePath $ "talks/" ++ slug ++ ".html"
      body = T.unpack $ description talk
  
  item <- makeItem body
  return $ item { itemIdentifier = identifier }

-- Create RSS context for talk items
talkRssContext :: [Talk] -> Context String
talkRssContext talks = mconcat
  [ field "title" $ \item -> do
      let slug = takeBaseName $ toFilePath $ itemIdentifier item
      case findTalkBySlug slug talks of
        Just talk -> return $ T.unpack $ title talk
        Nothing -> return "Unknown Talk"
  , field "description" $ \item -> do
      let slug = takeBaseName $ toFilePath $ itemIdentifier item
      case findTalkBySlug slug talks of
        Just talk -> return $ T.unpack $ description talk
        Nothing -> return $ itemBody item
  , field "published" $ \item -> do
      let slug = takeBaseName $ toFilePath $ itemIdentifier item
      case findTalkBySlug slug talks of
        Just talk -> return $ printf "%04d-%02d-01T00:00:00Z" (year talk) (month talk)
        Nothing -> return "2025-01-01T00:00:00Z"
  , field "updated" $ \item -> do
      let slug = takeBaseName $ toFilePath $ itemIdentifier item
      case findTalkBySlug slug talks of
        Just talk -> return $ printf "%04d-%02d-01T00:00:00Z" (year talk) (month talk)
        Nothing -> return "2025-01-01T00:00:00Z"
  , constField "author" "Robert Ellen"
  , urlField "url"
  ]

-- Helper function to find talk by slug
findTalkBySlug :: String -> [Talk] -> Maybe Talk
findTalkBySlug slug talks = 
  let expectedSlug = takeWhile (/= '.') slug  -- Remove .html extension
  in case filter (\talk -> talkSlug talk == expectedSlug) talks of
       (t:_) -> Just t
       [] -> Nothing

-- Generate a URL-safe slug from talk title
talkSlug :: Talk -> String
talkSlug talk = 
  let titleStr = T.unpack $ title talk
      -- Simple slug generation: lowercase, replace spaces/special chars with hyphens
      slug = map (\c -> if c `elem` (" ,.'\"!?()[]{}/" :: String) then '-' else c) $ 
             filter (\c -> c `notElem` ("&<>" :: String)) $
             map toLower titleStr
  in takeWhile (/= '\0') slug
  where
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
