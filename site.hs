--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Control.Monad (liftM)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  -- Get copyright years and create a context
  copyrightYearsCtx <- copyrightCtx
  let siteCtx = copyrightYearsCtx `mappend` defaultContext
  
  hakyll $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*.hs" $ do
      route $ setExtension "css"
      compile $ do
        item <- getUnderlying
        let file = toFilePath item
        css <- getResourceString >>= withItemBody (unixFilter "cabal" ["exec", "--", "runghc", file])
        return $ fmap compressCss css

    match "css/*.css" $ do
      route idRoute
      compile compressCssCompiler

    match "fonts/*" $ do
      route idRoute
      compile copyFileCompiler

    match (fromList ["about.md", "contact.markdown", "talks.markdown"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/page.html" siteCtx
          >>= loadAndApplyTemplate "templates/default.html" siteCtx
          >>= relativizeUrls

    match "posts/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html" (postCtx `mappend` siteCtx)
          >>= loadAndApplyTemplate "templates/default.html" (postCtx `mappend` siteCtx)
          >>= relativizeUrls

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx =
              listField "posts" postCtx (return posts)
                `mappend` constField "title" "Archives"
                `mappend` siteCtx

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

    match "index.html" $ do
      route $ setExtension "html"
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts)
                `mappend` siteCtx

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
-- Get copyright year string
getCopyrightYears :: IO String
getCopyrightYears = do
  now <- getCurrentTime
  let (year, _, _) = toGregorian $ utctDay now
  if year == 2025
    then return "2025"
    else return $ "2025-" ++ show year

-- Create a context with the copyright years
copyrightCtx :: IO (Context String)
copyrightCtx = do
  years <- getCopyrightYears
  return $ constField "copyright" years

-- Post context
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext
