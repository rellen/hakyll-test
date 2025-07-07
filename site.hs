--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
-- Import our custom modules

import Site.Context (copyrightCtx, getNoteTags, noteCtx, postCtx, renderNoteTagCloud)
import Site.Favicon (generateFaviconRules)
import Site.Util (applyTemplateChain)

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

    -- Generate favicons from source SVG using custom compilers
    generateFaviconRules

    -- Favicon generation is now handled in generateFaviconRules
    -- No need for manual copying as files are generated directly to their target locations

    match (fromList ["about.md", "contact.md", "talks.md"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= applyTemplateChain [("templates/page.html", siteCtx), ("templates/default.html", siteCtx)]
          >>= relativizeUrls

    match "notes/*" $ do
      route $ setExtension "html"
      compile $ do
        let noteContext = noteCtx `mappend` siteCtx
        pandocCompiler
          >>= applyTemplateChain [("templates/note.html", noteContext), ("templates/default.html", noteContext)]
          >>= relativizeUrls

    match "posts/*" $ do
      route $ setExtension "html"
      compile $ do
        let blogContext = postCtx `mappend` siteCtx
        pandocCompiler
          >>= applyTemplateChain [("templates/post.html", blogContext), ("templates/default.html", blogContext)]
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
          >>= applyTemplateChain [("templates/archive.html", archiveCtx), ("templates/default.html", archiveCtx)]
          >>= relativizeUrls

    -- Generate tag archive pages
    tags <- buildTagsWith getNoteTags "notes/*" (fromCapture "note-tags/*.html")

    create ["notes.html"] $ do
      route idRoute
      compile $ do
        notes <- loadAll "notes/*"
        tagCloud <- renderNoteTagCloud 0.8 1.6 tags
        let notesArchiveCtx =
              listField "notes" noteCtx (return notes)
                `mappend` constField "title" "Notes"
                `mappend` constField "tagcloud" tagCloud
                `mappend` siteCtx

        makeItem ""
          >>= applyTemplateChain [("templates/notes-archive.html", notesArchiveCtx), ("templates/default.html", notesArchiveCtx)]
          >>= relativizeUrls
    tagsRules tags $ \tag pattern -> do
      route idRoute
      compile $ do
        notes <- loadAll pattern
        let tagCtx =
              constField "tag" tag
                `mappend` listField "notes" noteCtx (return notes)
                `mappend` constField "title" ("Notes tagged \"" ++ tag ++ "\"")
                `mappend` siteCtx

        makeItem ""
          >>= applyTemplateChain [("templates/tag-archive.html", tagCtx), ("templates/default.html", tagCtx)]
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
          >>= applyTemplateChain [("templates/default.html", indexCtx)]
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
