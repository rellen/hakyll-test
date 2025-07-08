--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
import qualified Data.Text as T
import Text.Printf (printf)
import Text.Pandoc (def)
import Text.Pandoc.Options (WriterOptions(..))
-- Import our custom modules

import Site.Context (copyrightCtx, getNoteTags, getPostTags, noteCtx, postCtx, renderNoteTagCloud, renderPostTagCloud, renderCombinedTagCloud)
import Site.Favicon (generateFaviconRules)
import Site.Pdf (generatePdfRules)
import Site.Talks (loadTalks, talksContext, talkToItem, talkRssContext, Talk(..))
import Site.Util (applyTemplateChain)

--------------------------------------------------------------------------------
main :: IO ()
main = do
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

    -- Generate PDF files using custom compilers
    generatePdfRules

    match (fromList ["about.md", "contact.md"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= applyTemplateChain [("templates/page.html", siteCtx), ("templates/default.html", siteCtx)]
          >>= relativizeUrls

    -- Generate talks page from Dhall data
    create ["talks.html"] $ do
      route idRoute
      compile $ do
        talks <- unsafeCompiler $ loadTalks "data/talks.dhall"
        let talksPageCtx =
              talksContext talks
                `mappend` constField "title" "Talks"
                `mappend` siteCtx

        makeItem ""
          >>= applyTemplateChain [("templates/talks.html", talksPageCtx), ("templates/default.html", talksPageCtx)]
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
                `mappend` constField "title" "Posts"
                `mappend` siteCtx

        makeItem ""
          >>= applyTemplateChain [("templates/archive.html", archiveCtx), ("templates/default.html", archiveCtx)]
          >>= relativizeUrls

    -- Generate tag archive pages for notes and posts
    noteTags <- buildTagsWith getNoteTags "notes/*" (fromCapture "note-tags/*.html")
    postTags <- buildTagsWith getPostTags "posts/*" (fromCapture "post-tags/*.html")

    create ["notes.html"] $ do
      route idRoute
      compile $ do
        notes <- loadAll "notes/*"
        let notesArchiveCtx =
              listField "notes" noteCtx (return notes)
                `mappend` constField "title" "Notes"
                `mappend` siteCtx

        makeItem ""
          >>= applyTemplateChain [("templates/notes-archive.html", notesArchiveCtx), ("templates/default.html", notesArchiveCtx)]
          >>= relativizeUrls

    -- Note tags page
    create ["note-tags.html"] $ do
      route idRoute
      compile $ do
        tagCloud <- renderNoteTagCloud 0.8 1.6 noteTags
        let tagsPageCtx =
              constField "title" "Browse Notes by Tag"
                `mappend` constField "tagcloud" tagCloud
                `mappend` siteCtx

        makeItem ""
          >>= applyTemplateChain [("templates/note-tags-page.html", tagsPageCtx), ("templates/default.html", tagsPageCtx)]
          >>= relativizeUrls

    -- Post tags page
    create ["post-tags.html"] $ do
      route idRoute
      compile $ do
        tagCloud <- renderPostTagCloud 0.8 1.6 postTags
        let tagsPageCtx =
              constField "title" "Browse Posts by Tag"
                `mappend` constField "tagcloud" tagCloud
                `mappend` siteCtx

        makeItem ""
          >>= applyTemplateChain [("templates/post-tags-page.html", tagsPageCtx), ("templates/default.html", tagsPageCtx)]
          >>= relativizeUrls

    -- Combined tags page (all content)
    create ["tags.html"] $ do
      route idRoute
      compile $ do
        tagCloud <- renderCombinedTagCloud 0.8 1.6 noteTags postTags
        let tagsPageCtx =
              constField "title" "Browse All Content by Tag"
                `mappend` constField "tagcloud" tagCloud
                `mappend` siteCtx

        makeItem ""
          >>= applyTemplateChain [("templates/all-tags-page.html", tagsPageCtx), ("templates/default.html", tagsPageCtx)]
          >>= relativizeUrls

    -- Note tag archive pages
    tagsRules noteTags $ \tag notePattern -> do
      route idRoute
      compile $ do
        notes <- loadAll notePattern
        let tagCtx =
              constField "tag" tag
                `mappend` listField "notes" noteCtx (return notes)
                `mappend` constField "title" ("Notes tagged \"" ++ tag ++ "\"")
                `mappend` siteCtx

        makeItem ""
          >>= applyTemplateChain [("templates/note-tag-archive.html", tagCtx), ("templates/default.html", tagCtx)]
          >>= relativizeUrls

    -- Post tag archive pages
    tagsRules postTags $ \tag postPattern -> do
      route idRoute
      compile $ do
        posts <- loadAll postPattern
        let tagCtx =
              constField "tag" tag
                `mappend` listField "posts" postCtx (return posts)
                `mappend` constField "title" ("Posts tagged \"" ++ tag ++ "\"")
                `mappend` siteCtx

        makeItem ""
          >>= applyTemplateChain [("templates/post-tag-archive.html", tagCtx), ("templates/default.html", tagCtx)]
          >>= relativizeUrls

    match "index.html" $ do
      route $ setExtension "html"
      compile $ do
        posts <- fmap (take 8) $ recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts)
                `mappend` siteCtx

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= applyTemplateChain [("templates/default.html", indexCtx)]
          >>= relativizeUrls

    -- RSS Feeds
    create ["posts.xml"] $ do
      route idRoute
      compile $ do
        posts <- fmap (take 10) $ recentFirst =<< loadAll "posts/*"
        let rssCtx = constField "description" "Technical blog post from Rob's Ramblings" 
                   `mappend` postCtx `mappend` siteCtx
        renderRss feedConfiguration rssCtx posts

    create ["talks.xml"] $ do
      route idRoute
      compile $ do
        talks <- unsafeCompiler $ loadTalks "data/talks.dhall"
        talkItems <- mapM talkToItem talks
        let talkRssCtx = talkRssContext talks `mappend` siteCtx
        renderRss feedConfiguration talkRssCtx talkItems

    create ["feed.xml"] $ do
      route idRoute
      compile $ do
        posts <- fmap (take 5) $ recentFirst =<< loadAll "posts/*"
        talks <- unsafeCompiler $ loadTalks "data/talks.dhall"
        talkItems <- mapM talkToItem talks
        let allContent = posts ++ talkItems
            combinedCtx = mconcat
              [ postCtx
              , talkRssContext talks
              , constField "description" "Technical blog posts and talks from Rob's Ramblings"
              , siteCtx
              ]
        renderRss feedConfiguration combinedCtx allContent

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
-- RSS Feed Configuration
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle       = "Rob's Ramblings"
  , feedDescription = "Technical blog posts and talks on software engineering, programming, and technology"
  , feedAuthorName  = "Robert Ellen"
  , feedAuthorEmail = "noreply@example.com"
  , feedRoot        = "https://robertellen.dev"
  }
