--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Control.Monad (liftM)
import System.Process (callProcess, readProcessWithExitCode)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import Data.List (nub)

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
          >>= loadAndApplyTemplate "templates/page.html" siteCtx
          >>= loadAndApplyTemplate "templates/default.html" siteCtx
          >>= relativizeUrls

    match "notes/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/note.html" (noteCtx `mappend` siteCtx)
          >>= loadAndApplyTemplate "templates/default.html" (noteCtx `mappend` siteCtx)
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
          >>= loadAndApplyTemplate "templates/notes-archive.html" notesArchiveCtx
          >>= loadAndApplyTemplate "templates/default.html" notesArchiveCtx
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
          >>= loadAndApplyTemplate "templates/tag-archive.html" tagCtx
          >>= loadAndApplyTemplate "templates/default.html" tagCtx
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
-- Hakyll-style favicon generation rules
generateFaviconRules :: Rules ()
generateFaviconRules = do
  -- Generate PNG favicons from source SVG first
  mapM_ generatePNGFaviconRule faviconSizes

  -- Copy SVG favicon directly to root
  match "logo-base.svg" $ do
    route $ constRoute "favicon.svg"
    compile copyFileCompiler
  
  -- Generate web manifest directly to root
  create ["site.webmanifest"] $ do
    route idRoute
    compile $ makeItem webManifestContent
  
  -- Generate browserconfig directly to root
  create ["browserconfig.xml"] $ do
    route idRoute
    compile $ makeItem browserConfigContent

-- Generate a single PNG favicon rule
generatePNGFaviconRule :: (Int, String) -> Rules ()
generatePNGFaviconRule (size, filename) = do
  create [fromFilePath filename] $ do
    route idRoute
    compile $ do
      -- Get the current item's destination path
      itemId <- getUnderlying
      dest <- getRoute itemId
      case dest of
        Just destPath -> do
          unsafeCompiler $ do
            -- Use defaultConfiguration to get the default destination directory
            let destDir = destinationDirectory defaultConfiguration
            let fullPath = destDir </> destPath
            magickPath <- findImageMagick
            case magickPath of
              Just path -> callProcess path
                [ "-background", "transparent"
                , "-size", show size ++ "x" ++ show size
                , "logo-base.svg"
                , fullPath
                ]
              Nothing -> putStrLn $ "Error: ImageMagick not found, skipping " ++ filename
          makeItem ("" :: String)
        Nothing -> do
          unsafeCompiler $ putStrLn $ "Error: Could not determine output path for " ++ filename
          makeItem ("" :: String)

--------------------------------------------------------------------------------
-- Web manifest content as a string constant
webManifestContent :: String
webManifestContent = unlines
  [ "{"
  , "  \"name\": \"Rob's Ramblings\","
  , "  \"short_name\": \"r!\","
  , "  \"description\": \"Rob's technical blog and ramblings\","
  , "  \"icons\": ["
  , "    {"
  , "      \"src\": \"/android-chrome-192x192.png\","
  , "      \"sizes\": \"192x192\","
  , "      \"type\": \"image/png\""
  , "    },"
  , "    {"
  , "      \"src\": \"/android-chrome-512x512.png\","
  , "      \"sizes\": \"512x512\","
  , "      \"type\": \"image/png\""
  , "    }"
  , "  ],"
  , "  \"theme_color\": \"#3e8fb0\","
  , "  \"background_color\": \"#232136\","
  , "  \"display\": \"minimal-ui\","
  , "  \"start_url\": \"/\""
  , "}"
  ]

-- Browser config content as a string constant
browserConfigContent :: String
browserConfigContent = unlines
  [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
  , "<browserconfig>"
  , "    <msapplication>"
  , "        <tile>"
  , "            <square150x150logo src=\"/mstile-150x150.png\"/>"
  , "            <square310x310logo src=\"/mstile-310x310.png\"/>"
  , "            <TileColor>#3e8fb0</TileColor>"
  , "        </tile>"
  , "    </msapplication>"
  , "</browserconfig>"
  ]

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

-- Custom split function for comma-separated values
splitOnComma :: String -> [String]
splitOnComma [] = []
splitOnComma str = 
  let (before, after) = break (== ',') str
  in before : case after of
                [] -> []
                (_:rest) -> splitOnComma rest

-- Custom tag extractor for comma-separated tags
getNoteTags :: MonadMetadata m => Identifier -> m [String]
getNoteTags identifier = do
  metadata <- getMetadata identifier
  case lookupString "tags" metadata of
    Nothing -> return []
    Just tagsStr -> return $ map (dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse) $ splitOnComma tagsStr

-- Custom tags context that handles comma-separated tags with links
tagsCtx :: Context String
tagsCtx = field "tags" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  case lookupString "tags" metadata of
    Nothing -> noResult "No tags field"
    Just tagsStr -> do
      let tags = map (dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse) $ splitOnComma tagsStr
          tagLinks = map (\tag -> "<a href=\"/note-tags/" ++ urlEncode tag ++ ".html\" class=\"note-tag\">" ++ tag ++ "</a>") tags
      return $ unwords tagLinks

-- Simple URL encoding for tag names  
urlEncode :: String -> String
urlEncode = map encode
  where
    encode c
      | c == ' ' = '-'
      | c `elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" :: String) = c
      | otherwise = '-'  -- Replace any special characters with dash

-- Custom tag cloud renderer for note tags
renderNoteTagCloud :: Double -> Double -> Tags -> Compiler String
renderNoteTagCloud minSize maxSize tags = do
  let tagCounts = map (\(tag, identifiers) -> (tag, length identifiers)) $ tagsMap tags
  
  if null tagCounts
    then return ""
    else do
      let minCount = fromIntegral $ minimum $ map snd tagCounts
          maxCount = fromIntegral $ maximum $ map snd tagCounts
          sizeRange = if maxCount == minCount then 0 else maxSize - minSize
          
      let renderTag (tag, count) = 
            let size = if sizeRange == 0 
                      then minSize 
                      else minSize + (fromIntegral count - minCount) / (maxCount - minCount) * sizeRange
                sizeEm = size
                noteCount = show count
                plural = if count == 1 then "note" else "notes"
            in "<li><a href=\"/note-tags/" ++ urlEncode tag ++ ".html\" style=\"font-size: " ++ show sizeEm ++ "em\" aria-label=\"View " ++ noteCount ++ " " ++ plural ++ " tagged with " ++ tag ++ "\">" ++ tag ++ "</a></li>"
            
      let tagItems = unlines $ map renderTag tagCounts
      return $ "<nav aria-label=\"Browse notes by tag\">\n  <ul class=\"tag-list\">\n" ++ tagItems ++ "  </ul>\n</nav>"

-- Post context (uses frontmatter date)
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` dateField "datetime" "%Y-%m-%d"
    `mappend` tagsCtx
    `mappend` defaultContext

-- Note context (uses file modification date)
noteCtx :: Context String
noteCtx =
  modificationTimeField "date" "%B %e, %Y"
    `mappend` modificationTimeField "datetime" "%Y-%m-%d"
    `mappend` tagsCtx
    `mappend` defaultContext

--------------------------------------------------------------------------------
-- Favicon generation

-- Favicon sizes and filenames
faviconSizes :: [(Int, String)]
faviconSizes = 
  [ (16, "favicon-16x16.png")
  , (32, "favicon-32x32.png")
  , (48, "favicon-48x48.png")
  , (64, "favicon-64x64.png")
  , (180, "apple-touch-icon.png")
  , (152, "apple-touch-icon-152x152.png")
  , (120, "apple-touch-icon-120x120.png")
  , (76, "apple-touch-icon-76x76.png")
  , (192, "android-chrome-192x192.png")
  , (512, "android-chrome-512x512.png")
  , (144, "mstile-144x144.png")
  , (150, "mstile-150x150.png")
  , (310, "mstile-310x310.png")
  ]

-- Find ImageMagick executable using which command
findImageMagick :: IO (Maybe String)
findImageMagick = do
  -- Use 'which' command to find magick in PATH
  result <- readProcessWithExitCode "which" ["magick"] ""
  case result of
    (ExitSuccess, path, _) -> return $ Just (strip path)
    _ -> do
      -- Fallback to convert if magick not found
      result2 <- readProcessWithExitCode "which" ["convert"] ""
      case result2 of
        (ExitSuccess, path, _) -> return $ Just (strip path)
        _ -> return Nothing
  where
    strip = reverse . dropWhile (== '\n') . reverse


