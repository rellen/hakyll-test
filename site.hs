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

--------------------------------------------------------------------------------
main :: IO ()
main = do
  -- Generate favicon artifacts before building site
  generateFavicons
  
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

    -- Copy favicon files from _icons directory
    match "_icons/favicon.svg" $ do
      route $ gsubRoute "_icons/" (const "")
      compile copyFileCompiler
    
    match "_icons/favicon.ico" $ do
      route $ gsubRoute "_icons/" (const "")
      compile copyFileCompiler
    
    match "_icons/favicon-*.png" $ do
      route $ gsubRoute "_icons/" (const "")
      compile copyFileCompiler
    
    match "_icons/apple-touch-icon*.png" $ do
      route $ gsubRoute "_icons/" (const "")
      compile copyFileCompiler
    
    match "_icons/android-chrome-*.png" $ do
      route $ gsubRoute "_icons/" (const "")
      compile copyFileCompiler
    
    match "_icons/mstile-*.png" $ do
      route $ gsubRoute "_icons/" (const "")
      compile copyFileCompiler
    
    match "_icons/site.webmanifest" $ do
      route $ gsubRoute "_icons/" (const "")
      compile copyFileCompiler
    
    match "_icons/browserconfig.xml" $ do
      route $ gsubRoute "_icons/" (const "")
      compile copyFileCompiler
    
    -- Copy logo to images directory
    match "_icons/logo.svg" $ do
      route $ gsubRoute "_icons/" (const "images/")
      compile copyFileCompiler

    match (fromList ["about.md", "contact.md", "talks.md"]) $ do
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

-- Generate a single PNG favicon using ImageMagick
generateFaviconPNG :: Int -> String -> IO ()
generateFaviconPNG size filename = do
  putStrLn $ "Generating " ++ filename ++ " (" ++ show size ++ "x" ++ show size ++ ")"
  magickPath <- findImageMagick
  case magickPath of
    Just path -> callProcess path
      [ "-background", "transparent"
      , "-size", show size ++ "x" ++ show size
      , "logo-base.svg"
      , "_icons/" ++ filename
      ]
    Nothing -> putStrLn $ "Error: ImageMagick not found, skipping " ++ filename

-- Generate ICO file from PNG files
generateICO :: IO ()
generateICO = do
  putStrLn "Generating favicon.ico"
  magickPath <- findImageMagick
  case magickPath of
    Just path -> callProcess path ["_icons/favicon-16x16.png", "_icons/favicon-32x32.png", "_icons/favicon-48x48.png", "_icons/favicon.ico"]
    Nothing -> putStrLn "Error: ImageMagick not found, skipping favicon.ico"

-- Create web app manifest
createWebManifest :: IO ()
createWebManifest = do
  putStrLn "Creating site.webmanifest"
  writeFile "_icons/site.webmanifest" $ unlines
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

-- Create browserconfig.xml for Windows tiles
createBrowserConfig :: IO ()
createBrowserConfig = do
  putStrLn "Creating browserconfig.xml"
  writeFile "_icons/browserconfig.xml" $ unlines
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

-- Check if ImageMagick is available
checkImageMagick :: IO Bool
checkImageMagick = do
  magickPath <- findImageMagick
  case magickPath of
    Just _ -> return True
    Nothing -> do
      putStrLn "Warning: ImageMagick not found. PNG favicons will not be generated."
      putStrLn "Install with: brew install imagemagick (macOS) or sudo apt install imagemagick (Ubuntu)"
      return False

-- Main favicon generation function
generateFavicons :: IO ()
generateFavicons = do
  baseExists <- doesFileExist "logo-base.svg"
  if not baseExists
    then putStrLn "Warning: logo-base.svg not found, skipping favicon generation"
    else do
      putStrLn "Generating favicon artifacts from logo-base.svg"
      -- Ensure _icons directory exists
      callProcess "mkdir" ["-p", "_icons"]
      hasImageMagick <- checkImageMagick
      
      when hasImageMagick $ do
        -- Generate all PNG sizes
        mapM_ (uncurry generateFaviconPNG) faviconSizes
        
        -- Generate ICO file
        generateICO
      
      -- Copy base SVG for favicon and logo
      putStrLn "Copying logo-base.svg to favicon.svg"
      callProcess "cp" ["logo-base.svg", "_icons/favicon.svg"]
      putStrLn "Copying logo-base.svg to logo.svg"
      callProcess "cp" ["logo-base.svg", "_icons/logo.svg"]
      
      -- Create manifest and browserconfig
      createWebManifest
      createBrowserConfig
      
      putStrLn "✅ Favicon generation complete!"

-- Helper function for conditional execution
when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()
