--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Site.Favicon 
  ( generateFaviconRules
  , generatePNGFaviconRule
  , webManifestContent
  , browserConfigContent
  , faviconSizes
  , findImageMagick
  ) where

import Hakyll
import System.Process (callProcess, readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))

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