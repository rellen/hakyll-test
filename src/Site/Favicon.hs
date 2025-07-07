--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Site.Favicon
  ( generateFaviconRules,
    generatePNGFaviconRule,
    webManifestContent,
    faviconSizes,
    findImageMagick,
  )
where

import Hakyll
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (callProcess, readProcessWithExitCode)
import System.Directory (copyFile, getTemporaryDirectory)
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import qualified Data.ByteString as BS

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

-- Generate a single PNG favicon rule
generatePNGFaviconRule :: (Int, String) -> Rules ()
generatePNGFaviconRule (size, filename) = do
  create [fromFilePath filename] $ do
    route idRoute
    compile $ do
      unsafeCompiler $ do
        magickPath <- findImageMagick
        case magickPath of
          Just path -> do
            putStrLn $ "Generating " ++ filename ++ " at " ++ show size ++ "px"
            tempDir <- getTemporaryDirectory
            withTempFile tempDir "favicon.png" $ \tempFile handle -> do
              hClose handle
              callProcess
                path
                [ "-background",
                  "transparent",
                  "logo-base.svg",
                  "-resize",
                  show size ++ "x" ++ show size,
                  tempFile
                ]
              -- Read the generated file and return its content
              content <- BS.readFile tempFile
              putStrLn $ "Generated " ++ show (BS.length content) ++ " bytes for " ++ filename
              return content
          Nothing -> do
            putStrLn $ "Error: ImageMagick not found, skipping " ++ filename
            return BS.empty
      >>= makeItem

--------------------------------------------------------------------------------
-- Web manifest content as a string constant
webManifestContent :: String
webManifestContent =
  unlines
    [ "{",
      "  \"name\": \"Rob's Ramblings\",",
      "  \"short_name\": \"r!\",",
      "  \"description\": \"Rob's technical blog and ramblings\",",
      "  \"icons\": [",
      "    {",
      "      \"src\": \"/icon-192.png\",",
      "      \"sizes\": \"192x192\",",
      "      \"type\": \"image/png\"",
      "    },",
      "    {",
      "      \"src\": \"/icon-512.png\",",
      "      \"sizes\": \"512x512\",",
      "      \"type\": \"image/png\"",
      "    }",
      "  ],",
      "  \"start_url\": \"/\",",
      "  \"display\": \"minimal-ui\",",
      "  \"theme_color\": \"#3e8fb0\",",
      "  \"background_color\": \"#232136\"",
      "}"
    ]


--------------------------------------------------------------------------------
-- Favicon sizes and filenames
faviconSizes :: [(Int, String)]
faviconSizes =
  [ (32, "favicon-32x32.png"),  -- For favicon.ico generation
    (192, "icon-192.png"),      -- PWA/Android
    (512, "icon-512.png")       -- PWA/Android
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
