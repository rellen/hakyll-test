--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Site.Pdf
  ( generatePdfRules,
    generatePdfFromMarkdown,
  )
where

import Hakyll
import System.Process (system)
import qualified Data.ByteString as BS
import System.IO.Temp (withTempFile)
import System.Directory (getTemporaryDirectory)
import System.IO (hClose)

--------------------------------------------------------------------------------
-- Hakyll-style PDF generation rules
generatePdfRules :: Rules ()
generatePdfRules = do
  -- Generate PDF version of about page from markdown
  generateAboutPdf

-- Generate PDF for the about page
generateAboutPdf :: Rules ()
generateAboutPdf = do
  create ["about.pdf"] $ do
    route idRoute
    compile $ do
      aboutBody <- unsafeCompiler $ readFile "about.md"
      generatePdfFromMarkdown "About - Robert Ellen" "Robert Ellen" aboutBody

-- Generate a PDF from markdown content using Pandoc
generatePdfFromMarkdown :: String -> String -> String -> Compiler (Item BS.ByteString)
generatePdfFromMarkdown title author markdownContent = do
  unsafeCompiler $ do
    putStrLn $ "Generating PDF: " ++ title
    tempDir <- getTemporaryDirectory
    withTempFile tempDir "output.pdf" $ \tempFile handle -> do
      hClose handle
      -- Write markdown content to temporary file
      writeFile "/tmp/hakyll-source.md" markdownContent
      -- Generate PDF using Pandoc
      exitCode <- system $ "pandoc --from=markdown --to=pdf --pdf-engine=xelatex " ++
                  "--variable=title:'" ++ title ++ "' " ++
                  "--variable=author:'" ++ author ++ "' " ++
                  "--variable=date:\\\\today " ++
                  "--variable=geometry:margin=1in " ++
                  "/tmp/hakyll-source.md -o " ++ tempFile ++ " 2>&1"
      putStrLn $ "Pandoc exit code: " ++ show exitCode
      -- Read the generated PDF file and return its content
      content <- BS.readFile tempFile
      putStrLn $ "Generated " ++ show (BS.length content) ++ " bytes for " ++ title
      return content
  >>= makeItem