--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Site.Util 
  ( splitOnComma
  , urlEncode
  , applyTemplateChain
  ) where

import Hakyll

--------------------------------------------------------------------------------
-- Custom split function for comma-separated values
splitOnComma :: String -> [String]
splitOnComma [] = []
splitOnComma str = 
  let (before, after) = break (== ',') str
  in before : case after of
                [] -> []
                (_:rest) -> splitOnComma rest

-- Simple URL encoding for tag names  
urlEncode :: String -> String
urlEncode = map encode
  where
    encode c
      | c == ' ' = '-'
      | c `elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" :: String) = c
      | otherwise = '-'  -- Replace any special characters with dash

-- Helper function to apply a chain of templates with contexts
applyTemplateChain :: [(Identifier, Context String)] -> Item String -> Compiler (Item String)
applyTemplateChain [] item = return item
applyTemplateChain ((templateId, ctx):rest) item = do
  result <- loadAndApplyTemplate templateId ctx item
  applyTemplateChain rest result