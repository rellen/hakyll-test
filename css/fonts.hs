{-# LANGUAGE OverloadedStrings #-}

module GenerateFontCSS where

import           Clay
import Data.Map (Map)
import qualified Data.Map as Map


ahnFontWeightMap = Map.fromList
  [ ("ExtraLight", 200)
  , ("Light", 300)
  , ("Regular", 400)
  , ("Medium", 500)
  , ("SemiBold", 600)
  , ("Bold", 700)
  , ("ExtraBold", 800)
  ]
  
ahnFontFaceRule name suffix fweight styleSuffix = fontFace $ do
    fontFamily [name] []
    fontWeight $ (weight fweight)
    let style = case styleSuffix of
          "Italic" -> italic
          "" -> normal
        fontPath = "/fonts/" <> name <> "-" <> suffix <> styleSuffix <> ".woff2"
    fontStyle style
    fontFaceSrc [FontFaceSrcUrl fontPath (Just WOFF2)]

generateFontCSS :: Css
generateFontCSS = do
  sequence_ [ahnFontFaceRule "AtkinsonHyperlegibleNext" suffix weight "" | 
             (suffix, weight) <- Map.toList ahnFontWeightMap]
  
  sequence_ [ahnFontFaceRule "AtkinsonHyperlegibleNext" suffix weight "Italic" | 
             (suffix, weight) <- Map.toList ahnFontWeightMap]
   
main :: IO ()
main = putCss generateFontCSS
