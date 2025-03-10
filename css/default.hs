{-# LANGUAGE OverloadedStrings #-}

module DefaultCSS where

import Clay
import qualified Clay.Media as Media
import Data.Map (Map)
import qualified Data.Map as Map

fontSizeRem = fontSize . Clay.rem

paddingX x = do
  paddingLeft x
  paddingRight x

paddingY y = do
  paddingTop y
  paddingBottom y

ahnFontWeightMap = Map.fromList [("ExtraLight", 200), ("Light", 300), ("Regular", 400), ("Medium", 500), ("SemiBold", 600), ("Bold", 700), ("ExtraBold", 800)]

data FontStyleOption = Normal | Italic

ahnFontFaceRule name suffix fweight styleOption = fontFace $ do
  fontFamily [name] []
  fontWeight $ (weight fweight)
  let (style, styleSuffix) = case styleOption of
        Italic -> (italic, "Italic")
        Normal -> (normal, "")
      fontPath = "/fonts/" <> name <> "-" <> suffix <> styleSuffix <> ".woff2"
  fontStyle style
  fontFaceSrc [FontFaceSrcUrl fontPath (Just WOFF2)]

generateFontCSS :: Css
generateFontCSS = do
  sequence_
    [ ahnFontFaceRule "AtkinsonHyperlegibleNext" suffix weight Normal
      | (suffix, weight) <- Map.toList ahnFontWeightMap
    ]

  sequence_
    [ ahnFontFaceRule "AtkinsonHyperlegibleNext" suffix weight Italic
      | (suffix, weight) <- Map.toList ahnFontWeightMap
    ]

mediaQuery319 :: Css
mediaQuery319 = do
  query Media.screen [Media.maxWidth $ px 319] $ do
    body ? do
      width $ Clay.pct 90
      margin nil nil nil nil
      padding nil (Clay.pct 5) nil (Clay.pct 5)

    header ? do
      margin (Clay.rem 4.2) nil (Clay.rem 4.2) nil

    nav ? do
      margin nil auto (Clay.rem 3) auto
      textAlign center

    footer ?
      textAlign center

    nav ? do
      a ? do
        display block
        lineHeight $ unitless 1.6

    ".logo" ? do
      margin (Clay.rem 1) auto (Clay.rem 3) auto
      textAlign center

    ".logo" ? do
      a ? do
        fontSizeRem 2.4



mediaQuery320 :: Css
mediaQuery320 = do
  query Media.screen [Media.minWidth $ px 320] $ do
    body ? do
      width $ Clay.pct 90
      margin nil nil nil nil
      padding nil (Clay.pct 5) nil (Clay.pct 5)

    header ? do
      margin (Clay.rem 4.2) nil (Clay.rem 4.2) nil

    nav ? do
      margin nil auto (Clay.rem 3) auto
      textAlign center

    footer ?
      textAlign center

    nav ? do
      a ? do
        display inline
        margin nil (Clay.rem 0.6) nil (Clay.rem 0.6)

    ".logo" ? do
      margin (Clay.rem 1) auto (Clay.rem 3) auto
      textAlign center

    ".logo" ? do
      a ? do
        fontSizeRem 2.4


mediaQuery640 :: Css
mediaQuery640 = do
  query Media.screen [Media.minWidth $ px 640] $ do
    body ? do
      width $ Clay.rem 60
      margin nil auto nil auto
      padding nil nil nil nil

    header ? do
      margin nil nil (Clay.rem 3) nil
      paddingX nil
      paddingY $ Clay.rem 1.2

    nav ? do
      margin nil nil nil nil
      textAlign end

    nav ? do
      a ? do
        margin nil nil nil (Clay.rem 1.2)
        display inline

    footer ? do
      textAlign end

    ".logo" ? do
      margin nil nil nil nil
      textAlign start

    ".logo" ? do
      a ? do
        float floatLeft
        fontSizeRem 1.8


main :: IO ()
main = putCss $ do
  generateFontCSS

  html ? do
    fontSize $ pct 62.5
    fontFamily ["AtkinsonHyperlegibleNext", "ui-sans-serif"] []

  body ? do
    fontSizeRem 1.6
    color "#000"
    backgroundColor "#edd1b0"

  header ? do
    borderBottom (Clay.rem 0.2) solid "#000"

  nav ? do
    textAlign end

  nav ? do
    a ? do
      fontSizeRem 1.8
      fontWeight bold
      color black
      textDecoration none
      textTransform uppercase

  footer ? do
    marginTop $ Clay.rem 3
    paddingX nil
    paddingY $ Clay.rem 1.2
    borderTop (Clay.rem 0.2) solid "#000"
    fontSizeRem 1.2
    color "#555"
    fontStyle italic
    fontWeight $ weight 800

  h1 ? do
    fontSizeRem 2.4

  h2 ? do
    fontSizeRem 2

  article ? do
    ".header" ? do
      fontSizeRem 1.4
      fontStyle italic
      color "#555"

  ".logo" ? do
    a ? do
      fontWeight bold
      color "#000"
      textDecoration none

  mediaQuery319
  mediaQuery320
  mediaQuery640
