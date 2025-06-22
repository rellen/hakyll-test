{-# LANGUAGE OverloadedStrings #-}

module DefaultCSS where

import Clay
import qualified Clay.Media as Media
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text.Lazy

-- Rose Pine color palette
-- Dawn (Light theme)
dawnBase         = "#faf4ed" :: Color -- Base background
dawnSurface      = "#fffaf3" :: Color -- Surface background
dawnOverlay      = "#f2e9e1" :: Color -- Overlay background
dawnMuted        = "#9893a5" :: Color -- Muted foreground
dawnSubtle       = "#797593" :: Color -- Subtle foreground
dawnText         = "#575279" :: Color -- Text foreground
dawnLove         = "#b4637a" :: Color -- Love accent
dawnGold         = "#ea9d34" :: Color -- Gold accent
dawnRose         = "#d7827e" :: Color -- Rose accent
dawnPine         = "#286983" :: Color -- Pine accent
dawnFoam         = "#56949f" :: Color -- Foam accent
dawnIris         = "#907aa9" :: Color -- Iris accent
dawnHighlightLow = "#f4ede8" :: Color -- Highlight low
dawnHighlightMed = "#dfdad9" :: Color -- Highlight med
dawnHighlightHigh = "#cecacd" :: Color -- Highlight high

-- Rose Pine (Dark theme)
roseBase         = "#191724" :: Color -- Base background
roseSurface      = "#1f1d2e" :: Color -- Surface background
roseOverlay      = "#26233a" :: Color -- Overlay background
roseMuted        = "#6e6a86" :: Color -- Muted foreground
roseSubtle       = "#908caa" :: Color -- Subtle foreground
roseText         = "#e0def4" :: Color -- Text foreground
roseLove         = "#eb6f92" :: Color -- Love accent
roseGold         = "#f6c177" :: Color -- Gold accent
roseRose         = "#ebbcba" :: Color -- Rose accent
rosePine         = "#31748f" :: Color -- Pine accent
roseFoam         = "#9ccfd8" :: Color -- Foam accent
roseIris         = "#c4a7e7" :: Color -- Iris accent
roseHighlightLow = "#21202e" :: Color -- Highlight low
roseHighlightMed = "#403d52" :: Color -- Highlight med
roseHighlightHigh = "#524f67" :: Color -- Highlight high

-- Rose Pine Moon (Dark theme)
moonBase         = "#232136" :: Color -- Base background
moonSurface      = "#2a273f" :: Color -- Surface background
moonOverlay      = "#393552" :: Color -- Overlay background
moonMuted        = "#6e6a86" :: Color -- Muted foreground
moonSubtle       = "#908caa" :: Color -- Subtle foreground
moonText         = "#e0def4" :: Color -- Text foreground
moonLove         = "#eb6f92" :: Color -- Love accent
moonGold         = "#f6c177" :: Color -- Gold accent
moonRose         = "#ea9a97" :: Color -- Rose accent
moonPine         = "#3e8fb0" :: Color -- Pine accent
moonFoam         = "#9ccfd8" :: Color -- Foam accent
moonIris         = "#c4a7e7" :: Color -- Iris accent
moonHighlightLow = "#2a283e" :: Color -- Highlight low
moonHighlightMed = "#44415a" :: Color -- Highlight med
moonHighlightHigh = "#56526e" :: Color -- Highlight high

-- *** CHANGE HERE TO SWITCH DARK THEMES ***
-- Options: "rose" (original Rose Pine) or "moon" (Rose Pine Moon)
darkTheme = "moon"  -- Set to "rose" or "moon"

-- Function to select dark theme colors based on the theme choice
darkBase = if darkTheme == "rose" then roseBase else moonBase
darkSurface = if darkTheme == "rose" then roseSurface else moonSurface
darkOverlay = if darkTheme == "rose" then roseOverlay else moonOverlay
darkMuted = if darkTheme == "rose" then roseMuted else moonMuted
darkSubtle = if darkTheme == "rose" then roseSubtle else moonSubtle
darkText = if darkTheme == "rose" then roseText else moonText
darkLove = if darkTheme == "rose" then roseLove else moonLove
darkGold = if darkTheme == "rose" then roseGold else moonGold
darkRose = if darkTheme == "rose" then roseRose else moonRose
darkPine = if darkTheme == "rose" then rosePine else moonPine
darkFoam = if darkTheme == "rose" then roseFoam else moonFoam
darkIris = if darkTheme == "rose" then roseIris else moonIris
darkHighlightLow = if darkTheme == "rose" then roseHighlightLow else moonHighlightLow
darkHighlightMed = if darkTheme == "rose" then roseHighlightMed else moonHighlightMed
darkHighlightHigh = if darkTheme == "rose" then roseHighlightHigh else moonHighlightHigh
darkModeStyles :: Css
darkModeStyles = do
  query Media.screen [Media.prefersColorScheme Media.dark] $ do
    body ? do
      color darkText
      backgroundColor darkBase
    
    header ? borderBottomColor darkMuted
    
    nav ? do
      a ? do
        color darkPine
        hover & color darkRose
    
    footer ? do
      borderTopColor darkMuted
      color darkMuted
    
    h1 ? color darkLove
    h2 ? color darkIris
    
    article ? do
      ".header" ? color darkSubtle
    
    a ? do
      color darkPine
      borderBottomColor darkPine
      hover & do
        color darkRose
        borderBottomColor darkRose
    
    ".logo" ? do
      a ? do
        color darkText
        hover & color darkLove
    
    Clay.code ? backgroundColor darkOverlay
    pre ? backgroundColor darkOverlay
    
    blockquote ? do
      borderLeftColor darkMuted
      color darkSubtle
    
    table ? do
      thead ? borderBottomColor darkMuted
      td ? borderBottomColor darkMuted

-- Dark mode implementation is now using Clay's prefersColorScheme function

fontSizeRem = fontSize . Clay.rem

paddingX x = do
  paddingLeft x
  paddingRight x

paddingY y = do
  paddingTop y
  paddingBottom y

-- Font definitions
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

-- Common styles for shared elements across media queries
mobileBodyStyle :: Css
mobileBodyStyle = do
  width $ Clay.pct 90
  margin nil nil nil nil
  padding nil (Clay.pct 5) nil (Clay.pct 5)

mobileHeaderStyle :: Css
mobileHeaderStyle = do
  margin (Clay.rem 4.2) nil (Clay.rem 4.2) nil

mobileNavStyle :: Css
mobileNavStyle = do
  margin nil auto (Clay.rem 3) auto
  textAlign center

mobileLogoStyle :: Css
mobileLogoStyle = do
  margin (Clay.rem 1) auto (Clay.rem 3) auto
  textAlign center

mobileLogoAStyle :: Css
mobileLogoAStyle = do
  fontSizeRem 2.4

-- Media query definitions
mediaQuery319 :: Css
mediaQuery319 = do
  query Media.screen [Media.maxWidth $ px 319] $ do
    body ? mobileBodyStyle
    header ? mobileHeaderStyle
    nav ? mobileNavStyle
    footer ? textAlign center
    
    nav ? do
      a ? do
        display block
        lineHeight $ unitless 1.6
    
    ".logo" ? mobileLogoStyle
    ".logo" ? do
      a ? mobileLogoAStyle

mediaQuery320 :: Css
mediaQuery320 = do
  query Media.screen [Media.minWidth $ px 320] $ do
    body ? mobileBodyStyle
    header ? mobileHeaderStyle
    nav ? mobileNavStyle
    footer ? textAlign center
    
    nav ? do
      a ? do
        display inline
        margin nil (Clay.rem 0.6) nil (Clay.rem 0.6)
    
    ".logo" ? mobileLogoStyle
    ".logo" ? do
      a ? mobileLogoAStyle

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

    footer ? textAlign end

    ".logo" ? do
      margin nil nil nil nil
      textAlign start

    ".logo" ? do
      a ? do
        float floatLeft
        fontSizeRem 1.8

-- Neutral/structure styles without colors (for both light and dark modes)
baseStyles :: Css
baseStyles = do
  html ? do
    fontSize $ pct 62.5
    fontFamily ["AtkinsonHyperlegibleNext", "ui-sans-serif"] []

  body ? do
    fontSizeRem 1.6
    lineHeight $ unitless 1.6
    -- Add transition for theme changes
    transition "color" (sec 0.3) ease (sec 0)
    transition "background-color" (sec 0.3) ease (sec 0)

  header ? do
    borderBottomWidth (Clay.rem 0.2)
    borderBottomStyle solid

  nav ? do
    textAlign end

  nav ? do
    a ? do
      fontSizeRem 1.8
      fontWeight bold
      textDecoration none
      textTransform uppercase

  footer ? do
    marginTop $ Clay.rem 3
    paddingX nil
    paddingY $ Clay.rem 1.2
    borderTopWidth (Clay.rem 0.2)
    borderTopStyle solid
    fontSizeRem 1.2
    fontStyle italic
    fontWeight $ weight 800

  h1 ? do
    fontSizeRem 2.4
    marginBottom (Clay.rem 2)

  h2 ? do
    fontSizeRem 2
    marginTop (Clay.rem 2.5)
    marginBottom (Clay.rem 1.5)

  article ? do
    ".header" ? do
      fontSizeRem 1.4
      fontStyle italic

  a ? do
    textDecoration none
    borderBottomWidth (px 1)
    borderBottomStyle solid

  ".logo" ? do
    a ? do
      fontWeight bold
      textDecoration none
      borderBottomStyle none

  -- Code blocks and inline code
  Clay.code ? do
    fontFamily ["monospace"] []
    paddingTop (px 2)
    paddingRight (px 4)
    paddingBottom (px 2)
    paddingLeft (px 4)
    fontSize (pct 90)

  pre ? do
    padding (Clay.rem 1) (Clay.rem 1) (Clay.rem 1) (Clay.rem 1)
    overflowX auto
    
    Clay.code ? do
      padding nil nil nil nil
      backgroundColor transparent

  blockquote ? do
    borderLeftWidth (px 4)
    borderLeftStyle solid
    paddingLeft (Clay.rem 1)
    fontStyle italic

  -- Lists styling
  ul ? do
    paddingLeft (Clay.rem 2)
    listStyleType disc
    
    li ? do
      marginBottom (Clay.rem 0.5)
  
  ol ? do
    paddingLeft (Clay.rem 2)
    
    li ? do
      marginBottom (Clay.rem 0.5)

  -- Tables
  table ? do
    width (pct 100)
    borderCollapse collapse
    marginBottom (Clay.rem 1.5)
    
    thead ? do
      borderBottomWidth (px 2)
      borderBottomStyle solid
      
      th ? do
        paddingBottom (Clay.rem 0.5)
        textAlign $ alignSide sideLeft
    
    td ? do
      padding (Clay.rem 0.5) (Clay.rem 0.5) (Clay.rem 0.5) nil
      borderBottomWidth (px 1)
      borderBottomStyle solid

-- Light mode styles using Rose Pine Dawn theme
lightModeStyles :: Css
lightModeStyles = do
  query Media.screen [Media.prefersColorScheme Media.light] $ do
    body ? do
      color dawnText
      backgroundColor dawnBase
    
    header ? borderBottomColor dawnMuted
    
    nav ? do
      a ? do
        color dawnPine
        hover & color dawnRose
    
    footer ? do
      borderTopColor dawnMuted
      color dawnMuted
    
    h1 ? color dawnLove
    h2 ? color dawnIris
    
    article ? do
      ".header" ? color dawnSubtle
    
    a ? do
      color dawnPine
      borderBottomColor dawnPine
      hover & do
        color dawnRose
        borderBottomColor dawnRose
    
    ".logo" ? do
      a ? do
        color dawnText
        hover & color dawnLove
    
    Clay.code ? backgroundColor dawnOverlay
    pre ? backgroundColor dawnOverlay
    
    blockquote ? do
      borderLeftColor dawnMuted
      color dawnSubtle
    
    table ? do
      thead ? borderBottomColor dawnMuted
      td ? borderBottomColor dawnMuted

-- Fallback styles (light theme for browsers without prefers-color-scheme support)
fallbackStyles :: Css
fallbackStyles = do
  -- Copy the light theme colors without the media query wrapper
  body ? do
    color dawnText
    backgroundColor dawnBase
  
  header ? borderBottomColor dawnMuted
  
  nav ? do
    a ? do
      color dawnPine
      hover & color dawnRose
  
  footer ? do
    borderTopColor dawnMuted
    color dawnMuted
  
  h1 ? color dawnLove
  h2 ? color dawnIris
  
  article ? do
    ".header" ? color dawnSubtle
  
  a ? do
    color dawnPine
    borderBottomColor dawnPine
    hover & do
      color dawnRose
      borderBottomColor dawnRose
  
  ".logo" ? do
    a ? do
      color dawnText
      hover & color dawnLove
  
  Clay.code ? backgroundColor dawnOverlay
  pre ? backgroundColor dawnOverlay
  
  blockquote ? do
    borderLeftColor dawnMuted
    color dawnSubtle
  
  table ? do
    thead ? borderBottomColor dawnMuted
    td ? borderBottomColor dawnMuted

main :: IO ()
main = putCss $ do
  generateFontCSS
  baseStyles     -- Structure without colors
  fallbackStyles -- Light theme fallback for browsers without prefers-color-scheme
  lightModeStyles -- Light theme media query
  darkModeStyles  -- Dark theme media query
  mediaQuery319
  mediaQuery320
  mediaQuery640
