{-# LANGUAGE OverloadedStrings #-}

module DefaultCSS where

import Clay
import qualified Clay.Media as Media
import Clay.Stylesheet (Feature (..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text.Lazy

-- Custom media feature for prefers-reduced-motion
-- Following the same pattern as Clay's prefersColorScheme
prefersReducedMotion :: Feature
prefersReducedMotion = Feature "prefers-reduced-motion" (Just "reduce")

-- Rose Pine color palette
-- Dawn (Light theme)
dawnBase = "#faf4ed" :: Color -- Base background

dawnSurface = "#fffaf3" :: Color -- Surface background

dawnOverlay = "#f2e9e1" :: Color -- Overlay background

dawnMuted = "#9893a5" :: Color -- Muted foreground

dawnSubtle = "#797593" :: Color -- Subtle foreground

dawnText = "#575279" :: Color -- Text foreground

dawnLove = "#b4637a" :: Color -- Love accent

dawnGold = "#ea9d34" :: Color -- Gold accent

dawnRose = "#d7827e" :: Color -- Rose accent

dawnPine = "#286983" :: Color -- Pine accent

dawnFoam = "#56949f" :: Color -- Foam accent

dawnIris = "#907aa9" :: Color -- Iris accent

dawnHighlightLow = "#f4ede8" :: Color -- Highlight low

dawnHighlightMed = "#dfdad9" :: Color -- Highlight med

dawnHighlightHigh = "#cecacd" :: Color -- Highlight high

-- Rose Pine (Dark theme)
roseBase = "#191724" :: Color -- Base background

roseSurface = "#1f1d2e" :: Color -- Surface background

roseOverlay = "#26233a" :: Color -- Overlay background

roseMuted = "#6e6a86" :: Color -- Muted foreground

roseSubtle = "#908caa" :: Color -- Subtle foreground

roseText = "#e0def4" :: Color -- Text foreground

roseLove = "#eb6f92" :: Color -- Love accent

roseGold = "#f6c177" :: Color -- Gold accent

roseRose = "#ebbcba" :: Color -- Rose accent

rosePine = "#31748f" :: Color -- Pine accent

roseFoam = "#9ccfd8" :: Color -- Foam accent

roseIris = "#c4a7e7" :: Color -- Iris accent

roseHighlightLow = "#21202e" :: Color -- Highlight low

roseHighlightMed = "#403d52" :: Color -- Highlight med

roseHighlightHigh = "#524f67" :: Color -- Highlight high

-- Rose Pine Moon (Dark theme)
moonBase = "#232136" :: Color -- Base background

moonSurface = "#2a273f" :: Color -- Surface background

moonOverlay = "#393552" :: Color -- Overlay background

moonMuted = "#6e6a86" :: Color -- Muted foreground

moonSubtle = "#908caa" :: Color -- Subtle foreground

moonText = "#e0def4" :: Color -- Text foreground

moonLove = "#eb6f92" :: Color -- Love accent

moonGold = "#f6c177" :: Color -- Gold accent

moonRose = "#ea9a97" :: Color -- Rose accent

moonPine = "#3e8fb0" :: Color -- Pine accent

moonFoam = "#9ccfd8" :: Color -- Foam accent

moonIris = "#c4a7e7" :: Color -- Iris accent

moonHighlightLow = "#2a283e" :: Color -- Highlight low

moonHighlightMed = "#44415a" :: Color -- Highlight med

moonHighlightHigh = "#56526e" :: Color -- Highlight high

-- *** CHANGE HERE TO SWITCH DARK THEMES ***

-- Options: "rose" (original Rose Pine) or "moon" (Rose Pine Moon)
darkTheme = "moon" -- Set to "rose" or "moon"

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
        color darkFoam
        hover & color darkIris

    footer ? do
      borderTopColor darkMuted
      color darkSubtle

    h1 ? color darkLove
    h2 ? color darkIris

    article ? do
      ".header" ? color darkSubtle

    a ? do
      color darkFoam
      hover & do
        color darkIris

    ".keep-in-touch ul li a" ? do
      textDecoration none

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

    -- Focus indicators for dark mode
    a ? do
      focus & do
        outline solid (px 2) darkFoam

    -- Navigation focus indicators for dark mode
    nav ? do
      a ? do
        focus & do
          outline solid (px 3) darkFoam

    -- Close button dark mode styles
    ".nav-close" ? do
      color darkText

      hover & do
        color darkLove

      focus & do
        outline solid (px 2) darkFoam

    -- Note colors for dark mode
    ".note" ? do
      ".note-header" ? do
        borderBottomColor darkMuted

      ".note-title" ? do
        color darkLove

      ".note-date" ? do
        color darkSubtle

      ".note-tag" ? do
        backgroundColor darkOverlay
        color darkFoam

    -- Breadcrumb colors for dark mode
    ".breadcrumbs" ? do
      ".breadcrumb-link" ? do
        color darkFoam

        hover & do
          color darkIris

    -- Talks colors for dark mode
    ".talks-list" ? do
      ".talk" ? do
        borderBottomColor darkMuted

      ".video-link" ? do
        backgroundColor darkFoam
        color darkBase

        hover & do
          backgroundColor darkPine

      ".slides-link" ? do
        backgroundColor darkIris
        color darkBase

        hover & do
          backgroundColor darkLove

    -- Notes listing colors for dark mode
    ".notes-list" ? do
      ".note-listing" ? do
        borderBottomColor darkMuted

        "h2" ? do
          "a" ? do
            color darkLove

            hover & do
              color darkIris

    -- Tag cloud colors for dark mode
    ".tag-cloud" ? do
      "a" ? do
        backgroundColor darkOverlay
        color darkFoam

        hover & do
          backgroundColor darkMuted
          color darkText

-- Reduced motion preferences
reducedMotionStyles :: Css
reducedMotionStyles = do
  query Media.screen [prefersReducedMotion] $ do
    -- Disable all transitions and animations
    star ? do
      transition "none" (sec 0) ease (sec 0)

    -- Disable hamburger animations
    ".hamburger" ? do
      transition "none" (sec 0) ease (sec 0)

    ".hamburger:before" ? do
      transition "none" (sec 0) ease (sec 0)

    ".hamburger:after" ? do
      transition "none" (sec 0) ease (sec 0)

    -- Disable nav menu slide animations
    nav ? do
      transition "none" (sec 0) ease (sec 0)

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

-- Hamburger menu styles
hamburgerMenuStyle :: Css
hamburgerMenuStyle = do
  ".nav-toggle" ? do
    position absolute
    opacity 0
    left $ px (-999)
    height $ px 0
    width $ px 0

  ".nav-toggle-label" ? do
    position absolute
    top $ Clay.rem 0.6
    right $ Clay.rem 0.6
    display none
    height $ Clay.rem 2.8
    width $ Clay.rem 2.8
    cursor pointer
    alignItems center
    justifyContent center

  ".nav-toggle-label" ? do
    color dawnText -- Set explicit color for light mode
  ".hamburger" ? do
    position relative
    width $ Clay.rem 2
    height $ Clay.rem 0.25
    backgroundColor dawnText -- Explicit color for light mode
    borderRadius (px 2) (px 2) (px 2) (px 2)
    transition "transform" (sec 0.3) ease (sec 0)

  ".hamburger:before" ? do
    content (stringContent "")
    position absolute
    left nil
    top $ Clay.rem (-0.6)
    width $ Clay.pct 100
    height $ Clay.rem 0.25
    backgroundColor dawnText -- Explicit color for light mode
    borderRadius (px 2) (px 2) (px 2) (px 2)
    transition "top" (sec 0.3) ease (sec 0)
    transition "transform" (sec 0.3) ease (sec 0)

  ".hamburger:after" ? do
    content (stringContent "")
    position absolute
    left nil
    bottom $ Clay.rem (-0.6)
    width $ Clay.pct 100
    height $ Clay.rem 0.25
    backgroundColor dawnText -- Explicit color for light mode
    borderRadius (px 2) (px 2) (px 2) (px 2)
    transition "bottom" (sec 0.3) ease (sec 0)
    transition "transform" (sec 0.3) ease (sec 0)

  -- Dark mode hamburger color
  query Media.screen [Media.prefersColorScheme Media.dark] $ do
    ".nav-toggle-label" ? do
      color darkText

    ".hamburger" ? do
      backgroundColor darkText

    ".hamburger:before" ? do
      backgroundColor darkText

    ".hamburger:after" ? do
      backgroundColor darkText

  ".nav-toggle:checked + .nav-toggle-label .hamburger" ? do
    transform $ rotate (deg 45)

  ".nav-toggle:checked + .nav-toggle-label .hamburger:before" ? do
    top nil
    transform $ rotate (deg 90)

  ".nav-toggle:checked + .nav-toggle-label .hamburger:after" ? do
    bottom nil
    transform $ rotate (deg 90)

  -- Close button styles
  ".nav-close" ? do
    display block
    position absolute
    top $ Clay.rem 0.3
    right $ Clay.rem 0.3
    fontSize $ Clay.rem 2.4
    fontWeight bold
    lineHeight $ unitless 1
    padding (Clay.rem 0.8) (Clay.rem 1) (Clay.rem 0.8) (Clay.rem 1)
    cursor pointer
    color dawnText
    textDecoration none

    hover & do
      color dawnLove

    focus & do
      outline solid (px 2) dawnPine
      outlineOffset (px 2)

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
  display flex
  alignItems center
  justifyContent center

mobileLogoIconStyle :: Css
mobileLogoIconStyle = do
  width (Clay.rem 3)
  height (Clay.rem 3)
  marginRight (Clay.rem 0.7)

-- Media query definitions
mediaQuery319 :: Css
mediaQuery319 = do
  query Media.screen [Media.maxWidth $ px 319] $ do
    body ? mobileBodyStyle
    header ? do
      mobileHeaderStyle
      position relative
      paddingRight $ Clay.rem 3.5

    footer ? textAlign center

    -- Mobile menu styling
    ".nav-toggle-label" ? display flex

    nav ? do
      position fixed
      top $ Clay.rem 4
      left nil
      right nil
      backgroundColor dawnBase
      textAlign center
      paddingTop $ Clay.rem 3
      paddingBottom $ Clay.rem 1
      transform $ translateY (px (-100))
      opacity 0
      visibility hidden -- Hide until checkbox is checked
      transition "transform" (sec 0.3) ease (sec 0)
      transition "opacity" (sec 0.3) ease (sec 0)
      transition "visibility" (sec 0.3) ease (sec 0)
      width $ Clay.pct 100
      zIndex 100 -- Increased z-index to ensure menu appears above content
    ".nav-toggle:checked ~ nav" ? do
      transform $ translateY nil
      opacity 1
      visibility visible
      borderBottomStyle solid
      borderBottomWidth (px 1)
      borderBottomColor dawnMuted

    nav ? do
      a ? do
        display block
        lineHeight $ unitless 1.6
        paddingTop $ Clay.rem 0.5
        paddingBottom $ Clay.rem 0.5

    ".logo" ? mobileLogoStyle
    ".logo" ? do
      a ? mobileLogoAStyle

    ".logo-icon" ? mobileLogoIconStyle

    ".footer-content" ? do
      flexDirection column
      alignItems center

    ".keep-in-touch" ? do
      flexDirection column
      marginBottom $ Clay.rem 1.5

    ".copyright" ? do
      flexDirection column
      alignItems center

    ".hakyll-credit" ? do
      flexDirection column
      alignItems center

    -- Dark mode adjustments for mobile menu
    query Media.screen [Media.prefersColorScheme Media.dark] $ do
      nav ? backgroundColor darkBase

mediaQuery320 :: Css
mediaQuery320 = do
  query Media.screen [Media.minWidth $ px 320, Media.maxWidth $ px 639] $ do
    body ? mobileBodyStyle
    header ? do
      mobileHeaderStyle
      position relative
      paddingRight $ Clay.rem 3.5

    footer ? textAlign center

    -- Mobile menu styling
    ".nav-toggle-label" ? display flex

    nav ? do
      position fixed
      top $ Clay.rem 4
      left nil
      right nil
      backgroundColor dawnBase
      textAlign center
      paddingTop $ Clay.rem 3
      paddingBottom $ Clay.rem 1
      transform $ translateY (px (-100))
      opacity 0
      visibility hidden -- Hide until checkbox is checked
      transition "transform" (sec 0.3) ease (sec 0)
      transition "opacity" (sec 0.3) ease (sec 0)
      transition "visibility" (sec 0.3) ease (sec 0)
      width $ Clay.pct 100
      zIndex 100 -- Increased z-index to ensure menu appears above content
    ".nav-toggle:checked ~ nav" ? do
      transform $ translateY nil
      opacity 1
      visibility visible
      borderBottomStyle solid
      borderBottomWidth (px 1)
      borderBottomColor dawnMuted

    nav ? do
      a ? do
        display block
        lineHeight $ unitless 1.6
        paddingTop $ Clay.rem 0.5
        paddingBottom $ Clay.rem 0.5

    ".logo" ? mobileLogoStyle
    ".logo" ? do
      a ? mobileLogoAStyle

    ".logo-icon" ? mobileLogoIconStyle

    ".footer-content" ? do
      flexDirection column
      alignItems center

    ".keep-in-touch" ? do
      flexDirection column
      marginBottom $ Clay.rem 1.5

    ".copyright" ? do
      flexDirection column
      alignItems center

    ".hakyll-credit" ? do
      flexDirection column
      alignItems center

    -- Dark mode adjustments for mobile menu
    query Media.screen [Media.prefersColorScheme Media.dark] $ do
      nav ? backgroundColor darkBase

mediaQuery640 :: Css
mediaQuery640 = do
  query Media.screen [Media.minWidth $ px 640] $ do
    body ? do
      width $ ch 65
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

    -- Three-column footer layout on desktop
    footer ? do
      fontSizeRem 1.4

    ".footer-content" ? do
      justifyContent spaceBetween

    ".copyright" ? do
      textAlign center

    ".hakyll-credit" ? do
      textAlign end

    ".logo" ? do
      margin nil nil nil nil
      textAlign start

    ".logo" ? do
      a ? do
        float floatLeft
        fontSizeRem 1.8

    ".logo-icon" ? do
      width (Clay.rem 2.2)
      height (Clay.rem 2.2)
      marginRight (Clay.rem 0.5)

-- Neutral/structure styles without colors (for both light and dark modes)
baseStyles :: Css
baseStyles = do
  html ? do
    fontSize $ pct 62.5
    fontFamily ["AtkinsonHyperlegibleNext", "ui-sans-serif"] []
    -- Force scrollbar to always be visible to prevent layout shifts
    overflowY scroll

  body ? do
    fontSizeRem 1.6
    lineHeight $ unitless 1.6
    -- Reduce flash by smoothing initial render
    transition "color" (sec 0.1) ease (sec 0)
    transition "background-color" (sec 0.1) ease (sec 0)
    transition "opacity" (sec 0.1) ease (sec 0)

  -- Add text justification to main content
  "main p" ? do
    textAlign justify

  -- Screen reader only content
  ".sr-only" ? do
    position absolute
    width (px 1)
    height (px 1)
    paddingTop nil
    paddingRight nil
    paddingBottom nil
    paddingLeft nil
    marginTop (px (-1))
    marginRight (px (-1))
    marginBottom (px (-1))
    marginLeft (px (-1))
    overflow hidden
    clip $ rect (px 0) (px 0) (px 0) (px 0)
    borderWidth nil
    whiteSpace nowrap

  header ? do
    borderBottomWidth (Clay.rem 0.2)
    borderBottomStyle solid

  nav ? do
    textAlign end

  nav ? do
    a ? do
      fontSizeRem 1.8
      fontWeight bold
      -- Removed textTransform uppercase

      -- Enhanced focus for navigation links
      focus & do
        outline solid (px 3) transparent
        outlineOffset (px 3)

  footer ? do
    marginTop $ Clay.rem 3
    paddingX nil
    paddingY $ Clay.rem 1.2
    borderTopWidth (Clay.rem 0.2)
    borderTopStyle solid
    fontSizeRem 1.2

  ".footer-content" ? do
    display flex
    flexDirection row
    justifyContent spaceBetween
    alignItems baseline
    marginBottom $ Clay.rem 2

  ".keep-in-touch" ? do
    display flex
    flexDirection row
    alignItems center

    h3 ? do
      fontSizeRem 1.4
      marginRight $ Clay.rem 1
      fontWeight $ weight 600
      marginBottom nil

    ul ? do
      listStyleType none
      paddingLeft nil
      display flex
      flexDirection row
      marginBottom nil
      alignItems center

      li ? do
        marginBottom nil
        marginRight $ Clay.rem 1.5
        display flex
        alignItems center

    -- Skip link that overrides sr-only when focused
    ".skip-link" ? do
      focus & do
        position static
        width auto
        height auto
        padding (Clay.rem 0.5) (Clay.rem 1) (Clay.rem 0.5) (Clay.rem 1)
        margin nil nil nil nil
        overflow visible
        clip auto
        whiteSpace normal
        zIndex 1000
        display block
        fontWeight bold
        textDecoration none

    ".social-icon" ? do
      width $ Clay.rem 1.6
      height $ Clay.rem 1.6
      color inherit
      display inlineBlock
      verticalAlign middle

      hover & do
        color dawnRose

    -- Focus indicator for social icon parent links
    ".keep-in-touch ul li a" ? do
      focus & do
        outline solid (px 2) transparent
        outlineOffset (px 2)
        borderRadius (px 4) (px 4) (px 4) (px 4)

  ".copyright" ? do
    fontStyle italic
    fontWeight $ weight 400

    p ? do
      marginBottom nil

  ".hakyll-credit" ? do
    fontStyle italic
    fontWeight $ weight 400

    p ? do
      marginBottom nil

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

  -- Note-specific styling
  ".note" ? do
    marginBottom (Clay.rem 2)

    ".note-header" ? do
      marginBottom (Clay.rem 1.5)
      paddingBottom (Clay.rem 1)
      borderBottomWidth (px 1)
      borderBottomStyle solid

    ".note-title" ? do
      fontSizeRem 2
      marginBottom (Clay.rem 0.5)
      fontWeight $ weight 600

    ".note-date" ? do
      fontSizeRem 1.2
      fontStyle italic
      display block
      marginBottom (Clay.rem 0.5)

    ".note-dates" ? do
      display flex
      fontSize (Clay.rem 1.2)
      fontStyle italic
      marginBottom (Clay.rem 0.5)
      "flex-wrap" -: "wrap"

    ".note-dates > *" ? do
      marginRight (Clay.rem 1)

    ".note-published" ? do
      fontWeight $ weight 500

    ".note-updated" ? do
      opacity 0.8

    ".note-tags" ? do
      display flex
      "flex-wrap" -: "wrap"

    ".note-tag" ? do
      fontSizeRem 1.1
      paddingTop (Clay.rem 0.25)
      paddingRight (Clay.rem 0.5)
      paddingBottom (Clay.rem 0.25)
      paddingLeft (Clay.rem 0.5)
      marginRight (Clay.rem 0.5)
      marginBottom (Clay.rem 0.5)
      borderRadius (px 3) (px 3) (px 3) (px 3)
      fontWeight $ weight 500
      textDecoration none
      display inlineBlock

      hover & do
        textDecoration none
        opacity 0.8

    ".note-content" ? do
      lineHeight $ unitless 1.6

  -- Notes listing (for archive pages)
  ".notes-list" ? do
    marginTop (Clay.rem 2)

    ".note-listing" ? do
      marginBottom (Clay.rem 2)
      paddingBottom (Clay.rem 1.5)
      borderBottomWidth (px 1)
      borderBottomStyle solid

      "h2" ? do
        marginBottom (Clay.rem 0.5)
        fontSizeRem 1.8

        "a" ? do
          textDecoration none

          hover & do
            textDecoration underline

      ".note-date" ? do
        fontSize (Clay.rem 1.2)
        fontStyle italic
        display block
        marginBottom (Clay.rem 0.5)

      ".note-dates" ? do
        display flex
        fontSize (Clay.rem 1.1)
        fontStyle italic
        marginBottom (Clay.rem 0.5)
        "flex-wrap" -: "wrap"

      ".note-dates > *" ? do
        marginRight (Clay.rem 1)

      ".note-published" ? do
        fontWeight $ weight 500

      ".note-updated" ? do
        opacity 0.8

      ".note-tags" ? do
        marginTop (Clay.rem 0.5)

  -- Tag cloud styling
  ".tag-cloud" ? do
    marginTop (Clay.rem 2)
    marginBottom (Clay.rem 3)
    textAlign center
    lineHeight $ unitless 1.8

    "nav" ? do
      display block

    ".tag-list" ? do
      listStyleType none
      paddingLeft nil
      display flex
      "flex-wrap" -: "wrap"
      justifyContent center
      alignItems center
      marginBottom nil

      "li" ? do
        display inlineBlock
        marginBottom nil

    "a" ? do
      display inlineBlock
      margin (Clay.rem 0.3) (Clay.rem 0.5) (Clay.rem 0.3) (Clay.rem 0.5)
      padding (Clay.rem 0.4) (Clay.rem 0.8) (Clay.rem 0.4) (Clay.rem 0.8)
      textDecoration none
      borderRadius (px 20) (px 20) (px 20) (px 20)
      fontWeight $ weight 500
      transition "all" (sec 0.2) ease (sec 0)

      hover & do
        textDecoration none
        opacity 0.8

  -- Breadcrumbs styling
  ".breadcrumbs" ? do
    marginBottom (Clay.rem 1.5)
    fontSize (Clay.rem 1.4)

    ".breadcrumb-link" ? do
      textDecoration none
      fontWeight $ weight 500

      hover & do
        textDecoration underline

    ".breadcrumb-separator" ? do
      margin (Clay.rem 0) (Clay.rem 0.5) (Clay.rem 0) (Clay.rem 0.5)
      opacity 0.6

  ".breadcrumbs-bottom" ? do
    marginTop (Clay.rem 2)
    marginBottom (Clay.rem 1)

  -- Talks styling
  ".talks-list" ? do
    marginTop (Clay.rem 2)

    ".talk" ? do
      marginBottom (Clay.rem 3)
      paddingBottom (Clay.rem 2)
      borderBottomStyle solid
      borderBottomWidth (px 1)
      borderBottomColor "#e5e5e5"

      ".talk-header" ? do
        marginBottom (Clay.rem 1)

        ".talk-title" ? do
          fontSize (Clay.rem 1.8)
          fontWeight $ weight 600
          marginBottom (Clay.rem 0.5)
          lineHeight $ unitless 1.3

        ".talk-meta" ? do
          display flex
          "flex-wrap" -: "wrap"
          alignItems center
          fontSize (Clay.rem 1.4)
          "gap" -: "1rem"

          ".talk-organisation" ? do
            fontWeight $ weight 500

          ".talk-date" ? do
            fontStyle italic
            opacity 0.8

      ".talk-description" ? do
        marginBottom (Clay.rem 1.5)
        lineHeight $ unitless 1.6

        "p:last-child" ? do
          marginBottom nil

      ".talk-links" ? do
        display flex
        "gap" -: "1rem"
        "flex-wrap" -: "wrap"

        ".talk-link" ? do
          display inlineBlock
          padding (Clay.rem 0.5) (Clay.rem 1) (Clay.rem 0.5) (Clay.rem 1)
          textDecoration none
          borderRadius (px 6) (px 6) (px 6) (px 6)
          fontWeight $ weight 500
          fontSize (Clay.rem 1.4)
          transition "all" (sec 0.2) ease (sec 0)

          ".link-icon" ? do
            marginRight (Clay.rem 0.5)

          hover & do
            textDecoration none

  a ? do
    textDecoration underline
    -- Using the standard text-decoration property only
    -- Clay 0.16.0 doesn't support newer CSS text decoration properties

    -- Focus indicator for keyboard navigation
    focus & do
      outline solid (px 2) transparent
      outlineOffset (px 2)

  ".logo" ? do
    a ? do
      fontWeight bold
      textDecoration none
      display flex
      alignItems center

      -- Logo focus indicator
      focus & do
        outline solid (px 3) transparent
        outlineOffset (px 3)
        borderRadius (px 4) (px 4) (px 4) (px 4)

  ".logo-icon" ? do
    width (Clay.rem 2)
    height (Clay.rem 2)
    marginRight (Clay.rem 0.5)
    flexShrink 0

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

  -- Notes navigation list styling (overrides general list styles)
  "nav[aria-label=\"List of all notes\"]" ? do
    textAlign $ alignSide sideLeft

    "ul" ? do
      listStyleType none
      paddingLeft nil

      "li" ? do
        marginBottom (Clay.rem 0.8)
        display block
        textAlign $ alignSide sideLeft

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
      color dawnText

    h1 ? color dawnLove
    h2 ? color dawnIris

    article ? do
      ".header" ? color dawnSubtle

    a ? do
      color dawnPine
      hover & do
        color dawnRose

    ".keep-in-touch ul li a" ? do
      textDecoration none

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

    -- Focus indicators for light mode
    a ? do
      focus & do
        outline solid (px 2) dawnPine

    -- Navigation focus indicators for light mode
    nav ? do
      a ? do
        focus & do
          outline solid (px 3) dawnPine

    -- Note colors for light mode
    ".note" ? do
      ".note-header" ? do
        borderBottomColor dawnMuted

      ".note-title" ? do
        color dawnLove

      ".note-date" ? do
        color dawnSubtle

      ".note-tag" ? do
        backgroundColor dawnOverlay
        color dawnPine

    -- Breadcrumb colors for light mode
    ".breadcrumbs" ? do
      ".breadcrumb-link" ? do
        color dawnFoam

        hover & do
          color dawnIris

    -- Talks colors for light mode
    ".talks-list" ? do
      ".talk" ? do
        borderBottomColor dawnMuted

      ".video-link" ? do
        backgroundColor dawnFoam
        color dawnBase

        hover & do
          backgroundColor dawnPine

      ".slides-link" ? do
        backgroundColor dawnIris
        color dawnBase

        hover & do
          backgroundColor dawnLove

    -- Notes listing colors for light mode
    ".notes-list" ? do
      ".note-listing" ? do
        borderBottomColor dawnMuted

        "h2" ? do
          "a" ? do
            color dawnLove

            hover & do
              color dawnRose

    -- Tag cloud colors for light mode
    ".tag-cloud" ? do
      "a" ? do
        backgroundColor dawnOverlay
        color dawnPine

        hover & do
          backgroundColor dawnMuted
          color dawnText

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
    color dawnText

  h1 ? color dawnLove
  h2 ? color dawnIris

  article ? do
    ".header" ? color dawnSubtle

  a ? do
    color dawnPine
    hover & do
      color dawnRose

  ".keep-in-touch ul li a" ? do
    textDecoration none

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

  -- Fallback focus indicators
  a ? do
    focus & do
      outline solid (px 2) dawnPine

  -- Fallback navigation focus indicators
  nav ? do
    a ? do
      focus & do
        outline solid (px 3) dawnPine

  -- Note colors for fallback
  ".note" ? do
    ".note-header" ? do
      borderBottomColor dawnMuted

    ".note-title" ? do
      color dawnLove

    ".note-date" ? do
      color dawnSubtle

    ".note-tag" ? do
      backgroundColor dawnOverlay
      color dawnPine

  -- Breadcrumb colors for fallback
  ".breadcrumbs" ? do
    ".breadcrumb-link" ? do
      color dawnFoam

      hover & do
        color dawnIris

  -- Talk link colors for fallback
  ".talks-list" ? do
    ".talk" ? do
      borderBottomColor dawnMuted

    ".video-link" ? do
      backgroundColor dawnFoam
      color dawnBase

      hover & do
        backgroundColor dawnPine

    ".slides-link" ? do
      backgroundColor dawnIris
      color dawnBase

      hover & do
        backgroundColor dawnLove

  -- Notes listing colors for fallback
  ".notes-list" ? do
    ".note-listing" ? do
      borderBottomColor dawnMuted

      "h2" ? do
        "a" ? do
          color dawnLove

          hover & do
            color dawnRose

  -- Tag cloud colors for fallback
  ".tag-cloud" ? do
    "a" ? do
      backgroundColor dawnOverlay
      color dawnPine

      hover & do
        backgroundColor dawnMuted
        color dawnText

main :: IO ()
main = putCss $ do
  generateFontCSS
  baseStyles -- Structure without colors
  fallbackStyles -- Light theme fallback for browsers without prefers-color-scheme
  lightModeStyles -- Light theme media query
  darkModeStyles -- Dark theme media query
  hamburgerMenuStyle -- Hamburger menu CSS
  reducedMotionStyles -- Reduced motion preferences
  mediaQuery319
  mediaQuery320
  mediaQuery640
