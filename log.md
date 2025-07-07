# Hakyll Site Development Log

## Overview

This log documents key lessons learned while building a modern Hakyll-based static site with CSS preprocessing, favicon generation, and contemporary web standards.

## Favicon Generation & PWA Icons

### Problem
- Original console error: "Error while trying to use the following icon from the Manifest: http://127.0.0.1:8000/android-chrome-192x192.png (Download error or resource isn't a valid image)"
- Generated PNG files were 0 bytes despite ImageMagick commands working correctly

### Root Cause
In Hakyll's `unsafeCompiler`, the sequence was:
1. ImageMagick successfully generated valid PNG files in project root
2. `makeItem ("" :: String)` overwrote the binary content with empty strings
3. Result: 0-byte files in `_site/` directory

### Solution
```haskell
-- BROKEN:
unsafeCompiler $ callProcess magickPath [args...]
makeItem ("" :: String)

-- FIXED:
unsafeCompiler $ do
  callProcess magickPath [args...]
  content <- BS.readFile filename
  return content
>>= makeItem
```

### Modern Minimal Favicon Approach (2024/2025)
Reduced from 15+ legacy files to just 5 essential files:
- `favicon.svg` - Primary SVG favicon (scales to any size)
- `favicon-32x32.png` - PNG fallback for older browsers
- `icon-192.png` - PWA/Android home screen
- `icon-512.png` - PWA/Android splash screen  
- `site.webmanifest` - Web app manifest

**Why Apple Touch icons are no longer needed:**
- Modern iOS Safari (12+) supports SVG favicons
- PWA manifest icons (192px, 512px) serve the same purpose
- Automatic fallback behavior in modern browsers
- Better cross-platform compatibility

## ImageMagick Command Syntax

### Lesson Learned
SVG to PNG conversion requires correct argument order:

```bash
# WRONG (doesn't work with SVG input):
magick -background transparent -size 192x192 input.svg output.png

# CORRECT:
magick -background transparent input.svg -resize 192x192 output.png
```

The `-size` parameter doesn't work properly with SVG inputs; use `-resize` after the input file.

## Hakyll Binary File Compilation

### Key Insights
1. **Never use `makeItem ""` for binary files** - it overwrites with empty content
2. **Read binary content explicitly** with `BS.readFile` in `unsafeCompiler`
3. **Debug with content length** to verify binary data is being read
4. **Use `noResult` only when you don't want Hakyll to manage the file**

### Working Pattern for Binary Generation
```haskell
generateBinaryFile :: FilePath -> Rules ()
generateBinaryFile filename = do
  create [fromFilePath filename] $ do
    route idRoute
    compile $ do
      unsafeCompiler $ do
        -- Generate file with external tool
        callProcess tool [args..., filename]
        -- Read the generated binary content
        content <- BS.readFile filename
        return content
      >>= makeItem
```

## CSS Processing with Clay

### Clay 0.16.0 Syntax Requirements
```haskell
-- WRONG:
borderBottom solid (px 1) color

-- CORRECT:
borderBottomStyle solid
borderBottomWidth (px 1)  
borderBottomColor color
```

### Dark Mode Implementation
- Use `prefersColorScheme` from `Clay.Media`
- Structure: base styles → light theme → dark theme with media queries
- Define color variables at top for maintainability
- Include `prefers-reduced-motion` support for accessibility

## Build System Best Practices

### Cabal Commands
```bash
# Build Haskell changes first
cabal build

# Then run Hakyll commands  
cabal run site -- clean
cabal run site -- build
cabal run site -- watch
```

### CSS Compilation Testing
```bash
# Test Clay files independently before full build
cabal exec -- runghc css/default.hs
```

### Parallel Builds
Configure in `cabal.project`:
```
jobs: 8
with-compiler: /path/to/unwrapped/ghc  # Avoids nix wrapper warnings
```

## File Organization & Conventions

### Markdown Files
- Use `.md` extension consistently (not `.markdown`)
- Shorter, more common, better editor support
- Standard in modern static site generators

### Template Hierarchy
1. `default.html` - Base layout with head, navigation, footer
2. Content templates - `page.html`, `post.html`, `note.html`
3. List templates - `archive.html`, `notes-list.html`

## Accessibility Implementation

### Screen Reader Support
```haskell
".sr-only" ? do
  position absolute
  width (px 1)
  height (px 1)
  -- ... (visually hidden but available to screen readers)
```

### ARIA Best Practices
- `aria-label` for icon-only links
- `aria-hidden="true"` for decorative SVGs
- Proper heading hierarchy (h1 → h2 → h3)
- Landmark elements: `<header>`, `<main>`, `<footer>`, `<nav>`

### Mobile Navigation
- Hidden checkbox pattern for hamburger menu
- Keyboard accessible without JavaScript
- High z-index (100+) for overlay
- Explicit colors for visibility in all themes

## Progressive Enhancement

### Approach
1. **Core functionality first** - works without CSS/JS
2. **Feature detection** - media queries for modern features
3. **Graceful degradation** - fallbacks for older browsers

### Color Scheme Strategy
```css
/* Base structural styles */
body { font-family: ...; }

/* Fallback light theme */
body { color: light-color; background: light-bg; }

/* Explicit light theme */
@media (prefers-color-scheme: light) { ... }

/* Dark theme */  
@media (prefers-color-scheme: dark) { ... }
```

## Performance Optimizations

### Asset Strategy
- SVG icons instead of icon fonts
- Custom font loading with WOFF2 format
- CSS compression with `compressCss`
- Static site generation for fast loading
- Minimal dependencies

### Font Loading
```haskell
@font-face{
  font-family: "AtkinsonHyperlegibleNext";
  font-weight: 400;
  font-style: normal;
  src: url("/fonts/AtkinsonHyperlegibleNext-Regular.woff2") format("woff2")
}
```

## Debugging Techniques

### Hakyll Compilation Issues
1. **Add debug output** in `unsafeCompiler`
2. **Check file sizes** with `ls -la` and `file` commands
3. **Test external commands** manually before integrating
4. **Use `cabal run site -- build` instead of `cabal exec -- runghc site.hs`**

### CSS Issues
1. **Test Clay compilation** independently: `cabal exec -- runghc css/default.hs`
2. **Check syntax requirements** specific to Clay version
3. **Verify media query support** in target Clay version

## Modern Web Standards Applied

### PWA Manifest
- Minimal essential fields only
- Proper icon sizes (192px, 512px)
- Theme colors matching design system
- `display: "minimal-ui"` for clean app experience

### HTML Best Practices
- Semantic HTML5 elements
- Proper meta tags for viewport and theme
- Skip links for keyboard navigation
- Descriptive alt text for images

## Modern CSS Architecture & Specificity Management

### Problem: CSS Specificity Conflicts
Initial implementation used broad selectors like `nav a` that caused cascade conflicts:
- Navigation styles affected unrelated elements (note tags)
- CSS specificity "whack-a-mole" with increasingly specific selectors
- Difficult to maintain and debug

### Solution: Specific Class-Based Architecture
Replaced broad element-based selectors with specific classes:

```haskell
-- OLD (problematic):
nav ? do
  a ? do
    color navColor
    fontWeight bold

-- NEW (clean):
".nav-link" ? do
  color navColor
  fontWeight bold
```

### Implementation Steps
1. **Added specific classes** to HTML templates:
   ```html
   <!-- Before -->
   <nav><a href="/">Home</a></nav>
   
   <!-- After -->
   <nav><a href="/" class="nav-link">Home</a></nav>
   ```

2. **Replaced all CSS selectors** across media queries:
   - Main navigation styles
   - Mobile navigation (319px and 320-639px breakpoints)
   - Desktop navigation (640px+)
   - Focus indicators and accessibility features

3. **Eliminated cascade conflicts** between:
   - Navigation links (`.nav-link`)
   - Note tags (`.note-tag`) 
   - Content links (default `a`)

### Visual Hierarchy Refinement

#### Color Strategy
Implemented clear visual hierarchy using Rose Pine palette:

- **H1**: Love Pink (`#b4637a` / `#eb6f92`) - Highest priority
- **Navigation**: Iris Purple (`#907aa9` / `#c4a7e7`) - Distinct navigation identity
- **H2**: Iris Purple (`#907aa9` / `#c4a7e7`) - Major sections  
- **Content links**: Pine Blue (`#286983` / `#9ccfd8`) - Traditional links
- **Note tags**: Gold (`#ea9d34` / `#f6c177`) - Distinct categorization
- **Tag cloud**: Pine Blue (`#286983` / `#9ccfd8`) - Cohesive with content

#### Navigation Color Evolution
- **Previous**: Foam cyan (`#56949f` / `#9ccfd8`) - Too similar to content links
- **Current**: Iris purple (`#907aa9` / `#c4a7e7`) - Sophisticated, distinctive
- **Hover**: Love pink (`#b4637a` / `#eb6f92`) - Engaging interaction

### CSS Architecture Best Practices Applied

#### 1. Specific Class Names
```haskell
-- Avoid broad element selectors
nav a ? styles  -- ❌ Too broad, affects unrelated elements

-- Use specific classes  
".nav-link" ? styles  -- ✅ Precise, maintainable
".note-tag" ? styles  -- ✅ Clear separation of concerns
```

#### 2. Consistent Naming Convention
- `.nav-link` - Navigation links
- `.note-tag` - Note categorization tags
- `.note-title-link` - Note title links in listings
- Clear, semantic naming prevents confusion

#### 3. Cascade Control
- Each UI component has its own specific selectors
- No unintended inheritance between unrelated elements
- Easy to debug and maintain
- Scalable for future additions

### Accessibility Improvements
- Navigation focus indicators use consistent colors
- Clear visual hierarchy aids screen reader navigation
- Semantic HTML maintained alongside CSS improvements
- Keyboard navigation unaffected by specificity changes

### Performance Benefits
- Cleaner CSS with fewer override conflicts
- More predictable rendering
- Easier browser CSS parsing
- Reduced redundant style calculations

## Key Takeaways

1. **SVG-first approach** eliminates most favicon complexity
2. **Modern browsers need fewer fallbacks** than legacy implementations
3. **Hakyll binary compilation** requires explicit content reading
4. **ImageMagick syntax** differs for SVG vs raster inputs
5. **Debug output is essential** for complex compilation processes
6. **Clay CSS syntax** has specific requirements that differ from standard CSS
7. **Progressive enhancement** enables better accessibility and performance
8. **Modern web standards** often simplify rather than complicate implementations
9. **CSS specificity conflicts** are best solved with architectural changes, not increasing specificity
10. **Class-based CSS architecture** scales better than element-based selectors
11. **Visual hierarchy** through color requires careful consideration of context and purpose
12. **Modern CSS best practices** prevent technical debt and maintenance issues

## File Structure Achieved

```
project/
├── favicon.svg              # Primary favicon
├── icon-192.png            # PWA icon
├── icon-512.png            # PWA icon  
├── src/Site/Favicon.hs     # Favicon generation logic
├── css/default.hs          # Clay CSS with dark mode
├── templates/default.html  # Modern minimal favicon links
└── site.webmanifest       # PWA manifest
```

This represents a modern, accessible, performant static site that follows contemporary web standards while maintaining simplicity and maintainability.