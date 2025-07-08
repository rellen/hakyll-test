# Dhall, Pandoc, and Markdown Processing in the Talks System

This document breaks down the `markdownToHtml` function and explains how Dhall data structures integrate with Pandoc for markdown processing in the talks system.

## Overview

The talks system demonstrates a powerful pipeline:
1. **Dhall** → Structured data storage and loading
2. **Pandoc** → Markdown to HTML conversion
3. **Hakyll** → Static site generation with the processed content

## The `markdownToHtml` Function

### Location and Signature

```haskell
-- From src/Site/Talks.hs, lines 48-52
markdownToHtml :: T.Text -> String
markdownToHtml markdown =
  case runPure (readMarkdown def markdown >>= writeHtml5String def) of
    Left _ -> T.unpack markdown -- Fallback to original text if parsing fails
    Right html -> T.unpack html
```

### Function Breakdown

#### 1. Type Signature
```haskell
markdownToHtml :: T.Text -> String
```
- **Input**: `T.Text` (Data.Text, efficient Unicode text)
- **Output**: `String` (standard Haskell string for Hakyll compatibility)

#### 2. Core Processing Pipeline
```haskell
runPure (readMarkdown def markdown >>= writeHtml5String def)
```

This line contains several important components:

**`runPure`**
- Pandoc function that runs Pandoc operations in a pure context
- Returns `Either PandocError a` - either an error or the result
- Ensures no IO operations occur during document conversion

**`readMarkdown def markdown`**
- Parses markdown text into Pandoc's internal AST (Abstract Syntax Tree)
- `def` uses default markdown reader options
- Returns `PandocM Pandoc` (Pandoc monad containing the document)

**`>>= writeHtml5String def`**
- Monadic bind operator chains the parsing with HTML generation
- `writeHtml5String` converts the AST to HTML5 string format
- `def` uses default HTML writer options
- Returns `PandocM T.Text`

#### 3. Error Handling with Pattern Matching
```haskell
case ... of
  Left _ -> T.unpack markdown     -- Fallback on error
  Right html -> T.unpack html     -- Success case
```

**Fallback Strategy**:
- If Pandoc parsing fails, return the original markdown as plain text
- This ensures the system continues to work even with malformed markdown
- Uses `T.unpack` to convert `Text` back to `String`

**Success Case**:
- Convert the generated HTML `Text` to `String` for Hakyll
- The HTML is now ready for template injection

## Integration with Dhall Data

### Talk Data Structure
```haskell
data Talk = Talk
  { title :: T.Text,
    description :: T.Text, -- Markdown content stored here
    organisation :: T.Text,
    year :: Natural,
    month :: Natural,
    video :: Maybe T.Text,
    slides :: Maybe T.Text
  }
```

The `description` field contains raw markdown that gets processed by `markdownToHtml`.

### Dhall File Structure
From `data/talks.dhall`:
```dhall
[ { title = "Building Resilient Systems with Elixir"
  , description = ''
      Exploring how Elixir's **actor model** and fault-tolerance mechanisms 
      enable building highly resilient distributed systems. 
      
      We'll cover:
      - OTP principles and supervision trees
      - Real-world patterns for handling failures gracefully
      - Building distributed systems that self-heal
      ''
  , organisation = "Tech Conference"
  , year = 2024
  , month = 8
  , video = Some "https://example.com/video"
  , slides = Some "https://example.com/slides"
  }
]
```

### Processing Flow

1. **Dhall Loading**:
   ```haskell
   loadTalks :: FilePath -> IO [Talk]
   loadTalks filePath = Dhall.inputFile (Dhall.list Dhall.auto) filePath
   ```

2. **Context Creation**:
   ```haskell
   talkContext :: Talk -> Context String
   talkContext talk = mconcat
     [ constField "title" (T.unpack $ title talk),
       constField "description" (markdownToHtml $ description talk), -- ← HERE
       -- ... other fields
     ]
   ```

3. **Template Usage**:
   ```html
   <!-- In templates/talks.html -->
   <div class="talk-description">
     $description$ <!-- This contains the processed HTML -->
   </div>
   ```

## Pandoc Configuration

### Reader Options (`def` for readMarkdown)
The default markdown reader supports:
- **CommonMark** syntax
- **GitHub Flavored Markdown** extensions
- **Pandoc's markdown extensions** (tables, footnotes, etc.)

### Writer Options (`def` for writeHtml5String)
The default HTML5 writer produces:
- **Semantic HTML5** elements
- **Clean markup** without unnecessary attributes
- **UTF-8 encoded** output

### Customization Examples

If you wanted to customize the Pandoc processing:

```haskell
import Text.Pandoc.Options

customMarkdownToHtml :: T.Text -> String
customMarkdownToHtml markdown =
  let readerOpts = def { readerExtensions = enableExtension Ext_tables def }
      writerOpts = def { writerHTMLMathMethod = MathJax defaultMathJaxURL }
  in case runPure (readMarkdown readerOpts markdown >>= writeHtml5String writerOpts) of
       Left _ -> T.unpack markdown
       Right html -> T.unpack html
```

## Error Scenarios and Robustness

### Common Markdown Parsing Errors
1. **Malformed tables**
2. **Unclosed code blocks**
3. **Invalid link syntax**
4. **Encoding issues**

### Why the Fallback Works
```haskell
Left _ -> T.unpack markdown
```
- Rather than crashing the entire site build
- Displays the raw markdown (still readable)
- Allows the build process to continue
- Provides debugging information in the output

## Performance Considerations

### Text vs String
- **Input**: Uses `T.Text` for efficient Unicode handling
- **Output**: Converts to `String` for Hakyll compatibility
- **Trade-off**: Memory efficiency vs. ecosystem compatibility

### Pure Processing
- `runPure` ensures no IO operations
- Allows for concurrent processing of multiple talks
- Enables easy testing and debugging

## Integration Points

### 1. RSS Feed Generation
```haskell
talkRssContext :: [Talk] -> Context String
talkRssContext talks = mconcat
  [ field "description" $ \item -> do
      -- Raw markdown for RSS (not processed)
      return $ T.unpack $ description talk
  ]
```

Note: RSS feeds use raw markdown, not processed HTML.

### 2. Template Context
```haskell
field "description" (return . markdownToHtml . description . itemBody)
```

Used in list contexts for rendering multiple talks.

## Testing the Function

### Example Usage
```haskell
-- Input markdown
let markdown = "## Heading\n\nSome **bold** text with [links](http://example.com)"

-- Process through markdownToHtml
let html = markdownToHtml markdown

-- Expected output
-- "<h2>Heading</h2>\n<p>Some <strong>bold</strong> text with <a href=\"http://example.com\">links</a></p>"
```

### Edge Cases
```haskell
-- Empty input
markdownToHtml "" 
-- Result: ""

-- Plain text (no markdown)
markdownToHtml "Just plain text"
-- Result: "<p>Just plain text</p>"

-- Invalid markdown
markdownToHtml "Unclosed code block ```"
-- Result: "Unclosed code block ```" (fallback to original)
```

## Best Practices

### 1. Content Guidelines
- Use standard CommonMark syntax in Dhall files
- Test complex markdown before adding to talks data
- Keep descriptions focused and concise

### 2. Error Monitoring
- The fallback means errors are silent
- Consider logging failed conversions in production
- Validate Dhall content before deployment

### 3. Performance
- Pandoc processing is fast but not instant
- Consider caching for very large talk databases
- The pure nature allows for parallelization

## Extension Possibilities

### 1. Custom Markdown Extensions
```haskell
-- Enable specific Pandoc extensions
readerOpts = def { readerExtensions = enableExtension Ext_footnotes $
                                    enableExtension Ext_tables def }
```

### 2. Math Support
```haskell
-- Add MathJax support for mathematical notation
writerOpts = def { writerHTMLMathMethod = MathJax defaultMathJaxURL }
```

### 3. Syntax Highlighting
```haskell
-- Enable code syntax highlighting
writerOpts = def { writerHighlightStyle = Just kate }
```

## Conclusion

The `markdownToHtml` function demonstrates a clean, robust approach to processing structured content:

1. **Separation of Concerns**: Dhall for data, Pandoc for processing, Hakyll for site generation
2. **Error Resilience**: Graceful fallback ensures site builds always succeed
3. **Type Safety**: Strong typing catches many errors at compile time
4. **Performance**: Pure processing allows for optimization and testing

This pattern could be extended to other content types (posts, notes, etc.) where rich markdown processing is needed while maintaining the benefits of structured data storage.