# Understanding `unsafeCompiler` in Hakyll

This document explains why and how we can use `putStrLn` and other IO operations within Hakyll's `Compiler` monad through the `unsafeCompiler` function.

## The `Compiler` Monad

Hakyll's `Compiler` monad is designed to be:
- **Pure** - deterministic and cacheable
- **Dependency-aware** - tracks what files it depends on
- **Parallelizable** - can run multiple compilers concurrently

However, sometimes you need to perform IO operations (like running external commands or printing debug information).

## `unsafeCompiler` - The Escape Hatch

Looking at our PDF generation code:

```haskell
compile $ do
  aboutBody <- unsafeCompiler $ readFile "about.md"  -- IO operation
  unsafeCompiler $ do
    putStrLn "Generating about.pdf using Pandoc"     -- IO operation
    -- ... more IO operations
```

### How `unsafeCompiler` Works

```haskell
unsafeCompiler :: IO a -> Compiler a
```

This function:
1. **Lifts IO into Compiler**: Takes an `IO` action and makes it usable in the `Compiler` monad
2. **Breaks purity**: The "unsafe" name indicates this breaks Hakyll's purity guarantees
3. **Disables caching**: Operations inside `unsafeCompiler` can't be cached safely

## Why It's "Unsafe"

The function is called "unsafe" because:

### 1. **Non-deterministic Results**
```haskell
-- This could return different results on different runs
badCompiler = unsafeCompiler $ do
  time <- getCurrentTime
  putStrLn $ "Building at: " ++ show time
  return $ show time
```

### 2. **Side Effects**
```haskell
-- This has side effects that Hakyll can't track
fileWritingCompiler = unsafeCompiler $ do
  writeFile "debug.log" "Something happened"
  putStrLn "Wrote debug file"
```

### 3. **Breaks Dependency Tracking**
```haskell
-- Hakyll doesn't know this depends on external files
hiddenDependency = unsafeCompiler $ do
  content <- readFile "some-external-file.txt"  -- Hidden dependency!
  return content
```

## Safe Uses of `unsafeCompiler`

### 1. **Logging and Debug Output**
```haskell
compile $ do
  unsafeCompiler $ putStrLn "Processing about.pdf"  -- ✅ Safe
  -- ... rest of compilation
```

This is safe because:
- Doesn't affect the output
- Helps with debugging
- Deterministic relative to the actual compilation

### 2. **Running External Tools**
```haskell
unsafeCompiler $ do
  exitCode <- system "pandoc input.md -o output.pdf"
  putStrLn $ "Pandoc exit code: " ++ show exitCode
```

This is relatively safe when:
- The external tool is deterministic
- Input files are tracked by Hakyll
- Output is properly handled

### 3. **Reading Configuration Files**
```haskell
config <- unsafeCompiler $ readFile "config.json"
```

Safe if the config file is stable during builds.

## Examples from Our Codebase

### PDF Generation (Site.Pdf)
```haskell
generatePdfFromMarkdown :: String -> String -> String -> Compiler (Item BS.ByteString)
generatePdfFromMarkdown title author markdownContent = do
  unsafeCompiler $ do
    putStrLn $ "Generating PDF: " ++ title  -- Safe: logging
    tempDir <- getTemporaryDirectory
    withTempFile tempDir "output.pdf" $ \tempFile handle -> do
      hClose handle
      -- Write markdown content to temporary file
      writeFile "/tmp/hakyll-source.md" markdownContent
      -- Generate PDF using Pandoc
      exitCode <- system $ "pandoc --from=markdown --to=pdf..." 
      putStrLn $ "Pandoc exit code: " ++ show exitCode  -- Safe: logging
      -- Read the generated PDF file and return its content
      content <- BS.readFile tempFile
      putStrLn $ "Generated " ++ show (BS.length content) ++ " bytes"  -- Safe: logging
      return content
  >>= makeItem
```

**Why this is acceptable**:
- `putStrLn` calls are for build progress and debugging
- External `pandoc` command is deterministic given the same input
- File operations use temporary files that don't affect caching
- Binary content is properly returned to Hakyll

### Favicon Generation (Site.Favicon)
```haskell
generatePNGFaviconRule :: (Int, String) -> Rules ()
generatePNGFaviconRule (size, filename) = do
  create [fromFilePath filename] $ do
    route idRoute
    compile $ do
      unsafeCompiler $ do
        putStrLn $ "Generating " ++ filename ++ " at " ++ show size ++ "px"  -- Safe: logging
        -- ... ImageMagick operations
        content <- BS.readFile tempFile
        putStrLn $ "Generated " ++ show (BS.length content) ++ " bytes"  -- Safe: logging
        return content
      >>= makeItem
```

## Alternative Approaches

### 1. **Using Hakyll's Dependency Tracking**
Instead of:
```haskell
aboutBody <- unsafeCompiler $ readFile "about.md"  -- Hidden dependency
```

Use:
```haskell
aboutBody <- loadBody "about.md"  -- Tracked dependency
```

### 2. **Pure Logging**
For logging without IO:
```haskell
import Debug.Trace

compile $ do
  let result = trace "Processing about.pdf" someComputation
  -- ...
```

### 3. **Hakyll's Built-in Functions**
Many IO operations have Hakyll equivalents:
- `readFile` → `getResourceString`
- File copying → `copyFileCompiler`
- External commands → Custom compilers with proper dependency tracking

## When `putStrLn` is Acceptable

In our codebase, `putStrLn` is used appropriately for:

### 1. **Build Progress Information**
```haskell
putStrLn "Generating about.pdf using Pandoc"
putStrLn $ "Generated " ++ show (BS.length content) ++ " bytes for about.pdf"
```

This is **safe** because:
- Provides useful build feedback
- Doesn't affect the output content
- Helps with debugging and monitoring

### 2. **Error Reporting**
```haskell
putStrLn $ "Pandoc exit code: " ++ show exitCode
```

Useful for diagnosing build issues.

### 3. **Binary File Operations**
```haskell
putStrLn $ "Generated " ++ show (BS.length content) ++ " bytes for " ++ filename
```

Confirms that binary file generation worked correctly.

## Dangerous Uses to Avoid

### 1. **Hidden Dependencies**
```haskell
-- BAD: Hakyll doesn't know about this dependency
content <- unsafeCompiler $ readFile "some-config.txt"
```

### 2. **Non-deterministic Operations**
```haskell
-- BAD: Different results on each run
randomValue <- unsafeCompiler $ randomIO
```

### 3. **Modifying Source Files**
```haskell
-- BAD: Modifies source files that Hakyll tracks
unsafeCompiler $ writeFile "source.md" newContent
```

### 4. **Network Operations**
```haskell
-- BAD: Network requests can fail or return different results
content <- unsafeCompiler $ downloadFromURL "http://example.com/data"
```

## Best Practices

### 1. **Minimize `unsafeCompiler` Usage**
- Use Hakyll's built-in functions when possible
- Only use for truly necessary IO operations

### 2. **Document Why It's Safe**
```haskell
-- Safe: Only used for build progress logging
unsafeCompiler $ putStrLn "Generating PDF..."

-- Safe: External tool is deterministic with tracked inputs
exitCode <- unsafeCompiler $ system pandocCommand
```

### 3. **Ensure Determinism**
Make sure the IO operations produce the same results given the same inputs:
- External tools should be deterministic
- Use temporary files instead of modifying tracked files
- Avoid time-dependent or random operations

### 4. **Handle Errors Gracefully**
```haskell
result <- unsafeCompiler $ do
  putStrLn "Starting external process..."
  handle (\e -> do
    putStrLn $ "Error: " ++ show e
    throwIO e
  ) $ do
    -- ... external process
```

### 5. **Use for the Right Reasons**
Good reasons to use `unsafeCompiler`:
- ✅ Logging and debug output
- ✅ Running deterministic external tools
- ✅ Reading stable configuration files
- ✅ Binary file operations with temporary files

Bad reasons:
- ❌ Network requests
- ❌ Random number generation
- ❌ Modifying source files
- ❌ Time-dependent operations

## Performance Implications

### Caching Behavior
```haskell
-- This will always run, even if inputs haven't changed
result <- unsafeCompiler $ expensiveOperation

-- This respects Hakyll's caching
result <- someTrackableComputation
```

Operations inside `unsafeCompiler` bypass Hakyll's intelligent caching system.

### Parallelization
```haskell
-- Multiple unsafeCompiler operations can run in parallel
-- as long as they don't interfere with each other
```

But be careful about:
- File system conflicts
- Shared temporary directories
- Resource contention

## Debugging with `unsafeCompiler`

### Build Progress Tracking
```haskell
compile $ do
  unsafeCompiler $ putStrLn $ "Starting compilation of " ++ itemPath
  result <- expensiveComputation
  unsafeCompiler $ putStrLn $ "Finished compilation of " ++ itemPath
  return result
```

### Error Diagnosis
```haskell
unsafeCompiler $ do
  putStrLn $ "Input file size: " ++ show (length content)
  putStrLn $ "Running command: " ++ command
  exitCode <- system command
  putStrLn $ "Command exit code: " ++ show exitCode
  when (exitCode /= ExitSuccess) $
    putStrLn "Command failed - check the logs above"
```

## Summary

We can use `putStrLn` in Hakyll compilers because:

1. **`unsafeCompiler`** provides an escape hatch from the pure `Compiler` monad
2. **Logging is safe** - it doesn't affect the actual compilation output
3. **Debug information is valuable** - helps monitor build progress and diagnose issues
4. **It's deterministic** - logging the same events produces the same output

The key is using it responsibly:
- ✅ Use for logging, debugging, and running deterministic external tools
- ❌ Avoid for operations that break determinism or dependency tracking
- 📝 Always document why the usage is safe
- 🎯 Prefer Hakyll's built-in functions when available

Remember: the "unsafe" in `unsafeCompiler` is a warning, not a prohibition. Used correctly, it's a powerful tool for integrating external processes and providing visibility into the build process.