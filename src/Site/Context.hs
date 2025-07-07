--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Site.Context
  ( getCopyrightYears,
    copyrightCtx,
    noteTagsCtx,
    postTagsCtx,
    postCtx,
    noteCtx,
    getNoteTags,
    getPostTags,
    renderNoteTagCloud,
    renderPostTagCloud,
    renderCombinedTagCloud,
  )
where

import Data.Bifunctor (second)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import Hakyll
import Site.Util (splitOnComma, urlEncode)

--------------------------------------------------------------------------------
-- Get copyright year string
getCopyrightYears :: IO String
getCopyrightYears = do
  now <- getCurrentTime
  let (year, _, _) = toGregorian $ utctDay now
  if year == 2025
    then return "2025"
    else return $ "2025-" ++ show year

-- Create a context with the copyright years
copyrightCtx :: IO (Context String)
copyrightCtx = do
  constField "copyright" <$> getCopyrightYears

-- Generic tag extractor for comma-separated tags
getCommaSeparatedTags :: (MonadMetadata m) => Identifier -> m [String]
getCommaSeparatedTags identifier = do
  metadata <- getMetadata identifier
  case lookupString "tags" metadata of
    Nothing -> return []
    Just tagsStr -> return $ map (dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse) $ splitOnComma tagsStr

-- Tag extractor for notes (alias for consistency)
getNoteTags :: (MonadMetadata m) => Identifier -> m [String]
getNoteTags = getCommaSeparatedTags

-- Tag extractor for posts
getPostTags :: (MonadMetadata m) => Identifier -> m [String]
getPostTags = getCommaSeparatedTags

-- Custom tags context for notes (links to note-tags pages)
noteTagsCtx :: Context String
noteTagsCtx = field "tags" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  case lookupString "tags" metadata of
    Nothing -> noResult "No tags field"
    Just tagsStr -> do
      let tags = map (dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse) $ splitOnComma tagsStr
          tagLinks = map (\tag -> "<a href=\"/note-tags/" ++ urlEncode tag ++ ".html\" class=\"note-tag\">" ++ tag ++ "</a>") tags
      return $ unwords tagLinks

-- Custom tags context for posts (links to post-tags pages)
postTagsCtx :: Context String
postTagsCtx = field "tags" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  case lookupString "tags" metadata of
    Nothing -> noResult "No tags field"
    Just tagsStr -> do
      let tags = map (dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse) $ splitOnComma tagsStr
          tagLinks = map (\tag -> "<a href=\"/post-tags/" ++ urlEncode tag ++ ".html\" class=\"post-tag\">" ++ tag ++ "</a>") tags
      return $ unwords tagLinks

-- Custom tag cloud renderer for note tags
renderNoteTagCloud :: Double -> Double -> Tags -> Compiler String
renderNoteTagCloud minSize maxSize tags = do
  let tagCounts = map (second length) $ tagsMap tags

  if null tagCounts
    then return ""
    else do
      let minCount = fromIntegral $ minimum $ map snd tagCounts
          maxCount = fromIntegral $ maximum $ map snd tagCounts
          sizeRange = if maxCount == minCount then 0 else maxSize - minSize

      let renderTag (tag, count) =
            let size =
                  if sizeRange == 0
                    then minSize
                    else minSize + (fromIntegral count - minCount) / (maxCount - minCount) * sizeRange
                sizeEm = size
                noteCount = show count
                plural = if count == 1 then "note" else "notes"
             in "<li><a href=\"/note-tags/" ++ urlEncode tag ++ ".html\" style=\"font-size: " ++ show sizeEm ++ "em\" aria-label=\"View " ++ noteCount ++ " " ++ plural ++ " tagged with " ++ tag ++ "\">" ++ tag ++ "</a></li>"

      let tagItems = unlines $ map renderTag tagCounts
      return $ "<nav aria-label=\"Browse notes by tag\">\n  <ul class=\"tag-list\">\n" ++ tagItems ++ "  </ul>\n</nav>"

-- Custom tag cloud renderer for post tags
renderPostTagCloud :: Double -> Double -> Tags -> Compiler String
renderPostTagCloud minSize maxSize tags = do
  let tagCounts = map (second length) $ tagsMap tags

  if null tagCounts
    then return ""
    else do
      let minCount = fromIntegral $ minimum $ map snd tagCounts
          maxCount = fromIntegral $ maximum $ map snd tagCounts
          sizeRange = if maxCount == minCount then 0 else maxSize - minSize

      let renderTag (tag, count) =
            let size =
                  if sizeRange == 0
                    then minSize
                    else minSize + (fromIntegral count - minCount) / (maxCount - minCount) * sizeRange
                sizeEm = size
                postCount = show count
                plural = if count == 1 then "post" else "posts"
             in "<li><a href=\"/post-tags/" ++ urlEncode tag ++ ".html\" style=\"font-size: " ++ show sizeEm ++ "em\" aria-label=\"View " ++ postCount ++ " " ++ plural ++ " tagged with " ++ tag ++ "\">" ++ tag ++ "</a></li>"

      let tagItems = unlines $ map renderTag tagCounts
      return $ "<nav aria-label=\"Browse posts by tag\">\n  <ul class=\"tag-list\">\n" ++ tagItems ++ "  </ul>\n</nav>"

-- Combined tag cloud renderer that merges note and post tags
renderCombinedTagCloud :: Double -> Double -> Tags -> Tags -> Compiler String
renderCombinedTagCloud minSize maxSize noteTags postTags = do
  let noteTagCounts = map (second length) $ tagsMap noteTags
      postTagCounts = map (second length) $ tagsMap postTags
      
  if null noteTagCounts && null postTagCounts
    then return ""
    else do
      -- Combine and merge tag counts
      let combinedMap = combineTagCounts noteTagCounts postTagCounts
          allCounts = map (\(_, (noteCount, postCount)) -> noteCount + postCount) combinedMap
          minCount = fromIntegral $ minimum allCounts
          maxCount = fromIntegral $ maximum allCounts
          sizeRange = if maxCount == minCount then 0 else maxSize - minSize

      let renderTag (tag, (noteCount, postCount)) =
            let totalCount = noteCount + postCount
                size =
                  if sizeRange == 0
                    then minSize
                    else minSize + (fromIntegral totalCount - minCount) / (maxCount - minCount) * sizeRange
                sizeEm = size
                totalStr = show totalCount
                plural = if totalCount == 1 then "item" else "items"
             in "<li><a href=\"/tags/" ++ urlEncode tag ++ ".html\" style=\"font-size: " ++ show sizeEm ++ "em\" aria-label=\"View " ++ totalStr ++ " " ++ plural ++ " tagged with " ++ tag ++ "\">" ++ tag ++ "</a></li>"

      let tagItems = unlines $ map renderTag combinedMap
      return $ "<nav aria-label=\"Browse all content by tag\">\n  <ul class=\"tag-list\">\n" ++ tagItems ++ "  </ul>\n</nav>"

-- Helper function to combine tag counts from notes and posts
combineTagCounts :: [(String, Int)] -> [(String, Int)] -> [(String, (Int, Int))]
combineTagCounts noteTagCounts postTagCounts =
  let allTags = map fst noteTagCounts ++ map fst postTagCounts
      uniqueTags = foldl (\acc tag -> if tag `elem` acc then acc else acc ++ [tag]) [] allTags
      getNoteCount tag = maybe 0 id $ lookup tag noteTagCounts
      getPostCount tag = maybe 0 id $ lookup tag postTagCounts
   in map (\tag -> (tag, (getNoteCount tag, getPostCount tag))) uniqueTags

-- Post context (uses frontmatter date)
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` dateField "datetime" "%Y-%m-%d"
    `mappend` postTagsCtx
    `mappend` defaultContext

-- Note context (shows both published and updated dates)
noteCtx :: Context String
noteCtx =
  dateField "date" "%B %e, %Y"
    `mappend` dateField "datetime" "%Y-%m-%d"
    `mappend` dateField "published" "%B %e, %Y"
    `mappend` dateField "published-datetime" "%Y-%m-%d"
    `mappend` modificationTimeField "updated" "%B %e, %Y"
    `mappend` modificationTimeField "updated-datetime" "%Y-%m-%d"
    `mappend` noteTagsCtx
    `mappend` defaultContext
