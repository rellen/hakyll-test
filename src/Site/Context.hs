--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Site.Context
  ( getCopyrightYears,
    copyrightCtx,
    tagsCtx,
    postCtx,
    noteCtx,
    getNoteTags,
    renderNoteTagCloud,
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

-- Custom tag extractor for comma-separated tags
getNoteTags :: (MonadMetadata m) => Identifier -> m [String]
getNoteTags identifier = do
  metadata <- getMetadata identifier
  case lookupString "tags" metadata of
    Nothing -> return []
    Just tagsStr -> return $ map (dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse) $ splitOnComma tagsStr

-- Custom tags context that handles comma-separated tags with links
tagsCtx :: Context String
tagsCtx = field "tags" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  case lookupString "tags" metadata of
    Nothing -> noResult "No tags field"
    Just tagsStr -> do
      let tags = map (dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse) $ splitOnComma tagsStr
          tagLinks = map (\tag -> "<a href=\"/note-tags/" ++ urlEncode tag ++ ".html\" class=\"note-tag\">" ++ tag ++ "</a>") tags
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

-- Post context (uses frontmatter date)
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` dateField "datetime" "%Y-%m-%d"
    `mappend` tagsCtx
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
    `mappend` tagsCtx
    `mappend` defaultContext
