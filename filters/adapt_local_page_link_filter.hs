#!/usr/bin/env runhaskell

import Text.Pandoc.JSON
import Text.Pandoc.Walk

import Network.URI
import Data.List.Split
import System.FilePath
import System.Directory
import Data.Maybe
--import Debug.Trace


{-
  A filter that can adapt all links (both image and standard links) found
  in a page so that they can work in the context of a static web site
  consulted locally (without backend).

  It takes the following parameters:

   1. `targetFormatExtension`: The extension of the target format (e.g.: "html", "md", etc).
   2. `relPathToRootDir`: The relative path from the page where the link was found to the
      root of the site. (e.g.: "../.." for a page at "./myDir1/myDir2/myPage.md").
-}

data Args = Args { 
  targetFormatExtension :: String
, relPathToRootDir :: FilePath
} deriving Show

main = toJSONFilter $ \rawArgs inBlock -> 
  let maybeResult = do 
        args <- parseArgs rawArgs
        return $ adaptLinkFilter args inBlock
  in case maybeResult of 
    Nothing -> error "ERROR: adapt_local_page_link_filter failed to parse its arguments!"
    Just result -> result




adaptLinkFilter :: Args -> Inline -> [Inline]
-- Starting with pandoc-types 1.16.x, this pattern match will break due to new attr fist parameter. When
-- you find it has become the case, uncomment replacing existing pattern match with following line:

adaptLinkFilter args (Link attrs txt (url, title)) = 
  [Link attrs txt (adaptLink args url txt, title)]
adaptLinkFilter args (Image attrs txt (url, title)) = 
  [Image attrs txt (adaptLink args url txt, title)]      
adaptLinkFilter args x = [x]


adaptLink :: Args -> String -> [Inline] -> String
adaptLink args url txt =
  let tgtFmtExt = Just . targetFormatExtension $ args
      pathToRootDir = Just . relPathToRootDir $ args
      adaptedUri = do inUri <- parseURIReference (if null url then stringifyInlines txt else url)
                      return $ adaptLinkUri tgtFmtExt pathToRootDir inUri
  in maybe url (\x -> uriToString id x $ "") adaptedUri



parseArgs :: [String] -> Maybe Args
parseArgs rawArgs = do
  arg0 <- getArgAtIdx 0 rawArgs
  arg1 <- getArgAtIdx 1 rawArgs
  return $ Args { 
    targetFormatExtension = arg0,
    relPathToRootDir = arg1
  }

-- TODO: Use the same stringification algorithm as pandoc.
stringifyInline :: Inline -> String
stringifyInline (Str str) = str
stringifyInline _ = ""

stringifyInlines :: [Inline] -> String
stringifyInlines = concat . map stringifyInline


isLocalLinkUri :: URI  -> Bool
isLocalLinkUri = null . uriScheme




absPathToRelPath :: FilePath -> FilePath -> FilePath
absPathToRelPath pathToRootDir absPath 
  | isAbsPath absPath = pathToRootDir ++ absPath
  | otherwise = absPath -- Return unchanged.


addMissingExtensionToPath :: String -> FilePath -> FilePath
addMissingExtensionToPath extension path
  | null path = path -- Do not add extension for empty path (proper support `[my link](#tgt-anchor)` links).
  | ( ".md" == takeExtension path ) = dropExtension path ++ "." ++ extension
  | isPathWithExtension path = path -- Do not add extension when there already is one.
  | otherwise = path ++ "." ++ extension
  where
    hasSourceExt path = case (splitExtension path) of
      (_,ext) -> ext == ".md" -- TODO: Hardcoded, replace with arg. 


adaptLocalLinkPath :: Maybe String -> Maybe FilePath -> FilePath -> FilePath
adaptLocalLinkPath tgtFmtExt pathToRootDir =
    absToRel .
    addMissExt 
  where
    addMissExt path = maybe path (\x -> addMissingExtensionToPath x path) tgtFmtExt
    absToRel path = maybe path (\x -> absPathToRelPath x path) pathToRootDir

adaptLinkUri :: Maybe String -> Maybe FilePath -> URI -> URI
adaptLinkUri tgtFmtExt pathToRootDir uri 
  | not (isLocalLinkUri uri) = uri -- Unchanged when not a local url.
  | otherwise = 
      uri { uriPath = adaptLocalLinkPath tgtFmtExt pathToRootDir (uriPath uri) }



{-
  Generic functions that could be shared with other plugins.
-}

getArgAtIdx :: Int -> [String] -> Maybe String
getArgAtIdx idx = listToMaybe . take 1 . drop idx


isAbsPath :: FilePath -> Bool
isAbsPath path = take 1 path == "/"

isPathWithExtension :: FilePath -> Bool
isPathWithExtension path = 
  let pComps = splitOn "/" path
  in case pComps of
      xs@(x:_) -> not . null . tail . splitOn "." . last $ xs
      _ -> False




