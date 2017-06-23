#!/usr/bin/env runhaskell


import Text.Pandoc.JSON
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
-- from the utf8-string package on HackageDB:
import qualified Data.ByteString.Lazy.UTF8 as LC (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.FilePath ((</>), takeDirectory, normalise)
import System.Directory(createDirectoryIfMissing)
import Control.Monad.Trans (liftIO)

import Data.List (intersperse)
import Data.Maybe
import Data.List.Split

data Args = Args { 
  pageRelFileNameFromRoot :: FilePath
, outStaticImageDirRoot :: FilePath
, staticImageUriPrefix :: String
} deriving Show


main :: IO ()
main = toJSONFilter $ \rawArgs inBlock -> do
  let maybeArgs = parseArgs rawArgs
  case maybeArgs of
    Nothing -> error $ "Invalid arguments provided: `" ++ show rawArgs ++ "`. Was expecting 3."
    Just args -> transformBlock args inBlock


transformBlock :: Args -> Block -> IO Block
transformBlock args (CodeBlock (id, classes, namevals) contents) | "dot" `elem` classes =
  do
    let 
      handledClasses = ["dot"]
      unhandledClasses = filter (\e -> not (elem e handledClasses)) classes

      handledAttrs = ["type", "name"]
      unhandledAttrs = filter (\e -> not (elem (fst e) handledAttrs)) namevals

      relPageCompDir = getPageCompanionDir . pageRelFileNameFromRoot $ args

      maybeAttrType = lookup "type" namevals
      maybeAttrName = lookup "name" namevals

      outImgRelFilename = 
        getOutImgRelFileName relPageCompDir maybeAttrName maybeAttrType contents

      localOutFilename =  outStaticImageDirRoot args </> normalise outImgRelFilename

      imgTagName = maybe [] (\nameAttr -> [Str nameAttr]) maybeAttrName


    retVal <- runDot maybeAttrType localOutFilename contents
    case retVal of
      Left errorMsg -> return $ CodeBlock (id, classes, namevals) errorMsg
      Right localOutFileName -> 
        let 
          imgInline = Image (id, unhandledClasses, unhandledAttrs) imgTagName 
                            (staticImageUriPrefix args ++ "/" ++ outImgRelFilename, "")
          imgBlock = Para [imgInline]
        in
          return $ imgBlock
transformBlock _ block = return block


parseArgs :: [String] -> Maybe Args
parseArgs rawArgs = do
  arg0 <- getArgAtIdx 0 rawArgs
  arg1 <- getArgAtIdx 1 rawArgs
  arg2 <- getArgAtIdx 2 rawArgs
  return $ Args { 
    pageRelFileNameFromRoot = arg0,
    outStaticImageDirRoot = arg1,
    staticImageUriPrefix = arg2
  }


runDot :: Maybe String -> FilePath -> String -> IO (Either String FilePath)
runDot maybeOutType outFilename content = do
    createDirectoryIfMissing True . takeDirectory $  outFilename
    (ec, _out, stderr) <- readProcessWithExitCode exeName args content
    if ec == ExitSuccess
       then return . Right $ outFilename
       else return . Left $ errorMsg ec stderr
  where
    exeName = "dot"
    ftParams = case maybeOutType of
      Just ft -> ["-T" ++ ft]
      Nothing -> ["-Tpng"] -- It is assumed here that default is a `png` file.
    args = ftParams ++ ["-o", outFilename]
    errorMsg ec stderr = prettyPrintProcessError exeName args ec stderr

{-
  Generic functions that could be shared with other plugins.
-}

prettyPrintProcessError exeName args ec stderr = 
  "[content] | " ++ exeName ++ " " ++ (concat . intersperse " "  $ args) ++ "\n\n" ++
  prettyPrintErrorCode ec ++
  prettyPrintStdStream "stderr" stderr

prettyPrintErrorCode ExitSuccess = ""
prettyPrintErrorCode ec =
  "error code\n" ++
  "==========\n\n" ++ show ec ++ "\n\n";

prettyPrintStdStream _ "" = ""
prettyPrintStdStream streamName content = 
  streamName ++ "\n" ++ 
  "======\n\n" ++ content ++ "\n\n";


getOutImgRelFileName :: FilePath -> Maybe FilePath -> Maybe String -> String -> FilePath
getOutImgRelFileName companionRelDir maybeFileName maybeFileType contents =
    outFileName
  where
    outExt = case maybeFileType of
      Just ft -> '.':ft
      Nothing -> ".png" -- Defaults to png.
    outFileName = case maybeFileName of
      Just fileName | isRelPath fileName -> companionRelDir </> fileName ++ outExt
                    | otherwise -> drop 1 fileName ++ outExt
      Nothing -> "unnamed/" ++ uniqueName contents ++ outExt



isPathWithExtension :: FilePath -> Bool
isPathWithExtension path = 
  let pComps = splitOn "/" path
  in case pComps of
      xs@(x:_) -> not . null . tail . splitOn "." . last $ xs
      _ -> False



removeFileNameExtension :: FilePath -> FilePath
removeFileNameExtension fileName 
  | not . isPathWithExtension $ fileName = fileName 
  | otherwise = 
  case (splitOn "." fileName) of
    x:[] -> x -- When no extension, return string as was.
    xs -> concat . intersperse "." . init $ xs -- Drop the last part (the extension)

getPageCompanionDir :: FilePath -> FilePath 
getPageCompanionDir = removeFileNameExtension


getArgAtIdx :: Int -> [String] -> Maybe String
getArgAtIdx idx = listToMaybe . take 1 . drop idx

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . LC.fromString



isRelPath :: FilePath -> Bool
isRelPath path = take 1 path /= "/"



