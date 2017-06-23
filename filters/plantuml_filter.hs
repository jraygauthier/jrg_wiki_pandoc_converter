#!/usr/bin/env runhaskell


import Text.Pandoc.JSON

import System.Exit (ExitCode(ExitSuccess))
-- from the utf8-string package on HackageDB:
import qualified Data.ByteString.Lazy.UTF8 as LC (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.FilePath ((</>), takeDirectory, normalise)
import System.Directory(createDirectoryIfMissing)
import Control.Monad.Trans (liftIO)
import Control.Concurrent
import Data.List (intersperse)
import Data.Maybe
import Data.List.Split

import System.Process.Streaming
import qualified Pipes.Transduce.Text as PTLT
import qualified Data.Text.Lazy as LT
--import Pipes.Transduce.ByteString
import qualified Data.ByteString.Lazy as LB
import Data.String (fromString)

import Debug.Trace

{-

  A pandoc filter which convert a code blocks with `.plantuml` class to an
  image using the `plantuml` tool.

  The filter takes the following parameters:

   1. `pageRelFileNameFromRoot`: The relative file name of the page the code
      block is read from. This should usually be relative to the root of the
      site (e.g.: "./myDir1/myDir2/myPage.md").

   2. `outStaticImageDirRoot`: The directory under which all images will be
      stored (e.g.: "$MyOutputDir/img").

   3. `staticImageUriPrefix`: A prefix to add the the image link's uri (e.g.:
      "/img").


  A `name="MyName"` attribute can be specified which will result in the
  file being saved at specfied path inside specified `outStaticImageDirRoot`. 
  When path is relative, the image is saved under `outStaticImageDirRoot`
  in a directory which has the same name as the `pageRelFileNameFromRoot` (without
  its extension). When the path is absolute, the image is saved directly at a location
  relative to `outStaticImageDirRoot`.

  When a `staticImageUriPrefix` parameter is specified (non empty), it will
  be prefixed to the uri of the image link.


  TODO:
 
   -  Add support for cache directory argument where images are saved by sha. When
      the file already exists, copy it instead of calling plantuml. 

-}

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
transformBlock args (CodeBlock (id, classes, namevals) contents) | "plantuml" `elem` classes =
  do
    let
      handledClasses = ["plantuml"]
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

    retVal <- runPlantUML maybeAttrType localOutFilename contents
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


runPlantUML :: Maybe String -> FilePath -> String -> IO (Either String FilePath)
runPlantUML maybeOutType outFilename content = do
    createDirectoryIfMissing True . takeDirectory $  outFilename
    (o,stderr,ec) <- execute (piped (proc exeName args)) $ 
          (\_ o e ec -> (o,e,ec)) 
          <$>
          feedBytes (Just . fromString $ content) 
          <*> 
          foldOut intoLazyBytes
          <*>
          foldErr (transduce1 PTLT.utf8x PTLT.intoLazyText )
          <*>
          exitCode
    LB.writeFile outFilename o
    return $ if ec == ExitSuccess
      then
        Right outFilename
      else
        Left (errorMsg ec (LT.unpack stderr))
  where
    exeName = "plantuml"
    ftParams = case maybeOutType of
      Just ft -> ["-t" ++ ft]
      Nothing -> [] -- It is assumed here that default is a `png` file.
    args = ["-pipe"] ++ ftParams
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





