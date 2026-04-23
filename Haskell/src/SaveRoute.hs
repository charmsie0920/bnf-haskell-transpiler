{-# LANGUAGE OverloadedStrings #-}
module SaveRoute (saveRoute) where
import Web.Scotty (ScottyM, ActionM, post, json, formParam)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket_)
import Control.Concurrent.QSemN (QSemN, waitQSemN, signalQSemN)
import Data.Aeson ((.=), object)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Instances (Parser(..), ParseResult(..), parse)
import Types (ADT)
import Assignment ( bnfParser, getTime, firstParserName, generateHaskellCode)
import Validation ( validate, getValidADT)
import Data.List (isPrefixOf, isInfixOf, intercalate)
import GenerateHaskell (cap)

--saveRoute handles POST requests from frontend to save the grammar as a Haskell file.
saveRoute :: QSemN -> ScottyM ()
saveRoute sem = post "/api/save" $ do
  grammarTxt <- formParam "grammar" :: ActionM String
  --parse the grammar text using the BNF parser
  case parse bnfParser grammarTxt of
    -- if parse fails, send an error response in JSON
    r@(Error _) -> json $ object ["ok" .= (False :: Bool), "error" .= show r]
    Result _ bnf -> do
      let errors = validate bnf
      let cleanBnf = getValidADT bnf
      -- safely write the generated Haskell file to disk using the lock
      (outputFile, _) <- liftIO $ withLock sem $ do
        -- read the Haskell file template
        template <- readFile ("template" </> "BNFParser.template")
        currentTime <- getTime
        let (outputFile, finalContent) = processFile template currentTime cleanBnf
        --output directory
        createDirectoryIfMissing True (takeDirectory outputFile)
        writeFile outputFile finalContent
        --return file path
        pure (outputFile, ())

      json $ object
        [ "ok" .= (True :: Bool)
        , "errors" .= intercalate "\n" errors
        , "outputFile" .= outputFile
        ]

--builds the final Haskell code (header + generated parser code)
-- and returns both the output path and the completed content.
processFile :: String -> String -> ADT -> (FilePath, String)
processFile format currentTime bnf = 
    let haskellCode = format ++ "\n" ++ generateHaskellCode bnf
        modNameS = cap (firstParserName bnf)
        fixedHaskell = addModuleHeader modNameS haskellCode
        header = unlines [ "-- Saved at " ++ currentTime]
        final = unlines [header, fixedHaskell]
        outputFile = "output" </> (modNameS ++ ".hs")
    in (outputFile, final)




-- addModuleHeader adds or replaces the "module ..." line in generated code.
addModuleHeader :: String -> String -> String
addModuleHeader moduleName code =
  let fileLines = lines code
      -- helper: check if a line is empty or comment
      isCommentOrBlank l =
        let trimmed = strip l
        in null trimmed || isPrefixOf "--" trimmed
      -- split file into prefix comments and the rest
      (topComments, remainingLines) = span isCommentOrBlank fileLines
      -- build new module header line
      newHeader = "module " ++ moduleName ++ " where"
  in case remainingLines of
       -- if file already has "module ... where", replace it
       (line:rest)
         | let trimmed = strip line
         , isPrefixOf "module " trimmed && isInfixOf " where" trimmed
         -> unlines (topComments ++ (newHeader : rest))
       -- otherwise, just insert a new header after comments
       _ -> unlines (topComments ++ (newHeader : remainingLines))

--trimSpaces removes leading and trailing whitespace.
strip :: String -> String
strip = start . end
  where
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'
    start [] = []
    start (c:cs) 
      | isSpace c = start cs
      | otherwise = c:cs
    end = reverse . myTop . reverse
      where
        myTop [] = []
        myTop (c:cs) 
          | isSpace c = myTop cs
          | otherwise = c:cs


withLock :: QSemN -> IO a -> IO a
withLock sem = bracket_ (waitQSemN sem 1) (signalQSemN sem 1)
