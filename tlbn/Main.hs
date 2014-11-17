module Main (main) where

import qualified TLBNError
import qualified BaseUtils
import qualified Parser
import qualified Evaluator
import qualified Typing
import qualified TLBNShow

parseAndEval :: String -> TLBNError.ThrowsError String
parseAndEval progStr = do
    term <- Parser.parseTLBN progStr
    TLBNShow.showTerms term
--    typ  <- Typing.termType term
--    TLBNShow.showTypes typ

parseEvalAndPrint :: String -> IO ()
parseEvalAndPrint = putStrLn . TLBNError.runThrows . parseAndEval

-- main
main :: IO ()
main = do
    inputFileName <- BaseUtils.getInputFileName
    fileExists <- BaseUtils.fileExists inputFileName
    if not fileExists then
        error ("'" ++ inputFileName ++ "' does not exist. Cannot continue.")
    else
        putStrLn (":: processing: " ++ inputFileName)
    -- If we are here, then get the contents of the file
    fileContents <- BaseUtils.getFileContents inputFileName
    -- Sanity. Show the contents.
    putStrLn ":: begin input text"
    putStr fileContents
    putStrLn ":: end input text"
    -- Start the parse.
    parseEvalAndPrint fileContents
    -- Attempt to type the term. If successful, then the type will be returned.
    -- Otherwise, a typing error will be raised and no further work will be
    -- done.
    --putStrLn "-- Normal Form: --"
    -- All done.
    return ()
