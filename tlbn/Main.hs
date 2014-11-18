module Main (main) where

-- Implements main driver program.

import qualified TLBNError
import qualified BaseUtils
import qualified Parser
import qualified Evaluator
import qualified Typing
import qualified TLBNShow

-- Top-level call that invokes the parser, type checker, and evaluator. Passes
-- along the results to a convenience function that prints the results or error
-- string that is generated upon error.
parseAndShowResults :: String -> TLBNError.ThrowsError String
parseAndShowResults progStr = do
    term <- Parser.parseTLBN progStr
    typ  <- Typing.termType term
    nf   <- Evaluator.evalTerm term
    TLBNShow.showResults [term] [typ] [nf]

-- Convenience function that takes a string of source code and passes it along
-- to parseAndShowResults and has a convenient type for use in main.
parseEvalAndPrint :: String -> IO ()
parseEvalAndPrint = do
    putStrLn . TLBNError.runThrows . parseAndShowResults

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
    -- Do the work.
    parseEvalAndPrint fileContents
    -- All done.
    return ()
