module Main (main) where

import qualified TLBN
import qualified BaseUtils
import qualified Parser
import qualified Evaluator

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
    putStrLn ":: starting parse..."
    term <- Parser.parseTLBN fileContents
    -- If we are here, then the text was successfully parsed.
    putStrLn ":: done with parse"
    -- Show what we got.
    putStrLn "-- Term: --"
    print term
    putStrLn "-- Type: --"
    print $ TLBN.typeof term
    putStrLn "-- Normal Form: --"
    -- If we are here, then everything has type checked, so print the normal
    -- form of the given term by evaluating it.
    print $ Evaluator.eval term
    -- All done.
    return ()
