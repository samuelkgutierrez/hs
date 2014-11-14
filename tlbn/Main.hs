module Main (main) where

import qualified BaseUtils
import qualified Parser

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
    putStrLn ":: done with parse..."
    -- Show what we got.
    putStrLn "-- Term: --"
    print term
    putStrLn "-- Type: --"
    putStrLn "-- Normal Form: --"
    -- All done.
    return ()
