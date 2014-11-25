\documentclass{article}
%include polycode.fmt

\title{TLBN}
\date{\today}
\author{Samuel K. Gutierrez}
\begin{document}
\maketitle

\noindent
Haskell implementation of a type checker and an evaluator based on the small-step
evaluation relation for the simply-typed call-by-value lambda-calculus with
booleans and numbers.
\\

\noindent
Implements main driver program for TLBN.
\begin{code}
module Main (main) where

import qualified TLBNError
import qualified BaseUtils
import qualified Parser
import qualified Evaluator
import qualified Typing
import qualified TLBNShow
\end{code}

\noindent
Top-level call that invokes the parser, type checker, and evaluator. Passes
along the results to a convenience function that prints the results or error
string that is generated upon error.
\begin{code}
parseAndShowResults :: String -> TLBNError.ThrowsError String
parseAndShowResults progStr = do
    term <- Parser.parseTLBN progStr
    typ  <- Typing.termType term
    nf   <- Evaluator.evalTerm term
    TLBNShow.showResults [term] [typ] [nf]
\end{code}

\noindent
Convenience function that takes a string of source code and passes it along
to parseAndShowResults and has a convenient type for use in main.
\begin{code}
parseEvalAndPrint :: String -> IO ()
parseEvalAndPrint = do
    putStrLn . TLBNError.runThrows . parseAndShowResults
\end{code}

\noindent
Main
\begin{code}
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
\end{code}
\input TLBN
\input BaseUtils
\input Evaluator
\input Typing
\input Parser
\input TLBNShow
\input Context
\input TLBNError
\input testlog
\end{document}
