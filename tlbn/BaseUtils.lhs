%include polycode.fmt
\section{Base Utilities}

\noindent
Implements various utilities used in TLBN.

\begin{code}
module BaseUtils
    (
     usage,
     getInputFileName,
     fileExists,
     getFileContents
    ) where

import qualified System.Environment as SysEnv
import qualified System.IO as SysIO
import qualified System.Directory as SysDir

-- Returns usage string.
usage :: String
usage = "usage: TLBN /path/to/input/file"
\end{code}

\noindent
Parses command line arguments. Makes certain that the arguments are in the form
we expect. Retruns the input file name.
\begin{code}
getInputFileName :: IO String
getInputFileName = do
    args <- SysEnv.getArgs
    if length args /= 1 then
        error usage
    else
        return (head args)
\end{code}

\noindent
Returns whether or not a particular file exists. 
\begin{code}
fileExists :: FilePath -> IO Bool
fileExists = SysDir.doesFileExist
\end{code}

\noindent
Returns the contents of file. Assumes that file is valid.
\begin{code}
getFileContents :: FilePath -> IO String
getFileContents file = do
    fHandle <- SysIO.openFile file SysIO.ReadMode
    SysIO.hGetContents fHandle
\end{code}
