module BaseUtils
    (
     usage,
     getInputFileName,
     fileExists,
     getFileContents
    ) where

-- Implements various utilities used in TLBN.

import qualified System.Environment as SysEnv
import qualified System.IO as SysIO
import qualified System.Directory as SysDir

-- Returns usage string.
usage :: String
usage = "usage: TLBN /path/to/input/file"

-- Parses command line arguments. Makes certain that the arguments are in the
-- form we expect. Retruns the input file name.
getInputFileName :: IO String
getInputFileName = do
    args <- SysEnv.getArgs
    if length args /= 1 then
        error usage
    else
        return (head args)

-- Returns whether or not a particular file exists. 
fileExists :: FilePath -> IO Bool
fileExists = SysDir.doesFileExist

-- Returns the contents of file. Assumes that file is valid.
getFileContents :: FilePath -> IO String
getFileContents file = do
    fHandle <- SysIO.openFile file SysIO.ReadMode
    SysIO.hGetContents fHandle
