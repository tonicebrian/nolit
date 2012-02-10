module Main where

import System.Environment
import System.Directory
import System.FilePath.Posix
import NoLit
      

createFile :: TangledFile -> IO ()
createFile (TangledFile filename content) = do
    createDirectoryIfMissing True $ takeDirectory filename
    writeFile filename content
    
main = do
    args <- getArgs
    nwFile <- readFile (args !! 0)
    mapM createFile $ tangle nwFile
